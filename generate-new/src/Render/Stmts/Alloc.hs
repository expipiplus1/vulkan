{-# language AllowAmbiguousTypes #-}
module Render.Stmts.Alloc
  ( allocate
  ) where

import           Relude                  hiding ( Type
                                                , ask
                                                , asks
                                                , last
                                                , init
                                                , head
                                                , Const
                                                , Reader
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import           Data.Vector.Extra              ( pattern Empty
                                                , pattern (:<|)
                                                )
import qualified Data.Text.Extra               as T

import           Foreign.Marshal.Alloc
import           Control.Monad.Trans.Cont       ( ContT )
import           Control.Exception              ( bracket )

import           CType                         as C
import           Error
import           Haskell
import           Marshal.Marshalable
import           Marshal.Scheme
import           Render.Element
import           Render.SpecInfo
import           Render.Type
import           Render.Stmts
import           Render.Stmts.Poke
import           Render.Scheme
import           Render.Peek

-- | Allocated memory in the first ref, and peeks from it in the second
allocate
  :: ( HasErr r
     , Marshalable a
     , Show a
     , HasRenderElem r
     , HasSpecInfo r
     , HasRenderParams r
     , HasSiblingInfo a r
     , HasStmts r
     )
  => a
  -> MarshalScheme a
  -> Stmt s r (Ref s AddrDoc, Ref s ValueDoc)
allocate a scheme = do
  alloc <- case scheme of
    Normal fromTy -> normal (name a) (type' a) fromTy
    Vector (Normal _)      -> vector  a
    s             -> throw $ "Unhandled allocation for type " <> show s
  peek <-
    note "Unable to get peek for returned value"
      =<< peekStmtDirect a alloc scheme
  pure (alloc, peek)

normal
  :: (HasErr r, HasRenderElem r, HasRenderParams r, HasSpecInfo r)
  => Text
  -> CType
  -> CType
  -> Stmt s r (Ref s AddrDoc)
normal name toTy fromTy = do
  toElem <- unPtr toTy
  unless (toElem == fromTy)
    $  throw
    $  "Unable to allocate for differing types "
    <> show toElem
    <> " and "
    <> show fromTy

  isStruct <- case toElem of
    TypeName n -> isJust <$> getStruct n
    _          -> pure False

  stmt Nothing (Just . ("p" <>) . T.upperCaseFirst $ name)
    $   ContTAction
    .   AddrDoc
    <$> do
          tellImportWithAll ''ContT
          if isStruct
            then do
              tyDoc <- renderTypeHighPrec =<< cToHsType DoPreserve toElem
              tellImportWithAll (TyConName "ToCStruct")
              pure ("ContT" <+> parens ("withZeroCStruct @" <> tyDoc))
            else do
              tellImport 'free
              tellImport 'bracket
              tellImport 'calloc
              tyDoc <- renderTypeHighPrec =<< cToHsType DoPreserve toElem
              pure
                ("ContT $ bracket" <+> parens ("calloc @" <> tyDoc) <+> "free")

vector
  :: forall a r s
   . ( HasErr r
     , HasRenderElem r
     , HasRenderParams r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , Marshalable a
     , Show a
     )
  => a
  -> Stmt s r (Ref s AddrDoc)
vector vec = do
  RenderParams {..} <- ask
  let name' = name vec
      toTy  = type' vec
  toElem <- unPtr toTy
  lenRef <- stmt Nothing Nothing $ case lengths vec of
    Empty -> throw "Trying to allocate vector with no length"
    NamedLength len :<| Empty -> do
      ValueDoc value <- useViaName len
      pure . Pure AlwaysInline . ValueDoc $ value
    NamedMemberLength struct member :<| Empty -> do
      ValueDoc structValue <- useViaName struct
      case complexMemberLengthFunction struct member structValue of
        Just complex -> Pure InlineOnce . ValueDoc <$> complex
        Nothing      -> do
          SiblingInfo {..} <- getSiblingInfo @a struct
          structName       <-
            let nonStruct =
                  throw
                    $  "Trying to get length member from a non-struct type "
                    <> show siScheme
            in  case siScheme of
                  Normal (TypeName n) -> getStruct n >>= \case
                    Nothing -> nonStruct
                    Just _  -> pure n
                  _ -> nonStruct
          structTyDoc <-
            renderType
            =<< note
                  "Unable to get type for struct with length specifying member for vector allocation"
            =<< schemeType siScheme
          tellImportWithAll (TyConName structName)
          pure
            .   Pure AlwaysInline
            .   ValueDoc
            $   "fromIntegral $"
            <+> pretty (mkMemberName member)
            <+> parens (structValue <+> "::" <+> structTyDoc)
    NullTerminated :<| Empty -> throw "Trying to allocate a null terminated"
    _ -> throw "Trying to allocate vector with multiple lengths"

  allocArray name' toElem (Right lenRef)


unPtr :: HasErr r => CType -> Sem r CType
unPtr = \case
  Ptr NonConst t -> pure t
  t -> throw $ "Trying to allocate for a non non-const ptr type " <> show t


