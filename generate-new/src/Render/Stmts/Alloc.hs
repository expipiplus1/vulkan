{-# language AllowAmbiguousTypes #-}
module Render.Stmts.Alloc
  ( allocate
  , allocateAndPeek
  ) where

import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Relude                  hiding ( Const
                                                , Type
                                                , head
                                                , init
                                                , last
                                                )

import           Control.Exception              ( bracket )
import           Control.Monad.Trans.Cont       ( ContT )
import           Foreign.Marshal.Alloc

import           CType                         as C
import           Error
import           Haskell
import           Marshal.Marshalable
import           Marshal.Scheme
import           Render.Element
import           Render.Peek
import           Render.SpecInfo
import           Render.Stmts
import           Render.Stmts.Poke
import           Render.Type

-- | Allocates memory in the first ref, and peeks from it in the second
allocateAndPeek
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
allocateAndPeek a scheme = do
  alloc <- allocate a scheme
  peek  <-
    note "Unable to get peek for returned value"
      =<< peekStmtDirect a alloc scheme
  pure (alloc, peek)

-- | Allocates memory for some type
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
  -> Stmt s r (Ref s AddrDoc)
allocate a = \case
  Normal   fromTy     -> normal (name a) (type' a) fromTy
  Preserve fromTy     -> normal (name a) (type' a) fromTy
  Vector _ (Normal _) -> allocateVector a
  ByteString          -> allocateByteString a
  s                   -> throw $ "Unhandled allocation for type " <> show s

normal
  :: (HasErr r, HasRenderElem r, HasRenderParams r, HasSpecInfo r)
  => CName
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

  stmt Nothing (Just . ("p" <>) . T.upperCaseFirst $ unCName name)
    $   ContTAction
    .   AddrDoc
    <$> do
          tellImportWithAll ''ContT
          if isStruct
            then do
              tyDoc <-
                renderTypeHighPrec =<< cToHsTypeWithHoles DoPreserve toElem
              tellImportWithAll (TyConName "ToCStruct")
              pure ("ContT" <+> parens ("withZeroCStruct @" <> tyDoc))
            else do
              tellImport 'free
              tellImport 'bracket
              tellImport 'callocBytes
              tyDoc <- renderTypeHighPrec =<< cToHsType DoPreserve toElem
              (size, _align) <- getTypeSize toElem
              pure
                (   "ContT $ bracket"
                <+> parens ("callocBytes @" <> tyDoc <+> viaShow size)
                <+> "free"
                )

allocateVector
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
allocateVector vec = do
  let name' = name vec
      toTy  = type' vec
  toElem <- unPtr toTy
  lenRef <- getLenRef @a (lengths vec)
  allocArray Zeroed name' toElem (Right lenRef)

-- Currently the same implementation as allocateVector
allocateByteString
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
allocateByteString = allocateVector

unPtr :: HasErr r => CType -> Sem r CType
unPtr = \case
  Ptr NonConst t -> pure t
  t -> throw $ "Trying to allocate for a non non-const ptr type " <> show t


