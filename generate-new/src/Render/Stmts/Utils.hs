module Render.Stmts.Utils where

import qualified Data.Text.Extra               as T
import           Prettyprinter
import           Language.Haskell.TH            ( nameBase )
import           Polysemy
import           Relude                  hiding ( Const
                                                , Type
                                                , head
                                                , init
                                                , last
                                                )

import           Control.Exception              ( throwIO )
import           Control.Monad.Trans.Cont       ( ContT )
import           Foreign.Storable
import           GHC.IO.Exception

import           Error
import           Haskell
import           Marshal.Marshalable
import           Render.Element
import           Render.Names
import           Render.SpecInfo
import           Render.State
import           Render.Stmts

-- Store using 'poke' or 'pokeCStruct'
--
-- Structs are stored using 'poke' if they have a storable instance
storablePoke
  :: ( HasRenderElem r
     , HasRenderParams r
     , HasRenderedNames r
     , HasErr r
     , HasSpecInfo r
     , HasRenderState r
     )
  => Ref s AddrDoc
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
storablePoke addr value = do
  ty <- refType value
  let basicName = TyConName . T.pack . nameBase <$> case ty of
        ConT n :@ VarT _ -> Just n
        ConT n           -> Just n
        _                -> Nothing
  structOrUnionName <- case basicName of
    Nothing -> pure Nothing
    Just n  -> isStructOrUnion n <&> \case
      False -> Nothing
      True  -> Just n

  isNotStorableStructOrUnion <- case structOrUnionName of
    Nothing -> pure False
    Just n  -> not <$> isStorableStructOrUnion n

  if isNotStorableStructOrUnion
    then unitStmt $ do
      AddrDoc  a <- use addr
      ValueDoc v <- use value
      tellImportWithAll (TyConName "ToCStruct")
      tellImportWithAll ''ContT
      pure
        .   ContTAction
        .   ValueDoc
        $   "ContT $ pokeCStruct"
        <+> a
        <+> v
        <+> ". ($ ())"
    else unitStmt $ do
      tellImportWith ''Storable 'poke
      AddrDoc  a <- use addr
      ValueDoc v <- use value
      pure . IOAction . ValueDoc $ "poke" <+> a <+> v


-- | A doc which is an @IO a@ throwing an error as InvalidArgument unless some
-- condition is met
throwErrDoc
  :: (HasRenderElem r, HasRenderParams r)
  => Text
  -> Doc ()
  -> Sem r (Value (Doc ()))
throwErrDoc err cond = IOAction <$> throwErrDocStmtString err cond

throwErrDocStmtString
  :: (HasRenderElem r, HasRenderParams r) => Text -> Doc () -> Sem r (Doc ())
throwErrDocStmtString err cond = do
  tellImport 'throwIO
  tellImportWithAll ''IOException
  tellImportWithAll ''IOErrorType
  tellImport 'unless
  pure $ "unless" <+> cond <+> "$" <> line <> indent
    2
    (   "throwIO $ IOError Nothing InvalidArgument"
    <+> viaShow ("" :: Text) -- TODO: function name
    <+> viaShow err
    <+> "Nothing Nothing"
    )

stmtC
  :: forall k a (s :: k) (r :: [Effect])
   . (Typeable a, Coercible a (Doc ()))
  => Maybe Type
  -> CName
  -> Stmt s r (Value a)
  -> Stmt s r (Ref s a)
stmtC t n = stmt t (Just (unCName n))
