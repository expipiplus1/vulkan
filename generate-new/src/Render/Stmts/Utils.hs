module Render.Stmts.Utils
  where

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
import           Language.Haskell.TH            ( nameBase )
import qualified Data.Text.Extra               as T

import           Foreign.Storable
import           Control.Monad.Trans.Cont       ( ContT )
import           GHC.IO.Exception
import           Control.Exception              ( throwIO )

import           Error
import           Haskell
import           Marshal.Marshalable
import           Render.Element
import           Render.SpecInfo
import           Render.Stmts
import           Render.Names

-- Store using 'poke' or 'pokeCStruct'
storablePoke
  :: ( HasRenderElem r
     , HasRenderParams r
     , HasRenderedNames r
     , HasErr r
     , HasSpecInfo r
     )
  => Ref s AddrDoc
  -> Ref s ValueDoc
  -> Stmt s r (Ref s ValueDoc)
storablePoke addr value = do
  ty              <- refType value
  isStructOrUnion <- case ty of
    ConT n -> isStructOrUnion (TyConName . T.pack . nameBase $ n)
    _      -> pure False
  if isStructOrUnion
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
throwErrDoc err cond = do
  tellImport 'throwIO
  tellImportWithAll ''IOException
  tellImportWithAll ''IOErrorType
  tellImport 'unless
  pure . IOAction $ "unless" <+> cond <+> "$" <> line <> indent
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
