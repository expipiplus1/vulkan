module Render.Command
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Data.Text.Prettyprint.Doc
import           Language.Haskell.TH.Syntax
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector                   as V

import           Spec.Parse
import           Haskell                       as H
import           Marshal
import           Error
import           Render.Element
import           Render.Type
import           Render.Scheme

renderCommand
  :: (HasErr r, Member (Reader RenderParams) r)
  => MarshaledCommand
  -> Sem r RenderElement
renderCommand MarshaledCommand {..} = do
  RenderParams {..} <- ask
  genRe ("command " <> mcName) $ do
    let n = mkFunName mcName
    tellExport (ETerm n)
    ts <- V.mapMaybe id <$> traverseV paramType mcParams
    r  <- cToHsType DoPreserve mcReturn
    let t = foldr (~>) r ts
    tellDoc
      $   pretty n <+> "::" <+> indent 0 (renderType t)
      <>  hardline
      <>  pretty n <+> "=" <+> "undefined"

paramType
  :: (HasErr r, Member (Reader RenderParams) r)
  => MarshaledParam
  -> Sem r (Maybe H.Type)
paramType MarshaledParam {..} = do
  RenderParams {..} <- ask
  let Parameter {..} = mpParam
  fmap (namedTy (mkParamName pName)) <$> schemeType mpScheme

namedTy :: Text -> H.Type -> H.Type
namedTy name = InfixT (LitT (StrTyLit (toString name))) (mkName ":::")
