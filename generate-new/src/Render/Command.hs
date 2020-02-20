{-# language TemplateHaskellQuotes #-}
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
import           Marshal.Scheme
import           Error
import           CType as C
import           Render.Element
import           Render.Type
import           Render.Scheme

renderCommand
  :: (HasErr r, Member (Reader RenderParams) r)
  => MarshaledCommand
  -> Sem r RenderElement
renderCommand m@MarshaledCommand {..} = contextShow mcName $ do
  RenderParams {..} <- ask
  genRe ("command " <> mcName) $ do
    let n = mkFunName mcName
    tellExport (ETerm n)
    nts <- V.mapMaybe id <$> traverseV (paramType schemeType) mcParams
    r <- makeReturnType m
    let t = foldr (~>) r nts
    tString <- renderType t
    tellDoc
      $  (pretty n <+> "::" <+> indent 0 tString)
      <> hardline
      <> (pretty n <+> "=" <+> "undefined")

paramType
  :: (HasErr r, Member (Reader RenderParams) r)
  => (MarshalScheme Parameter -> Sem r (Maybe H.Type))
  -> MarshaledParam
  -> Sem r (Maybe H.Type)
paramType st MarshaledParam {..} = contextShow (pName mpParam) $ do
  RenderParams {..} <- ask
  let Parameter {..} = mpParam
  n <- st mpScheme
  pure $ namedTy (mkParamName pName) <$> n

makeReturnType
  :: (HasErr r, HasRenderParams r) => MarshaledCommand -> Sem r H.Type
makeReturnType MarshaledCommand {..} = do
  pts <- V.mapMaybe id <$> traverseV (paramType schemeTypePositive) mcParams
  r   <- case mcReturn of
    C.Void -> pure V.empty
    r      -> V.singleton <$> cToHsType DoNotPreserve r
  let ts = pts <> r
  pure $ ConT ''IO :@ foldl' (:@) (TupleT (length ts)) ts

