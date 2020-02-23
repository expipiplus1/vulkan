{-# language TemplateHaskellQuotes #-}
module Render.Command
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Extra                ( upperCaseFirst )
import           Language.Haskell.TH.Syntax
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector                   as V

import           Foreign.Ptr

import           Spec.Parse
import           Haskell                       as H
import           Marshal
import           Marshal.Scheme
import           Error
import           CType                         as C
import           Render.Element
import           Render.Type
import           Render.Scheme
import           Render.SpecInfo
import           Render.Poke

renderCommand
  :: (HasErr r, Member (Reader RenderParams) r, HasSpecInfo r)
  => MarshaledCommand
  -> Sem r RenderElement
renderCommand m@MarshaledCommand {..} = contextShow mcName $ do
  RenderParams {..} <- ask
  let Command {..} = mcCommand
  genRe ("command " <> mcName) $ do
    ffiTy <- cToHsType
      DoLower
      (Proto cReturnType
             [ (Nothing, pType) | Parameter {..} <- toList cParameters ]
      )
    dynamicBindType <- renderType $ ConT ''FunPtr :@ ffiTy ~> ffiTy
    let dynName = "mk" <> upperCaseFirst cName
    marshaledCommandCall dynName m
    tellDoc $ vsep
      [ mempty
      , "foreign import ccall"
      , "#if !defined(SAFE_FOREIGN_CALLS)"
      , indent 2 "unsafe"
      , "#endif"
      , indent 2 "\"dynamic\"" <+> pretty dynName
      , indent 2 ("::" <+> dynamicBindType)
      ]

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

----------------------------------------------------------------
-- Calling this command
----------------------------------------------------------------

marshaledCommandCall
  :: (HasErr r, HasSpecInfo r, HasRenderParams r, HasRenderElem r)
  => Text
  -> MarshaledCommand
  -> Sem r ()
marshaledCommandCall dynName m@MarshaledCommand {..} = do
  RenderParams {..} <- ask
  let n = mkFunName mcName
  tellExport (ETerm n)
  nts <- V.mapMaybe id <$> traverseV (paramType schemeType) mcParams
  r   <- makeReturnType m
  let t = foldr (~>) r nts
  tDoc  <- renderType t
  pokes <- forV mcParams
    $ \MarshaledParam {..} -> pure $ pretty (pName mpParam) <+> "<- undefined"
  let call = pretty dynName <+> parens "error \"Fun Ptr\"" <+> sep
        (pretty . pName . mpParam <$> toList mcParams)
  tellDoc
    . vsep
    $ [ pretty n <+> "::" <+> indent 0 tDoc
      , pretty n <+> "=" <+> doBlock (toList pokes <> [call])
      ]
