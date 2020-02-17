{-# language TemplateHaskellQuotes #-}
module Render.Constant
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Data.Text.Prettyprint.Doc
import           Language.Haskell.TH.Syntax
import           Polysemy
import           Polysemy.Reader

import           Spec.Parse
import           Spec.APIConstant
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.Type

renderConstant
  :: (HasErr r, Member (Reader RenderParams) r)
  => Constant
  -> Sem r RenderElement
renderConstant Constant {..} = contextShow conName $ do
  RenderParams {..} <- ask
  genRe ("constant " <> conName) $ do
    let n = mkPatternName conName
    tellExport (EPat n)
    let (t, v) = case conValue of
          StrValue i ->
            let a = mkName "a"
            in  ( ForallT [PlainTV a]
                          [ConT ''Eq :@ VarT a, ConT ''IsString :@ VarT a]
                          (VarT a)
                , viaShow i
                )
          IntegralValue i ->
            let a = mkName "a"
            in  ( ForallT [PlainTV a] [ConT ''Integral :@ VarT a] (VarT a)
                , viaShow i
                )
          FloatValue  i -> (ConT ''Float, viaShow i)
          Word32Value i -> (ConT ''Word32, viaShow i)
          Word64Value i -> (ConT ''Word64, viaShow i)

    tDoc <- renderType t
    tellDoc $ vsep
      [ "pattern" <+> pretty n <+> "::" <+> tDoc
      , "pattern" <+> pretty n <+> "=" <+> v
      ]

