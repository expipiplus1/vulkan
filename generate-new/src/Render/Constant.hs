{-# language TemplateHaskellQuotes #-}
module Render.Constant
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Data.Text.Prettyprint.Doc
import           Language.Haskell.TH.Syntax
import           Text.Printf
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
renderConstant Constant {..} = contextShow constName $ do
  RenderParams {..} <- ask
  fmap identicalBoot . genRe ("constant " <> constName) $ do
    let
      n               = mkPatternName constName
      tn              = mkTyName constName
      (t, v, hasType) = case constValue of
        StrValue i ->
          let a = typeName "a"
          in  ( ForallT [PlainTV a]
                        [ConT ''Eq :@ VarT a, ConT ''IsString :@ VarT a]
                        (VarT a)
              , viaShow i
              , True
              )
        IntegralValue i ->
          let a = typeName "a"
          in  ( ForallT [PlainTV a] [ConT ''Integral :@ VarT a] (VarT a)
              , viaShow i
              , True
              )
        FloatValue i -> (ConT ''Float, viaShow i, False)
        Word32Value i ->
          (ConT ''Word32, pretty @String (printf "0x%x" i), True)
        Word64Value i ->
          (ConT ''Word64, pretty @String (printf "0x%x" i), True)

    when hasType $ do
      -- let syn :: HasRenderElem r => Sem r ()
      --     syn = do
      tellExport (EType tn)
      tellDoc $ "type" <+> pretty tn <+> "=" <+> v
      -- syn
      -- tellBoot syn

    tellExport (EPat n)
    tDoc <- renderType t
    tellDoc $ vsep
      [ "pattern" <+> pretty n <+> "::" <+> tDoc
      , "pattern" <+> pretty n <+> "=" <+> v
      ]

