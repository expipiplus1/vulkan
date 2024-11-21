{-# language TemplateHaskellQuotes #-}
module Render.Constant where

import           Prettyprinter
import           Language.Haskell.TH.Syntax
import           Polysemy
import           Polysemy.Input
import           Relude
import           Text.Printf

import           CType                          ( CType(TypeName) )
import           Error
import           Haskell                       as H
import           Render.Element
import           Render.SpecInfo
import           Render.Utils                   ( plainTVcompat )
import           Spec.APIConstant
import           Spec.Parse

renderConstant
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Constant
  -> Sem r RenderElement
renderConstant Constant {..} = contextShow constName $ do
  RenderParams {..} <- input
  genRe ("constant " <> unCName constName) $ do
    let n  = mkPatternName constName
        tn = mkTyName constName
    (t, v, hasType) <- case constValue of
      StrValue i ->
        let a = mkName "a"
        in  pure
              ( ForallT [plainTVcompat a]
                        [ConT ''Eq :@ VarT a, ConT ''IsString :@ VarT a]
                        (VarT a)
              , viaShow i
              , True
              )
      IntegralValue i ->
        let a = mkName "a"
        in  pure
              ( ForallT [plainTVcompat a] [ConT ''Integral :@ VarT a] (VarT a)
              , viaShow i
              , i >= 0
              )
      FloatValue i -> pure (ConT ''Float, viaShow i, False)
      Word32Value i ->
        pure (ConT ''Word32, pretty @String (printf "0x%x" i), True)
      Word64Value i ->
        pure (ConT ''Word64, pretty @String (printf "0x%x" i), True)
      Int64Value i ->
        let s = if i >= 0 then printf "0x%x" i else printf "%d" i
        in  pure (ConT ''Int64, pretty @String s, True)
      SizeOfValue c -> do
        (size, _) <- getTypeSize (TypeName c)
        pure
          ( ConT ''Int
          , viaShow size <+> "{- sizeof" <> parens (pretty (unCName c)) <+> "-}"
          , True
          )

    when hasType $ do
      let syn :: HasRenderElem r => Sem r ()
          syn = do
            tellExport (EType tn)
            tellDoc $ "type" <+> pretty tn <+> "=" <+> v
      syn
      tellBoot syn

    tellExport (EPat n)
    tDoc <- renderType t
    tellDocWithHaddock $ \getDoc -> vsep
      [ getDoc (TopLevel constName)
      , "pattern" <+> pretty n <+> "::" <+> tDoc
      , "pattern" <+> pretty n <+> "=" <+> v
      ]
