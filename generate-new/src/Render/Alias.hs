module Render.Alias
  where

import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Input
import           Relude

import           Error
import           Render.Element
import           Render.SpecInfo
import           Spec.Parse

renderAlias
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Alias
  -> Sem r RenderElement
renderAlias Alias {..} = context (unCName aName) $ do
  RenderParams {..} <- input
  genRe ("alias " <> unCName aName) $ case aType of
    TypeAlias -> do
      let t = mkTyName aTarget
          n = mkTyName aName
      tellImport t
      let syn :: forall r . HasRenderElem r => Sem r ()
          syn = do
            tellDocWithHaddock $ \getDoc ->
              vsep
                [ getDoc (TopLevel aName)
                , "type" <+> pretty n <+> "=" <+> pretty t
                ]
            tellExport (EType n)
      syn
      tellBoot $ do
        syn
        tellSourceImport t
    TermAlias -> do
      let n = mkFunName aName
          t = mkFunName aTarget
      tellExport (ETerm n)
      tellImport t
      tellDocWithHaddock $ \getDoc ->
        vsep [getDoc (TopLevel aName), pretty n <+> "=" <+> pretty t]
    PatternAlias -> do
      let n = mkPatternName aName
          t = mkPatternName aTarget
      tellExport (EPat n)
      tellImport t
      tellDocWithHaddock $ \getDoc ->
        vsep
          [getDoc (TopLevel aName), "pattern" <+> pretty n <+> "=" <+> pretty t]
