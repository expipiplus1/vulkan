module Render.Alias
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.SpecInfo

renderAlias
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Alias
  -> Sem r RenderElement
renderAlias Alias {..} = context aName $ do
  RenderParams {..} <- ask
  genRe ("alias " <> aName) $ case aType of
    TypeAlias -> do
      isHandle <- isJust <$> getHandle aTarget
      let mkName = if isHandle then mkHandleName else mkTyName
          t = mkName aTarget
          n = mkName aName
      tellImport (TyConName t)
      let syn :: forall r . HasRenderElem r => Sem r ()
          syn = do
            tellDoc $ "type" <+> pretty n <+> "=" <+> pretty t
            tellExport (EType n)
      syn
      tellBoot $ do
        syn
        tellSourceImport (TyConName t)
    TermAlias -> do
      let n = mkFunName aName
          t = mkFunName aTarget
      tellExport (ETerm n)
      tellImport (TermName t)
      tellDoc $ pretty n <+> "=" <+> pretty t
    PatternAlias -> do
      let n = mkPatternName aName
          t = mkPatternName aTarget
      tellExport (EPat n)
      tellImport (ConName t)
      tellDoc $ "pattern" <+> pretty n <+> "=" <+> pretty t
