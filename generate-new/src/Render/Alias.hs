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

renderAlias
  :: (HasErr r, Member (Reader RenderParams) r) => Alias -> Sem r RenderElement
renderAlias Alias {..} = context aName $ do
  RenderParams {..} <- ask
  genRe ("alias " <> aName) $ case aType of
    TypeAlias -> do
      let n = mkTyName aName
          t = mkTyName aTarget
      tellExport (EType n)
      tellImport (TyConName t)
      tellDoc $ "type" <+> pretty n <+> "=" <+> pretty t
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
