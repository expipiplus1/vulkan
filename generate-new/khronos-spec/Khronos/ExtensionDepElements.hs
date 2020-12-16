{-# LANGUAGE QuasiQuotes #-}

module Khronos.ExtensionDepElements
  ( renderExtensionDepElements
  ) where

import           Data.Text.Prettyprint.Doc
import           Data.Traversable
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Error
import           Haskell                        ( HName(..)
                                                , renderType
                                                )
import           Khronos.Utils
import           Polysemy.Input
import           Relude
import           Render.Element
import           Render.SpecInfo
import           Render.Type                    ( cToHsType )
import           Render.Type.Preserve           ( Preserve(DoNotPreserve) )
import           Spec.Types

renderExtensionDepElements
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Vector Extension
  -> Sem r RenderElement
renderExtensionDepElements exts = genRe "Extension dependencies" $ do
  tellExplicitModule =<< mkModuleName ["Extensions", "Dependencies"]
  tellCanFormat
  renderDeps exts
  renderCoreRequirements exts

renderDeps
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => Vector Extension
  -> Sem r ()
renderDeps exts = do
  SpecInfo {..} <- input
  let defaultCase = "_ -> []"
  cases <-
    for [ e | e@Extension {..} <- V.toList exts, not (V.null exDependencies) ]
      $ \Extension {..} -> do
          namePattern <- fst <$> extensionPatterns exName
          let deps = siExtensionDeps exName
          depPatterns <- traverse (fmap fst . extensionPatterns) deps
          traverse_ tellImport (namePattern : depPatterns)
          pure $ pretty namePattern <+> "->" <+> list (pretty <$> depPatterns)
  tellImport (TyConName ":::")
  tellImport ''ByteString
  tellExport (ETerm (TermName "extensionDependencies"))
  tellDoc $ vsep
    [ "-- | The set of other extensions required to use this extension"
    , "extensionDependencies :: (\"extensionName\" ::: ByteString) -> [ByteString]"
    , "extensionDependencies = \\case" <> line <> indent
      2
      (vsep (cases <> [defaultCase]))
    ]

renderCoreRequirements
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => Vector Extension
  -> Sem r ()
renderCoreRequirements exts = do
  RenderParams {..} <- input
  tellImport (ConName "API_VERSION_1_0")
  let defaultCase = "_ -> API_VERSION_1_0"
  ver   <- renderType =<< cToHsType DoNotPreserve versionType
  cases <-
    for
        [ (exName, v)
        | Extension {..} <- V.toList exts
        , Just v         <- pure exRequiresCore
        ]
      $ \(name, v) -> do
          namePattern <- fst <$> extensionPatterns name
          tellImport namePattern
          vPat <- versionDoc v
          pure $ pretty namePattern <+> "->" <+> vPat
  tellImport (TyConName ":::")
  tellImport ''Word32
  tellExport (ETerm (TermName "extensionCoreRequirement"))
  tellDoc $ vsep
    [ "-- | The minimum required API version to use this extension"
    , "extensionCoreRequirement :: (\"extensionName\" ::: ByteString) ->"
      <+> ver
    , "extensionCoreRequirement = \\case" <> line <> indent
      2
      (vsep (cases <> [defaultCase]))
    ]
