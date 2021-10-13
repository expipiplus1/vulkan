module Khronos.Utils where

import           Data.Text
import qualified Data.Text                     as T
import           Data.Version
import           Error
import           Haskell.Name
import           Polysemy.Input
import           Prettyprinter
import           Relude
import           Render.Element
import           Spec.Name                      ( CName(CName) )

extensionPatterns :: HasRenderParams r => Text -> Sem r (HName, HName)
  -- ^ EXTENSION_NAME, SPEC_VERSION
extensionPatterns p = do
  RenderParams {..} <- input
  let patternPrefix = if
        | p == "VK_EXT_swapchain_colorspace"
        -> "EXT_SWAPCHAIN_COLOR_SPACE"
        | Just v <- readMaybe @Int [T.last p]
        , not ("16" `T.isSuffixOf` p)
        , not ("32" `T.isSuffixOf` p)
        , not ("64" `T.isSuffixOf` p)
        , not ("int8" `T.isSuffixOf` p)
        , not ('_' == p `T.index` (T.length p - 2))
        -> T.toUpper (T.init p) <> "_" <> T.pack (show v)
        | otherwise
        -> T.toUpper p
      nameName    = CName $ patternPrefix <> "_EXTENSION_NAME"
      versionName = CName $ patternPrefix <> "_SPEC_VERSION"
  pure (mkPatternName nameName, mkPatternName versionName)

versionDoc
  :: (HasRenderParams r, HasRenderElem r, HasErr r) => Version -> Sem r (Doc ())
versionDoc v = do
  tellImport (ConName "MAKE_API_VERSION")
  (ma, mi, pa) <- case versionBranch v of
    [ma]         -> pure (ma, 0, 0)
    [ma, mi]     -> pure (ma, mi, 0)
    [ma, mi, pa] -> pure (ma, mi, pa)
    []           -> throw "Version branch has no components"
    _            -> throw "Version branch has more than three components"
  pure $ "MAKE_API_VERSION" <+> hsep (viaShow <$> [ma, mi, pa])
