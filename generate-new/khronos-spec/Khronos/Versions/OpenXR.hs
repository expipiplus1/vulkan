{-# language QuasiQuotes #-}
module Khronos.Versions.OpenXR
  ( specVersions
  ) where

import           Data.Vector                    ( Vector )
import           Data.Version
import           Polysemy
import           Polysemy.Input
import           Relude
import           Text.InterpolatedString.Perl6.Unindented

import           Data.Bits

import           Error
import           Haskell.Name
import           Render.Element
import           Spec.Parse
import           VkModulePrefix

specVersions
  :: forall r
   . (HasErr r, HasRenderParams r)
  => Spec SpecXr
  -> Vector (Sem r RenderElement)
specVersions Spec {..} = fromList
  (versionType : versionConstruction : (featureVersion <$> toList specFeatures))

headerVersion
  :: (HasErr r, HasRenderParams r)
  => SpecHeaderVersion SpecVk
  -> Sem r RenderElement
headerVersion (VkVersion version) = genRe "header version" $ do
  RenderParams {..} <- input
  tellExplicitModule (vulkanModule ["Version"])
  let pat = mkPatternName "VK_HEADER_VERSION"
  tellExport (EPat pat)
  tellImport ''Word32
  tellDoc [qqi|
    pattern {pat} :: Word32
    pattern {pat} = {version}
  |]

headerVersionComplete
  :: (HasErr r, HasRenderParams r)
  => Version
  -> SpecHeaderVersion SpecVk
  -> Sem r RenderElement
headerVersionComplete lastFeatureVersion (VkVersion headerVersion) =
  genRe "header version complete" $ do
    RenderParams {..} <- input
    tellExplicitModule (vulkanModule ["Version"])
    let pat               = mkPatternName "VK_HEADER_VERSION_COMPLETE"
        major : minor : _ = versionBranch lastFeatureVersion
        makeVersion       = mkPatternName "VK_MAKE_VERSION"
    tellExport (EPat pat)
    tellImport ''Word32
    tellDoc [qqi|
    pattern {pat} :: Word32
    pattern {pat} = {makeVersion} {major} {minor} {headerVersion}
  |]

featureVersion
  :: (HasErr r, HasRenderParams r) => Feature -> Sem r RenderElement
featureVersion Feature {..} = genRe "feature version" $ do
  RenderParams {..} <- input
  let major : minor : _ = versionBranch fVersion
      pat               = mkPatternName
        (CName $ "XR_API_VERSION_" <> show major <> "_" <> show minor)
      make = mkPatternName "XR_MAKE_VERSION"
  tellExport (EPat pat)
  tellImport ''Word32
  tellImport make
  tellExplicitModule (vulkanModule ["Core" <> show major <> show minor])
  tellDoc [qqi|
    pattern {pat} :: Word32
    pattern {pat} = {make} {major} {minor} 0
  |]

versionType :: (HasErr r, HasRenderParams r) => Sem r RenderElement
versionType = genRe "version type" $ do
  RenderParams {..} <- input
  tellExplicitModule (vulkanModule ["Version"])
  tellImport ''Word64
  let t = mkTyName "XrVersion"
      c = mkConName "XrVersion" "XrVersion"
  tellExport (EType t)
  tellDocWithHaddock $ \getDoc -> [qqi|
  {getDoc (TopLevel "XrVersion")}
  newtype {t} = {c} \{ unVersion :: Word64 }
  |]

-- // OpenXR current version number.
-- #define XR_CURRENT_API_VERSION XR_MAKE_VERSION(1, 0, 12)

-- #define XR_MAKE_VERSION(major, minor, patch) \
--     ((((major) & 0xffffULL) << 48) | (((minor) & 0xffffULL) << 32) | ((patch) & 0xffffffffULL))
-- #define XR_VERSION_MAJOR(version) (uint16_t)(((uint64_t)(version) >> 48)& 0xffffULL)
-- #define XR_VERSION_MINOR(version) (uint16_t)(((uint64_t)(version) >> 32) & 0xffffULL)
-- #define XR_VERSION_PATCH(version) (uint32_t)((uint64_t)(version) & 0xffffffffULL)
versionConstruction :: (HasErr r, HasRenderParams r) => Sem r RenderElement
versionConstruction = genRe "version construction" $ do
  RenderParams {..} <- input
  tellExplicitModule (vulkanModule ["Version"])
  tellImport ''Word16
  tellImport ''Word32
  tellImport ''Word64
  tellImport '(.&.)
  tellImport '(.|.)
  tellImport 'shiftL
  tellImport 'shiftR
  let p = mkPatternName "XR_MAKE_VERSION"
  tellExport (EPat p)
  let patMajor = TermName ("_" <> unName (mkPatternName "XR_VERSION_MAJOR"))
      patMinor = TermName ("_" <> unName (mkPatternName "XR_VERSION_MINOR"))
      patPatch = TermName ("_" <> unName (mkPatternName "XR_VERSION_PATCH"))
  tellExport (ETerm patMajor)
  tellExport (ETerm patMinor)
  tellExport (ETerm patPatch)
  tellDoc [qqi|
    pattern {p} :: Word16 -> Word16 -> Word32 -> Version
    pattern {p} major minor patch <-
      (\\v -> ({patMajor} v, {patMinor} v, {patPatch} v) -> (major, minor, patch))
      where {p} major minor patch = Version
                                  $ fromIntegral major `shiftL` 48
                                .|. fromIntegral minor `shiftL` 32
                                .|. fromIntegral patch

    {patMajor} :: Version -> Word16
    {patMajor} v = fromIntegral $ (v `shiftR` 48) .&. 0xffff

    {patMinor} :: Version -> Word16
    {patMinor} v = fromIntegral $ (v `shiftR` 32) .&. 0xffff

    {patPatch} :: Version -> Word32
    {patPatch} v = fromIntegral $ v .&. 0xffffffff
  |]
