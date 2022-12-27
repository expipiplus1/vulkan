{-# language QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Khronos.Versions.OpenXR
  ( specVersions
  ) where

import           Data.Vector                    ( Vector )
import           Data.Version
import           Foreign.Storable               ( Storable )
import           Polysemy
import           Polysemy.Input
import           Relude
import           Text.InterpolatedString.Perl6.Unindented

import           Data.Bits

import           Error
import           Haskell.Name
import           Render.Element
import           Spec.Parse

specVersions
  :: forall r
   . (HasErr r, HasRenderParams r)
  => Spec SpecXr
  -> Vector (Sem r RenderElement)
specVersions Spec {..} = fromList
  ( currentVersion specHeaderVersion
  : versionTypeElem
  : versionConstruction
  : (featureVersion <$> toList specFeatures)
  )

currentVersion
  :: (HasRenderParams r, HasErr r)
  => SpecHeaderVersion SpecXr
  -> Sem r RenderElement
currentVersion (XrVersion ma mi pa) = genRe "current version" $ do
  RenderParams {..} <- input
  tellExplicitModule =<< mkModuleName ["Version"]
  let pat         = mkPatternName "XR_CURRENT_API_VERSION"
      makeVersion = mkPatternName "XR_MAKE_VERSION"
      ver         = mkTyName "XrVersion"
  tellImport makeVersion
  tellImport ver
  tellExport (EPat pat)
  tellDoc [qqi|
    pattern {pat} :: {ver}
    pattern {pat} = {makeVersion} {ma} {mi} {pa}
  |]


featureVersion
  :: (HasErr r, HasRenderParams r) => Feature -> Sem r RenderElement
featureVersion Feature {..} = genRe "feature version" $ do
  RenderParams {..} <- input
  let major : minor : _ = versionBranch fVersion
      pat               = mkPatternName
        (CName $ "XR_API_VERSION_" <> show major <> "_" <> show minor)
      make = mkPatternName "XR_MAKE_VERSION"
      ver  = mkTyName "XrVersion"
  tellExport (EPat pat)
  tellImport ''Word32
  tellImport make
  tellImport ver
  tellExplicitModule =<< mkModuleName ["Core" <> show major <> show minor]
  tellDoc [qqi|
    pattern {pat} :: {ver}
    pattern {pat} = {make} {major} {minor} 0
  |]

versionTypeElem :: (HasErr r, HasRenderParams r) => Sem r RenderElement
versionTypeElem = genRe "version type" $ do
  RenderParams {..} <- input
  tellExplicitModule =<< mkModuleName ["Version"]
  tellImport ''Word64
  tellImport ''Storable
  tellImport ''Generic
  tellImport ''Typeable
  tellImport (TyConName "Zero")
  let t = mkTyName "XrVersion"
      c = mkConName "XrVersion" "XrVersion"
  tellDataExport t
  tellDocWithHaddock $ \getDoc -> [qqi|
  {getDoc (TopLevel "XrVersion")}
  newtype {t} = {c} \{ unVersion :: Word64 }
    deriving stock (Typeable, Eq, Ord, Show, Read)
    deriving newtype (Storable, Zero)
  #if defined(GENERIC_INSTANCES)
  deriving instance Generic {t}
  #endif
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
  tellExplicitModule =<< mkModuleName ["Version"]
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

    \{-# complete {p} #-}

    {patMajor} :: Version -> Word16
    {patMajor} (Version v) = fromIntegral $ (v `shiftR` 48) .&. 0xffff

    {patMinor} :: Version -> Word16
    {patMinor} (Version v) = fromIntegral $ (v `shiftR` 32) .&. 0xffff

    {patPatch} :: Version -> Word32
    {patPatch} (Version v) = fromIntegral $ v .&. 0xffffffff
  |]
