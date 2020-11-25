{-# language QuasiQuotes #-}
module Render.Spec.Versions
  where

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
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
  => Spec
  -> Vector (Sem r RenderElement)
specVersions Spec {..} = fromList
  ( headerVersion specHeaderVersion
  : headerVersionComplete (fVersion (V.last specFeatures))
                          specHeaderVersion
  : versionConstruction
  : (featureVersion <$> toList specFeatures)
  )

headerVersion :: (HasErr r, HasRenderParams r) => Word -> Sem r RenderElement
headerVersion version = genRe "header version" $ do
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
  :: (HasErr r, HasRenderParams r) => Version -> Word -> Sem r RenderElement
headerVersionComplete lastFeatureVersion headerVersion =
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
        (CName $ "VK_API_VERSION_" <> show major <> "_" <> show minor)
      make = mkPatternName "VK_MAKE_VERSION"
  tellExport (EPat pat)
  tellImport ''Word32
  tellImport make
  tellExplicitModule (vulkanModule ["Core" <> show major <> show minor])
  tellDoc [qqi|
pattern {pat} :: Word32
pattern {pat} = {make} {major} {minor} 0
|]

versionConstruction :: (HasErr r, HasRenderParams r) => Sem r RenderElement
versionConstruction = genRe "version construction" $ do
  RenderParams {..} <- input
  tellExplicitModule (vulkanModule ["Version"])
  tellImport ''Word32
  tellImport '(.&.)
  tellImport '(.|.)
  tellImport 'shiftL
  tellImport 'shiftR
  tellExport (EPat (mkPatternName "VK_MAKE_VERSION"))
  let patMajor = TermName ("_" <> unName (mkPatternName "VK_VERSION_MAJOR"))
      patMinor = TermName ("_" <> unName (mkPatternName "VK_VERSION_MINOR"))
      patPatch = TermName ("_" <> unName (mkPatternName "VK_VERSION_PATCH"))
  tellExport (ETerm patMajor)
  tellExport (ETerm patMinor)
  tellExport (ETerm patPatch)
  tellDoc [qqi|
pattern {mkPatternName "VK_MAKE_VERSION"} :: Word32 -> Word32 -> Word32 -> Word32
pattern {mkPatternName "VK_MAKE_VERSION"} major minor patch <-
  (\\v -> ({patMajor} v, {patMinor} v, {patPatch} v) -> (major, minor, patch))
  where {mkPatternName "VK_MAKE_VERSION"} major minor patch = major `shiftL` 22 .|. minor `shiftL` 12 .|. patch

{patMajor} :: Word32 -> Word32
{patMajor} v = v `shiftR` 22

{patMinor} :: Word32 -> Word32
{patMinor} v = v `shiftR` 12 .&. 0x3ff

{patPatch} :: Word32 -> Word32
{patPatch} v = v .&. 0xfff
|]
