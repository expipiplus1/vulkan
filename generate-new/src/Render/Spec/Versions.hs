{-# language QuasiQuotes #-}
module Render.Spec.Versions
  ( specVersions
  ) where

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

specVersions
  :: forall r
   . (HasErr r, HasRenderParams r)
  => Spec SpecVk
  -> Vector (Sem r RenderElement)
specVersions Spec {..} =
  fromList
    $ [ headerVersion specHeaderVersion
      , headerVersionComplete (fVersion (V.last specFeatures)) specHeaderVersion
      ]
    <> (versionConstruction : (featureVersion <$> toList specFeatures))

headerVersion
  :: (HasErr r, HasRenderParams r)
  => SpecHeaderVersion SpecVk
  -> Sem r RenderElement
headerVersion (VkVersion version) = genRe "header version" $ do
  RenderParams {..} <- input
  tellExplicitModule =<< mkModuleName ["Version"]
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
    tellExplicitModule =<< mkModuleName ["Version"]
    let pat               = mkPatternName "VK_HEADER_VERSION_COMPLETE"
        major : minor : _ = versionBranch lastFeatureVersion
        makeVersion       = mkPatternName "VK_MAKE_API_VERSION"
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
      make = mkPatternName "VK_MAKE_API_VERSION"
  tellExport (EPat pat)
  tellImport ''Word32
  tellImport make
  tellExplicitModule =<< mkModuleName ["Core" <> show major <> show minor]
  tellDoc [qqi|
    pattern {pat} :: Word32
    pattern {pat} = {make} {major} {minor} 0
  |]

versionConstruction :: (HasErr r, HasRenderParams r) => Sem r RenderElement
versionConstruction = genRe "version construction" $ do
  RenderParams {..} <- input
  tellExplicitModule =<< mkModuleName ["Version"]
  tellImport ''Word32
  tellImport '(.&.)
  tellImport '(.|.)
  tellImport 'shiftL
  tellImport 'shiftR
  let patMajor = TermName ("_" <> unName (mkPatternName "VK_VERSION_MAJOR"))
      patMinor = TermName ("_" <> unName (mkPatternName "VK_VERSION_MINOR"))
      patPatch = TermName ("_" <> unName (mkPatternName "VK_VERSION_PATCH"))
      patApiMajor = TermName ("_" <> unName (mkPatternName "VK_API_VERSION_MAJOR"))
      patApiMinor = TermName ("_" <> unName (mkPatternName "VK_API_VERSION_MINOR"))
      patApiPatch = TermName ("_" <> unName (mkPatternName "VK_API_VERSION_PATCH"))
      makeApiVersion = mkPatternName "VK_MAKE_API_VERSION"
      makeVersion = mkPatternName "VK_MAKE_VERSION"
  tellExport (EPat makeApiVersion)
  tellExport (EPat makeVersion)
  tellExport (ETerm patMajor)
  tellExport (ETerm patMinor)
  tellExport (ETerm patPatch)
  tellExport (ETerm patApiMajor)
  tellExport (ETerm patApiMinor)
  tellExport (ETerm patApiPatch)
  tellDoc [qqi|
    pattern {makeApiVersion} :: Word32 -> Word32 -> Word32 -> Word32
    pattern {makeApiVersion} major minor patch <-
      (\\v -> ({patMajor} v, {patMinor} v, {patPatch} v) -> (major, minor, patch))
      where {makeApiVersion} major minor patch = major `shiftL` 22 .|. minor `shiftL` 12 .|. patch

    \{-# complete {makeApiVersion} #-}

    \{-# deprecated {makeVersion} "This pattern is deprecated. {makeApiVersion} should be used instead." #-}
    pattern {makeVersion} :: Word32 -> Word32 -> Word32 -> Word32
    pattern {makeVersion} major minor patch = {makeApiVersion} major minor patch

    \{-# complete {makeVersion} #-}

    \{-# deprecated {patMajor} "This function is deprecated. {patApiMajor} should be used instead." #-}
    {patMajor} :: Word32 -> Word32
    {patMajor} = {patApiMajor}

    \{-# deprecated {patMinor} "This function is deprecated. {patApiMinor} should be used instead." #-}
    {patMinor} :: Word32 -> Word32
    {patMinor} = {patApiMinor}

    \{-# deprecated {patPatch} "This function is deprecated. {patApiPatch} should be used instead." #-}
    {patPatch} :: Word32 -> Word32
    {patPatch} = {patApiPatch}

    {patApiMajor} :: Word32 -> Word32
    {patApiMajor} v = v `shiftR` 22

    {patApiMinor} :: Word32 -> Word32
    {patApiMinor} v = v `shiftR` 12 .&. 0x3ff

    {patApiPatch} :: Word32 -> Word32
    {patApiPatch} v = v .&. 0xfff
  |]
