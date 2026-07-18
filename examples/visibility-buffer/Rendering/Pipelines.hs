{-# LANGUAGE OverloadedRecordDot #-}

{-| The extent-independent pipelines and the attachment formats they're built from.

The formats are the cross-pipeline contract (the mesh pass writes 'visFormat', the
resolve reads it, and so on); everything downstream sizes targets against them.
-}
module Rendering.Pipelines
  ( visFormat
  , hdrFormat
  , colorFormat
  , depthFormat
  , aoFormat
  , normalsFormat
  , shadeStage
  , ScenePipelines (..)
  , allocatePipelines
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Pipeline (Pipeline)

import qualified Pipeline.Bloom as Bloom
import qualified Pipeline.Cull as Cull
import qualified Pipeline.Gamma as Gamma
import qualified Pipeline.HiZ as HiZ
import qualified Pipeline.Knot as Knot
import qualified Pipeline.Luminance as Luminance
import qualified Pipeline.Mesh as Mesh
import qualified Pipeline.Shade as Shade
import qualified Pipeline.Shadow as Shadow
import qualified Pipeline.Ssao as Ssao
import qualified Pipeline.Tonemap as Tonemap
import qualified Pipeline.Voxels as Voxels
import qualified Rendering.Shadows as Shadows

visFormat :: Vk.Format
visFormat = Vk.FORMAT_R32G32_UINT

-- | Scene-linear HDR (shade output, tonemap in/out): radiance may exceed 1.
hdrFormat :: Vk.Format
hdrFormat = Vk.FORMAT_R16G16B16A16_SFLOAT

-- | The display (gamma output / readback) format — 8-bit sRGB-encoded.
colorFormat :: Vk.Format
colorFormat = Vk.FORMAT_R8G8B8A8_UNORM

depthFormat :: Vk.Format
depthFormat = Vk.FORMAT_D32_SFLOAT

-- | SSAO obscurance factor (1 = open).
aoFormat :: Vk.Format
aoFormat = Vk.FORMAT_R16_SFLOAT

{- | SSAO prepass: world normal in xyz, view depth in w (0 = void) — the
shaders declare @rgba16f@, so this can't retune with the colour chain.
-}
normalsFormat :: Vk.Format
normalsFormat = Vk.FORMAT_R16G16B16A16_SFLOAT

-- | Shade reads/writes its storage images from the compute stage.
shadeStage :: Vk.PipelineStageFlags
shadeStage = Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT

-- | The extent-independent pipelines, built once.
data ScenePipelines = ScenePipelines
  { mesh :: Pipeline
  , shade :: Pipeline
  , luminance :: Pipeline
  , tonemap :: Pipeline
  , gamma :: Pipeline
  , bloom :: Bloom.Bloom
  , shadow :: Pipeline
  , generator :: Pipeline
  , knot :: Pipeline
  , cull :: Cull.Cull
  , hiz :: HiZ.HiZ
  , ssao :: Ssao.Ssao
  }

allocatePipelines :: Vk.Device -> ResourceT IO ScenePipelines
allocatePipelines dev = do
  mesh <- Mesh.allocatePipeline dev visFormat depthFormat
  shade <- Shade.allocatePipeline dev Shadows.params
  luminance <- Luminance.allocatePipeline dev
  tonemap <- Tonemap.allocatePipeline dev
  gamma <- Gamma.allocatePipeline dev
  bloom <- Bloom.allocateBloom dev
  shadow <- Shadow.allocateShadow dev Shadows.params Shadows.format depthFormat
  generator <- Voxels.allocateGenerator dev
  knot <- Knot.allocateKnot dev
  cull <- Cull.allocateCull dev
  hiz <- HiZ.allocateHiZ dev
  ssao <- Ssao.allocateSsao dev
  pure ScenePipelines{mesh, shade, luminance, tonemap, gamma, bloom, shadow, generator, knot, cull, hiz, ssao}
