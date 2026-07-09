{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The tonemap compute pipeline.

HDR radiance → display-linear, via exposure + the Uchimura curve
("Pipeline.Tonemap.Shader"). Set 0 is two storage images (hdr in, tone out); the
exposure push range is reflected from the shader.
-}
module Pipeline.Tonemap
  ( Pipeline (..)
  , PC (..)
  , allocatePipeline
  , allocateSet
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Data.Vector as V
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (combinedImageSamplerWrite, imageWrite)
import Vulkan.Utils.SpirV.Pipeline (allocateComputePipeline, allocateReflectedLayout, singleSetLayout)
import qualified Vulkan.Utils.SpirV.Pipeline
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Tonemap.Shader as Shader

-- Generate the @PC@ push-constant record (exposure, bloom strength, debug mode).
reflectShaderTypesBytes Shader.code

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  , descriptorSetLayout :: Vk.DescriptorSetLayout
  }

allocatePipeline :: Vk.Device -> ResourceT IO Pipeline
allocatePipeline dev = do
  reflected <- reflectBytes Shader.code
  (_, reflectedLayout) <- allocateReflectedLayout dev [reflected]
  descriptorSetLayout <- singleSetLayout reflectedLayout
  (_, pipeline) <- allocateComputePipeline dev reflectedLayout () (reflected, Shader.code)
  pure Pipeline{pipeline, pipelineLayout = reflectedLayout.pipelineLayout, descriptorSetLayout}

{- | A descriptor set for the tonemap composite.

The HDR input (0) and tonemapped output (1) storage images, plus the bloom mip-0
sampled through @sampler@ (2).
-}
allocateSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.ImageView -> Vk.Sampler -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl hdrView toneView sampler bloomView = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE 2
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
            ]
        }
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.descriptorSetLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL hdrView
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL toneView
    , combinedImageSamplerWrite set 2 sampler bloomView Vk.IMAGE_LAYOUT_GENERAL
    ]
    []
  pure set
