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

HDR radiance + bloom → display-linear, via exposure + the Uchimura curve
("Pipeline.Tonemap.Shader"). The push-constant record is reflected from the
shader.
-}
module Pipeline.Tonemap
  ( PC (..)
  , allocatePipeline
  , allocateSet
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite, imageWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)

import qualified Pipeline.Tonemap.Shader as Shader

-- Generate the @PC@ push-constant record (bloom strength).
reflectShaderTypesBytes Shader.code

allocatePipeline :: Vk.Device -> ResourceT IO Pipeline
allocatePipeline dev = allocateCompute dev () Shader.code

{- | A descriptor set for the tonemap composite.

The HDR input (0) and tonemapped output (1) storage images, the bloom mip-0
sampled through @sampler@ (2), and the metering buffer carrying the exposure (3).
-}
allocateSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.ImageView -> Vk.Sampler -> Vk.ImageView -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl hdrView toneView sampler bloomView exposureBuffer = do
  set <- Pipeline.allocateSet dev pl 0
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL hdrView
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL toneView
    , combinedImageSamplerWrite set 2 sampler bloomView Vk.IMAGE_LAYOUT_GENERAL
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER exposureBuffer
    ]
    []
  pure set
