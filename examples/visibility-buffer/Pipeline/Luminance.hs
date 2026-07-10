{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The luminance-reduction compute pipeline.

Bloom mip → average log-luminance buffer ("Pipeline.Luminance.Shader"), for
auto-exposure; dispatch a single workgroup.
-}
module Pipeline.Luminance
  ( Pipeline (..)
  , bufferBytes
  , allocatePipeline
  , allocateSet
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, imageWrite)
import Vulkan.Utils.SpirV.Pipeline (allocateComputePipeline, allocateReflectedLayout, singleSetLayout)
import qualified Vulkan.Utils.SpirV.Pipeline
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Luminance.Shader as Shader

-- | Bytes for the output buffer: @{ float avgLogLum; float geoMean; }@.
bufferBytes :: Vk.DeviceSize
bufferBytes = 8

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

-- | A descriptor set binding the source image (0) and the output SSBO (1).
allocateSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl srcView lumBuffer = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE 1
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 1
            ]
        }
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.descriptorSetLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL srcView
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER lumBuffer
    ]
    []
  pure set
