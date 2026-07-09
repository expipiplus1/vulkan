{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The luminance-reduction compute pipeline.

Bloom mip → average log-luminance buffer ("Pipeline.Luminance.Shader"), for
auto-exposure. Set 0 is the source storage image (0) and the output SSBO (1); dispatch
a single workgroup.
-}
module Pipeline.Luminance
  ( Pipeline (..)
  , bufferBytes
  , allocatePipeline
  , allocateSet
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Shader (shaderModuleStage)
import Vulkan.Utils.SpirV.Descriptors (pushConstantRanges, singleDescriptorSetLayoutInfo)
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
  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero{Vk.setLayouts = [descriptorSetLayout], Vk.pushConstantRanges = V.fromList (pushConstantRanges reflected)}
      Nothing
      allocate
  (_, stage) <- shaderModuleStage dev Vk.SHADER_STAGE_COMPUTE_BIT Nothing Shader.code
  let createInfo = zero{Vk.layout = pipelineLayout, Vk.stage = stage} :: Vk.ComputePipelineCreateInfo '[]
  (_, (_, [pipeline])) <- Vk.withComputePipelines dev zero [SomeStruct createInfo] Nothing allocate
  pure Pipeline{pipeline, pipelineLayout, descriptorSetLayout}

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
    [ SomeStruct
        zero
          { Vk.dstSet = set
          , Vk.dstBinding = 0
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
          , Vk.descriptorCount = 1
          , Vk.imageInfo = [zero{Vk.imageView = srcView, Vk.imageLayout = Vk.IMAGE_LAYOUT_GENERAL} :: Vk.DescriptorImageInfo]
          }
    , SomeStruct
        zero
          { Vk.dstSet = set
          , Vk.dstBinding = 1
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
          , Vk.descriptorCount = 1
          , Vk.bufferInfo = [Vk.DescriptorBufferInfo lumBuffer 0 Vk.WHOLE_SIZE]
          }
    ]
    []
  pure set
