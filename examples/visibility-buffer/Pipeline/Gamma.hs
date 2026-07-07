{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The gamma (output-encode) compute pipeline.

Display-linear → sRGB ("Pipeline.Gamma.Shader"). Set 0 is two storage images (linear
in, sRGB out).
-}
module Pipeline.Gamma
  ( Pipeline (..)
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

import qualified Pipeline.Gamma.Shader as Shader

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

-- | A descriptor set binding the linear input (0) and sRGB output (1) images.
allocateSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl linView srgbView = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero{Vk.maxSets = 1, Vk.poolSizes = [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE 2]}
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.descriptorSetLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets dev [storageImage set 0 linView, storageImage set 1 srgbView] []
  pure set
  where
    storageImage set binding view =
      SomeStruct
        zero
          { Vk.dstSet = set
          , Vk.dstBinding = binding
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
          , Vk.descriptorCount = 1
          , Vk.imageInfo = [zero{Vk.imageView = view, Vk.imageLayout = Vk.IMAGE_LAYOUT_GENERAL} :: Vk.DescriptorImageInfo]
          }
