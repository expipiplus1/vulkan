{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The gamma (output-encode) compute pipeline.

Display-linear → sRGB ("Pipeline.Gamma.Shader").
-}
module Pipeline.Gamma
  ( Pipeline (..)
  , allocatePipeline
  , allocateSet
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (imageWrite)
import Vulkan.Utils.SpirV.Pipeline (allocateComputePipeline, allocateReflectedLayout, singleSetLayout)
import qualified Vulkan.Utils.SpirV.Pipeline
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
  (_, reflectedLayout) <- allocateReflectedLayout dev [reflected]
  descriptorSetLayout <- singleSetLayout reflectedLayout
  (_, pipeline) <- allocateComputePipeline dev reflectedLayout () (reflected, Shader.code)
  pure Pipeline{pipeline, pipelineLayout = reflectedLayout.pipelineLayout, descriptorSetLayout}

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
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL linView
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL srgbView
    ]
    []
  pure set
