{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The gamma (output-encode) compute pipeline.

Display-linear → sRGB ("Pipeline.Gamma.Shader").
-}
module Pipeline.Gamma
  ( allocatePipeline
  , allocateSet
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (imageWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)

import qualified Pipeline.Gamma.Shader as Shader

allocatePipeline :: Vk.Device -> ResourceT IO Pipeline
allocatePipeline dev = allocateCompute dev () Shader.code

-- | A descriptor set binding the linear input (0) and sRGB output (1) images.
allocateSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl linView srgbView = do
  set <- Pipeline.allocateSet dev pl 0
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL linView
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL srgbView
    ]
    []
  pure set
