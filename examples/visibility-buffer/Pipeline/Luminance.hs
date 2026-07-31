{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The luminance-reduction compute pipeline.

Bloom mip → average log-luminance buffer ("Pipeline.Luminance.Shader"), for
auto-exposure; dispatch a single workgroup.
-}
module Pipeline.Luminance
  ( bufferBytes
  , allocatePipeline
  , allocateSet
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, imageWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)

import qualified Pipeline.Luminance.Shader as Shader

-- | Bytes for the output buffer: @{ float avgLogLum; float geoMean; }@.
bufferBytes :: Vk.DeviceSize
bufferBytes = 8

allocatePipeline :: Vk.Device -> ResourceT IO Pipeline
allocatePipeline dev = allocateCompute dev () Shader.code

-- | A descriptor set binding the source image (0) and the output SSBO (1).
allocateSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl srcView lumBuffer = do
  set <- Pipeline.allocateSet dev pl 0
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL srcView
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER lumBuffer
    ]
    []
  pure set
