{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkCoarseSampleLocationNV
  , VkCoarseSampleOrderCustomNV
  , VkCoarseSampleOrderTypeNV
  , VkPhysicalDeviceShadingRateImageFeaturesNV
  , VkPhysicalDeviceShadingRateImagePropertiesNV
  , VkPipelineViewportCoarseSampleOrderStateCreateInfoNV
  , VkPipelineViewportShadingRateImageStateCreateInfoNV
  , VkShadingRatePaletteEntryNV
  , VkShadingRatePaletteNV
  , FN_vkCmdBindShadingRateImageNV
  , PFN_vkCmdBindShadingRateImageNV
  , FN_vkCmdSetCoarseSampleOrderNV
  , PFN_vkCmdSetCoarseSampleOrderNV
  , FN_vkCmdSetViewportShadingRatePaletteNV
  , PFN_vkCmdSetViewportShadingRatePaletteNV
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout
  )
import {-# source #-} Graphics.Vulkan.C.Core10.ImageView
  ( VkImageView
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkCoarseSampleLocationNV

data VkCoarseSampleOrderCustomNV

data VkCoarseSampleOrderTypeNV

data VkPhysicalDeviceShadingRateImageFeaturesNV

data VkPhysicalDeviceShadingRateImagePropertiesNV

data VkPipelineViewportCoarseSampleOrderStateCreateInfoNV

data VkPipelineViewportShadingRateImageStateCreateInfoNV

data VkShadingRatePaletteEntryNV

data VkShadingRatePaletteNV

type FN_vkCmdBindShadingRateImageNV = ("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ()
type PFN_vkCmdBindShadingRateImageNV = FunPtr FN_vkCmdBindShadingRateImageNV

type FN_vkCmdSetCoarseSampleOrderNV = ("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ()
type PFN_vkCmdSetCoarseSampleOrderNV = FunPtr FN_vkCmdSetCoarseSampleOrderNV

type FN_vkCmdSetViewportShadingRatePaletteNV = ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ()
type PFN_vkCmdSetViewportShadingRatePaletteNV = FunPtr FN_vkCmdSetViewportShadingRatePaletteNV
