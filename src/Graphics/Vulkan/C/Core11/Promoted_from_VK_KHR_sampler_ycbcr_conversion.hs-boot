{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkBindImagePlaneMemoryInfo
  , VkChromaLocation
  , VkImagePlaneMemoryRequirementsInfo
  , VkPhysicalDeviceSamplerYcbcrConversionFeatures
  , VkSamplerYcbcrConversion
  , VkSamplerYcbcrConversionCreateInfo
  , VkSamplerYcbcrConversionImageFormatProperties
  , VkSamplerYcbcrConversionInfo
  , VkSamplerYcbcrModelConversion
  , VkSamplerYcbcrRange
  , FN_vkCreateSamplerYcbcrConversion
  , PFN_vkCreateSamplerYcbcrConversion
  , FN_vkDestroySamplerYcbcrConversion
  , PFN_vkDestroySamplerYcbcrConversion
  ) where

import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  )


data VkBindImagePlaneMemoryInfo

data VkChromaLocation

data VkImagePlaneMemoryRequirementsInfo

data VkPhysicalDeviceSamplerYcbcrConversionFeatures

-- | Dummy data to tag the 'Ptr' with
data VkSamplerYcbcrConversion_T
-- | VkSamplerYcbcrConversion - Opaque handle to a device-specific sampler
-- Y’CBCR conversion description
--
-- = See Also
--
-- 'VkSamplerYcbcrConversionInfo', 'vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR',
-- 'vkDestroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversionKHR'
type VkSamplerYcbcrConversion = Ptr VkSamplerYcbcrConversion_T

data VkSamplerYcbcrConversionCreateInfo

data VkSamplerYcbcrConversionImageFormatProperties

data VkSamplerYcbcrConversionInfo

data VkSamplerYcbcrModelConversion

data VkSamplerYcbcrRange

type FN_vkCreateSamplerYcbcrConversion = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult
type PFN_vkCreateSamplerYcbcrConversion = FunPtr FN_vkCreateSamplerYcbcrConversion

type FN_vkDestroySamplerYcbcrConversion = ("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySamplerYcbcrConversion = FunPtr FN_vkDestroySamplerYcbcrConversion
