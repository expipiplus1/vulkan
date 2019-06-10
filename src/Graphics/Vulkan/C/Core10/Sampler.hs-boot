{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Sampler
  ( VkBorderColor
  , VkFilter
  , VkSampler
  , VkSamplerAddressMode
  , VkSamplerCreateFlagBits
  , VkSamplerCreateFlags
  , VkSamplerCreateInfo
  , VkSamplerMipmapMode
  , FN_vkCreateSampler
  , PFN_vkCreateSampler
  , FN_vkDestroySampler
  , PFN_vkDestroySampler
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


data VkBorderColor

data VkFilter

-- | Dummy data to tag the 'Ptr' with
data VkSampler_T
-- No documentation found for TopLevel "VkSampler"
type VkSampler = Ptr VkSampler_T

data VkSamplerAddressMode

data VkSamplerCreateFlagBits

-- No documentation found for TopLevel "VkSamplerCreateFlags"
type VkSamplerCreateFlags = VkSamplerCreateFlagBits

data VkSamplerCreateInfo

data VkSamplerMipmapMode

type FN_vkCreateSampler = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult
type PFN_vkCreateSampler = FunPtr FN_vkCreateSampler

type FN_vkDestroySampler = ("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySampler = FunPtr FN_vkDestroySampler
