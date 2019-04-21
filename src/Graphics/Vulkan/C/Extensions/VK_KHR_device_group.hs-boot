{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( VkBindBufferMemoryDeviceGroupInfoKHR
  , VkBindImageMemoryDeviceGroupInfoKHR
  , VkDeviceGroupBindSparseInfoKHR
  , VkDeviceGroupCommandBufferBeginInfoKHR
  , VkDeviceGroupRenderPassBeginInfoKHR
  , VkDeviceGroupSubmitInfoKHR
  , VkMemoryAllocateFlagBitsKHR
  , VkMemoryAllocateFlagsInfoKHR
  , VkMemoryAllocateFlagsKHR
  , VkPeerMemoryFeatureFlagBitsKHR
  , VkPeerMemoryFeatureFlagsKHR
  , FN_vkGetDeviceGroupSurfacePresentModes2EXT
  , PFN_vkGetDeviceGroupSurfacePresentModes2EXT
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
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkDeviceGroupBindSparseInfo
  , VkDeviceGroupCommandBufferBeginInfo
  , VkDeviceGroupRenderPassBeginInfo
  , VkDeviceGroupSubmitInfo
  , VkMemoryAllocateFlagBits
  , VkMemoryAllocateFlagsInfo
  , VkPeerMemoryFeatureFlagBits
  , VkMemoryAllocateFlags
  , VkPeerMemoryFeatureFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( VkBindBufferMemoryDeviceGroupInfo
  , VkBindImageMemoryDeviceGroupInfo
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkDeviceGroupPresentModeFlagsKHR
  )


-- No documentation found for TopLevel "VkBindBufferMemoryDeviceGroupInfoKHR"
type VkBindBufferMemoryDeviceGroupInfoKHR = VkBindBufferMemoryDeviceGroupInfo

-- No documentation found for TopLevel "VkBindImageMemoryDeviceGroupInfoKHR"
type VkBindImageMemoryDeviceGroupInfoKHR = VkBindImageMemoryDeviceGroupInfo

-- No documentation found for TopLevel "VkDeviceGroupBindSparseInfoKHR"
type VkDeviceGroupBindSparseInfoKHR = VkDeviceGroupBindSparseInfo

-- No documentation found for TopLevel "VkDeviceGroupCommandBufferBeginInfoKHR"
type VkDeviceGroupCommandBufferBeginInfoKHR = VkDeviceGroupCommandBufferBeginInfo

-- No documentation found for TopLevel "VkDeviceGroupRenderPassBeginInfoKHR"
type VkDeviceGroupRenderPassBeginInfoKHR = VkDeviceGroupRenderPassBeginInfo

-- No documentation found for TopLevel "VkDeviceGroupSubmitInfoKHR"
type VkDeviceGroupSubmitInfoKHR = VkDeviceGroupSubmitInfo

-- No documentation found for TopLevel "VkMemoryAllocateFlagBitsKHR"
type VkMemoryAllocateFlagBitsKHR = VkMemoryAllocateFlagBits

-- No documentation found for TopLevel "VkMemoryAllocateFlagsInfoKHR"
type VkMemoryAllocateFlagsInfoKHR = VkMemoryAllocateFlagsInfo

-- No documentation found for TopLevel "VkMemoryAllocateFlagsKHR"
type VkMemoryAllocateFlagsKHR = VkMemoryAllocateFlags

-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagBitsKHR"
type VkPeerMemoryFeatureFlagBitsKHR = VkPeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagsKHR"
type VkPeerMemoryFeatureFlagsKHR = VkPeerMemoryFeatureFlags

type FN_vkGetDeviceGroupSurfacePresentModes2EXT = ("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult
type PFN_vkGetDeviceGroupSurfacePresentModes2EXT = FunPtr FN_vkGetDeviceGroupSurfacePresentModes2EXT
