{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( MemoryAllocateFlagBits
  , MemoryAllocateFlagBitsKHR
  , MemoryAllocateFlags
  , MemoryAllocateFlagsKHR
  , PeerMemoryFeatureFlagBits
  , PeerMemoryFeatureFlagBitsKHR
  , PeerMemoryFeatureFlags
  , PeerMemoryFeatureFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkMemoryAllocateFlagBits
  , VkPeerMemoryFeatureFlagBits
  )


-- | VkMemoryAllocateFlagBits - Bitmask specifying flags for a device memory
-- allocation
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlags'
type MemoryAllocateFlagBits = VkMemoryAllocateFlagBits

-- No documentation found for TopLevel "MemoryAllocateFlagBitsKHR"
type MemoryAllocateFlagBitsKHR = MemoryAllocateFlagBits

-- | VkMemoryAllocateFlags - Bitmask of VkMemoryAllocateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo'
type MemoryAllocateFlags = MemoryAllocateFlagBits

-- No documentation found for TopLevel "MemoryAllocateFlagsKHR"
type MemoryAllocateFlagsKHR = MemoryAllocateFlags

-- | VkPeerMemoryFeatureFlagBits - Bitmask specifying supported peer memory
-- features
--
-- = Description
--
-- __Note__
--
-- The peer memory features of a memory heap also apply to any accesses
-- that /may/ be performed during
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>.
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_PEER_MEMORY_FEATURE_COPY_DST_BIT'
-- /must/ be supported for all host local heaps and for at least one device
-- local heap.
--
-- If a device does not support a peer memory feature, it is still valid to
-- use a resource that includes both local and peer memory bindings with
-- the corresponding access type as long as only the local bindings are
-- actually accessed. For example, an application doing split-frame
-- rendering would use framebuffer attachments that include both local and
-- peer memory bindings, but would scissor the rendering to only update
-- local memory.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlags'
type PeerMemoryFeatureFlagBits = VkPeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "PeerMemoryFeatureFlagBitsKHR"
type PeerMemoryFeatureFlagBitsKHR = PeerMemoryFeatureFlagBits

-- | VkPeerMemoryFeatureFlags - Bitmask of VkPeerMemoryFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeatures',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeaturesKHR'
type PeerMemoryFeatureFlags = PeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "PeerMemoryFeatureFlagsKHR"
type PeerMemoryFeatureFlagsKHR = PeerMemoryFeatureFlags
