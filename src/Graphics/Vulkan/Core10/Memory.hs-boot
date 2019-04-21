{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  , MemoryMapFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Memory
  ( VkMemoryMapFlags
  , VkDeviceMemory
  )


-- | VkDeviceMemory - Opaque handle to a device memory object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.C.Core10.Memory.vkFreeMemory',
-- 'Graphics.Vulkan.C.Core10.Memory.vkGetDeviceMemoryCommitment',
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory',
-- 'Graphics.Vulkan.C.Core10.Memory.vkUnmapMemory'
type DeviceMemory = VkDeviceMemory

-- | VkMemoryMapFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryMapFlags' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory'
type MemoryMapFlags = VkMemoryMapFlags
