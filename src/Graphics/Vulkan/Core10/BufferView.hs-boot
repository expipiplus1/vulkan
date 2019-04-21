{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.BufferView
  ( BufferView
  , BufferViewCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferViewCreateFlags
  , VkBufferView
  )


-- | VkBufferView - Opaque handle to a buffer view object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkCreateBufferView',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkDestroyBufferView'
type BufferView = VkBufferView

-- | VkBufferViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateFlags' is a
-- bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo'
type BufferViewCreateFlags = VkBufferViewCreateFlags
