{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Buffer
  ( BufferCreateFlagBits
  , BufferCreateFlags
  , BufferUsageFlagBits
  , BufferUsageFlags
  , SharingMode
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits
  , VkBufferUsageFlagBits
  , VkSharingMode
  )


-- | VkBufferCreateFlagBits - Bitmask specifying additional parameters of a
-- buffer
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features Physical Device Features>
-- for details of the sparse memory features supported on a device.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlags'
type BufferCreateFlagBits = VkBufferCreateFlagBits

-- | VkBufferCreateFlags - Bitmask of VkBufferCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
type BufferCreateFlags = BufferCreateFlagBits

-- | VkBufferUsageFlagBits - Bitmask specifying allowed usage of a buffer
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlags'
type BufferUsageFlagBits = VkBufferUsageFlagBits

-- | VkBufferUsageFlags - Bitmask of VkBufferUsageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
type BufferUsageFlags = BufferUsageFlagBits

-- | VkSharingMode - Buffer and image sharing modes
--
-- = Description
--
-- __Note__
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT' /may/
-- result in lower performance access to the buffer or image than
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE'.
--
-- Ranges of buffers and image subresources of image objects created using
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE' /must/ only
-- be accessed by queues in the queue family that has /ownership/ of the
-- resource. Upon creation, such resources are not owned by any queue
-- family; ownership is implicitly acquired upon first use within a queue.
-- Once a resource using
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE' is owned by
-- some queue family, the application /must/ perform a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
-- to make the memory contents of a range or image subresource accessible
-- to a different queue family.
--
-- __Note__
--
-- Images still require a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-layouts layout transition>
-- from 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' or
-- 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED' before
-- being used on the first queue.
--
-- A queue family /can/ take ownership of an image subresource or buffer
-- range of a resource created with
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE', without an
-- ownership transfer, in the same way as for a resource that was just
-- created; however, taking ownership in this way has the effect that the
-- contents of the image subresource or buffer range are undefined.
--
-- Ranges of buffers and image subresources of image objects created using
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT' /must/ only
-- be accessed by queues from the queue families specified through the
-- @queueFamilyIndexCount@ and @pQueueFamilyIndices@ members of the
-- corresponding create info structures.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkPhysicalDeviceImageDrmFormatModifierInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
type SharingMode = VkSharingMode
