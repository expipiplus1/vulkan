{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlagBits
  , ImageAspectFlags
  , SparseImageFormatFlagBits
  , SparseImageFormatFlags
  , SparseMemoryBindFlagBits
  , SparseMemoryBindFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlagBits
  , VkSparseImageFormatFlagBits
  , VkSparseMemoryBindFlagBits
  )


-- | VkImageAspectFlagBits - Bitmask specifying which aspects of an image are
-- included in a view
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
type ImageAspectFlagBits = VkImageAspectFlagBits

-- | VkImageAspectFlags - Bitmask of VkImageAspectFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearAttachment',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageSubresource',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkInputAttachmentAspectReference',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
type ImageAspectFlags = ImageAspectFlagBits

-- | VkSparseImageFormatFlagBits - Bitmask specifying additional information
-- about a sparse image resource
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatFlags'
type SparseImageFormatFlagBits = VkSparseImageFormatFlagBits

-- | VkSparseImageFormatFlags - Bitmask of VkSparseImageFormatFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatFlagBits',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
type SparseImageFormatFlags = SparseImageFormatFlagBits

-- | VkSparseMemoryBindFlagBits - Bitmask specifying usage of a sparse memory
-- binding operation
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlags'
type SparseMemoryBindFlagBits = VkSparseMemoryBindFlagBits

-- | VkSparseMemoryBindFlags - Bitmask of VkSparseMemoryBindFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBindFlagBits'
type SparseMemoryBindFlags = SparseMemoryBindFlagBits
