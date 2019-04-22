{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.CommandBufferBuilding
  ( IndexType
  , StencilFaceFlagBits
  , StencilFaceFlags
  , SubpassContents
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkIndexType
  , VkStencilFaceFlagBits
  , VkSubpassContents
  )


-- | VkIndexType - Type of index buffer indices
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryTrianglesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableIndexBufferEntryNVX',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'
type IndexType = VkIndexType

-- | VkStencilFaceFlagBits - Bitmask specifying sets of stencil state for
-- which to update the compare mask
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlags'
type StencilFaceFlagBits = VkStencilFaceFlagBits

-- | VkStencilFaceFlags - Bitmask of VkStencilFaceFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilReference',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilWriteMask'
type StencilFaceFlags = StencilFaceFlagBits

-- | VkSubpassContents - Specify how commands in the first subpass of a
-- render pass are provided
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassBeginInfoKHR',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdNextSubpass'
type SubpassContents = VkSubpassContents
