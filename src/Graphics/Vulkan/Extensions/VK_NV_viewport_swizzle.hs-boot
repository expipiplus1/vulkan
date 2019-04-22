{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
  ( PipelineViewportSwizzleStateCreateFlagsNV
  , ViewportCoordinateSwizzleNV
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle
  ( VkPipelineViewportSwizzleStateCreateFlagsNV
  , VkViewportCoordinateSwizzleNV
  )


-- | VkPipelineViewportSwizzleStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle.VkPipelineViewportSwizzleStateCreateFlagsNV'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle.VkPipelineViewportSwizzleStateCreateInfoNV'
type PipelineViewportSwizzleStateCreateFlagsNV = VkPipelineViewportSwizzleStateCreateFlagsNV

-- | VkViewportCoordinateSwizzleNV - Specify how a viewport coordinate is
-- swizzled
--
-- = Description
--
-- These values are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vertexpostproc-viewport-swizzle Viewport Swizzle>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle.VkViewportSwizzleNV'
type ViewportCoordinateSwizzleNV = VkViewportCoordinateSwizzleNV
