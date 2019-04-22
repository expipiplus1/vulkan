{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  ( DiscardRectangleModeEXT
  , PipelineDiscardRectangleStateCreateFlagsEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( VkDiscardRectangleModeEXT
  , VkPipelineDiscardRectangleStateCreateFlagsEXT
  )


-- | VkDiscardRectangleModeEXT - Specify the discard rectangle mode
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VkPipelineDiscardRectangleStateCreateInfoEXT'
type DiscardRectangleModeEXT = VkDiscardRectangleModeEXT

-- | VkPipelineDiscardRectangleStateCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VkPipelineDiscardRectangleStateCreateFlagsEXT'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.VkPipelineDiscardRectangleStateCreateInfoEXT'
type PipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT
