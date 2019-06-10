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


-- No documentation found for TopLevel "DiscardRectangleModeEXT"
type DiscardRectangleModeEXT = VkDiscardRectangleModeEXT

-- No documentation found for TopLevel "PipelineDiscardRectangleStateCreateFlagsEXT"
type PipelineDiscardRectangleStateCreateFlagsEXT = VkPipelineDiscardRectangleStateCreateFlagsEXT
