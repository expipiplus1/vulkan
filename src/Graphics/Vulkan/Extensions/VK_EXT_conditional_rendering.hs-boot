{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering
  ( ConditionalRenderingFlagBitsEXT
  , ConditionalRenderingFlagsEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkConditionalRenderingFlagBitsEXT
  )


-- No documentation found for TopLevel "ConditionalRenderingFlagBitsEXT"
type ConditionalRenderingFlagBitsEXT = VkConditionalRenderingFlagBitsEXT

-- No documentation found for TopLevel "ConditionalRenderingFlagsEXT"
type ConditionalRenderingFlagsEXT = ConditionalRenderingFlagBitsEXT
