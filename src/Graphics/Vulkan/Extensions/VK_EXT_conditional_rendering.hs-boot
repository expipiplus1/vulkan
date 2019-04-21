{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering
  ( ConditionalRenderingFlagBitsEXT
  , ConditionalRenderingFlagsEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkConditionalRenderingFlagBitsEXT
  )


-- | VkConditionalRenderingFlagBitsEXT - Specify the behavior of conditional
-- rendering
--
-- = See Also
--
-- No cross-references are available
type ConditionalRenderingFlagBitsEXT = VkConditionalRenderingFlagBitsEXT

-- | VkConditionalRenderingFlagsEXT - Bitmask of
-- VkConditionalRenderingFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingFlagBitsEXT'.
--
-- = See Also
--
-- No cross-references are available
type ConditionalRenderingFlagsEXT = ConditionalRenderingFlagBitsEXT
