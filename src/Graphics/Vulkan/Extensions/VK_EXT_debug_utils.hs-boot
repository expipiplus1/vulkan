{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_debug_utils
  ( DebugUtilsMessageSeverityFlagBitsEXT
  , DebugUtilsMessageSeverityFlagsEXT
  , DebugUtilsMessageTypeFlagBitsEXT
  , DebugUtilsMessageTypeFlagsEXT
  , DebugUtilsMessengerCallbackDataFlagsEXT
  , DebugUtilsMessengerCreateFlagsEXT
  , DebugUtilsMessengerEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( VkDebugUtilsMessageSeverityFlagBitsEXT
  , VkDebugUtilsMessageTypeFlagBitsEXT
  , VkDebugUtilsMessengerCallbackDataFlagsEXT
  , VkDebugUtilsMessengerCreateFlagsEXT
  , VkDebugUtilsMessengerEXT
  )


-- | VkDebugUtilsMessageSeverityFlagBitsEXT - Bitmask specifying which
-- severities of events cause a debug messenger callback
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageSeverityFlagsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkSubmitDebugUtilsMessageEXT'
type DebugUtilsMessageSeverityFlagBitsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT

-- | VkDebugUtilsMessageSeverityFlagsEXT - Bitmask of
-- VkDebugUtilsMessageSeverityFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageSeverityFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageSeverityFlagBitsEXT'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageSeverityFlagBitsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCreateInfoEXT'
type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

-- | VkDebugUtilsMessageTypeFlagBitsEXT - Bitmask specifying which types of
-- events cause a debug messenger callback
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageTypeFlagsEXT'
type DebugUtilsMessageTypeFlagBitsEXT = VkDebugUtilsMessageTypeFlagBitsEXT

-- | VkDebugUtilsMessageTypeFlagsEXT - Bitmask of
-- VkDebugUtilsMessageTypeFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageTypeFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageTypeFlagBitsEXT'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessageTypeFlagBitsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCreateInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkSubmitDebugUtilsMessageEXT'
type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT

-- No documentation found for TopLevel "DebugUtilsMessengerCallbackDataFlagsEXT"
type DebugUtilsMessengerCallbackDataFlagsEXT = VkDebugUtilsMessengerCallbackDataFlagsEXT

-- No documentation found for TopLevel "DebugUtilsMessengerCreateFlagsEXT"
type DebugUtilsMessengerCreateFlagsEXT = VkDebugUtilsMessengerCreateFlagsEXT

-- | VkDebugUtilsMessengerEXT - Opaque handle to a debug messenger object
--
-- = Description
--
-- The debug messenger will provide detailed feedback on the application’s
-- use of Vulkan when events of interest occur. When an event of interest
-- does occur, the debug messenger will submit a debug message to the debug
-- callback that was provided during its creation. Additionally, the debug
-- messenger is responsible with filtering out debug messages that the
-- callback is not interested in and will only provide desired debug
-- messages.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCreateDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkDestroyDebugUtilsMessengerEXT'
type DebugUtilsMessengerEXT = VkDebugUtilsMessengerEXT
