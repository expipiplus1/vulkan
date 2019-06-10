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


-- No documentation found for TopLevel "DebugUtilsMessageSeverityFlagBitsEXT"
type DebugUtilsMessageSeverityFlagBitsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT

-- No documentation found for TopLevel "DebugUtilsMessageSeverityFlagsEXT"
type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

-- No documentation found for TopLevel "DebugUtilsMessageTypeFlagBitsEXT"
type DebugUtilsMessageTypeFlagBitsEXT = VkDebugUtilsMessageTypeFlagBitsEXT

-- No documentation found for TopLevel "DebugUtilsMessageTypeFlagsEXT"
type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT

-- No documentation found for TopLevel "DebugUtilsMessengerCallbackDataFlagsEXT"
type DebugUtilsMessengerCallbackDataFlagsEXT = VkDebugUtilsMessengerCallbackDataFlagsEXT

-- No documentation found for TopLevel "DebugUtilsMessengerCreateFlagsEXT"
type DebugUtilsMessengerCreateFlagsEXT = VkDebugUtilsMessengerCreateFlagsEXT

-- No documentation found for TopLevel "DebugUtilsMessengerEXT"
type DebugUtilsMessengerEXT = VkDebugUtilsMessengerEXT
