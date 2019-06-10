{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( DebugReportCallbackEXT
  , DebugReportFlagBitsEXT
  , DebugReportFlagsEXT
  , DebugReportObjectTypeEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackEXT
  , VkDebugReportFlagBitsEXT
  , VkDebugReportObjectTypeEXT
  )


-- No documentation found for TopLevel "DebugReportCallbackEXT"
type DebugReportCallbackEXT = VkDebugReportCallbackEXT

-- No documentation found for TopLevel "DebugReportFlagBitsEXT"
type DebugReportFlagBitsEXT = VkDebugReportFlagBitsEXT

-- No documentation found for TopLevel "DebugReportFlagsEXT"
type DebugReportFlagsEXT = DebugReportFlagBitsEXT

-- No documentation found for TopLevel "DebugReportObjectTypeEXT"
type DebugReportObjectTypeEXT = VkDebugReportObjectTypeEXT
