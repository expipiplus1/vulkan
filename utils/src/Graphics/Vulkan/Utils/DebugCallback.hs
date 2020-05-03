module Graphics.Vulkan.Utils.DebugCallback
  ( debugCallbackPtr
  ) where

import           Graphics.Vulkan.Extensions.VK_EXT_debug_utils

-- | A debug callback which prints the message prefixed with "Validation: " to
-- stderr.
foreign import ccall unsafe "DebugCallback.c &debugCallback"
  debugCallbackPtr :: PFN_vkDebugUtilsMessengerCallbackEXT
