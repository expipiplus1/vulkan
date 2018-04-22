{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display
  ( pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  , vkReleaseDisplayEXT
  ) where

import Data.String
  ( IsString
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )


-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION"
pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME"
pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = "VK_EXT_direct_mode_display"
-- | vkReleaseDisplayEXT - Release access to an acquired VkDisplayKHR
--
-- = Parameters
--
-- -   @physicalDevice@ The physical device the display is on.
--
-- -   @display@ The display to release control of.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @display@ /must/ be a valid @VkDisplayKHR@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkReleaseDisplayEXT" vkReleaseDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult
