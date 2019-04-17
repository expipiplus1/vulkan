{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display
  ( 
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  vkReleaseDisplayEXT
  , 
#endif
  FN_vkReleaseDisplayEXT
  , PFN_vkReleaseDisplayEXT
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME
  , pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkReleaseDisplayEXT - Release access to an acquired VkDisplayKHR
--
-- = Parameters
--
-- -   @physicalDevice@ The physical device the display is on.
--
-- -   @display@ The display to release control of.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkReleaseDisplayEXT" vkReleaseDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult

#endif
type FN_vkReleaseDisplayEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult
type PFN_vkReleaseDisplayEXT = FunPtr FN_vkReleaseDisplayEXT
-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME"
pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME = "VK_EXT_direct_mode_display"
-- No documentation found for TopLevel "VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION"
pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION = 1
