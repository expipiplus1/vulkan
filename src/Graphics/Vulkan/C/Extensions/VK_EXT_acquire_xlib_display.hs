{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkAcquireXlibDisplayEXT
#endif
  , FN_vkAcquireXlibDisplayEXT
  , PFN_vkAcquireXlibDisplayEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetRandROutputDisplayEXT
#endif
  , FN_vkGetRandROutputDisplayEXT
  , PFN_vkGetRandROutputDisplayEXT
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
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
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "RROutput"
type RROutput = Word64
  
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkAcquireXlibDisplayEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAcquireXlibDisplayEXT" vkAcquireXlibDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult

#endif
type FN_vkAcquireXlibDisplayEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult
type PFN_vkAcquireXlibDisplayEXT = FunPtr FN_vkAcquireXlibDisplayEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetRandROutputDisplayEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetRandROutputDisplayEXT" vkGetRandROutputDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult

#endif
type FN_vkGetRandROutputDisplayEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult
type PFN_vkGetRandROutputDisplayEXT = FunPtr FN_vkGetRandROutputDisplayEXT
-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME"
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME = "VK_EXT_acquire_xlib_display"
-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION"
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1
