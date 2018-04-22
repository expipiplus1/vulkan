{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  , vkAcquireXlibDisplayEXT
  , vkGetRandROutputDisplayEXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  )
import Foreign.Ptr
  ( Ptr
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
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  )


-- No documentation found for TopLevel "RROutput"
type RROutput = Word64
-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION"
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME"
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME = "VK_EXT_acquire_xlib_display"
-- | vkAcquireXlibDisplayEXT - Acquire access to a VkDisplayKHR using Xlib
--
-- = Parameters
--
-- -   @physicalDevice@ The physical device the display is on.
--
-- -   @dpy@ A connection to the X11 server that currently owns @display@.
--
-- -   @display@ The display the caller wishes to control in Vulkan.
--
-- = Description
--
-- All permissions necessary to control the display are granted to the
-- Vulkan instance associated with @physicalDevice@ until the display is
-- released or the X11 connection specified by @dpy@ is terminated.
-- Permission to access the display /may/ be temporarily revoked during
-- periods when the X11 server from which control was acquired itself
-- looses access to @display@. During such periods, operations which
-- require access to the display /must/ fail with an approriate error code.
-- If the X11 server associated with @dpy@ does not own @display@, or if
-- permission to access it has already been acquired by another entity, the
-- call /must/ return the error code @VK_ERROR_INITIALIZATION_FAILED@.
--
-- __Note__
--
-- One example of when an X11 server loses access to a display is when it
-- loses ownership of its virtual terminal.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @dpy@ /must/ be a valid pointer to a @Display@ value
--
-- -   @display@ /must/ be a valid @VkDisplayKHR@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_INITIALIZATION_FAILED@
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAcquireXlibDisplayEXT" vkAcquireXlibDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult
-- | vkGetRandROutputDisplayEXT - Query the VkDisplayKHR corresponding to an
-- X11 RandR Output
--
-- = Parameters
--
-- -   @physicalDevice@ The physical device to query the display handle on.
--
-- -   @dpy@ A connection to the X11 server from which @rrOutput@ was
--     queried.
--
-- -   @rrOutput@ An X11 RandR output ID.
--
-- -   @pDisplay@ The corresponding
--     'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayKHR' handle will
--     be returned here.
--
-- = Description
--
-- If there is no 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayKHR'
-- corresponding to @rrOutput@ on @physicalDevice@,
-- 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE' /must/ be returned in
-- @pDisplay@.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid @VkPhysicalDevice@ handle
--
-- -   @dpy@ /must/ be a valid pointer to a @Display@ value
--
-- -   @pDisplay@ /must/ be a valid pointer to a @VkDisplayKHR@ handle
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
  "vkGetRandROutputDisplayEXT" vkGetRandROutputDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult
