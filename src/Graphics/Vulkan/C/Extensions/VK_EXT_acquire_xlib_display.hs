{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
  , FN_vkAcquireXlibDisplayEXT
  , PFN_vkAcquireXlibDisplayEXT
  , vkAcquireXlibDisplayEXT
  , FN_vkGetRandROutputDisplayEXT
  , PFN_vkGetRandROutputDisplayEXT
  , vkGetRandROutputDisplayEXT
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
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
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
-- call /must/ return the error code
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED'.
--
-- __Note__
--
-- One example of when an X11 server loses access to a display is when it
-- loses ownership of its virtual terminal.
--
-- Unresolved directive in vkAcquireXlibDisplayEXT.txt -
-- include::{generated}\/validity\/protos\/vkAcquireXlibDisplayEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAcquireXlibDisplayEXT" vkAcquireXlibDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult
#else
vkAcquireXlibDisplayEXT :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult
vkAcquireXlibDisplayEXT deviceCmds = mkVkAcquireXlibDisplayEXT (pVkAcquireXlibDisplayEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireXlibDisplayEXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult)
#endif

type FN_vkAcquireXlibDisplayEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult
type PFN_vkAcquireXlibDisplayEXT = FunPtr FN_vkAcquireXlibDisplayEXT

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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR' handle
--     will be returned here.
--
-- = Description
--
-- If there is no
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayKHR' corresponding
-- to @rrOutput@ on @physicalDevice@,
-- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' /must/ be returned
-- in @pDisplay@.
--
-- Unresolved directive in vkGetRandROutputDisplayEXT.txt -
-- include::{generated}\/validity\/protos\/vkGetRandROutputDisplayEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetRandROutputDisplayEXT" vkGetRandROutputDisplayEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult
#else
vkGetRandROutputDisplayEXT :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult
vkGetRandROutputDisplayEXT deviceCmds = mkVkGetRandROutputDisplayEXT (pVkGetRandROutputDisplayEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRandROutputDisplayEXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult)
#endif

type FN_vkGetRandROutputDisplayEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult
type PFN_vkGetRandROutputDisplayEXT = FunPtr FN_vkGetRandROutputDisplayEXT

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME"
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME = "VK_EXT_acquire_xlib_display"

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION"
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1
