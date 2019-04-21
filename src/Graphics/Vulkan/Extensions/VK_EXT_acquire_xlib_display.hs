{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display
  ( acquireXlibDisplayEXT
  , getRandROutputDisplayEXT
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
  , vkAcquireXlibDisplayEXT
  , vkGetRandROutputDisplayEXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
  , pattern VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
  )



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
acquireXlibDisplayEXT :: PhysicalDevice ->  DisplayKHR ->  IO (Display)
acquireXlibDisplayEXT = \(PhysicalDevice physicalDevice' commandTable) -> \display' -> alloca (\pDpy' -> vkAcquireXlibDisplayEXT commandTable physicalDevice' pDpy' display' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pDpy')))


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
getRandROutputDisplayEXT :: PhysicalDevice ->  RROutput ->  IO (Display, DisplayKHR)
getRandROutputDisplayEXT = \(PhysicalDevice physicalDevice' commandTable) -> \rrOutput' -> alloca (\pDisplay' -> alloca (\pDpy' -> vkGetRandROutputDisplayEXT commandTable physicalDevice' pDpy' rrOutput' pDisplay' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> peek pDpy'<*>peek pDisplay'))))
