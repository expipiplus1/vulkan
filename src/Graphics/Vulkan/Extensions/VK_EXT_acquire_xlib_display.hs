{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display  ( acquireXlibDisplayEXT
                                                               , getRandROutputDisplayEXT
                                                               , EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
                                                               , pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
                                                               , EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
                                                               , pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
                                                               , DisplayKHR(..)
                                                               , Display
                                                               , RROutput
                                                               ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.Storable (Storable(peek))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Extensions.WSITypes (Display)
import Graphics.Vulkan.Extensions.Handles (DisplayKHR)
import Graphics.Vulkan.Extensions.Handles (DisplayKHR(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkAcquireXlibDisplayEXT))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetRandROutputDisplayEXT))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Extensions.WSITypes (RROutput)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.WSITypes (Display)
import Graphics.Vulkan.Extensions.Handles (DisplayKHR(..))
import Graphics.Vulkan.Extensions.WSITypes (RROutput)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireXlibDisplayEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Display -> DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Display -> DisplayKHR -> IO Result

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
-- periods when the X11 server from which control was acquired itself loses
-- access to @display@. During such periods, operations which require
-- access to the display /must/ fail with an approriate error code. If the
-- X11 server associated with @dpy@ does not own @display@, or if
-- permission to access it has already been acquired by another entity, the
-- call /must/ return the error code
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'.
--
-- Note
--
-- One example of when an X11 server loses access to a display is when it
-- loses ownership of its virtual terminal.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.DisplayKHR',
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice'
acquireXlibDisplayEXT :: forall io . MonadIO io => PhysicalDevice -> ("dpy" ::: Ptr Display) -> DisplayKHR -> io ()
acquireXlibDisplayEXT physicalDevice dpy display = liftIO $ do
  let vkAcquireXlibDisplayEXT' = mkVkAcquireXlibDisplayEXT (pVkAcquireXlibDisplayEXT (instanceCmds (physicalDevice :: PhysicalDevice)))
  r <- vkAcquireXlibDisplayEXT' (physicalDeviceHandle (physicalDevice)) (dpy) (display)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRandROutputDisplayEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Display -> RROutput -> Ptr DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Display -> RROutput -> Ptr DisplayKHR -> IO Result

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
--     'Graphics.Vulkan.Extensions.Handles.DisplayKHR' handle will be
--     returned here.
--
-- = Description
--
-- If there is no 'Graphics.Vulkan.Extensions.Handles.DisplayKHR'
-- corresponding to @rrOutput@ on @physicalDevice@,
-- 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be returned in
-- @pDisplay@.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.DisplayKHR',
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice'
getRandROutputDisplayEXT :: forall io . MonadIO io => PhysicalDevice -> ("dpy" ::: Ptr Display) -> RROutput -> io (DisplayKHR)
getRandROutputDisplayEXT physicalDevice dpy rrOutput = liftIO . evalContT $ do
  let vkGetRandROutputDisplayEXT' = mkVkGetRandROutputDisplayEXT (pVkGetRandROutputDisplayEXT (instanceCmds (physicalDevice :: PhysicalDevice)))
  pPDisplay <- ContT $ bracket (callocBytes @DisplayKHR 8) free
  _ <- lift $ vkGetRandROutputDisplayEXT' (physicalDeviceHandle (physicalDevice)) (dpy) (rrOutput) (pPDisplay)
  pDisplay <- lift $ peek @DisplayKHR pPDisplay
  pure $ (pDisplay)


type EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION"
pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION = 1


type EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME = "VK_EXT_acquire_xlib_display"

-- No documentation found for TopLevel "VK_EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME"
pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME = "VK_EXT_acquire_xlib_display"

