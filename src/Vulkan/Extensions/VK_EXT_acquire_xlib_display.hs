{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_acquire_xlib_display  ( acquireXlibDisplayEXT
                                                      , getRandROutputDisplayEXT
                                                      , EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
                                                      , pattern EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION
                                                      , EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
                                                      , pattern EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME
                                                      , RROutput
                                                      , DisplayKHR(..)
                                                      , Display
                                                      ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.Storable (Storable(peek))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.VK_KHR_xlib_surface (Display)
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Dynamic (InstanceCmds(pVkAcquireXlibDisplayEXT))
import Vulkan.Dynamic (InstanceCmds(pVkGetRandROutputDisplayEXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_xlib_surface (Display)
import Vulkan.Extensions.Handles (DisplayKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireXlibDisplayEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Display -> DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Display -> DisplayKHR -> IO Result

-- | vkAcquireXlibDisplayEXT - Acquire access to a VkDisplayKHR using Xlib
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
-- 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'.
--
-- Note
--
-- One example of when an X11 server loses access to a display is when it
-- loses ownership of its virtual terminal.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkAcquireXlibDisplayEXT-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkAcquireXlibDisplayEXT-dpy-parameter# @dpy@ /must/ be a valid
--     pointer to a 'Vulkan.Extensions.VK_KHR_xlib_surface.Display' value
--
-- -   #VUID-vkAcquireXlibDisplayEXT-display-parameter# @display@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.DisplayKHR' handle
--
-- -   #VUID-vkAcquireXlibDisplayEXT-display-parent# @display@ /must/ have
--     been created, allocated, or retrieved from @physicalDevice@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
acquireXlibDisplayEXT :: forall io
                       . (MonadIO io)
                      => -- | @physicalDevice@ The physical device the display is on.
                         PhysicalDevice
                      -> -- | @dpy@ A connection to the X11 server that currently owns @display@.
                         ("dpy" ::: Ptr Display)
                      -> -- | @display@ The display the caller wishes to control in Vulkan.
                         DisplayKHR
                      -> io ()
acquireXlibDisplayEXT physicalDevice dpy display = liftIO $ do
  let vkAcquireXlibDisplayEXTPtr = pVkAcquireXlibDisplayEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  unless (vkAcquireXlibDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireXlibDisplayEXT is null" Nothing Nothing
  let vkAcquireXlibDisplayEXT' = mkVkAcquireXlibDisplayEXT vkAcquireXlibDisplayEXTPtr
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
-- = Description
--
-- If there is no 'Vulkan.Extensions.Handles.DisplayKHR' corresponding to
-- @rrOutput@ on @physicalDevice@, 'Vulkan.Core10.APIConstants.NULL_HANDLE'
-- /must/ be returned in @pDisplay@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetRandROutputDisplayEXT-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetRandROutputDisplayEXT-dpy-parameter# @dpy@ /must/ be a
--     valid pointer to a 'Vulkan.Extensions.VK_KHR_xlib_surface.Display'
--     value
--
-- -   #VUID-vkGetRandROutputDisplayEXT-pDisplay-parameter# @pDisplay@
--     /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.DisplayKHR' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getRandROutputDisplayEXT :: forall io
                          . (MonadIO io)
                         => -- | @physicalDevice@ The physical device to query the display handle on.
                            PhysicalDevice
                         -> -- | @dpy@ A connection to the X11 server from which @rrOutput@ was queried.
                            ("dpy" ::: Ptr Display)
                         -> -- | @rrOutput@ An X11 RandR output ID.
                            RROutput
                         -> io (DisplayKHR)
getRandROutputDisplayEXT physicalDevice dpy rrOutput = liftIO . evalContT $ do
  let vkGetRandROutputDisplayEXTPtr = pVkGetRandROutputDisplayEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetRandROutputDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRandROutputDisplayEXT is null" Nothing Nothing
  let vkGetRandROutputDisplayEXT' = mkVkGetRandROutputDisplayEXT vkGetRandROutputDisplayEXTPtr
  pPDisplay <- ContT $ bracket (callocBytes @DisplayKHR 8) free
  r <- lift $ vkGetRandROutputDisplayEXT' (physicalDeviceHandle (physicalDevice)) (dpy) (rrOutput) (pPDisplay)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
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


type RROutput = Word64

