{-# language CPP #-}
-- | = Name
--
-- VK_EXT_acquire_xlib_display - instance extension
--
-- == VK_EXT_acquire_xlib_display
--
-- [__Name String__]
--     @VK_EXT_acquire_xlib_display@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     90
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_EXT_direct_mode_display@ to be enabled
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_acquire_xlib_display] @cubanismo%0A<<Here describe the issue or question you have about the VK_EXT_acquire_xlib_display extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Dave Airlie, Red Hat
--
--     -   Pierre Boudier, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   Damien Leone, NVIDIA
--
--     -   Pierre-Loup Griffais, Valve
--
--     -   Liam Middlebrook, NVIDIA
--
--     -   Daniel Vetter, Intel
--
-- == Description
--
-- This extension allows an application to take exclusive control on a
-- display currently associated with an X11 screen. When control is
-- acquired, the display will be deassociated from the X11 screen until
-- control is released or the specified display connection is closed.
-- Essentially, the X11 screen will behave as if the monitor has been
-- unplugged until control is released.
--
-- == New Commands
--
-- -   'acquireXlibDisplayEXT'
--
-- -   'getRandROutputDisplayEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ACQUIRE_XLIB_DISPLAY_EXTENSION_NAME'
--
-- -   'EXT_ACQUIRE_XLIB_DISPLAY_SPEC_VERSION'
--
-- == Issues
--
-- 1) Should 'acquireXlibDisplayEXT' take an RandR display ID, or a Vulkan
-- display handle as input?
--
-- __RESOLVED__: A Vulkan display handle. Otherwise there would be no way
-- to specify handles to displays that had been prevented from being
-- included in the X11 display list by some native platform or
-- vendor-specific mechanism.
--
-- 2) How does an application figure out which RandR display corresponds to
-- a Vulkan display?
--
-- __RESOLVED__: A new function, 'getRandROutputDisplayEXT', is introduced
-- for this purpose.
--
-- 3) Should 'getRandROutputDisplayEXT' be part of this extension, or a
-- general Vulkan \/ RandR or Vulkan \/ Xlib extension?
--
-- __RESOLVED__: To avoid yet another extension, include it in this
-- extension.
--
-- == Version History
--
-- -   Revision 1, 2016-12-13 (James Jones)
--
--     -   Initial draft
--
-- == See Also
--
-- 'acquireXlibDisplayEXT', 'getRandROutputDisplayEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_acquire_xlib_display Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

import Vulkan.Internal.Utils (traceAroundEvent)
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
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
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
-- access to the display /must/ fail with an appropriate error code. If the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_acquire_xlib_display VK_EXT_acquire_xlib_display>,
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
acquireXlibDisplayEXT :: forall io
                       . (MonadIO io)
                      => -- | @physicalDevice@ The physical device the display is on.
                         --
                         -- #VUID-vkAcquireXlibDisplayEXT-physicalDevice-parameter# @physicalDevice@
                         -- /must/ be a valid 'Vulkan.Core10.Handles.PhysicalDevice' handle
                         PhysicalDevice
                      -> -- | @dpy@ A connection to the X11 server that currently owns @display@.
                         --
                         -- #VUID-vkAcquireXlibDisplayEXT-dpy-parameter# @dpy@ /must/ be a valid
                         -- pointer to a 'Vulkan.Extensions.VK_KHR_xlib_surface.Display' value
                         ("dpy" ::: Ptr Display)
                      -> -- | @display@ The display the caller wishes to control in Vulkan.
                         --
                         -- #VUID-vkAcquireXlibDisplayEXT-display-parameter# @display@ /must/ be a
                         -- valid 'Vulkan.Extensions.Handles.DisplayKHR' handle
                         --
                         -- #VUID-vkAcquireXlibDisplayEXT-display-parent# @display@ /must/ have been
                         -- created, allocated, or retrieved from @physicalDevice@
                         DisplayKHR
                      -> io ()
acquireXlibDisplayEXT physicalDevice dpy display = liftIO $ do
  let vkAcquireXlibDisplayEXTPtr = pVkAcquireXlibDisplayEXT (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkAcquireXlibDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireXlibDisplayEXT is null" Nothing Nothing
  let vkAcquireXlibDisplayEXT' = mkVkAcquireXlibDisplayEXT vkAcquireXlibDisplayEXTPtr
  r <- traceAroundEvent "vkAcquireXlibDisplayEXT" (vkAcquireXlibDisplayEXT' (physicalDeviceHandle (physicalDevice)) (dpy) (display))
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_acquire_xlib_display VK_EXT_acquire_xlib_display>,
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getRandROutputDisplayEXT :: forall io
                          . (MonadIO io)
                         => -- | @physicalDevice@ The physical device to query the display handle on.
                            --
                            -- #VUID-vkGetRandROutputDisplayEXT-physicalDevice-parameter#
                            -- @physicalDevice@ /must/ be a valid
                            -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                            PhysicalDevice
                         -> -- | @dpy@ A connection to the X11 server from which @rrOutput@ was queried.
                            --
                            -- #VUID-vkGetRandROutputDisplayEXT-dpy-parameter# @dpy@ /must/ be a valid
                            -- pointer to a 'Vulkan.Extensions.VK_KHR_xlib_surface.Display' value
                            ("dpy" ::: Ptr Display)
                         -> -- | @rrOutput@ An X11 RandR output ID.
                            RROutput
                         -> io (DisplayKHR)
getRandROutputDisplayEXT physicalDevice dpy rrOutput = liftIO . evalContT $ do
  let vkGetRandROutputDisplayEXTPtr = pVkGetRandROutputDisplayEXT (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetRandROutputDisplayEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRandROutputDisplayEXT is null" Nothing Nothing
  let vkGetRandROutputDisplayEXT' = mkVkGetRandROutputDisplayEXT vkGetRandROutputDisplayEXTPtr
  pPDisplay <- ContT $ bracket (callocBytes @DisplayKHR 8) free
  r <- lift $ traceAroundEvent "vkGetRandROutputDisplayEXT" (vkGetRandROutputDisplayEXT' (physicalDeviceHandle (physicalDevice)) (dpy) (rrOutput) (pPDisplay))
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

