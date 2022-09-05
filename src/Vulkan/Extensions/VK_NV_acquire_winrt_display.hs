{-# language CPP #-}
-- | = Name
--
-- VK_NV_acquire_winrt_display - device extension
--
-- == VK_NV_acquire_winrt_display
--
-- [__Name String__]
--     @VK_NV_acquire_winrt_display@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     346
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_EXT_direct_mode_display@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Jeff Juliano
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_acquire_winrt_display] @jjuliano%0A<<Here describe the issue or question you have about the VK_NV_acquire_winrt_display extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-09-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Juliano, NVIDIA
--
-- == Description
--
-- This extension allows an application to take exclusive control of a
-- display on Windows 10 provided that the display is not already
-- controlled by a compositor. Examples of compositors include the Windows
-- desktop compositor, other applications using this Vulkan extension, and
-- applications that
-- <https://docs.microsoft.com/en-us/uwp/api/windows.devices.display.core.displaymanager.tryacquiretarget “Acquire”>
-- a
-- <https://docs.microsoft.com/en-us/uwp/api/windows.devices.display.core.displaytarget “DisplayTarget”>
-- using a <https://docs.microsoft.com/en-us/uwp/api/ “WinRT”> command such
-- as
-- <https://docs.microsoft.com/en-us/uwp/api/windows.devices.display.core.displaymanager.tryacquiretarget “winrt::Windows::Devices::Display::Core::DisplayManager.TryAcquireTarget()”>.
--
-- When control is acquired the application has exclusive access to the
-- display until control is released or the application terminates. An
-- application’s attempt to acquire is denied if a different application
-- has already acquired the display.
--
-- == New Commands
--
-- -   'acquireWinrtDisplayNV'
--
-- -   'getWinrtDisplayNV'
--
-- == New Enum Constants
--
-- -   'NV_ACQUIRE_WINRT_DISPLAY_EXTENSION_NAME'
--
-- -   'NV_ACQUIRE_WINRT_DISPLAY_SPEC_VERSION'
--
-- == Issues
--
-- 1) What should the platform substring be for this extension:
--
-- __RESOLVED__: The platform substring is “Winrt”.
--
-- The substring “Winrt” matches the fact that the OS API exposing the
-- acquire and release functionality is called “WinRT”.
--
-- The substring “Win32” is wrong because the related “WinRT” API is
-- explicitly __not__ a “Win32” API. “WinRT” is a competing API family to
-- the “Win32” API family.
--
-- The substring “Windows” is suboptimal because there could be more than
-- one relevant API on the Windows platform. There is preference to use the
-- more-specific substring “Winrt”.
--
-- 2) Should 'acquireWinrtDisplayNV' take a winRT DisplayTarget, or a
-- Vulkan display handle as input?
--
-- __RESOLVED__: A Vulkan display handle. This matches the design of
-- 'Vulkan.Extensions.VK_EXT_acquire_xlib_display.acquireXlibDisplayEXT'.
--
-- 3) Should the acquire command be platform-independent named
-- “vkAcquireDisplayNV”, or platform-specific named
-- “vkAcquireWinrtDisplayNV”?
--
-- __RESOLVED__: Add a platform-specific command.
--
-- The inputs to the Acquire command are all Vulkan types. None are WinRT
-- types. This opens the possibility of the winrt extension defining a
-- platform-independent acquire command.
--
-- The X11 acquire command does need to accept a platform-specific
-- parameter. This could be handled by adding to a platform-independent
-- acquire command a params struct to which platform-dependent types can be
-- chained by @pNext@ pointer.
--
-- The prevailing opinion is that it would be odd to create a second
-- platform-independent function that is used on the Windows 10 platform,
-- but that is not used for the X11 platform. Since a Windows 10
-- platform-specific command is needed anyway for converting between
-- vkDisplayKHR and platform-native handles, opinion was to create a
-- platform-specific acquire function.
--
-- 4) Should the 'getWinrtDisplayNV' parameter identifying a display be
-- named “deviceRelativeId” or “adapterRelativeId”?
--
-- __RESOLVED__: The WinRT name is “AdapterRelativeId”. The name “adapter”
-- is the Windows analog to a Vulkan “physical device”. Vulkan already has
-- precedent to use the name @deviceLUID@ for the concept that Windows APIs
-- call “AdapterLuid”. Keeping form with this precedent, the name
-- “deviceRelativeId” is chosen.
--
-- 5) Does 'acquireWinrtDisplayNV' cause the Windows desktop compositor to
-- release a display?
--
-- __RESOLVED__: No. 'acquireWinrtDisplayNV' does not itself cause the
-- Windows desktop compositor to release a display. This action must be
-- performed outside of Vulkan.
--
-- Beginning with Windows 10 version 2004 it is possible to cause the
-- Windows desktop compositor to release a display by using the “Advanced
-- display settings” sub-page of the “Display settings” control panel. See
-- <https://docs.microsoft.com/en-us/windows-hardware/drivers/display/specialized-monitors>
--
-- 6) Where can one find additional information about custom compositors
-- for Windows 10?
--
-- __RESOLVED__: Relevant references are as follows.
--
-- According to Microsoft’s documentation on
-- <https://docs.microsoft.com/en-us/windows-hardware/drivers/display/specialized-monitors-compositor \"building a custom compositor\">,
-- the ability to write a custom compositor is not a replacement for a
-- fullscreen desktop window. The feature is for writing compositor apps
-- that drive specialized hardware.
--
-- Only certain editions of Windows 10 support custom compositors,
-- <https://docs.microsoft.com/en-us/windows-hardware/drivers/display/specialized-monitors#windows-10-version-2004 \"documented here\">.
-- The product type can be queried from Windows 10. See
-- <https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getproductinfo>
--
-- == Version History
--
-- -   Revision 1, 2020-09-29 (Jeff Juliano)
--
--     -   Initial draft
--
-- == See Also
--
-- 'acquireWinrtDisplayNV', 'getWinrtDisplayNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_acquire_winrt_display Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_acquire_winrt_display  ( acquireWinrtDisplayNV
                                                      , getWinrtDisplayNV
                                                      , NV_ACQUIRE_WINRT_DISPLAY_SPEC_VERSION
                                                      , pattern NV_ACQUIRE_WINRT_DISPLAY_SPEC_VERSION
                                                      , NV_ACQUIRE_WINRT_DISPLAY_EXTENSION_NAME
                                                      , pattern NV_ACQUIRE_WINRT_DISPLAY_EXTENSION_NAME
                                                      , DisplayKHR(..)
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
import Data.Word (Word32)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Dynamic (InstanceCmds(pVkAcquireWinrtDisplayNV))
import Vulkan.Dynamic (InstanceCmds(pVkGetWinrtDisplayNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DisplayKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireWinrtDisplayNV
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayKHR -> IO Result

-- | vkAcquireWinrtDisplayNV - Acquire access to a VkDisplayKHR
--
-- = Description
--
-- All permissions necessary to control the display are granted to the
-- Vulkan instance associated with @physicalDevice@ until the display is
-- released or the application is terminated. Permission to access the
-- display /may/ be revoked by events that cause Windows 10 itself to lose
-- access to @display@. If this has happened, operations which require
-- access to the display /must/ fail with an appropriate error code. If
-- permission to access @display@ has already been acquired by another
-- entity, the call /must/ return the error code
-- 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'.
--
-- Note
--
-- The Vulkan instance acquires control of a
-- <https://docs.microsoft.com/en-us/uwp/api/windows.devices.display.core.displaytarget “winrt::Windows::Devices::Display::Core::DisplayTarget”>
-- by performing an operation equivalent to
-- <https://docs.microsoft.com/en-us/uwp/api/windows.devices.display.core.displaymanager.tryacquiretarget “winrt::Windows::Devices::Display::Core::DisplayManager.TryAcquireTarget()”>
-- on the “DisplayTarget”.
--
-- Note
--
-- One example of when Windows 10 loses access to a display is when the
-- display is hot-unplugged.
--
-- Note
--
-- One example of when a display has already been acquired by another
-- entity is when the Windows desktop compositor (DWM) is in control of the
-- display. Beginning with Windows 10 version 2004 it is possible to cause
-- DWM to release a display by using the “Advanced display settings”
-- sub-page of the “Display settings” control panel.
-- 'acquireWinrtDisplayNV' does not itself cause DWM to release a display;
-- this action must be performed outside of Vulkan.
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_acquire_winrt_display VK_NV_acquire_winrt_display>,
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
acquireWinrtDisplayNV :: forall io
                       . (MonadIO io)
                      => -- | @physicalDevice@ The physical device the display is on.
                         --
                         -- #VUID-vkAcquireWinrtDisplayNV-physicalDevice-parameter# @physicalDevice@
                         -- /must/ be a valid 'Vulkan.Core10.Handles.PhysicalDevice' handle
                         PhysicalDevice
                      -> -- | @display@ The display the caller wishes to control in Vulkan.
                         --
                         -- #VUID-vkAcquireWinrtDisplayNV-display-parameter# @display@ /must/ be a
                         -- valid 'Vulkan.Extensions.Handles.DisplayKHR' handle
                         --
                         -- #VUID-vkAcquireWinrtDisplayNV-display-parent# @display@ /must/ have been
                         -- created, allocated, or retrieved from @physicalDevice@
                         DisplayKHR
                      -> io ()
acquireWinrtDisplayNV physicalDevice display = liftIO $ do
  let vkAcquireWinrtDisplayNVPtr = pVkAcquireWinrtDisplayNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkAcquireWinrtDisplayNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireWinrtDisplayNV is null" Nothing Nothing
  let vkAcquireWinrtDisplayNV' = mkVkAcquireWinrtDisplayNV vkAcquireWinrtDisplayNVPtr
  r <- traceAroundEvent "vkAcquireWinrtDisplayNV" (vkAcquireWinrtDisplayNV' (physicalDeviceHandle (physicalDevice)) (display))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetWinrtDisplayNV
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr DisplayKHR -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> Ptr DisplayKHR -> IO Result

-- | vkGetWinrtDisplayNV - Query the VkDisplayKHR corresponding to a WinRT
-- DisplayTarget
--
-- = Description
--
-- If there is no 'Vulkan.Extensions.Handles.DisplayKHR' corresponding to
-- @deviceRelativeId@ on @physicalDevice@,
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be returned in
-- @pDisplay@.
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_acquire_winrt_display VK_NV_acquire_winrt_display>,
-- 'Vulkan.Extensions.Handles.DisplayKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getWinrtDisplayNV :: forall io
                   . (MonadIO io)
                  => -- | @physicalDevice@ The physical device on which to query the display
                     -- handle.
                     --
                     -- #VUID-vkGetWinrtDisplayNV-physicalDevice-parameter# @physicalDevice@
                     -- /must/ be a valid 'Vulkan.Core10.Handles.PhysicalDevice' handle
                     PhysicalDevice
                  -> -- | @deviceRelativeId@ The value of the
                     -- <https://docs.microsoft.com/en-us/uwp/api/windows.devices.display.core.displaytarget.adapterrelativeid “AdapterRelativeId”>
                     -- property of a
                     -- <https://docs.microsoft.com/en-us/uwp/api/windows.devices.display.core.displaytarget “DisplayTarget”>
                     -- that is enumerated by a
                     -- <https://docs.microsoft.com/en-us/uwp/api/windows.devices.display.core.displayadapter “DisplayAdapter”>
                     -- with an
                     -- <https://docs.microsoft.com/en-us/uwp/api/windows.devices.display.core.displayadapter.id “Id”>
                     -- property matching the @deviceLUID@ property of a
                     -- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'
                     -- for @physicalDevice@.
                     ("deviceRelativeId" ::: Word32)
                  -> io (DisplayKHR)
getWinrtDisplayNV physicalDevice deviceRelativeId = liftIO . evalContT $ do
  let vkGetWinrtDisplayNVPtr = pVkGetWinrtDisplayNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetWinrtDisplayNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetWinrtDisplayNV is null" Nothing Nothing
  let vkGetWinrtDisplayNV' = mkVkGetWinrtDisplayNV vkGetWinrtDisplayNVPtr
  pPDisplay <- ContT $ bracket (callocBytes @DisplayKHR 8) free
  r <- lift $ traceAroundEvent "vkGetWinrtDisplayNV" (vkGetWinrtDisplayNV' (physicalDeviceHandle (physicalDevice)) (deviceRelativeId) (pPDisplay))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDisplay <- lift $ peek @DisplayKHR pPDisplay
  pure $ (pDisplay)


type NV_ACQUIRE_WINRT_DISPLAY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_ACQUIRE_WINRT_DISPLAY_SPEC_VERSION"
pattern NV_ACQUIRE_WINRT_DISPLAY_SPEC_VERSION :: forall a . Integral a => a
pattern NV_ACQUIRE_WINRT_DISPLAY_SPEC_VERSION = 1


type NV_ACQUIRE_WINRT_DISPLAY_EXTENSION_NAME = "VK_NV_acquire_winrt_display"

-- No documentation found for TopLevel "VK_NV_ACQUIRE_WINRT_DISPLAY_EXTENSION_NAME"
pattern NV_ACQUIRE_WINRT_DISPLAY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_ACQUIRE_WINRT_DISPLAY_EXTENSION_NAME = "VK_NV_acquire_winrt_display"

