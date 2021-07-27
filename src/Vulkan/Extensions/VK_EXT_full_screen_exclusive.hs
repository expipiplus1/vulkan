{-# language CPP #-}
-- | = Name
--
-- VK_EXT_full_screen_exclusive - device extension
--
-- == VK_EXT_full_screen_exclusive
--
-- [__Name String__]
--     @VK_EXT_full_screen_exclusive@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     256
--
-- [__Revision__]
--     4
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
--     -   Requires @VK_KHR_surface@
--
--     -   Requires @VK_KHR_get_surface_capabilities2@
--
--     -   Requires @VK_KHR_swapchain@
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_full_screen_exclusive:%20&body=@cubanismo%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-03-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with Vulkan 1.1
--
--     -   Interacts with @VK_KHR_device_group@
--
--     -   Interacts with @VK_KHR_win32_surface@
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, ARM
--
--     -   Slawomir Grajewski, Intel
--
--     -   Tobias Hector, AMD
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Joshua Schnarr, NVIDIA
--
--     -   Aaron Hagan, AMD
--
-- == Description
--
-- This extension allows applications to set the policy for swapchain
-- creation and presentation mechanisms relating to full-screen access.
-- Implementations may be able to acquire exclusive access to a particular
-- display for an application window that covers the whole screen. This can
-- increase performance on some systems by bypassing composition, however
-- it can also result in disruptive or expensive transitions in the
-- underlying windowing system when a change occurs.
--
-- Applications can choose between explicitly disallowing or allowing this
-- behavior, letting the implementation decide, or managing this mode of
-- operation directly using the new 'acquireFullScreenExclusiveModeEXT' and
-- 'releaseFullScreenExclusiveModeEXT' commands.
--
-- == New Commands
--
-- -   'acquireFullScreenExclusiveModeEXT'
--
-- -   'getPhysicalDeviceSurfacePresentModes2EXT'
--
-- -   'releaseFullScreenExclusiveModeEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>
-- is supported:
--
-- -   'getDeviceGroupSurfacePresentModes2EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   'getDeviceGroupSurfacePresentModes2EXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR',
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SurfaceFullScreenExclusiveInfoEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfaceCapabilitiesFullScreenExclusiveEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_win32_surface VK_KHR_win32_surface>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR',
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SurfaceFullScreenExclusiveWin32InfoEXT'
--
-- == New Enums
--
-- -   'FullScreenExclusiveEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME'
--
-- -   'EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_win32_surface VK_KHR_win32_surface>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT'
--
-- == Issues
--
-- 1) What should the extension & flag be called?
--
-- __RESOLVED__: VK_EXT_full_screen_exclusive.
--
-- Other options considered (prior to the app-controlled mode) were:
--
-- -   VK_EXT_smooth_fullscreen_transition
--
-- -   VK_EXT_fullscreen_behavior
--
-- -   VK_EXT_fullscreen_preference
--
-- -   VK_EXT_fullscreen_hint
--
-- -   VK_EXT_fast_fullscreen_transition
--
-- -   VK_EXT_avoid_fullscreen_exclusive
--
-- 2) Do we need more than a boolean toggle?
--
-- __RESOLVED__: Yes.
--
-- Using an enum with default\/allowed\/disallowed\/app-controlled enables
-- applications to accept driver default behavior, specifically override it
-- in either direction without implying the driver is ever required to use
-- full-screen exclusive mechanisms, or manage this mode explicitly.
--
-- 3) Should this be a KHR or EXT extension?
--
-- __RESOLVED__: EXT, in order to allow it to be shipped faster.
--
-- 4) Can the fullscreen hint affect the surface capabilities, and if so,
-- should the hint also be specified as input when querying the surface
-- capabilities?
--
-- __RESOLVED__: Yes on both accounts.
--
-- While the hint does not guarantee a particular fullscreen mode will be
-- used when the swapchain is created, it can sometimes imply particular
-- modes will NOT be used. If the driver determines that it will opt-out of
-- using a particular mode based on the policy, and knows it can only
-- support certain capabilities if that mode is used, it would be confusing
-- at best to the application to report those capabilities in such cases.
-- Not allowing implementations to report this state to applications could
-- result in situations where applications are unable to determine why
-- swapchain creation fails when they specify certain hint values, which
-- could result in never- terminating surface creation loops.
--
-- 5) Should full-screen be one word or two?
--
-- __RESOLVED__: Two words.
--
-- \"Fullscreen\" is not in my dictionary, and web searches did not turn up
-- definitive proof that it is a colloquially accepted compound word.
-- Documentation for the corresponding Windows API mechanisms dithers. The
-- text consistently uses a hyphen, but none-the-less, there is a
-- SetFullscreenState method in the DXGI swapchain object. Given this
-- inconclusive external guidance, it is best to adhere to the Vulkan style
-- guidelines and avoid inventing new compound words.
--
-- == Version History
--
-- -   Revision 4, 2019-03-12 (Tobias Hector)
--
--     -   Added application-controlled mode, and related functions
--
--     -   Tidied up appendix
--
-- -   Revision 3, 2019-01-03 (James Jones)
--
--     -   Renamed to VK_EXT_full_screen_exclusive
--
--     -   Made related adjustments to the tri-state enumerant names.
--
-- -   Revision 2, 2018-11-27 (James Jones)
--
--     -   Renamed to VK_KHR_fullscreen_behavior
--
--     -   Switched from boolean flag to tri-state enum
--
-- -   Revision 1, 2018-11-06 (James Jones)
--
--     -   Internal revision
--
-- = See Also
--
-- 'FullScreenExclusiveEXT', 'SurfaceCapabilitiesFullScreenExclusiveEXT',
-- 'SurfaceFullScreenExclusiveInfoEXT',
-- 'acquireFullScreenExclusiveModeEXT',
-- 'getPhysicalDeviceSurfacePresentModes2EXT',
-- 'releaseFullScreenExclusiveModeEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_full_screen_exclusive Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_full_screen_exclusive  ( getPhysicalDeviceSurfacePresentModes2EXT
                                                       , getDeviceGroupSurfacePresentModes2EXT
                                                       , acquireFullScreenExclusiveModeEXT
                                                       , releaseFullScreenExclusiveModeEXT
                                                       , SurfaceFullScreenExclusiveInfoEXT(..)
                                                       , SurfaceFullScreenExclusiveWin32InfoEXT(..)
                                                       , SurfaceCapabilitiesFullScreenExclusiveEXT(..)
                                                       , FullScreenExclusiveEXT( FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT
                                                                               , FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT
                                                                               , FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT
                                                                               , FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT
                                                                               , ..
                                                                               )
                                                       , EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
                                                       , pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
                                                       , EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
                                                       , pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
                                                       , HMONITOR
                                                       , SurfaceKHR(..)
                                                       , SwapchainKHR(..)
                                                       , PhysicalDeviceSurfaceInfo2KHR(..)
                                                       , PresentModeKHR(..)
                                                       , DeviceGroupPresentModeFlagBitsKHR(..)
                                                       , DeviceGroupPresentModeFlagsKHR
                                                       ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAcquireFullScreenExclusiveModeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceGroupSurfacePresentModes2EXT))
import Vulkan.Dynamic (DeviceCmds(pVkReleaseFullScreenExclusiveModeEXT))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagsKHR)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfacePresentModes2EXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (PhysicalDeviceSurfaceInfo2KHR)
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagsKHR)
import Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (PhysicalDeviceSurfaceInfo2KHR(..))
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModes2EXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result

-- | vkGetPhysicalDeviceSurfacePresentModes2EXT - Query supported
-- presentation modes
--
-- = Description
--
-- 'getPhysicalDeviceSurfacePresentModes2EXT' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR',
-- with the ability to specify extended inputs via chained input
-- structures.
--
-- == Valid Usage
--
-- -   [[VUID-{refpage}-pSurfaceInfo-06210]] @pSurfaceInfo->surface@ /must/
--     be supported by @physicalDevice@, as reported by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
--     or an equivalent platform-specific mechanism
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModes2EXT-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModes2EXT-pSurfaceInfo-parameter#
--     @pSurfaceInfo@ /must/ be a valid pointer to a valid
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
--     structure
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModes2EXT-pPresentModeCount-parameter#
--     @pPresentModeCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModes2EXT-pPresentModes-parameter#
--     If the value referenced by @pPresentModeCount@ is not @0@, and
--     @pPresentModes@ is not @NULL@, @pPresentModes@ /must/ be a valid
--     pointer to an array of @pPresentModeCount@
--     'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' values
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR'
getPhysicalDeviceSurfacePresentModes2EXT :: forall a io
                                          . (Extendss PhysicalDeviceSurfaceInfo2KHR a, PokeChain a, MonadIO io)
                                         => -- | @physicalDevice@ is the physical device that will be associated with the
                                            -- swapchain to be created, as described for
                                            -- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                            PhysicalDevice
                                         -> -- | @pSurfaceInfo@ is a pointer to a
                                            -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
                                            -- structure describing the surface and other fixed parameters that would
                                            -- be consumed by 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                            (PhysicalDeviceSurfaceInfo2KHR a)
                                         -> io (Result, ("presentModes" ::: Vector PresentModeKHR))
getPhysicalDeviceSurfacePresentModes2EXT physicalDevice surfaceInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfacePresentModes2EXTPtr = pVkGetPhysicalDeviceSurfacePresentModes2EXT (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfacePresentModes2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfacePresentModes2EXT is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfacePresentModes2EXT' = mkVkGetPhysicalDeviceSurfacePresentModes2EXT vkGetPhysicalDeviceSurfacePresentModes2EXTPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pSurfaceInfo <- ContT $ withCStruct (surfaceInfo)
  let x9 = forgetExtensions pSurfaceInfo
  pPPresentModeCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfacePresentModes2EXT" (vkGetPhysicalDeviceSurfacePresentModes2EXT' physicalDevice' x9 (pPPresentModeCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPresentModeCount <- lift $ peek @Word32 pPPresentModeCount
  pPPresentModes <- ContT $ bracket (callocBytes @PresentModeKHR ((fromIntegral (pPresentModeCount)) * 4)) free
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfacePresentModes2EXT" (vkGetPhysicalDeviceSurfacePresentModes2EXT' physicalDevice' x9 (pPPresentModeCount) (pPPresentModes))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPresentModeCount' <- lift $ peek @Word32 pPPresentModeCount
  pPresentModes' <- lift $ generateM (fromIntegral (pPresentModeCount')) (\i -> peek @PresentModeKHR ((pPPresentModes `advancePtrBytes` (4 * (i)) :: Ptr PresentModeKHR)))
  pure $ ((r'), pPresentModes')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupSurfacePresentModes2EXT
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr DeviceGroupPresentModeFlagsKHR -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr DeviceGroupPresentModeFlagsKHR -> IO Result

-- | vkGetDeviceGroupSurfacePresentModes2EXT - Query device group present
-- capabilities for a surface
--
-- = Description
--
-- 'getDeviceGroupSurfacePresentModes2EXT' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupSurfacePresentModesKHR',
-- with the ability to specify extended inputs via chained input
-- structures.
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentModeFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
getDeviceGroupSurfacePresentModes2EXT :: forall a io
                                       . (Extendss PhysicalDeviceSurfaceInfo2KHR a, PokeChain a, MonadIO io)
                                      => -- | @device@ is the logical device.
                                         --
                                         -- #VUID-vkGetDeviceGroupSurfacePresentModes2EXT-device-parameter# @device@
                                         -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                         Device
                                      -> -- | @pSurfaceInfo@ is a pointer to a
                                         -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
                                         -- structure describing the surface and other fixed parameters that would
                                         -- be consumed by 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                         --
                                         -- #VUID-vkGetDeviceGroupSurfacePresentModes2EXT-pSurfaceInfo-parameter#
                                         -- @pSurfaceInfo@ /must/ be a valid pointer to a valid
                                         -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
                                         -- structure
                                         (PhysicalDeviceSurfaceInfo2KHR a)
                                      -> io (("modes" ::: DeviceGroupPresentModeFlagsKHR))
getDeviceGroupSurfacePresentModes2EXT device surfaceInfo = liftIO . evalContT $ do
  let vkGetDeviceGroupSurfacePresentModes2EXTPtr = pVkGetDeviceGroupSurfacePresentModes2EXT (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceGroupSurfacePresentModes2EXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceGroupSurfacePresentModes2EXT is null" Nothing Nothing
  let vkGetDeviceGroupSurfacePresentModes2EXT' = mkVkGetDeviceGroupSurfacePresentModes2EXT vkGetDeviceGroupSurfacePresentModes2EXTPtr
  pSurfaceInfo <- ContT $ withCStruct (surfaceInfo)
  pPModes <- ContT $ bracket (callocBytes @DeviceGroupPresentModeFlagsKHR 4) free
  r <- lift $ traceAroundEvent "vkGetDeviceGroupSurfacePresentModes2EXT" (vkGetDeviceGroupSurfacePresentModes2EXT' (deviceHandle (device)) (forgetExtensions pSurfaceInfo) (pPModes))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pModes <- lift $ peek @DeviceGroupPresentModeFlagsKHR pPModes
  pure $ (pModes)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireFullScreenExclusiveModeEXT
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> IO Result) -> Ptr Device_T -> SwapchainKHR -> IO Result

-- | vkAcquireFullScreenExclusiveModeEXT - Acquire full-screen exclusive mode
-- for a swapchain
--
-- == Valid Usage
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-swapchain-02674#
--     @swapchain@ /must/ not be in the retired state
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-swapchain-02675#
--     @swapchain@ /must/ be a swapchain created with a
--     'SurfaceFullScreenExclusiveInfoEXT' structure, with
--     @fullScreenExclusive@ set to
--     'FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-swapchain-02676#
--     @swapchain@ /must/ not currently have exclusive full-screen access
--
-- A return value of 'Vulkan.Core10.Enums.Result.SUCCESS' indicates that
-- the @swapchain@ successfully acquired exclusive full-screen access. The
-- swapchain will retain this exclusivity until either the application
-- releases exclusive full-screen access with
-- 'releaseFullScreenExclusiveModeEXT', destroys the swapchain, or if any
-- of the swapchain commands return
-- 'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
-- indicating that the mode was lost because of platform-specific changes.
--
-- If the swapchain was unable to acquire exclusive full-screen access to
-- the display then
-- 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED' is returned. An
-- application /can/ attempt to acquire exclusive full-screen access again
-- for the same swapchain even if this command fails, or if
-- 'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
-- has been returned by a swapchain command.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkAcquireFullScreenExclusiveModeEXT-commonparent# Both of
--     @device@, and @swapchain@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Instance'
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.SwapchainKHR'
acquireFullScreenExclusiveModeEXT :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the device associated with @swapchain@.
                                     Device
                                  -> -- | @swapchain@ is the swapchain to acquire exclusive full-screen access
                                     -- for.
                                     SwapchainKHR
                                  -> io ()
acquireFullScreenExclusiveModeEXT device swapchain = liftIO $ do
  let vkAcquireFullScreenExclusiveModeEXTPtr = pVkAcquireFullScreenExclusiveModeEXT (deviceCmds (device :: Device))
  unless (vkAcquireFullScreenExclusiveModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireFullScreenExclusiveModeEXT is null" Nothing Nothing
  let vkAcquireFullScreenExclusiveModeEXT' = mkVkAcquireFullScreenExclusiveModeEXT vkAcquireFullScreenExclusiveModeEXTPtr
  r <- traceAroundEvent "vkAcquireFullScreenExclusiveModeEXT" (vkAcquireFullScreenExclusiveModeEXT' (deviceHandle (device)) (swapchain))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseFullScreenExclusiveModeEXT
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> IO Result) -> Ptr Device_T -> SwapchainKHR -> IO Result

-- | vkReleaseFullScreenExclusiveModeEXT - Release full-screen exclusive mode
-- from a swapchain
--
-- = Description
--
-- Note
--
-- Applications will not be able to present to @swapchain@ after this call
-- until exclusive full-screen access is reacquired. This is usually useful
-- to handle when an application is minimised or otherwise intends to stop
-- presenting for a time.
--
-- == Valid Usage
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.SwapchainKHR'
releaseFullScreenExclusiveModeEXT :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the device associated with @swapchain@.
                                     Device
                                  -> -- | @swapchain@ is the swapchain to release exclusive full-screen access
                                     -- from.
                                     --
                                     -- #VUID-vkReleaseFullScreenExclusiveModeEXT-swapchain-02677# @swapchain@
                                     -- /must/ not be in the retired state
                                     --
                                     -- #VUID-vkReleaseFullScreenExclusiveModeEXT-swapchain-02678# @swapchain@
                                     -- /must/ be a swapchain created with a 'SurfaceFullScreenExclusiveInfoEXT'
                                     -- structure, with @fullScreenExclusive@ set to
                                     -- 'FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
                                     SwapchainKHR
                                  -> io ()
releaseFullScreenExclusiveModeEXT device swapchain = liftIO $ do
  let vkReleaseFullScreenExclusiveModeEXTPtr = pVkReleaseFullScreenExclusiveModeEXT (deviceCmds (device :: Device))
  unless (vkReleaseFullScreenExclusiveModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseFullScreenExclusiveModeEXT is null" Nothing Nothing
  let vkReleaseFullScreenExclusiveModeEXT' = mkVkReleaseFullScreenExclusiveModeEXT vkReleaseFullScreenExclusiveModeEXTPtr
  r <- traceAroundEvent "vkReleaseFullScreenExclusiveModeEXT" (vkReleaseFullScreenExclusiveModeEXT' (deviceHandle (device)) (swapchain))
  when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkSurfaceFullScreenExclusiveInfoEXT - Structure specifying the preferred
-- full-screen transition behavior
--
-- = Description
--
-- If this structure is not present, @fullScreenExclusive@ is considered to
-- be 'FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'FullScreenExclusiveEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceFullScreenExclusiveInfoEXT = SurfaceFullScreenExclusiveInfoEXT
  { -- | @fullScreenExclusive@ is a 'FullScreenExclusiveEXT' value specifying the
    -- preferred full-screen transition behavior.
    --
    -- #VUID-VkSurfaceFullScreenExclusiveInfoEXT-fullScreenExclusive-parameter#
    -- @fullScreenExclusive@ /must/ be a valid 'FullScreenExclusiveEXT' value
    fullScreenExclusive :: FullScreenExclusiveEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceFullScreenExclusiveInfoEXT)
#endif
deriving instance Show SurfaceFullScreenExclusiveInfoEXT

instance ToCStruct SurfaceFullScreenExclusiveInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceFullScreenExclusiveInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FullScreenExclusiveEXT)) (fullScreenExclusive)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FullScreenExclusiveEXT)) (zero)
    f

instance FromCStruct SurfaceFullScreenExclusiveInfoEXT where
  peekCStruct p = do
    fullScreenExclusive <- peek @FullScreenExclusiveEXT ((p `plusPtr` 16 :: Ptr FullScreenExclusiveEXT))
    pure $ SurfaceFullScreenExclusiveInfoEXT
             fullScreenExclusive

instance Storable SurfaceFullScreenExclusiveInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceFullScreenExclusiveInfoEXT where
  zero = SurfaceFullScreenExclusiveInfoEXT
           zero


-- | VkSurfaceFullScreenExclusiveWin32InfoEXT - Structure specifying
-- additional creation parameters specific to Win32 fullscreen exclusive
-- mode
--
-- = Description
--
-- Note
--
-- If @hmonitor@ is invalidated (e.g. the monitor is unplugged) during the
-- lifetime of a swapchain created with this structure, operations on that
-- swapchain will return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'.
--
-- Note
--
-- It is the responsibility of the application to change the display
-- settings of the targeted Win32 display using the appropriate platform
-- APIs. Such changes /may/ alter the surface capabilities reported for the
-- created surface.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceFullScreenExclusiveWin32InfoEXT = SurfaceFullScreenExclusiveWin32InfoEXT
  { -- | @hmonitor@ is the Win32 'HMONITOR' handle identifying the display to
    -- create the surface with.
    --
    -- #VUID-VkSurfaceFullScreenExclusiveWin32InfoEXT-hmonitor-02673#
    -- @hmonitor@ /must/ be a valid 'HMONITOR'
    hmonitor :: HMONITOR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceFullScreenExclusiveWin32InfoEXT)
#endif
deriving instance Show SurfaceFullScreenExclusiveWin32InfoEXT

instance ToCStruct SurfaceFullScreenExclusiveWin32InfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceFullScreenExclusiveWin32InfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HMONITOR)) (hmonitor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HMONITOR)) (zero)
    f

instance FromCStruct SurfaceFullScreenExclusiveWin32InfoEXT where
  peekCStruct p = do
    hmonitor <- peek @HMONITOR ((p `plusPtr` 16 :: Ptr HMONITOR))
    pure $ SurfaceFullScreenExclusiveWin32InfoEXT
             hmonitor

instance Storable SurfaceFullScreenExclusiveWin32InfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceFullScreenExclusiveWin32InfoEXT where
  zero = SurfaceFullScreenExclusiveWin32InfoEXT
           zero


-- | VkSurfaceCapabilitiesFullScreenExclusiveEXT - Structure describing full
-- screen exclusive capabilities of a surface
--
-- = Description
--
-- This structure /can/ be included in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR'
-- to determine support for exclusive full-screen access. If
-- @fullScreenExclusiveSupported@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE', it indicates that exclusive
-- full-screen access is not obtainable for this surface.
--
-- Applications /must/ not attempt to create swapchains with
-- 'FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT' set if
-- @fullScreenExclusiveSupported@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceCapabilitiesFullScreenExclusiveEXT = SurfaceCapabilitiesFullScreenExclusiveEXT
  { -- No documentation found for Nested "VkSurfaceCapabilitiesFullScreenExclusiveEXT" "fullScreenExclusiveSupported"
    fullScreenExclusiveSupported :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilitiesFullScreenExclusiveEXT)
#endif
deriving instance Show SurfaceCapabilitiesFullScreenExclusiveEXT

instance ToCStruct SurfaceCapabilitiesFullScreenExclusiveEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilitiesFullScreenExclusiveEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fullScreenExclusiveSupported))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SurfaceCapabilitiesFullScreenExclusiveEXT where
  peekCStruct p = do
    fullScreenExclusiveSupported <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SurfaceCapabilitiesFullScreenExclusiveEXT
             (bool32ToBool fullScreenExclusiveSupported)

instance Storable SurfaceCapabilitiesFullScreenExclusiveEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceCapabilitiesFullScreenExclusiveEXT where
  zero = SurfaceCapabilitiesFullScreenExclusiveEXT
           zero


-- | VkFullScreenExclusiveEXT - Hint values an application can specify
-- affecting full-screen transition behavior
--
-- = See Also
--
-- 'SurfaceFullScreenExclusiveInfoEXT'
newtype FullScreenExclusiveEXT = FullScreenExclusiveEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT' indicates the implementation
-- /should/ determine the appropriate full-screen method by whatever means
-- it deems appropriate.
pattern FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT                = FullScreenExclusiveEXT 0
-- | 'FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT' indicates the implementation /may/
-- use full-screen exclusive mechanisms when available. Such mechanisms
-- /may/ result in better performance and\/or the availability of different
-- presentation capabilities, but /may/ require a more disruptive
-- transition during swapchain initialization, first presentation and\/or
-- destruction.
pattern FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT                = FullScreenExclusiveEXT 1
-- | 'FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT' indicates the implementation
-- /should/ avoid using full-screen mechanisms which rely on disruptive
-- transitions.
pattern FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT             = FullScreenExclusiveEXT 2
-- | 'FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT' indicates the
-- application will manage full-screen exclusive mode by using the
-- 'acquireFullScreenExclusiveModeEXT' and
-- 'releaseFullScreenExclusiveModeEXT' commands.
pattern FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = FullScreenExclusiveEXT 3
{-# complete FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT,
             FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT,
             FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT,
             FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT :: FullScreenExclusiveEXT #-}

conNameFullScreenExclusiveEXT :: String
conNameFullScreenExclusiveEXT = "FullScreenExclusiveEXT"

enumPrefixFullScreenExclusiveEXT :: String
enumPrefixFullScreenExclusiveEXT = "FULL_SCREEN_EXCLUSIVE_"

showTableFullScreenExclusiveEXT :: [(FullScreenExclusiveEXT, String)]
showTableFullScreenExclusiveEXT =
  [ (FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT               , "DEFAULT_EXT")
  , (FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT               , "ALLOWED_EXT")
  , (FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT            , "DISALLOWED_EXT")
  , (FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT, "APPLICATION_CONTROLLED_EXT")
  ]

instance Show FullScreenExclusiveEXT where
  showsPrec = enumShowsPrec enumPrefixFullScreenExclusiveEXT
                            showTableFullScreenExclusiveEXT
                            conNameFullScreenExclusiveEXT
                            (\(FullScreenExclusiveEXT x) -> x)
                            (showsPrec 11)

instance Read FullScreenExclusiveEXT where
  readPrec = enumReadPrec enumPrefixFullScreenExclusiveEXT
                          showTableFullScreenExclusiveEXT
                          conNameFullScreenExclusiveEXT
                          FullScreenExclusiveEXT


type EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION"
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 4


type EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = "VK_EXT_full_screen_exclusive"

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME"
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = "VK_EXT_full_screen_exclusive"


type HMONITOR = Ptr ()

