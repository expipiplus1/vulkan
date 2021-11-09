{-# language CPP #-}
-- | = Name
--
-- VK_KHR_surface - instance extension
--
-- == VK_KHR_surface
--
-- [__Name String__]
--     @VK_KHR_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     1
--
-- [__Revision__]
--     25
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_surface] @cubanismo%0A<<Here describe the issue or question you have about the VK_KHR_surface extension>> >
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_surface] @ianelliottus%0A<<Here describe the issue or question you have about the VK_KHR_surface extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-08-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Patrick Doane, Blizzard
--
--     -   Ian Elliott, LunarG
--
--     -   Jesse Hall, Google
--
--     -   James Jones, NVIDIA
--
--     -   David Mao, AMD
--
--     -   Norbert Nopper, Freescale
--
--     -   Alon Or-bach, Samsung
--
--     -   Daniel Rakos, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Jeff Vigil, Qualcomm
--
--     -   Chia-I Wu, LunarG
--
--     -   Jason Ekstrand, Intel
--
-- == Description
--
-- The @VK_KHR_surface@ extension is an instance extension. It introduces
-- 'Vulkan.Extensions.Handles.SurfaceKHR' objects, which abstract native
-- platform surface or window objects for use with Vulkan. It also provides
-- a way to determine whether a queue family in a physical device supports
-- presenting to particular surface.
--
-- Separate extensions for each platform provide the mechanisms for
-- creating 'Vulkan.Extensions.Handles.SurfaceKHR' objects, but once
-- created they may be used in this and other platform-independent
-- extensions, in particular the @VK_KHR_swapchain@ extension.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.SurfaceKHR'
--
-- == New Commands
--
-- -   'destroySurfaceKHR'
--
-- -   'getPhysicalDeviceSurfaceCapabilitiesKHR'
--
-- -   'getPhysicalDeviceSurfaceFormatsKHR'
--
-- -   'getPhysicalDeviceSurfacePresentModesKHR'
--
-- -   'getPhysicalDeviceSurfaceSupportKHR'
--
-- == New Structures
--
-- -   'SurfaceCapabilitiesKHR'
--
-- -   'SurfaceFormatKHR'
--
-- == New Enums
--
-- -   'ColorSpaceKHR'
--
-- -   'CompositeAlphaFlagBitsKHR'
--
-- -   'PresentModeKHR'
--
-- -   'SurfaceTransformFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'CompositeAlphaFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_SURFACE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
-- == Examples
--
-- Note
--
-- The example code for the @VK_KHR_surface@ and @VK_KHR_swapchain@
-- extensions was removed from the appendix after revision 1.0.29. This WSI
-- example code was ported to the cube demo that is shipped with the
-- official Khronos SDK, and is being kept up-to-date in that location
-- (see:
-- <https://github.com/KhronosGroup/Vulkan-Tools/blob/master/cube/cube.c>).
--
-- == Issues
--
-- 1) Should this extension include a method to query whether a physical
-- device supports presenting to a specific window or native surface on a
-- given platform?
--
-- __RESOLVED__: Yes. Without this, applications would need to create a
-- device instance to determine whether a particular window can be
-- presented to. Knowing that a device supports presentation to a platform
-- in general is not sufficient, as a single machine might support multiple
-- seats, or instances of the platform that each use different underlying
-- physical devices. Additionally, on some platforms, such as the X Window
-- System, different drivers and devices might be used for different
-- windows depending on which section of the desktop they exist on.
--
-- 2) Should the 'getPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'getPhysicalDeviceSurfaceFormatsKHR', and
-- 'getPhysicalDeviceSurfacePresentModesKHR' functions be in this extension
-- and operate on physical devices, rather than being in @VK_KHR_swapchain@
-- (i.e. device extension) and being dependent on
-- 'Vulkan.Core10.Handles.Device'?
--
-- __RESOLVED__: Yes. While it might be useful to depend on
-- 'Vulkan.Core10.Handles.Device' (and therefore on enabled extensions and
-- features) for the queries, Vulkan was released only with the
-- 'Vulkan.Core10.Handles.PhysicalDevice' versions. Many cases can be
-- resolved by a Valid Usage statement, and\/or by a separate @pNext@ chain
-- version of the query struct specific to a given extension or parameters,
-- via extensible versions of the queries:
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.getPhysicalDeviceSurfacePresentModes2EXT',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR',
-- and
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceFormats2KHR'.
--
-- 3) Should Vulkan support Xlib or XCB as the API for accessing the X
-- Window System platform?
--
-- __RESOLVED__: Both. XCB is a more modern and efficient API, but Xlib
-- usage is deeply ingrained in many applications and likely will remain in
-- use for the foreseeable future. Not all drivers necessarily need to
-- support both, but including both as options in the core specification
-- will probably encourage support, which should in turn ease adoption of
-- the Vulkan API in older codebases. Additionally, the performance
-- improvements possible with XCB likely will not have a measurable impact
-- on the performance of Vulkan presentation and other minimal window
-- system interactions defined here.
--
-- 4) Should the GBM platform be included in the list of platform enums?
--
-- __RESOLVED__: Deferred, and will be addressed with a platform-specific
-- extension to be written in the future.
--
-- == Version History
--
-- -   Revision 1, 2015-05-20 (James Jones)
--
--     -   Initial draft, based on LunarG KHR spec, other KHR specs,
--         patches attached to bugs.
--
-- -   Revision 2, 2015-05-22 (Ian Elliott)
--
--     -   Created initial Description section.
--
--     -   Removed query for whether a platform requires the use of a queue
--         for presentation, since it was decided that presentation will
--         always be modeled as being part of the queue.
--
--     -   Fixed typos and other minor mistakes.
--
-- -   Revision 3, 2015-05-26 (Ian Elliott)
--
--     -   Improved the Description section.
--
-- -   Revision 4, 2015-05-27 (James Jones)
--
--     -   Fixed compilation errors in example code.
--
-- -   Revision 5, 2015-06-01 (James Jones)
--
--     -   Added issues 1 and 2 and made related spec updates.
--
-- -   Revision 6, 2015-06-01 (James Jones)
--
--     -   Merged the platform type mappings table previously removed from
--         VK_KHR_swapchain with the platform description table in this
--         spec.
--
--     -   Added issues 3 and 4 documenting choices made when building the
--         initial list of native platforms supported.
--
-- -   Revision 7, 2015-06-11 (Ian Elliott)
--
--     -   Updated table 1 per input from the KHR TSG.
--
--     -   Updated issue 4 (GBM) per discussion with Daniel Stone. He will
--         create a platform-specific extension sometime in the future.
--
-- -   Revision 8, 2015-06-17 (James Jones)
--
--     -   Updated enum-extending values using new convention.
--
--     -   Fixed the value of VK_SURFACE_PLATFORM_INFO_TYPE_SUPPORTED_KHR.
--
-- -   Revision 9, 2015-06-17 (James Jones)
--
--     -   Rebased on Vulkan API version 126.
--
-- -   Revision 10, 2015-06-18 (James Jones)
--
--     -   Marked issues 2 and 3 resolved.
--
-- -   Revision 11, 2015-06-23 (Ian Elliott)
--
--     -   Examples now show use of function pointers for extension
--         functions.
--
--     -   Eliminated extraneous whitespace.
--
-- -   Revision 12, 2015-07-07 (Daniel Rakos)
--
--     -   Added error section describing when each error is expected to be
--         reported.
--
--     -   Replaced the term “queue node index” with “queue family index”
--         in the spec as that is the agreed term to be used in the latest
--         version of the core header and spec.
--
--     -   Replaced bool32_t with VkBool32.
--
-- -   Revision 13, 2015-08-06 (Daniel Rakos)
--
--     -   Updated spec against latest core API header version.
--
-- -   Revision 14, 2015-08-20 (Ian Elliott)
--
--     -   Renamed this extension and all of its enumerations, types,
--         functions, etc. This makes it compliant with the proposed
--         standard for Vulkan extensions.
--
--     -   Switched from “revision” to “version”, including use of the
--         VK_MAKE_VERSION macro in the header file.
--
--     -   Did miscellaneous cleanup, etc.
--
-- -   Revision 15, 2015-08-20 (Ian Elliott—​porting a 2015-07-29 change
--     from James Jones)
--
--     -   Moved the surface transform enums here from VK_WSI_swapchain so
--         they could be reused by VK_WSI_display.
--
-- -   Revision 16, 2015-09-01 (James Jones)
--
--     -   Restore single-field revision number.
--
-- -   Revision 17, 2015-09-01 (James Jones)
--
--     -   Fix example code compilation errors.
--
-- -   Revision 18, 2015-09-26 (Jesse Hall)
--
--     -   Replaced VkSurfaceDescriptionKHR with the VkSurfaceKHR object,
--         which is created via layered extensions. Added
--         VkDestroySurfaceKHR.
--
-- -   Revision 19, 2015-09-28 (Jesse Hall)
--
--     -   Renamed from VK_EXT_KHR_swapchain to VK_EXT_KHR_surface.
--
-- -   Revision 20, 2015-09-30 (Jeff Vigil)
--
--     -   Add error result VK_ERROR_SURFACE_LOST_KHR.
--
-- -   Revision 21, 2015-10-15 (Daniel Rakos)
--
--     -   Updated the resolution of issue #2 and include the surface
--         capability queries in this extension.
--
--     -   Renamed SurfaceProperties to SurfaceCapabilities as it better
--         reflects that the values returned are the capabilities of the
--         surface on a particular device.
--
--     -   Other minor cleanup and consistency changes.
--
-- -   Revision 22, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_surface to VK_KHR_surface.
--
-- -   Revision 23, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to vkDestroySurfaceKHR.
--
-- -   Revision 24, 2015-11-10 (Jesse Hall)
--
--     -   Removed VkSurfaceTransformKHR. Use VkSurfaceTransformFlagBitsKHR
--         instead.
--
--     -   Rename VkSurfaceCapabilitiesKHR member maxImageArraySize to
--         maxImageArrayLayers.
--
-- -   Revision 25, 2016-01-14 (James Jones)
--
--     -   Moved VK_ERROR_NATIVE_WINDOW_IN_USE_KHR from the
--         VK_KHR_android_surface to the VK_KHR_surface extension.
--
-- -   2016-08-23 (Ian Elliott)
--
--     -   Update the example code, to not have so many characters per
--         line, and to split out a new example to show how to obtain
--         function pointers.
--
-- -   2016-08-25 (Ian Elliott)
--
--     -   A note was added at the beginning of the example code, stating
--         that it will be removed from future versions of the appendix.
--
-- == See Also
--
-- 'ColorSpaceKHR', 'CompositeAlphaFlagBitsKHR', 'CompositeAlphaFlagsKHR',
-- 'PresentModeKHR', 'SurfaceCapabilitiesKHR', 'SurfaceFormatKHR',
-- 'Vulkan.Extensions.Handles.SurfaceKHR', 'SurfaceTransformFlagBitsKHR',
-- 'destroySurfaceKHR', 'getPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'getPhysicalDeviceSurfaceFormatsKHR',
-- 'getPhysicalDeviceSurfacePresentModesKHR',
-- 'getPhysicalDeviceSurfaceSupportKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_surface  ( destroySurfaceKHR
                                         , getPhysicalDeviceSurfaceSupportKHR
                                         , getPhysicalDeviceSurfaceCapabilitiesKHR
                                         , getPhysicalDeviceSurfaceFormatsKHR
                                         , getPhysicalDeviceSurfacePresentModesKHR
                                         , pattern COLORSPACE_SRGB_NONLINEAR_KHR
                                         , SurfaceCapabilitiesKHR(..)
                                         , SurfaceFormatKHR(..)
                                         , PresentModeKHR( PRESENT_MODE_IMMEDIATE_KHR
                                                         , PRESENT_MODE_MAILBOX_KHR
                                                         , PRESENT_MODE_FIFO_KHR
                                                         , PRESENT_MODE_FIFO_RELAXED_KHR
                                                         , PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
                                                         , PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
                                                         , ..
                                                         )
                                         , ColorSpaceKHR( COLOR_SPACE_SRGB_NONLINEAR_KHR
                                                        , COLOR_SPACE_DISPLAY_NATIVE_AMD
                                                        , COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT
                                                        , COLOR_SPACE_PASS_THROUGH_EXT
                                                        , COLOR_SPACE_ADOBERGB_NONLINEAR_EXT
                                                        , COLOR_SPACE_ADOBERGB_LINEAR_EXT
                                                        , COLOR_SPACE_HDR10_HLG_EXT
                                                        , COLOR_SPACE_DOLBYVISION_EXT
                                                        , COLOR_SPACE_HDR10_ST2084_EXT
                                                        , COLOR_SPACE_BT2020_LINEAR_EXT
                                                        , COLOR_SPACE_BT709_NONLINEAR_EXT
                                                        , COLOR_SPACE_BT709_LINEAR_EXT
                                                        , COLOR_SPACE_DCI_P3_NONLINEAR_EXT
                                                        , COLOR_SPACE_DISPLAY_P3_LINEAR_EXT
                                                        , COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT
                                                        , COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
                                                        , ..
                                                        )
                                         , CompositeAlphaFlagsKHR
                                         , CompositeAlphaFlagBitsKHR( COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                                                                    , COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
                                                                    , COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
                                                                    , COMPOSITE_ALPHA_INHERIT_BIT_KHR
                                                                    , ..
                                                                    )
                                         , SurfaceTransformFlagsKHR
                                         , SurfaceTransformFlagBitsKHR( SURFACE_TRANSFORM_IDENTITY_BIT_KHR
                                                                      , SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
                                                                      , SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
                                                                      , SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
                                                                      , SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
                                                                      , SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
                                                                      , SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
                                                                      , SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
                                                                      , SURFACE_TRANSFORM_INHERIT_BIT_KHR
                                                                      , ..
                                                                      )
                                         , KHR_SURFACE_SPEC_VERSION
                                         , pattern KHR_SURFACE_SPEC_VERSION
                                         , KHR_SURFACE_EXTENSION_NAME
                                         , pattern KHR_SURFACE_EXTENSION_NAME
                                         , SurfaceKHR(..)
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
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkDestroySurfaceKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceCapabilitiesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceFormatsKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfacePresentModesKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceSupportKHR))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.Handles (SurfaceKHR)
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySurfaceKHR
  :: FunPtr (Ptr Instance_T -> SurfaceKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Instance_T -> SurfaceKHR -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroySurfaceKHR - Destroy a VkSurfaceKHR object
--
-- = Description
--
-- Destroying a 'Vulkan.Extensions.Handles.SurfaceKHR' merely severs the
-- connection between Vulkan and the native surface, and does not imply
-- destroying the native surface, closing a window, or similar behavior.
--
-- == Valid Usage
--
-- -   #VUID-vkDestroySurfaceKHR-surface-01266# All
--     'Vulkan.Extensions.Handles.SwapchainKHR' objects created for
--     @surface@ /must/ have been destroyed prior to destroying @surface@
--
-- -   #VUID-vkDestroySurfaceKHR-surface-01267# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @surface@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroySurfaceKHR-surface-01268# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @surface@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroySurfaceKHR-instance-parameter# @instance@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkDestroySurfaceKHR-surface-parameter# If @surface@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @surface@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   #VUID-vkDestroySurfaceKHR-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroySurfaceKHR-surface-parent# If @surface@ is a valid
--     handle, it /must/ have been created, allocated, or retrieved from
--     @instance@
--
-- == Host Synchronization
--
-- -   Host access to @surface@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Instance', 'Vulkan.Extensions.Handles.SurfaceKHR'
destroySurfaceKHR :: forall io
                   . (MonadIO io)
                  => -- | @instance@ is the instance used to create the surface.
                     Instance
                  -> -- | @surface@ is the surface to destroy.
                     SurfaceKHR
                  -> -- | @pAllocator@ is the allocator used for host memory allocated for the
                     -- surface object when there is no more specific allocator available (see
                     -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io ()
destroySurfaceKHR instance' surface allocator = liftIO . evalContT $ do
  let vkDestroySurfaceKHRPtr = pVkDestroySurfaceKHR (instanceCmds (instance' :: Instance))
  lift $ unless (vkDestroySurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroySurfaceKHR is null" Nothing Nothing
  let vkDestroySurfaceKHR' = mkVkDestroySurfaceKHR vkDestroySurfaceKHRPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroySurfaceKHR" (vkDestroySurfaceKHR' (instanceHandle (instance')) (surface) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceSupportKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> SurfaceKHR -> Ptr Bool32 -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> SurfaceKHR -> Ptr Bool32 -> IO Result

-- | vkGetPhysicalDeviceSurfaceSupportKHR - Query if presentation is
-- supported
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceSupportKHR-queueFamilyIndex-01269#
--     @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     for the given @physicalDevice@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceSupportKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceSupportKHR-surface-parameter#
--     @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceSupportKHR-pSupported-parameter#
--     @pSupported@ /must/ be a valid pointer to a
--     'Vulkan.Core10.FundamentalTypes.Bool32' value
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceSupportKHR-commonparent# Both of
--     @physicalDevice@, and @surface@ /must/ have been created, allocated,
--     or retrieved from the same 'Vulkan.Core10.Handles.Instance'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfaceSupportKHR :: forall io
                                    . (MonadIO io)
                                   => -- | @physicalDevice@ is the physical device.
                                      PhysicalDevice
                                   -> -- | @queueFamilyIndex@ is the queue family.
                                      ("queueFamilyIndex" ::: Word32)
                                   -> -- | @surface@ is the surface.
                                      SurfaceKHR
                                   -> io (("supported" ::: Bool))
getPhysicalDeviceSurfaceSupportKHR physicalDevice queueFamilyIndex surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceSupportKHRPtr = pVkGetPhysicalDeviceSurfaceSupportKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceSupportKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceSupportKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceSupportKHR' = mkVkGetPhysicalDeviceSurfaceSupportKHR vkGetPhysicalDeviceSurfaceSupportKHRPtr
  pPSupported <- ContT $ bracket (callocBytes @Bool32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfaceSupportKHR" (vkGetPhysicalDeviceSurfaceSupportKHR' (physicalDeviceHandle (physicalDevice)) (queueFamilyIndex) (surface) (pPSupported))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSupported <- lift $ peek @Bool32 pPSupported
  pure $ ((bool32ToBool pSupported))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilitiesKHR -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr SurfaceCapabilitiesKHR -> IO Result

-- | vkGetPhysicalDeviceSurfaceCapabilitiesKHR - Query surface capabilities
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilitiesKHR-surface-06211#
--     @surface@ /must/ be supported by @physicalDevice@, as reported by
--     'getPhysicalDeviceSurfaceSupportKHR' or an equivalent
--     platform-specific mechanism
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilitiesKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilitiesKHR-surface-parameter#
--     @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilitiesKHR-pSurfaceCapabilities-parameter#
--     @pSurfaceCapabilities@ /must/ be a valid pointer to a
--     'SurfaceCapabilitiesKHR' structure
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilitiesKHR-commonparent# Both
--     of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'SurfaceCapabilitiesKHR',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfaceCapabilitiesKHR :: forall io
                                         . (MonadIO io)
                                        => -- | @physicalDevice@ is the physical device that will be associated with the
                                           -- swapchain to be created, as described for
                                           -- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                           PhysicalDevice
                                        -> -- | @surface@ is the surface that will be associated with the swapchain.
                                           SurfaceKHR
                                        -> io (SurfaceCapabilitiesKHR)
getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceCapabilitiesKHRPtr = pVkGetPhysicalDeviceSurfaceCapabilitiesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceCapabilitiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceCapabilitiesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceCapabilitiesKHR' = mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR vkGetPhysicalDeviceSurfaceCapabilitiesKHRPtr
  pPSurfaceCapabilities <- ContT (withZeroCStruct @SurfaceCapabilitiesKHR)
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" (vkGetPhysicalDeviceSurfaceCapabilitiesKHR' (physicalDeviceHandle (physicalDevice)) (surface) (pPSurfaceCapabilities))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceCapabilities <- lift $ peekCStruct @SurfaceCapabilitiesKHR pPSurfaceCapabilities
  pure $ (pSurfaceCapabilities)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceFormatsKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr SurfaceFormatKHR -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr SurfaceFormatKHR -> IO Result

-- | vkGetPhysicalDeviceSurfaceFormatsKHR - Query color formats supported by
-- surface
--
-- = Description
--
-- If @pSurfaceFormats@ is @NULL@, then the number of format pairs
-- supported for the given @surface@ is returned in @pSurfaceFormatCount@.
-- Otherwise, @pSurfaceFormatCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pSurfaceFormats@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pSurfaceFormats@. If the value of
-- @pSurfaceFormatCount@ is less than the number of format pairs supported,
-- at most @pSurfaceFormatCount@ structures will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available format pairs were returned.
--
-- The number of format pairs supported /must/ be greater than or equal to
-- 1. @pSurfaceFormats@ /must/ not contain an entry whose value for
-- @format@ is 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'.
--
-- If @pSurfaceFormats@ includes an entry whose value for @colorSpace@ is
-- 'COLOR_SPACE_SRGB_NONLINEAR_KHR' and whose value for @format@ is a UNORM
-- (or SRGB) format and the corresponding SRGB (or UNORM) format is a color
-- renderable format for
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', then
-- @pSurfaceFormats@ /must/ also contain an entry with the same value for
-- @colorSpace@ and @format@ equal to the corresponding SRGB (or UNORM)
-- format.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormatsKHR-surface-06211# @surface@
--     /must/ be supported by @physicalDevice@, as reported by
--     'getPhysicalDeviceSurfaceSupportKHR' or an equivalent
--     platform-specific mechanism
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormatsKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormatsKHR-surface-parameter#
--     @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormatsKHR-pSurfaceFormatCount-parameter#
--     @pSurfaceFormatCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormatsKHR-pSurfaceFormats-parameter#
--     If the value referenced by @pSurfaceFormatCount@ is not @0@, and
--     @pSurfaceFormats@ is not @NULL@, @pSurfaceFormats@ /must/ be a valid
--     pointer to an array of @pSurfaceFormatCount@ 'SurfaceFormatKHR'
--     structures
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormatsKHR-commonparent# Both of
--     @physicalDevice@, and @surface@ /must/ have been created, allocated,
--     or retrieved from the same 'Vulkan.Core10.Handles.Instance'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'SurfaceFormatKHR',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfaceFormatsKHR :: forall io
                                    . (MonadIO io)
                                   => -- | @physicalDevice@ is the physical device that will be associated with the
                                      -- swapchain to be created, as described for
                                      -- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                      PhysicalDevice
                                   -> -- | @surface@ is the surface that will be associated with the swapchain.
                                      SurfaceKHR
                                   -> io (Result, ("surfaceFormats" ::: Vector SurfaceFormatKHR))
getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceFormatsKHRPtr = pVkGetPhysicalDeviceSurfaceFormatsKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfaceFormatsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceFormatsKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceFormatsKHR' = mkVkGetPhysicalDeviceSurfaceFormatsKHR vkGetPhysicalDeviceSurfaceFormatsKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPSurfaceFormatCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfaceFormatsKHR" (vkGetPhysicalDeviceSurfaceFormatsKHR' physicalDevice' (surface) (pPSurfaceFormatCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceFormatCount <- lift $ peek @Word32 pPSurfaceFormatCount
  pPSurfaceFormats <- ContT $ bracket (callocBytes @SurfaceFormatKHR ((fromIntegral (pSurfaceFormatCount)) * 8)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSurfaceFormats `advancePtrBytes` (i * 8) :: Ptr SurfaceFormatKHR) . ($ ())) [0..(fromIntegral (pSurfaceFormatCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfaceFormatsKHR" (vkGetPhysicalDeviceSurfaceFormatsKHR' physicalDevice' (surface) (pPSurfaceFormatCount) ((pPSurfaceFormats)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pSurfaceFormatCount' <- lift $ peek @Word32 pPSurfaceFormatCount
  pSurfaceFormats' <- lift $ generateM (fromIntegral (pSurfaceFormatCount')) (\i -> peekCStruct @SurfaceFormatKHR (((pPSurfaceFormats) `advancePtrBytes` (8 * (i)) :: Ptr SurfaceFormatKHR)))
  pure $ ((r'), pSurfaceFormats')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result) -> Ptr PhysicalDevice_T -> SurfaceKHR -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result

-- | vkGetPhysicalDeviceSurfacePresentModesKHR - Query supported presentation
-- modes
--
-- = Description
--
-- If @pPresentModes@ is @NULL@, then the number of presentation modes
-- supported for the given @surface@ is returned in @pPresentModeCount@.
-- Otherwise, @pPresentModeCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pPresentModes@ array, and on
-- return the variable is overwritten with the number of values actually
-- written to @pPresentModes@. If the value of @pPresentModeCount@ is less
-- than the number of presentation modes supported, at most
-- @pPresentModeCount@ values will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available modes were returned.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModesKHR-surface-06211#
--     @surface@ /must/ be supported by @physicalDevice@, as reported by
--     'getPhysicalDeviceSurfaceSupportKHR' or an equivalent
--     platform-specific mechanism
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModesKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModesKHR-surface-parameter#
--     @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModesKHR-pPresentModeCount-parameter#
--     @pPresentModeCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModesKHR-pPresentModes-parameter#
--     If the value referenced by @pPresentModeCount@ is not @0@, and
--     @pPresentModes@ is not @NULL@, @pPresentModes@ /must/ be a valid
--     pointer to an array of @pPresentModeCount@ 'PresentModeKHR' values
--
-- -   #VUID-vkGetPhysicalDeviceSurfacePresentModesKHR-commonparent# Both
--     of @physicalDevice@, and @surface@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Instance'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'PresentModeKHR',
-- 'Vulkan.Extensions.Handles.SurfaceKHR'
getPhysicalDeviceSurfacePresentModesKHR :: forall io
                                         . (MonadIO io)
                                        => -- | @physicalDevice@ is the physical device that will be associated with the
                                           -- swapchain to be created, as described for
                                           -- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                           PhysicalDevice
                                        -> -- | @surface@ is the surface that will be associated with the swapchain.
                                           SurfaceKHR
                                        -> io (Result, ("presentModes" ::: Vector PresentModeKHR))
getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfacePresentModesKHRPtr = pVkGetPhysicalDeviceSurfacePresentModesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceSurfacePresentModesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfacePresentModesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfacePresentModesKHR' = mkVkGetPhysicalDeviceSurfacePresentModesKHR vkGetPhysicalDeviceSurfacePresentModesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPresentModeCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfacePresentModesKHR" (vkGetPhysicalDeviceSurfacePresentModesKHR' physicalDevice' (surface) (pPPresentModeCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPresentModeCount <- lift $ peek @Word32 pPPresentModeCount
  pPPresentModes <- ContT $ bracket (callocBytes @PresentModeKHR ((fromIntegral (pPresentModeCount)) * 4)) free
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfacePresentModesKHR" (vkGetPhysicalDeviceSurfacePresentModesKHR' physicalDevice' (surface) (pPPresentModeCount) (pPPresentModes))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPresentModeCount' <- lift $ peek @Word32 pPPresentModeCount
  pPresentModes' <- lift $ generateM (fromIntegral (pPresentModeCount')) (\i -> peek @PresentModeKHR ((pPPresentModes `advancePtrBytes` (4 * (i)) :: Ptr PresentModeKHR)))
  pure $ ((r'), pPresentModes')


-- No documentation found for TopLevel "VK_COLORSPACE_SRGB_NONLINEAR_KHR"
pattern COLORSPACE_SRGB_NONLINEAR_KHR = COLOR_SPACE_SRGB_NONLINEAR_KHR


-- | VkSurfaceCapabilitiesKHR - Structure describing capabilities of a
-- surface
--
-- = Description
--
-- Note
--
-- Supported usage flags of a presentable image when using
-- 'PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR' or
-- 'PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR' presentation mode are
-- provided by
-- 'Vulkan.Extensions.VK_KHR_shared_presentable_image.SharedPresentSurfaceCapabilitiesKHR'::@sharedPresentSupportedUsageFlags@.
--
-- Note
--
-- Formulas such as min(N, @maxImageCount@) are not correct, since
-- @maxImageCount@ /may/ be zero.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'CompositeAlphaFlagsKHR', 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR',
-- 'SurfaceTransformFlagBitsKHR', 'SurfaceTransformFlagsKHR',
-- 'getPhysicalDeviceSurfaceCapabilitiesKHR'
data SurfaceCapabilitiesKHR = SurfaceCapabilitiesKHR
  { -- | @minImageCount@ is the minimum number of images the specified device
    -- supports for a swapchain created for the surface, and will be at least
    -- one.
    minImageCount :: Word32
  , -- | @maxImageCount@ is the maximum number of images the specified device
    -- supports for a swapchain created for the surface, and will be either 0,
    -- or greater than or equal to @minImageCount@. A value of 0 means that
    -- there is no limit on the number of images, though there /may/ be limits
    -- related to the total amount of memory used by presentable images.
    maxImageCount :: Word32
  , -- | @currentExtent@ is the current width and height of the surface, or the
    -- special value (0xFFFFFFFF, 0xFFFFFFFF) indicating that the surface size
    -- will be determined by the extent of a swapchain targeting the surface.
    currentExtent :: Extent2D
  , -- | @minImageExtent@ contains the smallest valid swapchain extent for the
    -- surface on the specified device. The @width@ and @height@ of the extent
    -- will each be less than or equal to the corresponding @width@ and
    -- @height@ of @currentExtent@, unless @currentExtent@ has the special
    -- value described above.
    minImageExtent :: Extent2D
  , -- | @maxImageExtent@ contains the largest valid swapchain extent for the
    -- surface on the specified device. The @width@ and @height@ of the extent
    -- will each be greater than or equal to the corresponding @width@ and
    -- @height@ of @minImageExtent@. The @width@ and @height@ of the extent
    -- will each be greater than or equal to the corresponding @width@ and
    -- @height@ of @currentExtent@, unless @currentExtent@ has the special
    -- value described above.
    maxImageExtent :: Extent2D
  , -- | @maxImageArrayLayers@ is the maximum number of layers presentable images
    -- /can/ have for a swapchain created for this device and surface, and will
    -- be at least one.
    maxImageArrayLayers :: Word32
  , -- | @supportedTransforms@ is a bitmask of 'SurfaceTransformFlagBitsKHR'
    -- indicating the presentation transforms supported for the surface on the
    -- specified device. At least one bit will be set.
    supportedTransforms :: SurfaceTransformFlagsKHR
  , -- | @currentTransform@ is 'SurfaceTransformFlagBitsKHR' value indicating the
    -- surface’s current transform relative to the presentation engine’s
    -- natural orientation.
    currentTransform :: SurfaceTransformFlagBitsKHR
  , -- | @supportedCompositeAlpha@ is a bitmask of 'CompositeAlphaFlagBitsKHR',
    -- representing the alpha compositing modes supported by the presentation
    -- engine for the surface on the specified device, and at least one bit
    -- will be set. Opaque composition /can/ be achieved in any alpha
    -- compositing mode by either using an image format that has no alpha
    -- component, or by ensuring that all pixels in the presentable images have
    -- an alpha value of 1.0.
    supportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- | @supportedUsageFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' representing
    -- the ways the application /can/ use the presentable images of a swapchain
    -- created with 'PresentModeKHR' set to 'PRESENT_MODE_IMMEDIATE_KHR',
    -- 'PRESENT_MODE_MAILBOX_KHR', 'PRESENT_MODE_FIFO_KHR' or
    -- 'PRESENT_MODE_FIFO_RELAXED_KHR' for the surface on the specified device.
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
    -- /must/ be included in the set. Implementations /may/ support additional
    -- usages.
    supportedUsageFlags :: ImageUsageFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilitiesKHR)
#endif
deriving instance Show SurfaceCapabilitiesKHR

instance ToCStruct SurfaceCapabilitiesKHR where
  withCStruct x f = allocaBytes 52 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilitiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (minImageCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (maxImageCount)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (currentExtent)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (minImageExtent)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (maxImageExtent)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxImageArrayLayers)
    poke ((p `plusPtr` 36 :: Ptr SurfaceTransformFlagsKHR)) (supportedTransforms)
    poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (currentTransform)
    poke ((p `plusPtr` 44 :: Ptr CompositeAlphaFlagsKHR)) (supportedCompositeAlpha)
    poke ((p `plusPtr` 48 :: Ptr ImageUsageFlags)) (supportedUsageFlags)
    f
  cStructSize = 52
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr SurfaceTransformFlagsKHR)) (zero)
    poke ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR)) (zero)
    poke ((p `plusPtr` 44 :: Ptr CompositeAlphaFlagsKHR)) (zero)
    poke ((p `plusPtr` 48 :: Ptr ImageUsageFlags)) (zero)
    f

instance FromCStruct SurfaceCapabilitiesKHR where
  peekCStruct p = do
    minImageCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    maxImageCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    currentExtent <- peekCStruct @Extent2D ((p `plusPtr` 8 :: Ptr Extent2D))
    minImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    maxImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    maxImageArrayLayers <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    supportedTransforms <- peek @SurfaceTransformFlagsKHR ((p `plusPtr` 36 :: Ptr SurfaceTransformFlagsKHR))
    currentTransform <- peek @SurfaceTransformFlagBitsKHR ((p `plusPtr` 40 :: Ptr SurfaceTransformFlagBitsKHR))
    supportedCompositeAlpha <- peek @CompositeAlphaFlagsKHR ((p `plusPtr` 44 :: Ptr CompositeAlphaFlagsKHR))
    supportedUsageFlags <- peek @ImageUsageFlags ((p `plusPtr` 48 :: Ptr ImageUsageFlags))
    pure $ SurfaceCapabilitiesKHR
             minImageCount maxImageCount currentExtent minImageExtent maxImageExtent maxImageArrayLayers supportedTransforms currentTransform supportedCompositeAlpha supportedUsageFlags

instance Storable SurfaceCapabilitiesKHR where
  sizeOf ~_ = 52
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceCapabilitiesKHR where
  zero = SurfaceCapabilitiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkSurfaceFormatKHR - Structure describing a supported swapchain
-- format-color space pair
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'ColorSpaceKHR', 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceFormat2KHR',
-- 'getPhysicalDeviceSurfaceFormatsKHR'
data SurfaceFormatKHR = SurfaceFormatKHR
  { -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' that is compatible
    -- with the specified surface.
    format :: Format
  , -- | @colorSpace@ is a presentation 'ColorSpaceKHR' that is compatible with
    -- the surface.
    colorSpace :: ColorSpaceKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceFormatKHR)
#endif
deriving instance Show SurfaceFormatKHR

instance ToCStruct SurfaceFormatKHR where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceFormatKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Format)) (format)
    poke ((p `plusPtr` 4 :: Ptr ColorSpaceKHR)) (colorSpace)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 4 :: Ptr ColorSpaceKHR)) (zero)
    f

instance FromCStruct SurfaceFormatKHR where
  peekCStruct p = do
    format <- peek @Format ((p `plusPtr` 0 :: Ptr Format))
    colorSpace <- peek @ColorSpaceKHR ((p `plusPtr` 4 :: Ptr ColorSpaceKHR))
    pure $ SurfaceFormatKHR
             format colorSpace

instance Storable SurfaceFormatKHR where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceFormatKHR where
  zero = SurfaceFormatKHR
           zero
           zero


-- | VkPresentModeKHR - Presentation mode supported for a surface
--
-- = Description
--
-- The supported
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' of the
-- presentable images of a swapchain created for a surface /may/ differ
-- depending on the presentation mode, and can be determined as per the
-- table below:
--
-- +----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
-- | Presentation mode                            | Image usage flags                                                                                                           |
-- +==============================================+=============================================================================================================================+
-- | 'PRESENT_MODE_IMMEDIATE_KHR'                 | 'SurfaceCapabilitiesKHR'::@supportedUsageFlags@                                                                             |
-- +----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_MAILBOX_KHR'                   | 'SurfaceCapabilitiesKHR'::@supportedUsageFlags@                                                                             |
-- +----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_FIFO_KHR'                      | 'SurfaceCapabilitiesKHR'::@supportedUsageFlags@                                                                             |
-- +----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_FIFO_RELAXED_KHR'              | 'SurfaceCapabilitiesKHR'::@supportedUsageFlags@                                                                             |
-- +----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'     | 'Vulkan.Extensions.VK_KHR_shared_presentable_image.SharedPresentSurfaceCapabilitiesKHR'::@sharedPresentSupportedUsageFlags@ |
-- +----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
-- | 'PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR' | 'Vulkan.Extensions.VK_KHR_shared_presentable_image.SharedPresentSurfaceCapabilitiesKHR'::@sharedPresentSupportedUsageFlags@ |
-- +----------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+
--
-- Presentable image usage queries
--
-- Note
--
-- For reference, the mode indicated by 'PRESENT_MODE_FIFO_KHR' is
-- equivalent to the behavior of {wgl|glX|egl}SwapBuffers with a swap
-- interval of 1, while the mode indicated by
-- 'PRESENT_MODE_FIFO_RELAXED_KHR' is equivalent to the behavior of
-- {wgl|glX}SwapBuffers with a swap interval of -1 (from the
-- {WGL|GLX}_EXT_swap_control_tear extensions).
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.getPhysicalDeviceSurfacePresentModes2EXT',
-- 'getPhysicalDeviceSurfacePresentModesKHR'
newtype PresentModeKHR = PresentModeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PRESENT_MODE_IMMEDIATE_KHR' specifies that the presentation engine does
-- not wait for a vertical blanking period to update the current image,
-- meaning this mode /may/ result in visible tearing. No internal queuing
-- of presentation requests is needed, as the requests are applied
-- immediately.
pattern PRESENT_MODE_IMMEDIATE_KHR                 = PresentModeKHR 0
-- | 'PRESENT_MODE_MAILBOX_KHR' specifies that the presentation engine waits
-- for the next vertical blanking period to update the current image.
-- Tearing /cannot/ be observed. An internal single-entry queue is used to
-- hold pending presentation requests. If the queue is full when a new
-- presentation request is received, the new request replaces the existing
-- entry, and any images associated with the prior entry become available
-- for re-use by the application. One request is removed from the queue and
-- processed during each vertical blanking period in which the queue is
-- non-empty.
pattern PRESENT_MODE_MAILBOX_KHR                   = PresentModeKHR 1
-- | 'PRESENT_MODE_FIFO_KHR' specifies that the presentation engine waits for
-- the next vertical blanking period to update the current image. Tearing
-- /cannot/ be observed. An internal queue is used to hold pending
-- presentation requests. New requests are appended to the end of the
-- queue, and one request is removed from the beginning of the queue and
-- processed during each vertical blanking period in which the queue is
-- non-empty. This is the only value of @presentMode@ that is /required/ to
-- be supported.
pattern PRESENT_MODE_FIFO_KHR                      = PresentModeKHR 2
-- | 'PRESENT_MODE_FIFO_RELAXED_KHR' specifies that the presentation engine
-- generally waits for the next vertical blanking period to update the
-- current image. If a vertical blanking period has already passed since
-- the last update of the current image then the presentation engine does
-- not wait for another vertical blanking period for the update, meaning
-- this mode /may/ result in visible tearing in this case. This mode is
-- useful for reducing visual stutter with an application that will mostly
-- present a new image before the next vertical blanking period, but may
-- occasionally be late, and present a new image just after the next
-- vertical blanking period. An internal queue is used to hold pending
-- presentation requests. New requests are appended to the end of the
-- queue, and one request is removed from the beginning of the queue and
-- processed during or after each vertical blanking period in which the
-- queue is non-empty.
pattern PRESENT_MODE_FIFO_RELAXED_KHR              = PresentModeKHR 3
-- | 'PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR' specifies that the
-- presentation engine and application have concurrent access to a single
-- image, which is referred to as a /shared presentable image/. The
-- presentation engine periodically updates the current image on its
-- regular refresh cycle. The application is only required to make one
-- initial presentation request, after which the presentation engine /must/
-- update the current image without any need for further presentation
-- requests. The application /can/ indicate the image contents have been
-- updated by making a presentation request, but this does not guarantee
-- the timing of when it will be updated. This mode /may/ result in visible
-- tearing if rendering to the image is not timed correctly.
pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = PresentModeKHR 1000111001
-- | 'PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR' specifies that the presentation
-- engine and application have concurrent access to a single image, which
-- is referred to as a /shared presentable image/. The presentation engine
-- is only required to update the current image after a new presentation
-- request is received. Therefore the application /must/ make a
-- presentation request whenever an update is required. However, the
-- presentation engine /may/ update the current image at any point, meaning
-- this mode /may/ result in visible tearing.
pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR     = PresentModeKHR 1000111000
{-# complete PRESENT_MODE_IMMEDIATE_KHR,
             PRESENT_MODE_MAILBOX_KHR,
             PRESENT_MODE_FIFO_KHR,
             PRESENT_MODE_FIFO_RELAXED_KHR,
             PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR,
             PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR :: PresentModeKHR #-}

conNamePresentModeKHR :: String
conNamePresentModeKHR = "PresentModeKHR"

enumPrefixPresentModeKHR :: String
enumPrefixPresentModeKHR = "PRESENT_MODE_"

showTablePresentModeKHR :: [(PresentModeKHR, String)]
showTablePresentModeKHR =
  [ (PRESENT_MODE_IMMEDIATE_KHR                , "IMMEDIATE_KHR")
  , (PRESENT_MODE_MAILBOX_KHR                  , "MAILBOX_KHR")
  , (PRESENT_MODE_FIFO_KHR                     , "FIFO_KHR")
  , (PRESENT_MODE_FIFO_RELAXED_KHR             , "FIFO_RELAXED_KHR")
  , (PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR, "SHARED_CONTINUOUS_REFRESH_KHR")
  , (PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR    , "SHARED_DEMAND_REFRESH_KHR")
  ]

instance Show PresentModeKHR where
  showsPrec = enumShowsPrec enumPrefixPresentModeKHR
                            showTablePresentModeKHR
                            conNamePresentModeKHR
                            (\(PresentModeKHR x) -> x)
                            (showsPrec 11)

instance Read PresentModeKHR where
  readPrec = enumReadPrec enumPrefixPresentModeKHR showTablePresentModeKHR conNamePresentModeKHR PresentModeKHR


-- | VkColorSpaceKHR - Supported color space of the presentation engine
--
-- = Description
--
-- Note
--
-- In the initial release of the @VK_KHR_surface@ and @VK_KHR_swapchain@
-- extensions, the token 'COLORSPACE_SRGB_NONLINEAR_KHR' was used. Starting
-- in the 2016-05-13 updates to the extension branches, matching release
-- 1.0.13 of the core API specification, 'COLOR_SPACE_SRGB_NONLINEAR_KHR'
-- is used instead for consistency with Vulkan naming rules. The older enum
-- is still available for backwards compatibility.
--
-- Note
--
-- In older versions of this extension 'COLOR_SPACE_DISPLAY_P3_LINEAR_EXT'
-- was misnamed
-- 'Vulkan.Extensions.VK_EXT_swapchain_colorspace.COLOR_SPACE_DCI_P3_LINEAR_EXT'.
-- This has been updated to indicate that it uses RGB color encoding, not
-- XYZ. The old name is deprecated but is maintained for backwards
-- compatibility.
--
-- The color components of non-linear color space swap chain images /must/
-- have had the appropriate transfer function applied. The color space
-- selected for the swap chain image will not affect the processing of data
-- written into the image by the implementation. Vulkan requires that all
-- implementations support the sRGB transfer function by use of an SRGB
-- pixel format. Other transfer functions, such as SMPTE 170M or SMPTE2084,
-- /can/ be performed by the application shader. This extension defines
-- enums for 'ColorSpaceKHR' that correspond to the following color spaces:
--
-- +--------------+----------+----------+----------+-------------+------------+
-- | Name         | Red      | Green    | Blue     | White-point | Transfer   |
-- |              | Primary  | Primary  | Primary  |             | function   |
-- +==============+==========+==========+==========+=============+============+
-- | DCI-P3       | 1.000,   | 0.000,   | 0.000,   | 0.3333,     | DCI P3     |
-- |              | 0.000    | 1.000    | 0.000    | 0.3333      |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | Display-P3   | 0.680,   | 0.265,   | 0.150,   | 0.3127,     | Display-P3 |
-- |              | 0.320    | 0.690    | 0.060    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | BT709        | 0.640,   | 0.300,   | 0.150,   | 0.3127,     | ITU (SMPTE |
-- |              | 0.330    | 0.600    | 0.060    | 0.3290      | 170M)      |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | sRGB         | 0.640,   | 0.300,   | 0.150,   | 0.3127,     | sRGB       |
-- |              | 0.330    | 0.600    | 0.060    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | extended     | 0.640,   | 0.300,   | 0.150,   | 0.3127,     | extended   |
-- | sRGB         | 0.330    | 0.600    | 0.060    | 0.3290      | sRGB       |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | HDR10_ST2084 | 0.708,   | 0.170,   | 0.131,   | 0.3127,     | ST2084 PQ  |
-- |              | 0.292    | 0.797    | 0.046    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | DOLBYVISION  | 0.708,   | 0.170,   | 0.131,   | 0.3127,     | ST2084 PQ  |
-- |              | 0.292    | 0.797    | 0.046    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | HDR10_HLG    | 0.708,   | 0.170,   | 0.131,   | 0.3127,     | HLG        |
-- |              | 0.292    | 0.797    | 0.046    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | AdobeRGB     | 0.640,   | 0.210,   | 0.150,   | 0.3127,     | AdobeRGB   |
-- |              | 0.330    | 0.710    | 0.060    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
--
-- Color Spaces and Attributes
--
-- The transfer functions are described in the “Transfer Functions” chapter
-- of the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- Except Display-P3 OETF, which is:
--
-- \[\begin{aligned}
-- E & =
--   \begin{cases}
--     1.055 \times L^{1 \over 2.4} - 0.055 & \text{for}\  0.0030186 \leq L \leq 1 \\
--     12.92 \times L                       & \text{for}\  0 \leq L < 0.0030186
--   \end{cases}
-- \end{aligned}\]
--
-- where L is the linear value of a color component and E is the encoded
-- value (as stored in the image in memory).
--
-- Note
--
-- For most uses, the sRGB OETF is equivalent.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'SurfaceFormatKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
newtype ColorSpaceKHR = ColorSpaceKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COLOR_SPACE_SRGB_NONLINEAR_KHR' specifies support for the sRGB color
-- space.
pattern COLOR_SPACE_SRGB_NONLINEAR_KHR          = ColorSpaceKHR 0
-- | 'COLOR_SPACE_DISPLAY_NATIVE_AMD' specifies support for the display’s
-- native color space. This matches the color space expectations of AMD’s
-- FreeSync2 standard, for displays supporting it.
pattern COLOR_SPACE_DISPLAY_NATIVE_AMD          = ColorSpaceKHR 1000213000
-- | 'COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT' specifies support for the
-- extended sRGB color space to be displayed using an sRGB EOTF.
pattern COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT = ColorSpaceKHR 1000104014
-- | 'COLOR_SPACE_PASS_THROUGH_EXT' specifies that color components are used
-- “as is”. This is intended to allow applications to supply data for color
-- spaces not described here.
pattern COLOR_SPACE_PASS_THROUGH_EXT            = ColorSpaceKHR 1000104013
-- | 'COLOR_SPACE_ADOBERGB_NONLINEAR_EXT' specifies support for the AdobeRGB
-- color space to be displayed using the Gamma 2.2 EOTF.
pattern COLOR_SPACE_ADOBERGB_NONLINEAR_EXT      = ColorSpaceKHR 1000104012
-- | 'COLOR_SPACE_ADOBERGB_LINEAR_EXT' specifies support for the AdobeRGB
-- color space to be displayed using a linear EOTF.
pattern COLOR_SPACE_ADOBERGB_LINEAR_EXT         = ColorSpaceKHR 1000104011
-- | 'COLOR_SPACE_HDR10_HLG_EXT' specifies support for the HDR10 (BT2020
-- color space) to be displayed using the Hybrid Log Gamma (HLG) EOTF.
pattern COLOR_SPACE_HDR10_HLG_EXT               = ColorSpaceKHR 1000104010
-- | 'COLOR_SPACE_DOLBYVISION_EXT' specifies support for the Dolby Vision
-- (BT2020 color space), proprietary encoding, to be displayed using the
-- SMPTE ST2084 EOTF.
pattern COLOR_SPACE_DOLBYVISION_EXT             = ColorSpaceKHR 1000104009
-- | 'COLOR_SPACE_HDR10_ST2084_EXT' specifies support for the HDR10 (BT2020
-- color) space to be displayed using the SMPTE ST2084 Perceptual Quantizer
-- (PQ) EOTF.
pattern COLOR_SPACE_HDR10_ST2084_EXT            = ColorSpaceKHR 1000104008
-- | 'COLOR_SPACE_BT2020_LINEAR_EXT' specifies support for the BT2020 color
-- space to be displayed using a linear EOTF.
pattern COLOR_SPACE_BT2020_LINEAR_EXT           = ColorSpaceKHR 1000104007
-- | 'COLOR_SPACE_BT709_NONLINEAR_EXT' specifies support for the BT709 color
-- space to be displayed using the SMPTE 170M EOTF.
pattern COLOR_SPACE_BT709_NONLINEAR_EXT         = ColorSpaceKHR 1000104006
-- | 'COLOR_SPACE_BT709_LINEAR_EXT' specifies support for the BT709 color
-- space to be displayed using a linear EOTF.
pattern COLOR_SPACE_BT709_LINEAR_EXT            = ColorSpaceKHR 1000104005
-- | 'COLOR_SPACE_DCI_P3_NONLINEAR_EXT' specifies support for the DCI-P3
-- color space to be displayed using the DCI-P3 EOTF. Note that values in
-- such an image are interpreted as XYZ encoded color data by the
-- presentation engine.
pattern COLOR_SPACE_DCI_P3_NONLINEAR_EXT        = ColorSpaceKHR 1000104004
-- | 'COLOR_SPACE_DISPLAY_P3_LINEAR_EXT' specifies support for the Display-P3
-- color space to be displayed using a linear EOTF.
pattern COLOR_SPACE_DISPLAY_P3_LINEAR_EXT       = ColorSpaceKHR 1000104003
-- | 'COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT' specifies support for the
-- extended sRGB color space to be displayed using a linear EOTF.
pattern COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT    = ColorSpaceKHR 1000104002
-- | 'COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT' specifies support for the
-- Display-P3 color space to be displayed using an sRGB-like EOTF (defined
-- below).
pattern COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT    = ColorSpaceKHR 1000104001
{-# complete COLOR_SPACE_SRGB_NONLINEAR_KHR,
             COLOR_SPACE_DISPLAY_NATIVE_AMD,
             COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT,
             COLOR_SPACE_PASS_THROUGH_EXT,
             COLOR_SPACE_ADOBERGB_NONLINEAR_EXT,
             COLOR_SPACE_ADOBERGB_LINEAR_EXT,
             COLOR_SPACE_HDR10_HLG_EXT,
             COLOR_SPACE_DOLBYVISION_EXT,
             COLOR_SPACE_HDR10_ST2084_EXT,
             COLOR_SPACE_BT2020_LINEAR_EXT,
             COLOR_SPACE_BT709_NONLINEAR_EXT,
             COLOR_SPACE_BT709_LINEAR_EXT,
             COLOR_SPACE_DCI_P3_NONLINEAR_EXT,
             COLOR_SPACE_DISPLAY_P3_LINEAR_EXT,
             COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT,
             COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT :: ColorSpaceKHR #-}

conNameColorSpaceKHR :: String
conNameColorSpaceKHR = "ColorSpaceKHR"

enumPrefixColorSpaceKHR :: String
enumPrefixColorSpaceKHR = "COLOR_SPACE_"

showTableColorSpaceKHR :: [(ColorSpaceKHR, String)]
showTableColorSpaceKHR =
  [ (COLOR_SPACE_SRGB_NONLINEAR_KHR         , "SRGB_NONLINEAR_KHR")
  , (COLOR_SPACE_DISPLAY_NATIVE_AMD         , "DISPLAY_NATIVE_AMD")
  , (COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT, "EXTENDED_SRGB_NONLINEAR_EXT")
  , (COLOR_SPACE_PASS_THROUGH_EXT           , "PASS_THROUGH_EXT")
  , (COLOR_SPACE_ADOBERGB_NONLINEAR_EXT     , "ADOBERGB_NONLINEAR_EXT")
  , (COLOR_SPACE_ADOBERGB_LINEAR_EXT        , "ADOBERGB_LINEAR_EXT")
  , (COLOR_SPACE_HDR10_HLG_EXT              , "HDR10_HLG_EXT")
  , (COLOR_SPACE_DOLBYVISION_EXT            , "DOLBYVISION_EXT")
  , (COLOR_SPACE_HDR10_ST2084_EXT           , "HDR10_ST2084_EXT")
  , (COLOR_SPACE_BT2020_LINEAR_EXT          , "BT2020_LINEAR_EXT")
  , (COLOR_SPACE_BT709_NONLINEAR_EXT        , "BT709_NONLINEAR_EXT")
  , (COLOR_SPACE_BT709_LINEAR_EXT           , "BT709_LINEAR_EXT")
  , (COLOR_SPACE_DCI_P3_NONLINEAR_EXT       , "DCI_P3_NONLINEAR_EXT")
  , (COLOR_SPACE_DISPLAY_P3_LINEAR_EXT      , "DISPLAY_P3_LINEAR_EXT")
  , (COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT   , "EXTENDED_SRGB_LINEAR_EXT")
  , (COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT   , "DISPLAY_P3_NONLINEAR_EXT")
  ]

instance Show ColorSpaceKHR where
  showsPrec = enumShowsPrec enumPrefixColorSpaceKHR
                            showTableColorSpaceKHR
                            conNameColorSpaceKHR
                            (\(ColorSpaceKHR x) -> x)
                            (showsPrec 11)

instance Read ColorSpaceKHR where
  readPrec = enumReadPrec enumPrefixColorSpaceKHR showTableColorSpaceKHR conNameColorSpaceKHR ColorSpaceKHR


type CompositeAlphaFlagsKHR = CompositeAlphaFlagBitsKHR

-- | VkCompositeAlphaFlagBitsKHR - Alpha compositing modes supported on a
-- device
--
-- = Description
--
-- These values are described as follows:
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'CompositeAlphaFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
newtype CompositeAlphaFlagBitsKHR = CompositeAlphaFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'COMPOSITE_ALPHA_OPAQUE_BIT_KHR': The alpha component, if it exists, of
-- the images is ignored in the compositing process. Instead, the image is
-- treated as if it has a constant alpha of 1.0.
pattern COMPOSITE_ALPHA_OPAQUE_BIT_KHR          = CompositeAlphaFlagBitsKHR 0x00000001
-- | 'COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR': The alpha component, if it
-- exists, of the images is respected in the compositing process. The
-- non-alpha components of the image are expected to already be multiplied
-- by the alpha component by the application.
pattern COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR  = CompositeAlphaFlagBitsKHR 0x00000002
-- | 'COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR': The alpha component, if it
-- exists, of the images is respected in the compositing process. The
-- non-alpha components of the image are not expected to already be
-- multiplied by the alpha component by the application; instead, the
-- compositor will multiply the non-alpha components of the image by the
-- alpha component during compositing.
pattern COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = CompositeAlphaFlagBitsKHR 0x00000004
-- | 'COMPOSITE_ALPHA_INHERIT_BIT_KHR': The way in which the presentation
-- engine treats the alpha component in the images is unknown to the Vulkan
-- API. Instead, the application is responsible for setting the composite
-- alpha blending mode using native window system commands. If the
-- application does not set the blending mode using native window system
-- commands, then a platform-specific default will be used.
pattern COMPOSITE_ALPHA_INHERIT_BIT_KHR         = CompositeAlphaFlagBitsKHR 0x00000008

conNameCompositeAlphaFlagBitsKHR :: String
conNameCompositeAlphaFlagBitsKHR = "CompositeAlphaFlagBitsKHR"

enumPrefixCompositeAlphaFlagBitsKHR :: String
enumPrefixCompositeAlphaFlagBitsKHR = "COMPOSITE_ALPHA_"

showTableCompositeAlphaFlagBitsKHR :: [(CompositeAlphaFlagBitsKHR, String)]
showTableCompositeAlphaFlagBitsKHR =
  [ (COMPOSITE_ALPHA_OPAQUE_BIT_KHR         , "OPAQUE_BIT_KHR")
  , (COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR , "PRE_MULTIPLIED_BIT_KHR")
  , (COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR, "POST_MULTIPLIED_BIT_KHR")
  , (COMPOSITE_ALPHA_INHERIT_BIT_KHR        , "INHERIT_BIT_KHR")
  ]

instance Show CompositeAlphaFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixCompositeAlphaFlagBitsKHR
                            showTableCompositeAlphaFlagBitsKHR
                            conNameCompositeAlphaFlagBitsKHR
                            (\(CompositeAlphaFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read CompositeAlphaFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixCompositeAlphaFlagBitsKHR
                          showTableCompositeAlphaFlagBitsKHR
                          conNameCompositeAlphaFlagBitsKHR
                          CompositeAlphaFlagBitsKHR


type SurfaceTransformFlagsKHR = SurfaceTransformFlagBitsKHR

-- | VkSurfaceTransformFlagBitsKHR - Presentation transforms supported on a
-- device
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>,
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM',
-- 'Vulkan.Extensions.VK_QCOM_rotated_copy_commands.CopyCommandTransformInfoQCOM',
-- 'Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM',
-- 'Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCapabilities2EXT',
-- 'SurfaceCapabilitiesKHR', 'SurfaceTransformFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
newtype SurfaceTransformFlagBitsKHR = SurfaceTransformFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SURFACE_TRANSFORM_IDENTITY_BIT_KHR' specifies that image content is
-- presented without being transformed.
pattern SURFACE_TRANSFORM_IDENTITY_BIT_KHR                     = SurfaceTransformFlagBitsKHR 0x00000001
-- | 'SURFACE_TRANSFORM_ROTATE_90_BIT_KHR' specifies that image content is
-- rotated 90 degrees clockwise.
pattern SURFACE_TRANSFORM_ROTATE_90_BIT_KHR                    = SurfaceTransformFlagBitsKHR 0x00000002
-- | 'SURFACE_TRANSFORM_ROTATE_180_BIT_KHR' specifies that image content is
-- rotated 180 degrees clockwise.
pattern SURFACE_TRANSFORM_ROTATE_180_BIT_KHR                   = SurfaceTransformFlagBitsKHR 0x00000004
-- | 'SURFACE_TRANSFORM_ROTATE_270_BIT_KHR' specifies that image content is
-- rotated 270 degrees clockwise.
pattern SURFACE_TRANSFORM_ROTATE_270_BIT_KHR                   = SurfaceTransformFlagBitsKHR 0x00000008
-- | 'SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR' specifies that image
-- content is mirrored horizontally.
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR            = SurfaceTransformFlagBitsKHR 0x00000010
-- | 'SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR' specifies that
-- image content is mirrored horizontally, then rotated 90 degrees
-- clockwise.
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR  = SurfaceTransformFlagBitsKHR 0x00000020
-- | 'SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR' specifies that
-- image content is mirrored horizontally, then rotated 180 degrees
-- clockwise.
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = SurfaceTransformFlagBitsKHR 0x00000040
-- | 'SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR' specifies that
-- image content is mirrored horizontally, then rotated 270 degrees
-- clockwise.
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = SurfaceTransformFlagBitsKHR 0x00000080
-- | 'SURFACE_TRANSFORM_INHERIT_BIT_KHR' specifies that the presentation
-- transform is not specified, and is instead determined by
-- platform-specific considerations and mechanisms outside Vulkan.
pattern SURFACE_TRANSFORM_INHERIT_BIT_KHR                      = SurfaceTransformFlagBitsKHR 0x00000100

conNameSurfaceTransformFlagBitsKHR :: String
conNameSurfaceTransformFlagBitsKHR = "SurfaceTransformFlagBitsKHR"

enumPrefixSurfaceTransformFlagBitsKHR :: String
enumPrefixSurfaceTransformFlagBitsKHR = "SURFACE_TRANSFORM_"

showTableSurfaceTransformFlagBitsKHR :: [(SurfaceTransformFlagBitsKHR, String)]
showTableSurfaceTransformFlagBitsKHR =
  [ (SURFACE_TRANSFORM_IDENTITY_BIT_KHR                    , "IDENTITY_BIT_KHR")
  , (SURFACE_TRANSFORM_ROTATE_90_BIT_KHR                   , "ROTATE_90_BIT_KHR")
  , (SURFACE_TRANSFORM_ROTATE_180_BIT_KHR                  , "ROTATE_180_BIT_KHR")
  , (SURFACE_TRANSFORM_ROTATE_270_BIT_KHR                  , "ROTATE_270_BIT_KHR")
  , (SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR           , "HORIZONTAL_MIRROR_BIT_KHR")
  , (SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR , "HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR")
  , (SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR, "HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR")
  , (SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR, "HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR")
  , (SURFACE_TRANSFORM_INHERIT_BIT_KHR                     , "INHERIT_BIT_KHR")
  ]

instance Show SurfaceTransformFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixSurfaceTransformFlagBitsKHR
                            showTableSurfaceTransformFlagBitsKHR
                            conNameSurfaceTransformFlagBitsKHR
                            (\(SurfaceTransformFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SurfaceTransformFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixSurfaceTransformFlagBitsKHR
                          showTableSurfaceTransformFlagBitsKHR
                          conNameSurfaceTransformFlagBitsKHR
                          SurfaceTransformFlagBitsKHR


type KHR_SURFACE_SPEC_VERSION = 25

-- No documentation found for TopLevel "VK_KHR_SURFACE_SPEC_VERSION"
pattern KHR_SURFACE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SURFACE_SPEC_VERSION = 25


type KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"

-- No documentation found for TopLevel "VK_KHR_SURFACE_EXTENSION_NAME"
pattern KHR_SURFACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"

