{-# language CPP #-}
-- | = Name
--
-- VK_KHR_get_surface_capabilities2 - instance extension
--
-- == VK_KHR_get_surface_capabilities2
--
-- [__Name String__]
--     @VK_KHR_get_surface_capabilities2@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     120
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_get_surface_capabilities2] @cubanismo%0A<<Here describe the issue or question you have about the VK_KHR_get_surface_capabilities2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-02-27
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ian Elliott, Google
--
--     -   James Jones, NVIDIA
--
--     -   Alon Or-bach, Samsung
--
-- == Description
--
-- This extension provides new entry points to query device surface
-- capabilities in a way that can be easily extended by other extensions,
-- without introducing any further entry points. This extension can be
-- considered the @VK_KHR_surface@ equivalent of the
-- @VK_KHR_get_physical_device_properties2@ extension.
--
-- == New Commands
--
-- -   'getPhysicalDeviceSurfaceCapabilities2KHR'
--
-- -   'getPhysicalDeviceSurfaceFormats2KHR'
--
-- == New Structures
--
-- -   'PhysicalDeviceSurfaceInfo2KHR'
--
-- -   'SurfaceCapabilities2KHR'
--
-- -   'SurfaceFormat2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME'
--
-- -   'KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR'
--
-- == Issues
--
-- 1) What should this extension be named?
--
-- __RESOLVED__: @VK_KHR_get_surface_capabilities2@. Other alternatives:
--
-- -   @VK_KHR_surface2@
--
-- -   One extension, combining a separate display-specific query
--     extension.
--
-- 2) Should additional WSI query functions be extended?
--
-- __RESOLVED__:
--
-- -   'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR':
--     Yes. The need for this motivated the extension.
--
-- -   'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR':
--     No. Currently only has boolean output. Extensions should instead
--     extend 'getPhysicalDeviceSurfaceCapabilities2KHR'.
--
-- -   'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR':
--     Yes.
--
-- -   'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR':
--     No. Recent discussion concluded this introduced too much variability
--     for applications to deal with. Extensions should instead extend
--     'getPhysicalDeviceSurfaceCapabilities2KHR'.
--
-- -   'Vulkan.Extensions.VK_KHR_xlib_surface.getPhysicalDeviceXlibPresentationSupportKHR':
--     Not in this extension.
--
-- -   'Vulkan.Extensions.VK_KHR_xcb_surface.getPhysicalDeviceXcbPresentationSupportKHR':
--     Not in this extension.
--
-- -   'Vulkan.Extensions.VK_KHR_wayland_surface.getPhysicalDeviceWaylandPresentationSupportKHR':
--     Not in this extension.
--
-- -   'Vulkan.Extensions.VK_KHR_win32_surface.getPhysicalDeviceWin32PresentationSupportKHR':
--     Not in this extension.
--
-- == Version History
--
-- -   Revision 1, 2017-02-27 (James Jones)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceSurfaceInfo2KHR', 'SurfaceCapabilities2KHR',
-- 'SurfaceFormat2KHR', 'getPhysicalDeviceSurfaceCapabilities2KHR',
-- 'getPhysicalDeviceSurfaceFormats2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_get_surface_capabilities2  ( getPhysicalDeviceSurfaceCapabilities2KHR
                                                           , getPhysicalDeviceSurfaceFormats2KHR
                                                           , PhysicalDeviceSurfaceInfo2KHR(..)
                                                           , SurfaceCapabilities2KHR(..)
                                                           , SurfaceFormat2KHR(..)
                                                           , KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
                                                           , pattern KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
                                                           , KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
                                                           , pattern KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
                                                           , SurfaceKHR(..)
                                                           , SurfaceCapabilitiesKHR(..)
                                                           , SurfaceFormatKHR(..)
                                                           , ColorSpaceKHR(..)
                                                           , CompositeAlphaFlagBitsKHR(..)
                                                           , CompositeAlphaFlagsKHR
                                                           , SurfaceTransformFlagBitsKHR(..)
                                                           , SurfaceTransformFlagsKHR
                                                           ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_display_native_hdr (DisplayNativeHdrSurfaceCapabilitiesAMD)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (ImageCompressionPropertiesEXT)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceCapabilities2KHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSurfaceFormats2KHR))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shared_presentable_image (SharedPresentSurfaceCapabilitiesKHR)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceCapabilitiesFullScreenExclusiveEXT)
import Vulkan.Extensions.VK_KHR_surface (SurfaceCapabilitiesKHR)
import Vulkan.Extensions.VK_KHR_surface (SurfaceFormatKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveWin32InfoEXT)
import Vulkan.Extensions.Handles (SurfaceKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface_protected_capabilities (SurfaceProtectedCapabilitiesKHR)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (CompositeAlphaFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface (SurfaceCapabilitiesKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceFormatKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilities2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr (SomeStruct SurfaceCapabilities2KHR) -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr (SomeStruct SurfaceCapabilities2KHR) -> IO Result

-- | vkGetPhysicalDeviceSurfaceCapabilities2KHR - Reports capabilities of a
-- surface on a physical device
--
-- = Description
--
-- 'getPhysicalDeviceSurfaceCapabilities2KHR' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
-- with the ability to specify extended inputs via chained input
-- structures, and to return extended information via chained output
-- structures.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2KHR-pSurfaceInfo-06520#
--     @pSurfaceInfo->surface@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2KHR-pSurfaceInfo-06210#
--     @pSurfaceInfo->surface@ /must/ be supported by @physicalDevice@, as
--     reported by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
--     or an equivalent platform-specific mechanism
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2KHR-pNext-02671# If a
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceCapabilitiesFullScreenExclusiveEXT'
--     structure is included in the @pNext@ chain of
--     @pSurfaceCapabilities@, a
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveWin32InfoEXT'
--     structure /must/ be included in the @pNext@ chain of @pSurfaceInfo@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2KHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2KHR-pSurfaceInfo-parameter#
--     @pSurfaceInfo@ /must/ be a valid pointer to a valid
--     'PhysicalDeviceSurfaceInfo2KHR' structure
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceCapabilities2KHR-pSurfaceCapabilities-parameter#
--     @pSurfaceCapabilities@ /must/ be a valid pointer to a
--     'SurfaceCapabilities2KHR' structure
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'PhysicalDeviceSurfaceInfo2KHR',
-- 'SurfaceCapabilities2KHR'
getPhysicalDeviceSurfaceCapabilities2KHR :: forall a b io
                                          . (Extendss PhysicalDeviceSurfaceInfo2KHR a, PokeChain a, Extendss SurfaceCapabilities2KHR b, PokeChain b, PeekChain b, MonadIO io)
                                         => -- | @physicalDevice@ is the physical device that will be associated with the
                                            -- swapchain to be created, as described for
                                            -- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                            PhysicalDevice
                                         -> -- | @pSurfaceInfo@ is a pointer to a 'PhysicalDeviceSurfaceInfo2KHR'
                                            -- structure describing the surface and other fixed parameters that would
                                            -- be consumed by 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                            (PhysicalDeviceSurfaceInfo2KHR a)
                                         -> io (SurfaceCapabilities2KHR b)
getPhysicalDeviceSurfaceCapabilities2KHR physicalDevice surfaceInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceCapabilities2KHRPtr = pVkGetPhysicalDeviceSurfaceCapabilities2KHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceSurfaceCapabilities2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceCapabilities2KHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceCapabilities2KHR' = mkVkGetPhysicalDeviceSurfaceCapabilities2KHR vkGetPhysicalDeviceSurfaceCapabilities2KHRPtr
  pSurfaceInfo <- ContT $ withCStruct (surfaceInfo)
  pPSurfaceCapabilities <- ContT (withZeroCStruct @(SurfaceCapabilities2KHR _))
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfaceCapabilities2KHR" (vkGetPhysicalDeviceSurfaceCapabilities2KHR' (physicalDeviceHandle (physicalDevice)) (forgetExtensions pSurfaceInfo) (forgetExtensions (pPSurfaceCapabilities)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceCapabilities <- lift $ peekCStruct @(SurfaceCapabilities2KHR _) pPSurfaceCapabilities
  pure $ (pSurfaceCapabilities)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceFormats2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr Word32 -> Ptr (SomeStruct SurfaceFormat2KHR) -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR) -> Ptr Word32 -> Ptr (SomeStruct SurfaceFormat2KHR) -> IO Result

-- | vkGetPhysicalDeviceSurfaceFormats2KHR - Query color formats supported by
-- surface
--
-- = Description
--
-- 'getPhysicalDeviceSurfaceFormats2KHR' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR',
-- with the ability to be extended via @pNext@ chains.
--
-- If @pSurfaceFormats@ is @NULL@, then the number of format tuples
-- supported for the given @surface@ is returned in @pSurfaceFormatCount@.
-- Otherwise, @pSurfaceFormatCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pSurfaceFormats@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pSurfaceFormats@. If the value of
-- @pSurfaceFormatCount@ is less than the number of format tuples
-- supported, at most @pSurfaceFormatCount@ structures will be written, and
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available values were returned.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormats2KHR-pSurfaceInfo-06521# If
--     the @VK_GOOGLE_surfaceless_query@ extension is not enabled,
--     @pSurfaceInfo->surface@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormats2KHR-pSurfaceInfo-06522# If
--     @pSurfaceInfo->surface@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', it /must/ be supported by
--     @physicalDevice@, as reported by
--     'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
--     or an equivalent platform-specific mechanism
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormats2KHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormats2KHR-pSurfaceInfo-parameter#
--     @pSurfaceInfo@ /must/ be a valid pointer to a valid
--     'PhysicalDeviceSurfaceInfo2KHR' structure
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormats2KHR-pSurfaceFormatCount-parameter#
--     @pSurfaceFormatCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   #VUID-vkGetPhysicalDeviceSurfaceFormats2KHR-pSurfaceFormats-parameter#
--     If the value referenced by @pSurfaceFormatCount@ is not @0@, and
--     @pSurfaceFormats@ is not @NULL@, @pSurfaceFormats@ /must/ be a valid
--     pointer to an array of @pSurfaceFormatCount@ 'SurfaceFormat2KHR'
--     structures
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'PhysicalDeviceSurfaceInfo2KHR',
-- 'SurfaceFormat2KHR'
getPhysicalDeviceSurfaceFormats2KHR :: forall a b io
                                     . (Extendss PhysicalDeviceSurfaceInfo2KHR a, PokeChain a, Extendss SurfaceFormat2KHR b, PokeChain b, PeekChain b, MonadIO io)
                                    => -- | @physicalDevice@ is the physical device that will be associated with the
                                       -- swapchain to be created, as described for
                                       -- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                       PhysicalDevice
                                    -> -- | @pSurfaceInfo@ is a pointer to a 'PhysicalDeviceSurfaceInfo2KHR'
                                       -- structure describing the surface and other fixed parameters that would
                                       -- be consumed by 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR'.
                                       (PhysicalDeviceSurfaceInfo2KHR a)
                                    -> io (Result, ("surfaceFormats" ::: Vector (SurfaceFormat2KHR b)))
getPhysicalDeviceSurfaceFormats2KHR physicalDevice surfaceInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSurfaceFormats2KHRPtr = pVkGetPhysicalDeviceSurfaceFormats2KHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceSurfaceFormats2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSurfaceFormats2KHR is null" Nothing Nothing
  let vkGetPhysicalDeviceSurfaceFormats2KHR' = mkVkGetPhysicalDeviceSurfaceFormats2KHR vkGetPhysicalDeviceSurfaceFormats2KHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pSurfaceInfo <- ContT $ withCStruct (surfaceInfo)
  let x9 = forgetExtensions pSurfaceInfo
  pPSurfaceFormatCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfaceFormats2KHR" (vkGetPhysicalDeviceSurfaceFormats2KHR' physicalDevice' x9 (pPSurfaceFormatCount) (forgetExtensions (nullPtr)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSurfaceFormatCount <- lift $ peek @Word32 pPSurfaceFormatCount
  pPSurfaceFormats <- ContT $ bracket (callocBytes @(SurfaceFormat2KHR _) ((fromIntegral (pSurfaceFormatCount)) * 24)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPSurfaceFormats `advancePtrBytes` (i * 24) :: Ptr (SurfaceFormat2KHR _)) . ($ ())) [0..(fromIntegral (pSurfaceFormatCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceSurfaceFormats2KHR" (vkGetPhysicalDeviceSurfaceFormats2KHR' physicalDevice' x9 (pPSurfaceFormatCount) (forgetExtensions ((pPSurfaceFormats))))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pSurfaceFormatCount' <- lift $ peek @Word32 pPSurfaceFormatCount
  pSurfaceFormats' <- lift $ generateM (fromIntegral (pSurfaceFormatCount')) (\i -> peekCStruct @(SurfaceFormat2KHR _) (((pPSurfaceFormats) `advancePtrBytes` (24 * (i)) :: Ptr (SurfaceFormat2KHR _))))
  pure $ ((r'), pSurfaceFormats')


-- | VkPhysicalDeviceSurfaceInfo2KHR - Structure specifying a surface and
-- related swapchain creation parameters
--
-- = Description
--
-- The members of 'PhysicalDeviceSurfaceInfo2KHR' correspond to the
-- arguments to
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
-- with @sType@ and @pNext@ added for extensibility.
--
-- Additional capabilities of a surface /may/ be available to swapchains
-- created with different full-screen exclusive settings - particularly if
-- exclusive full-screen access is application controlled. These additional
-- capabilities /can/ be queried by adding a
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveInfoEXT'
-- structure to the @pNext@ chain of this structure when used to query
-- surface properties. Additionally, for Win32 surfaces with application
-- controlled exclusive full-screen access, chaining a
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveWin32InfoEXT'
-- structure /may/ also report additional surface capabilities. These
-- additional capabilities only apply to swapchains created with the same
-- parameters included in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'.
--
-- == Valid Usage
--
-- -   #VUID-VkPhysicalDeviceSurfaceInfo2KHR-pNext-02672# If the @pNext@
--     chain includes a
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveInfoEXT'
--     structure with its @fullScreenExclusive@ member set to
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT',
--     and @surface@ was created using
--     'Vulkan.Extensions.VK_KHR_win32_surface.createWin32SurfaceKHR', a
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveWin32InfoEXT'
--     structure /must/ be included in the @pNext@ chain
--
-- -   #VUID-VkPhysicalDeviceSurfaceInfo2KHR-pSurfaceInfo-06526# When
--     passed as the @pSurfaceInfo@ parameter of
--     'getPhysicalDeviceSurfaceCapabilities2KHR', if the
--     @VK_GOOGLE_surfaceless_query@ extension is enabled and the @pNext@
--     chain of the @pSurfaceCapabilities@ parameter includes
--     'Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR',
--     then @surface@ /can/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--     Otherwise, @surface@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- -   #VUID-VkPhysicalDeviceSurfaceInfo2KHR-pSurfaceInfo-06527# When
--     passed as the @pSurfaceInfo@ parameter of
--     'getPhysicalDeviceSurfaceFormats2KHR', if the
--     @VK_GOOGLE_surfaceless_query@ extension is enabled, then @surface@
--     /can/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'. Otherwise,
--     @surface@ /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR'
--     handle
--
-- -   #VUID-VkPhysicalDeviceSurfaceInfo2KHR-pSurfaceInfo-06528# When
--     passed as the @pSurfaceInfo@ parameter of
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.getPhysicalDeviceSurfacePresentModes2EXT',
--     if the @VK_GOOGLE_surfaceless_query@ extension is enabled, then
--     @surface@ /can/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--     Otherwise, @surface@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceSurfaceInfo2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR'
--
-- -   #VUID-VkPhysicalDeviceSurfaceInfo2KHR-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveInfoEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveWin32InfoEXT'
--
-- -   #VUID-VkPhysicalDeviceSurfaceInfo2KHR-sType-unique# The @sType@
--     value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPhysicalDeviceSurfaceInfo2KHR-surface-parameter# If
--     @surface@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @surface@
--     /must/ be a valid 'Vulkan.Extensions.Handles.SurfaceKHR' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SurfaceKHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.getDeviceGroupSurfacePresentModes2EXT',
-- 'getPhysicalDeviceSurfaceCapabilities2KHR',
-- 'getPhysicalDeviceSurfaceFormats2KHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.getPhysicalDeviceSurfacePresentModes2EXT'
data PhysicalDeviceSurfaceInfo2KHR (es :: [Type]) = PhysicalDeviceSurfaceInfo2KHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @surface@ is the surface that will be associated with the swapchain.
    surface :: SurfaceKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSurfaceInfo2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PhysicalDeviceSurfaceInfo2KHR es)

instance Extensible PhysicalDeviceSurfaceInfo2KHR where
  extensibleTypeName = "PhysicalDeviceSurfaceInfo2KHR"
  setNext PhysicalDeviceSurfaceInfo2KHR{..} next' = PhysicalDeviceSurfaceInfo2KHR{next = next', ..}
  getNext PhysicalDeviceSurfaceInfo2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PhysicalDeviceSurfaceInfo2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SurfaceFullScreenExclusiveWin32InfoEXT = Just f
    | Just Refl <- eqT @e @SurfaceFullScreenExclusiveInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss PhysicalDeviceSurfaceInfo2KHR es, PokeChain es) => ToCStruct (PhysicalDeviceSurfaceInfo2KHR es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSurfaceInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceKHR)) (surface)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss PhysicalDeviceSurfaceInfo2KHR es, PeekChain es) => FromCStruct (PhysicalDeviceSurfaceInfo2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    surface <- peek @SurfaceKHR ((p `plusPtr` 16 :: Ptr SurfaceKHR))
    pure $ PhysicalDeviceSurfaceInfo2KHR
             next surface

instance es ~ '[] => Zero (PhysicalDeviceSurfaceInfo2KHR es) where
  zero = PhysicalDeviceSurfaceInfo2KHR
           ()
           zero


-- | VkSurfaceCapabilities2KHR - Structure describing capabilities of a
-- surface
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSurfaceCapabilities2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR'
--
-- -   #VUID-VkSurfaceCapabilities2KHR-pNext-pNext# Each @pNext@ member of
--     any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_AMD_display_native_hdr.DisplayNativeHdrSurfaceCapabilitiesAMD',
--     'Vulkan.Extensions.VK_KHR_shared_presentable_image.SharedPresentSurfaceCapabilitiesKHR',
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceCapabilitiesFullScreenExclusiveEXT',
--     or
--     'Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR'
--
-- -   #VUID-VkSurfaceCapabilities2KHR-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR',
-- 'getPhysicalDeviceSurfaceCapabilities2KHR'
data SurfaceCapabilities2KHR (es :: [Type]) = SurfaceCapabilities2KHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @surfaceCapabilities@ is a
    -- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR' structure
    -- describing the capabilities of the specified surface.
    surfaceCapabilities :: SurfaceCapabilitiesKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilities2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SurfaceCapabilities2KHR es)

instance Extensible SurfaceCapabilities2KHR where
  extensibleTypeName = "SurfaceCapabilities2KHR"
  setNext SurfaceCapabilities2KHR{..} next' = SurfaceCapabilities2KHR{next = next', ..}
  getNext SurfaceCapabilities2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SurfaceCapabilities2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SurfaceCapabilitiesFullScreenExclusiveEXT = Just f
    | Just Refl <- eqT @e @SurfaceProtectedCapabilitiesKHR = Just f
    | Just Refl <- eqT @e @SharedPresentSurfaceCapabilitiesKHR = Just f
    | Just Refl <- eqT @e @DisplayNativeHdrSurfaceCapabilitiesAMD = Just f
    | otherwise = Nothing

instance (Extendss SurfaceCapabilities2KHR es, PokeChain es) => ToCStruct (SurfaceCapabilities2KHR es) where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilities2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceCapabilitiesKHR)) (surfaceCapabilities)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceCapabilitiesKHR)) (zero)
    lift $ f

instance (Extendss SurfaceCapabilities2KHR es, PeekChain es) => FromCStruct (SurfaceCapabilities2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    surfaceCapabilities <- peekCStruct @SurfaceCapabilitiesKHR ((p `plusPtr` 16 :: Ptr SurfaceCapabilitiesKHR))
    pure $ SurfaceCapabilities2KHR
             next surfaceCapabilities

instance es ~ '[] => Zero (SurfaceCapabilities2KHR es) where
  zero = SurfaceCapabilities2KHR
           ()
           zero


-- | VkSurfaceFormat2KHR - Structure describing a supported swapchain format
-- tuple
--
-- == Valid Usage
--
-- -   #VUID-VkSurfaceFormat2KHR-pNext-06750# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-imageCompressionControlSwapchain imageCompressionControlSwapchain>
--     feature is not enabled, the @pNext@ chain /must/ not include an
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionPropertiesEXT'
--     structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSurfaceFormat2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR'
--
-- -   #VUID-VkSurfaceFormat2KHR-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionPropertiesEXT'
--
-- -   #VUID-VkSurfaceFormat2KHR-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceFormatKHR',
-- 'getPhysicalDeviceSurfaceFormats2KHR'
data SurfaceFormat2KHR (es :: [Type]) = SurfaceFormat2KHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @surfaceFormat@ is a 'Vulkan.Extensions.VK_KHR_surface.SurfaceFormatKHR'
    -- structure describing a format-color space pair that is compatible with
    -- the specified surface.
    surfaceFormat :: SurfaceFormatKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceFormat2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SurfaceFormat2KHR es)

instance Extensible SurfaceFormat2KHR where
  extensibleTypeName = "SurfaceFormat2KHR"
  setNext SurfaceFormat2KHR{..} next' = SurfaceFormat2KHR{next = next', ..}
  getNext SurfaceFormat2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SurfaceFormat2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImageCompressionPropertiesEXT = Just f
    | otherwise = Nothing

instance (Extendss SurfaceFormat2KHR es, PokeChain es) => ToCStruct (SurfaceFormat2KHR es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceFormat2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceFormatKHR)) (surfaceFormat)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr SurfaceFormatKHR)) (zero)
    lift $ f

instance (Extendss SurfaceFormat2KHR es, PeekChain es) => FromCStruct (SurfaceFormat2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    surfaceFormat <- peekCStruct @SurfaceFormatKHR ((p `plusPtr` 16 :: Ptr SurfaceFormatKHR))
    pure $ SurfaceFormat2KHR
             next surfaceFormat

instance es ~ '[] => Zero (SurfaceFormat2KHR es) where
  zero = SurfaceFormat2KHR
           ()
           zero


type KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION"
pattern KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = 1


type KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME = "VK_KHR_get_surface_capabilities2"

-- No documentation found for TopLevel "VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME"
pattern KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME = "VK_KHR_get_surface_capabilities2"

