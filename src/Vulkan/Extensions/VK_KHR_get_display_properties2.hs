{-# language CPP #-}
-- | = Name
--
-- VK_KHR_get_display_properties2 - instance extension
--
-- == VK_KHR_get_display_properties2
--
-- [__Name String__]
--     @VK_KHR_get_display_properties2@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     122
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_display@ to be enabled
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_get_display_properties2] @cubanismo%0A<<Here describe the issue or question you have about the VK_KHR_get_display_properties2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-02-21
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
-- == Description
--
-- This extension provides new entry points to query device display
-- properties and capabilities in a way that can be easily extended by
-- other extensions, without introducing any further entry points. This
-- extension can be considered the @VK_KHR_display@ equivalent of the
-- @VK_KHR_get_physical_device_properties2@ extension.
--
-- == New Commands
--
-- -   'getDisplayModeProperties2KHR'
--
-- -   'getDisplayPlaneCapabilities2KHR'
--
-- -   'getPhysicalDeviceDisplayPlaneProperties2KHR'
--
-- -   'getPhysicalDeviceDisplayProperties2KHR'
--
-- == New Structures
--
-- -   'DisplayModeProperties2KHR'
--
-- -   'DisplayPlaneCapabilities2KHR'
--
-- -   'DisplayPlaneInfo2KHR'
--
-- -   'DisplayPlaneProperties2KHR'
--
-- -   'DisplayProperties2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME'
--
-- -   'KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR'
--
-- == Issues
--
-- 1) What should this extension be named?
--
-- __RESOLVED__: @VK_KHR_get_display_properties2@. Other alternatives:
--
-- -   @VK_KHR_display2@
--
-- -   One extension, combined with @VK_KHR_surface_capabilites2@.
--
-- 2) Should extensible input structs be added for these new functions:
--
-- __RESOLVED__:
--
-- -   'getPhysicalDeviceDisplayProperties2KHR': No. The only current input
--     is a 'Vulkan.Core10.Handles.PhysicalDevice'. Other inputs would not
--     make sense.
--
-- -   'getPhysicalDeviceDisplayPlaneProperties2KHR': No. The only current
--     input is a 'Vulkan.Core10.Handles.PhysicalDevice'. Other inputs
--     would not make sense.
--
-- -   'getDisplayModeProperties2KHR': No. The only current inputs are a
--     'Vulkan.Core10.Handles.PhysicalDevice' and a
--     'Vulkan.Extensions.Handles.DisplayModeKHR'. Other inputs would not
--     make sense.
--
-- 3) Should additional display query functions be extended?
--
-- __RESOLVED__:
--
-- -   'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneSupportedDisplaysKHR':
--     No. Extensions should instead extend
--     'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneCapabilitiesKHR'().
--
-- == Version History
--
-- -   Revision 1, 2017-02-21 (James Jones)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'DisplayModeProperties2KHR', 'DisplayPlaneCapabilities2KHR',
-- 'DisplayPlaneInfo2KHR', 'DisplayPlaneProperties2KHR',
-- 'DisplayProperties2KHR', 'getDisplayModeProperties2KHR',
-- 'getDisplayPlaneCapabilities2KHR',
-- 'getPhysicalDeviceDisplayPlaneProperties2KHR',
-- 'getPhysicalDeviceDisplayProperties2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_get_display_properties2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_get_display_properties2  ( getPhysicalDeviceDisplayProperties2KHR
                                                         , getPhysicalDeviceDisplayPlaneProperties2KHR
                                                         , getDisplayModeProperties2KHR
                                                         , getDisplayPlaneCapabilities2KHR
                                                         , DisplayProperties2KHR(..)
                                                         , DisplayPlaneProperties2KHR(..)
                                                         , DisplayModeProperties2KHR(..)
                                                         , DisplayPlaneInfo2KHR(..)
                                                         , DisplayPlaneCapabilities2KHR(..)
                                                         , KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
                                                         , pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
                                                         , KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
                                                         , pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
                                                         , DisplayKHR(..)
                                                         , DisplayModeKHR(..)
                                                         , DisplayPropertiesKHR(..)
                                                         , DisplayPlanePropertiesKHR(..)
                                                         , DisplayModeParametersKHR(..)
                                                         , DisplayModePropertiesKHR(..)
                                                         , DisplayPlaneCapabilitiesKHR(..)
                                                         , DisplayPlaneAlphaFlagBitsKHR(..)
                                                         , DisplayPlaneAlphaFlagsKHR
                                                         , SurfaceTransformFlagBitsKHR(..)
                                                         , SurfaceTransformFlagsKHR
                                                         ) where

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
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
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
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Extensions.Handles (DisplayModeKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayModePropertiesKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayPlaneCapabilitiesKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayPlanePropertiesKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayPropertiesKHR)
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayModeProperties2KHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetDisplayPlaneCapabilities2KHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDisplayPlaneProperties2KHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDisplayProperties2KHR))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Extensions.Handles (DisplayModeKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayModeParametersKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayModePropertiesKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayPlaneAlphaFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayPlaneAlphaFlagsKHR)
import Vulkan.Extensions.VK_KHR_display (DisplayPlaneCapabilitiesKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayPlanePropertiesKHR(..))
import Vulkan.Extensions.VK_KHR_display (DisplayPropertiesKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface (SurfaceTransformFlagsKHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayProperties2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayProperties2KHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayProperties2KHR -> IO Result

-- | vkGetPhysicalDeviceDisplayProperties2KHR - Query information about the
-- available displays
--
-- = Description
--
-- 'getPhysicalDeviceDisplayProperties2KHR' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_display.getPhysicalDeviceDisplayPropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceDisplayProperties2KHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceDisplayProperties2KHR-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceDisplayProperties2KHR-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'DisplayProperties2KHR' structures
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>,
-- 'DisplayProperties2KHR', 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceDisplayProperties2KHR :: forall io
                                        . (MonadIO io)
                                       => -- | @physicalDevice@ is a physical device.
                                          PhysicalDevice
                                       -> io (Result, ("properties" ::: Vector DisplayProperties2KHR))
getPhysicalDeviceDisplayProperties2KHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceDisplayProperties2KHRPtr = pVkGetPhysicalDeviceDisplayProperties2KHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceDisplayProperties2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDisplayProperties2KHR is null" Nothing Nothing
  let vkGetPhysicalDeviceDisplayProperties2KHR' = mkVkGetPhysicalDeviceDisplayProperties2KHR vkGetPhysicalDeviceDisplayProperties2KHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceDisplayProperties2KHR" (vkGetPhysicalDeviceDisplayProperties2KHR' physicalDevice' (pPPropertyCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayProperties2KHR ((fromIntegral (pPropertyCount)) * 64)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 64) :: Ptr DisplayProperties2KHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceDisplayProperties2KHR" (vkGetPhysicalDeviceDisplayProperties2KHR' physicalDevice' (pPPropertyCount) ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayProperties2KHR (((pPProperties) `advancePtrBytes` (64 * (i)) :: Ptr DisplayProperties2KHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPlaneProperties2KHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr DisplayPlaneProperties2KHR -> IO Result

-- | vkGetPhysicalDeviceDisplayPlaneProperties2KHR - Query information about
-- the available display planes.
--
-- = Description
--
-- 'getPhysicalDeviceDisplayPlaneProperties2KHR' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_display.getPhysicalDeviceDisplayPlanePropertiesKHR',
-- with the ability to return extended information via chained output
-- structures.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceDisplayPlaneProperties2KHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceDisplayPlaneProperties2KHR-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceDisplayPlaneProperties2KHR-pProperties-parameter#
--     If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'DisplayPlaneProperties2KHR'
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>,
-- 'DisplayPlaneProperties2KHR', 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceDisplayPlaneProperties2KHR :: forall io
                                             . (MonadIO io)
                                            => -- | @physicalDevice@ is a physical device.
                                               PhysicalDevice
                                            -> io (Result, ("properties" ::: Vector DisplayPlaneProperties2KHR))
getPhysicalDeviceDisplayPlaneProperties2KHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceDisplayPlaneProperties2KHRPtr = pVkGetPhysicalDeviceDisplayPlaneProperties2KHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceDisplayPlaneProperties2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDisplayPlaneProperties2KHR is null" Nothing Nothing
  let vkGetPhysicalDeviceDisplayPlaneProperties2KHR' = mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR vkGetPhysicalDeviceDisplayPlaneProperties2KHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceDisplayPlaneProperties2KHR" (vkGetPhysicalDeviceDisplayPlaneProperties2KHR' physicalDevice' (pPPropertyCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayPlaneProperties2KHR ((fromIntegral (pPropertyCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 32) :: Ptr DisplayPlaneProperties2KHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceDisplayPlaneProperties2KHR" (vkGetPhysicalDeviceDisplayPlaneProperties2KHR' physicalDevice' (pPPropertyCount) ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayPlaneProperties2KHR (((pPProperties) `advancePtrBytes` (32 * (i)) :: Ptr DisplayPlaneProperties2KHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayModeProperties2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> Ptr Word32 -> Ptr DisplayModeProperties2KHR -> IO Result) -> Ptr PhysicalDevice_T -> DisplayKHR -> Ptr Word32 -> Ptr DisplayModeProperties2KHR -> IO Result

-- | vkGetDisplayModeProperties2KHR - Query information about the available
-- display modes.
--
-- = Description
--
-- 'getDisplayModeProperties2KHR' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_display.getDisplayModePropertiesKHR', with the
-- ability to return extended information via chained output structures.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDisplayModeProperties2KHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetDisplayModeProperties2KHR-display-parameter# @display@
--     /must/ be a valid 'Vulkan.Extensions.Handles.DisplayKHR' handle
--
-- -   #VUID-vkGetDisplayModeProperties2KHR-pPropertyCount-parameter#
--     @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetDisplayModeProperties2KHR-pProperties-parameter# If the
--     value referenced by @pPropertyCount@ is not @0@, and @pProperties@
--     is not @NULL@, @pProperties@ /must/ be a valid pointer to an array
--     of @pPropertyCount@ 'DisplayModeProperties2KHR' structures
--
-- -   #VUID-vkGetDisplayModeProperties2KHR-display-parent# @display@
--     /must/ have been created, allocated, or retrieved from
--     @physicalDevice@
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>,
-- 'Vulkan.Extensions.Handles.DisplayKHR', 'DisplayModeProperties2KHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getDisplayModeProperties2KHR :: forall io
                              . (MonadIO io)
                             => -- | @physicalDevice@ is the physical device associated with @display@.
                                PhysicalDevice
                             -> -- | @display@ is the display to query.
                                DisplayKHR
                             -> io (Result, ("properties" ::: Vector DisplayModeProperties2KHR))
getDisplayModeProperties2KHR physicalDevice display = liftIO . evalContT $ do
  let vkGetDisplayModeProperties2KHRPtr = pVkGetDisplayModeProperties2KHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetDisplayModeProperties2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayModeProperties2KHR is null" Nothing Nothing
  let vkGetDisplayModeProperties2KHR' = mkVkGetDisplayModeProperties2KHR vkGetDisplayModeProperties2KHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetDisplayModeProperties2KHR" (vkGetDisplayModeProperties2KHR' physicalDevice' (display) (pPPropertyCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @DisplayModeProperties2KHR ((fromIntegral (pPropertyCount)) * 40)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 40) :: Ptr DisplayModeProperties2KHR) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetDisplayModeProperties2KHR" (vkGetDisplayModeProperties2KHR' physicalDevice' (display) (pPPropertyCount) ((pPProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @DisplayModeProperties2KHR (((pPProperties) `advancePtrBytes` (40 * (i)) :: Ptr DisplayModeProperties2KHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneCapabilities2KHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr DisplayPlaneInfo2KHR -> Ptr DisplayPlaneCapabilities2KHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr DisplayPlaneInfo2KHR -> Ptr DisplayPlaneCapabilities2KHR -> IO Result

-- | vkGetDisplayPlaneCapabilities2KHR - Query capabilities of a mode and
-- plane combination
--
-- = Description
--
-- 'getDisplayPlaneCapabilities2KHR' behaves similarly to
-- 'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneCapabilitiesKHR', with
-- the ability to specify extended inputs via chained input structures, and
-- to return extended information via chained output structures.
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>,
-- 'DisplayPlaneCapabilities2KHR', 'DisplayPlaneInfo2KHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getDisplayPlaneCapabilities2KHR :: forall io
                                 . (MonadIO io)
                                => -- | @physicalDevice@ is the physical device associated with
                                   -- @pDisplayPlaneInfo@.
                                   --
                                   -- #VUID-vkGetDisplayPlaneCapabilities2KHR-physicalDevice-parameter#
                                   -- @physicalDevice@ /must/ be a valid
                                   -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                   PhysicalDevice
                                -> -- | @pDisplayPlaneInfo@ is a pointer to a 'DisplayPlaneInfo2KHR' structure
                                   -- describing the plane and mode.
                                   --
                                   -- #VUID-vkGetDisplayPlaneCapabilities2KHR-pDisplayPlaneInfo-parameter#
                                   -- @pDisplayPlaneInfo@ /must/ be a valid pointer to a valid
                                   -- 'DisplayPlaneInfo2KHR' structure
                                   DisplayPlaneInfo2KHR
                                -> io (DisplayPlaneCapabilities2KHR)
getDisplayPlaneCapabilities2KHR physicalDevice displayPlaneInfo = liftIO . evalContT $ do
  let vkGetDisplayPlaneCapabilities2KHRPtr = pVkGetDisplayPlaneCapabilities2KHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetDisplayPlaneCapabilities2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDisplayPlaneCapabilities2KHR is null" Nothing Nothing
  let vkGetDisplayPlaneCapabilities2KHR' = mkVkGetDisplayPlaneCapabilities2KHR vkGetDisplayPlaneCapabilities2KHRPtr
  pDisplayPlaneInfo <- ContT $ withCStruct (displayPlaneInfo)
  pPCapabilities <- ContT (withZeroCStruct @DisplayPlaneCapabilities2KHR)
  r <- lift $ traceAroundEvent "vkGetDisplayPlaneCapabilities2KHR" (vkGetDisplayPlaneCapabilities2KHR' (physicalDeviceHandle (physicalDevice)) pDisplayPlaneInfo (pPCapabilities))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCapabilities <- lift $ peekCStruct @DisplayPlaneCapabilities2KHR pPCapabilities
  pure $ (pCapabilities)


-- | VkDisplayProperties2KHR - Structure describing an available display
-- device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>,
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceDisplayProperties2KHR'
data DisplayProperties2KHR = DisplayProperties2KHR
  { -- | @displayProperties@ is a
    -- 'Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR' structure.
    displayProperties :: DisplayPropertiesKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayProperties2KHR)
#endif
deriving instance Show DisplayProperties2KHR

instance ToCStruct DisplayProperties2KHR where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayProperties2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DisplayPropertiesKHR)) (displayProperties) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DisplayPropertiesKHR)) (zero) . ($ ())
    lift $ f

instance FromCStruct DisplayProperties2KHR where
  peekCStruct p = do
    displayProperties <- peekCStruct @DisplayPropertiesKHR ((p `plusPtr` 16 :: Ptr DisplayPropertiesKHR))
    pure $ DisplayProperties2KHR
             displayProperties

instance Zero DisplayProperties2KHR where
  zero = DisplayProperties2KHR
           zero


-- | VkDisplayPlaneProperties2KHR - Structure describing an available display
-- plane
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>,
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPlanePropertiesKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceDisplayPlaneProperties2KHR'
data DisplayPlaneProperties2KHR = DisplayPlaneProperties2KHR
  { -- | @displayPlaneProperties@ is a
    -- 'Vulkan.Extensions.VK_KHR_display.DisplayPlanePropertiesKHR' structure.
    displayPlaneProperties :: DisplayPlanePropertiesKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlaneProperties2KHR)
#endif
deriving instance Show DisplayPlaneProperties2KHR

instance ToCStruct DisplayPlaneProperties2KHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlaneProperties2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPlanePropertiesKHR)) (displayPlaneProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPlanePropertiesKHR)) (zero)
    f

instance FromCStruct DisplayPlaneProperties2KHR where
  peekCStruct p = do
    displayPlaneProperties <- peekCStruct @DisplayPlanePropertiesKHR ((p `plusPtr` 16 :: Ptr DisplayPlanePropertiesKHR))
    pure $ DisplayPlaneProperties2KHR
             displayPlaneProperties

instance Storable DisplayPlaneProperties2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlaneProperties2KHR where
  zero = DisplayPlaneProperties2KHR
           zero


-- | VkDisplayModeProperties2KHR - Structure describing an available display
-- mode
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>,
-- 'Vulkan.Extensions.VK_KHR_display.DisplayModePropertiesKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDisplayModeProperties2KHR'
data DisplayModeProperties2KHR = DisplayModeProperties2KHR
  { -- | @displayModeProperties@ is a
    -- 'Vulkan.Extensions.VK_KHR_display.DisplayModePropertiesKHR' structure.
    displayModeProperties :: DisplayModePropertiesKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayModeProperties2KHR)
#endif
deriving instance Show DisplayModeProperties2KHR

instance ToCStruct DisplayModeProperties2KHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayModeProperties2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModePropertiesKHR)) (displayModeProperties)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModePropertiesKHR)) (zero)
    f

instance FromCStruct DisplayModeProperties2KHR where
  peekCStruct p = do
    displayModeProperties <- peekCStruct @DisplayModePropertiesKHR ((p `plusPtr` 16 :: Ptr DisplayModePropertiesKHR))
    pure $ DisplayModeProperties2KHR
             displayModeProperties

instance Storable DisplayModeProperties2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayModeProperties2KHR where
  zero = DisplayModeProperties2KHR
           zero


-- | VkDisplayPlaneInfo2KHR - Structure defining the intended configuration
-- of a display plane
--
-- = Description
--
-- Note
--
-- This parameter also implicitly specifies a display.
--
-- -   @planeIndex@ is the plane which the application intends to use with
--     the display.
--
-- The members of 'DisplayPlaneInfo2KHR' correspond to the arguments to
-- 'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneCapabilitiesKHR', with
-- @sType@ and @pNext@ added for extensibility.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDisplayPlaneInfo2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR'
--
-- -   #VUID-VkDisplayPlaneInfo2KHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkDisplayPlaneInfo2KHR-mode-parameter# @mode@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.DisplayModeKHR' handle
--
-- == Host Synchronization
--
-- -   Host access to @mode@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>,
-- 'Vulkan.Extensions.Handles.DisplayModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDisplayPlaneCapabilities2KHR'
data DisplayPlaneInfo2KHR = DisplayPlaneInfo2KHR
  { -- | @mode@ is the display mode the application intends to program when using
    -- the specified plane.
    mode :: DisplayModeKHR
  , -- No documentation found for Nested "VkDisplayPlaneInfo2KHR" "planeIndex"
    planeIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlaneInfo2KHR)
#endif
deriving instance Show DisplayPlaneInfo2KHR

instance ToCStruct DisplayPlaneInfo2KHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlaneInfo2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModeKHR)) (mode)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (planeIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayModeKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct DisplayPlaneInfo2KHR where
  peekCStruct p = do
    mode <- peek @DisplayModeKHR ((p `plusPtr` 16 :: Ptr DisplayModeKHR))
    planeIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DisplayPlaneInfo2KHR
             mode planeIndex

instance Storable DisplayPlaneInfo2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlaneInfo2KHR where
  zero = DisplayPlaneInfo2KHR
           zero
           zero


-- | VkDisplayPlaneCapabilities2KHR - Structure describing the capabilities
-- of a mode and plane combination
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>,
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPlaneCapabilitiesKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDisplayPlaneCapabilities2KHR'
data DisplayPlaneCapabilities2KHR = DisplayPlaneCapabilities2KHR
  { -- | @capabilities@ is a
    -- 'Vulkan.Extensions.VK_KHR_display.DisplayPlaneCapabilitiesKHR'
    -- structure.
    capabilities :: DisplayPlaneCapabilitiesKHR }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPlaneCapabilities2KHR)
#endif
deriving instance Show DisplayPlaneCapabilities2KHR

instance ToCStruct DisplayPlaneCapabilities2KHR where
  withCStruct x f = allocaBytes 88 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPlaneCapabilities2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPlaneCapabilitiesKHR)) (capabilities)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPlaneCapabilitiesKHR)) (zero)
    f

instance FromCStruct DisplayPlaneCapabilities2KHR where
  peekCStruct p = do
    capabilities <- peekCStruct @DisplayPlaneCapabilitiesKHR ((p `plusPtr` 16 :: Ptr DisplayPlaneCapabilitiesKHR))
    pure $ DisplayPlaneCapabilities2KHR
             capabilities

instance Storable DisplayPlaneCapabilities2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPlaneCapabilities2KHR where
  zero = DisplayPlaneCapabilities2KHR
           zero


type KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION"
pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1


type KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_display_properties2"

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME"
pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_display_properties2"

