{-# language CPP #-}
-- | = Name
--
-- VK_NV_present_metering - device extension
--
-- = VK_NV_present_metering
--
-- [__Name String__]
--     @VK_NV_present_metering@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     614
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__Contact__]
--
--     -   Charles Hansen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_present_metering] @chansen%0A*Here describe the issue or question you have about the VK_NV_present_metering extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-08
--
-- [__Provisional__]
--     *This extension is /provisional/ and /should/ not be used in
--     production applications. The functionality defined by this extension
--     /may/ change in ways that break backwards compatibility between
--     revisions, and before the final release of the non-provisional
--     version of this extension.
--
-- [__Contributors__]
--
--     -   Charles Hansen, NVIDIA
--
--     -   Lionel Duc, NVIDIA
--
-- == Description
--
-- This extension is used to evenly meter presents.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentMeteringFeaturesNV'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'SetPresentConfigNV'
--
-- == New Enum Constants
--
-- -   'NV_PRESENT_METERING_EXTENSION_NAME'
--
-- -   'NV_PRESENT_METERING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_METERING_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SET_PRESENT_CONFIG_NV'
--
-- == Version History
--
-- -   Revision 1, 2025-01-08 (Charles Hansen)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_present_metering Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_present_metering  ( SetPresentConfigNV(..)
                                                 , PhysicalDevicePresentMeteringFeaturesNV(..)
                                                 , NV_PRESENT_METERING_SPEC_VERSION
                                                 , pattern NV_PRESENT_METERING_SPEC_VERSION
                                                 , NV_PRESENT_METERING_EXTENSION_NAME
                                                 , pattern NV_PRESENT_METERING_EXTENSION_NAME
                                                 ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_METERING_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SET_PRESENT_CONFIG_NV))
-- | VkSetPresentConfigNV - Structure specifying present metering
-- configuration
--
-- = Description
--
-- The metering configuration applies to all swapchains in the array in
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'. The configuration
-- specified by 'SetPresentConfigNV' applies to the next
-- @numFramesPerBatch@ calls to
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' and needs to be
-- updated every @numFramesPerBatch@ presents.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_present_metering VK_NV_present_metering>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SetPresentConfigNV = SetPresentConfigNV
  { -- | @numFramesPerBatch@ is the number of frames to batch
    --
    -- #VUID-VkSetPresentConfigNV-numFramesPerBatch-10581# @numFramesPerBatch@
    -- /must/ not be larger than 8
    numFramesPerBatch :: Word32
  , -- | @presentConfigFeedback@ will return the success or error status
    presentConfigFeedback :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SetPresentConfigNV)
#endif
deriving instance Show SetPresentConfigNV

instance ToCStruct SetPresentConfigNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SetPresentConfigNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SET_PRESENT_CONFIG_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (numFramesPerBatch)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (presentConfigFeedback)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SET_PRESENT_CONFIG_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct SetPresentConfigNV where
  peekCStruct p = do
    numFramesPerBatch <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    presentConfigFeedback <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ SetPresentConfigNV
             numFramesPerBatch presentConfigFeedback

instance Storable SetPresentConfigNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SetPresentConfigNV where
  zero = SetPresentConfigNV
           zero
           zero


-- | VkPhysicalDevicePresentMeteringFeaturesNV - Structure describing whether
-- the present metering features can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDevicePresentMeteringFeaturesNV' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDevicePresentMeteringFeaturesNV', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_present_metering VK_NV_present_metering>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePresentMeteringFeaturesNV = PhysicalDevicePresentMeteringFeaturesNV
  { -- | #features-presentMetering# @presentMetering@ indicates whether the
    -- implementation supports present metering capability.
    presentMetering :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePresentMeteringFeaturesNV)
#endif
deriving instance Show PhysicalDevicePresentMeteringFeaturesNV

instance ToCStruct PhysicalDevicePresentMeteringFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePresentMeteringFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_METERING_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentMetering))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_METERING_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePresentMeteringFeaturesNV where
  peekCStruct p = do
    presentMetering <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePresentMeteringFeaturesNV
             (bool32ToBool presentMetering)

instance Storable PhysicalDevicePresentMeteringFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePresentMeteringFeaturesNV where
  zero = PhysicalDevicePresentMeteringFeaturesNV
           zero


type NV_PRESENT_METERING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_PRESENT_METERING_SPEC_VERSION"
pattern NV_PRESENT_METERING_SPEC_VERSION :: forall a . Integral a => a
pattern NV_PRESENT_METERING_SPEC_VERSION = 1


type NV_PRESENT_METERING_EXTENSION_NAME = "VK_NV_present_metering"

-- No documentation found for TopLevel "VK_NV_PRESENT_METERING_EXTENSION_NAME"
pattern NV_PRESENT_METERING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_PRESENT_METERING_EXTENSION_NAME = "VK_NV_present_metering"

