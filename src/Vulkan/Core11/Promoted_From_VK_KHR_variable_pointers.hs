{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_variable_pointers"
module Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
                                                             , PhysicalDeviceVariablePointersFeatures(..)
                                                             , PhysicalDeviceVariablePointerFeatures
                                                             , StructureType(..)
                                                             ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES = STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES


-- | VkPhysicalDeviceVariablePointersFeatures - Structure describing variable
-- pointers features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to a structure extending this
--     structure.
--
-- -   #extension-features-variablePointersStorageBuffer#
--     @variablePointersStorageBuffer@ specifies whether the implementation
--     supports the SPIR-V @VariablePointersStorageBuffer@ capability. When
--     this feature is not enabled, shader modules /must/ not declare the
--     @SPV_KHR_variable_pointers@ extension or the
--     @VariablePointersStorageBuffer@ capability.
--
-- -   #extension-features-variablePointers# @variablePointers@ specifies
--     whether the implementation supports the SPIR-V @VariablePointers@
--     capability. When this feature is not enabled, shader modules /must/
--     not declare the @VariablePointers@ capability.
--
-- If the 'PhysicalDeviceVariablePointersFeatures' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceVariablePointersFeatures' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage
--
-- -   #VUID-VkPhysicalDeviceVariablePointersFeatures-variablePointers-01431#
--     If @variablePointers@ is enabled then
--     @variablePointersStorageBuffer@ /must/ also be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceVariablePointersFeatures-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_variable_pointers VK_KHR_variable_pointers>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVariablePointersFeatures = PhysicalDeviceVariablePointersFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceVariablePointersFeatures" "variablePointersStorageBuffer"
    variablePointersStorageBuffer :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceVariablePointersFeatures" "variablePointers"
    variablePointers :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVariablePointersFeatures)
#endif
deriving instance Show PhysicalDeviceVariablePointersFeatures

instance ToCStruct PhysicalDeviceVariablePointersFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVariablePointersFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (variablePointersStorageBuffer))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (variablePointers))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVariablePointersFeatures where
  peekCStruct p = do
    variablePointersStorageBuffer <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    variablePointers <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceVariablePointersFeatures
             (bool32ToBool variablePointersStorageBuffer)
             (bool32ToBool variablePointers)

instance Storable PhysicalDeviceVariablePointersFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVariablePointersFeatures where
  zero = PhysicalDeviceVariablePointersFeatures
           zero
           zero


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointerFeatures"
type PhysicalDeviceVariablePointerFeatures = PhysicalDeviceVariablePointersFeatures

