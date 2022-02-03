{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_zero_initialize_workgroup_memory"
module Vulkan.Core13.Promoted_From_VK_KHR_zero_initialize_workgroup_memory  ( PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceZeroInitializeWorkgroupMemoryFeatures - Structure
-- describing support for zero initialization of workgroup memory by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_zero_initialize_workgroup_memory VK_KHR_zero_initialize_workgroup_memory>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures = PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures
  { -- | #extension-features-shaderZeroInitializeWorkgroupMemory#
    -- @shaderZeroInitializeWorkgroupMemory@ specifies whether the
    -- implementation supports initializing a variable in Workgroup storage
    -- class.
    shaderZeroInitializeWorkgroupMemory :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures)
#endif
deriving instance Show PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures

instance ToCStruct PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderZeroInitializeWorkgroupMemory))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures where
  peekCStruct p = do
    shaderZeroInitializeWorkgroupMemory <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures
             (bool32ToBool shaderZeroInitializeWorkgroupMemory)

instance Storable PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures where
  zero = PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures
           zero

