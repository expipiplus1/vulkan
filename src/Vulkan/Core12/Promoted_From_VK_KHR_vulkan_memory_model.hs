{-# language CPP #-}
module Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model  ( PhysicalDeviceVulkanMemoryModelFeatures(..)
                                                               , StructureType(..)
                                                               ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceVulkanMemoryModelFeatures - Structure describing
-- features supported by the memory model
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkanMemoryModelFeatures = PhysicalDeviceVulkanMemoryModelFeatures
  { -- | @vulkanMemoryModel@ indicates whether the Vulkan Memory Model is
    -- supported, as defined in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-model Vulkan Memory Model>.
    -- This also indicates whether shader modules /can/ declare the
    -- @VulkanMemoryModel@ capability.
    vulkanMemoryModel :: Bool
  , -- | @vulkanMemoryModelDeviceScope@ indicates whether the Vulkan Memory Model
    -- can use 'Vulkan.Core10.Handles.Device' scope synchronization. This also
    -- indicates whether shader modules /can/ declare the
    -- @VulkanMemoryModelDeviceScope@ capability.
    vulkanMemoryModelDeviceScope :: Bool
  , -- | @vulkanMemoryModelAvailabilityVisibilityChains@ indicates whether the
    -- Vulkan Memory Model can use
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-model-availability-visibility availability and visibility chains>
    -- with more than one element.
    vulkanMemoryModelAvailabilityVisibilityChains :: Bool
  }
  deriving (Typeable, Eq)
deriving instance Show PhysicalDeviceVulkanMemoryModelFeatures

instance ToCStruct PhysicalDeviceVulkanMemoryModelFeatures where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceVulkanMemoryModelFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (vulkanMemoryModel))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (vulkanMemoryModelDeviceScope))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (vulkanMemoryModelAvailabilityVisibilityChains))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceVulkanMemoryModelFeatures where
  peekCStruct p = do
    vulkanMemoryModel <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    vulkanMemoryModelDeviceScope <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    vulkanMemoryModelAvailabilityVisibilityChains <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceVulkanMemoryModelFeatures
             (bool32ToBool vulkanMemoryModel) (bool32ToBool vulkanMemoryModelDeviceScope) (bool32ToBool vulkanMemoryModelAvailabilityVisibilityChains)

instance Storable PhysicalDeviceVulkanMemoryModelFeatures where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceVulkanMemoryModelFeatures where
  zero = PhysicalDeviceVulkanMemoryModelFeatures
           zero
           zero
           zero

