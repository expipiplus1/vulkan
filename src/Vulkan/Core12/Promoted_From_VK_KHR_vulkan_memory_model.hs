{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_vulkan_memory_model"
module Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model  ( PhysicalDeviceVulkanMemoryModelFeatures(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceVulkanMemoryModelFeatures - Structure describing
-- features supported by the memory model
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the
-- 'Vulkan.Extensions.VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeaturesKHR'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported.
-- 'Vulkan.Extensions.VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeaturesKHR'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceVulkanMemoryModelFeatures = PhysicalDeviceVulkanMemoryModelFeatures
  { -- | #extension-features-vulkanMemoryModel# @vulkanMemoryModel@ indicates
    -- whether the Vulkan Memory Model is supported, as defined in
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-model Vulkan Memory Model>.
    -- This also indicates whether shader modules /can/ declare the
    -- @VulkanMemoryModel@ capability.
    vulkanMemoryModel :: Bool
  , -- | #extension-features-vulkanMemoryModelDeviceScope#
    -- @vulkanMemoryModelDeviceScope@ indicates whether the Vulkan Memory Model
    -- can use 'Vulkan.Core10.Handles.Device' scope synchronization. This also
    -- indicates whether shader modules /can/ declare the
    -- @VulkanMemoryModelDeviceScope@ capability.
    vulkanMemoryModelDeviceScope :: Bool
  , -- | #extension-features-vulkanMemoryModelAvailabilityVisibilityChains#
    -- @vulkanMemoryModelAvailabilityVisibilityChains@ indicates whether the
    -- Vulkan Memory Model can use
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-model-availability-visibility availability and visibility chains>
    -- with more than one element.
    vulkanMemoryModelAvailabilityVisibilityChains :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceVulkanMemoryModelFeatures)
#endif
deriving instance Show PhysicalDeviceVulkanMemoryModelFeatures

instance ToCStruct PhysicalDeviceVulkanMemoryModelFeatures where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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

