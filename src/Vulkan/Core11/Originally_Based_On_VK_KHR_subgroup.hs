{-# language CPP #-}
-- No documentation found for Chapter "Originally_Based_On_VK_KHR_subgroup"
module Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup  ( PhysicalDeviceSubgroupProperties(..)
                                                          , StructureType(..)
                                                          , SubgroupFeatureFlagBits(..)
                                                          , SubgroupFeatureFlags
                                                          ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(..))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)

-- No documentation found for TopLevel "VkPhysicalDeviceSubgroupProperties"
data PhysicalDeviceSubgroupProperties = PhysicalDeviceSubgroupProperties
  { -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "subgroupSize"
    subgroupSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "supportedStages"
    supportedStages :: ShaderStageFlags
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "supportedOperations"
    supportedOperations :: SubgroupFeatureFlags
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupProperties" "quadOperationsInAllStages"
    quadOperationsInAllStages :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubgroupProperties)
#endif
deriving instance Show PhysicalDeviceSubgroupProperties

instance ToCStruct PhysicalDeviceSubgroupProperties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSubgroupProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (subgroupSize)
    poke ((p `plusPtr` 20 :: Ptr ShaderStageFlags)) (supportedStages)
    poke ((p `plusPtr` 24 :: Ptr SubgroupFeatureFlags)) (supportedOperations)
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (quadOperationsInAllStages))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 24 :: Ptr SubgroupFeatureFlags)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSubgroupProperties where
  peekCStruct p = do
    subgroupSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    supportedStages <- peek @ShaderStageFlags ((p `plusPtr` 20 :: Ptr ShaderStageFlags))
    supportedOperations <- peek @SubgroupFeatureFlags ((p `plusPtr` 24 :: Ptr SubgroupFeatureFlags))
    quadOperationsInAllStages <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceSubgroupProperties
             subgroupSize supportedStages supportedOperations (bool32ToBool quadOperationsInAllStages)


instance Storable PhysicalDeviceSubgroupProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSubgroupProperties where
  zero = PhysicalDeviceSubgroupProperties
           zero
           zero
           zero
           zero

