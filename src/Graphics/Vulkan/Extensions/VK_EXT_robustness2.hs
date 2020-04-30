{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_robustness2  ( PhysicalDeviceRobustness2FeaturesEXT(..)
                                                      , PhysicalDeviceRobustness2PropertiesEXT(..)
                                                      , EXT_ROBUSTNESS_2_SPEC_VERSION
                                                      , pattern EXT_ROBUSTNESS_2_SPEC_VERSION
                                                      , EXT_ROBUSTNESS_2_EXTENSION_NAME
                                                      , pattern EXT_ROBUSTNESS_2_EXTENSION_NAME
                                                      ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT))
-- No documentation found for TopLevel "VkPhysicalDeviceRobustness2FeaturesEXT"
data PhysicalDeviceRobustness2FeaturesEXT = PhysicalDeviceRobustness2FeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceRobustness2FeaturesEXT" "robustBufferAccess2"
    robustBufferAccess2 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRobustness2FeaturesEXT" "robustImageAccess2"
    robustImageAccess2 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRobustness2FeaturesEXT" "nullDescriptor"
    nullDescriptor :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceRobustness2FeaturesEXT

instance ToCStruct PhysicalDeviceRobustness2FeaturesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRobustness2FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (robustBufferAccess2))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (robustImageAccess2))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (nullDescriptor))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRobustness2FeaturesEXT where
  peekCStruct p = do
    robustBufferAccess2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    robustImageAccess2 <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    nullDescriptor <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceRobustness2FeaturesEXT
             (bool32ToBool robustBufferAccess2) (bool32ToBool robustImageAccess2) (bool32ToBool nullDescriptor)

instance Storable PhysicalDeviceRobustness2FeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRobustness2FeaturesEXT where
  zero = PhysicalDeviceRobustness2FeaturesEXT
           zero
           zero
           zero


-- No documentation found for TopLevel "VkPhysicalDeviceRobustness2PropertiesEXT"
data PhysicalDeviceRobustness2PropertiesEXT = PhysicalDeviceRobustness2PropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceRobustness2PropertiesEXT" "robustStorageBufferAccessSizeAlignment"
    robustStorageBufferAccessSizeAlignment :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceRobustness2PropertiesEXT" "robustUniformBufferAccessSizeAlignment"
    robustUniformBufferAccessSizeAlignment :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceRobustness2PropertiesEXT

instance ToCStruct PhysicalDeviceRobustness2PropertiesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRobustness2PropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (robustStorageBufferAccessSizeAlignment)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (robustUniformBufferAccessSizeAlignment)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceRobustness2PropertiesEXT where
  peekCStruct p = do
    robustStorageBufferAccessSizeAlignment <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    robustUniformBufferAccessSizeAlignment <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ PhysicalDeviceRobustness2PropertiesEXT
             robustStorageBufferAccessSizeAlignment robustUniformBufferAccessSizeAlignment

instance Storable PhysicalDeviceRobustness2PropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRobustness2PropertiesEXT where
  zero = PhysicalDeviceRobustness2PropertiesEXT
           zero
           zero


type EXT_ROBUSTNESS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_ROBUSTNESS_2_SPEC_VERSION"
pattern EXT_ROBUSTNESS_2_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_ROBUSTNESS_2_SPEC_VERSION = 1


type EXT_ROBUSTNESS_2_EXTENSION_NAME = "VK_EXT_robustness2"

-- No documentation found for TopLevel "VK_EXT_ROBUSTNESS_2_EXTENSION_NAME"
pattern EXT_ROBUSTNESS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_ROBUSTNESS_2_EXTENSION_NAME = "VK_EXT_robustness2"

