{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_robustness2  ( PhysicalDeviceRobustness2FeaturesEXT(..)
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
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.Core10.BaseType (DeviceSize)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT))
-- | VkPhysicalDeviceRobustness2FeaturesEXT - Structure describing the
-- out-of-bounds behavior for an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceRobustness2FeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- -   @robustBufferAccess2@ indicates whether buffer accesses are tightly
--     bounds-checked against the range of the descriptor. Uniform buffers
--     /must/ be bounds-checked to the range of the descriptor, where the
--     range is rounded up to a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-robustUniformBufferAccessSizeAlignment robustUniformBufferAccessSizeAlignment>.
--     Storage buffers /must/ be bounds-checked to the range of the
--     descriptor, where the range is rounded up to a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-robustStorageBufferAccessSizeAlignment robustStorageBufferAccessSizeAlignment>.
--     Out of bounds buffer loads will return zero values, and formatted
--     loads will have (0,0,1) values inserted for missing G, B, or A
--     components based on the format.
--
-- -   @robustImageAccess2@ indicates whether image accesses are tightly
--     bounds-checked against the dimensions of the image view. Out of
--     bounds image loads will return zero values, with (0,0,1) values
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-conversion-to-rgba inserted for missing G, B, or A components>
--     based on the format.
--
-- -   @nullDescriptor@ indicates whether descriptors /can/ be written with
--     a 'Vulkan.Core10.APIConstants.NULL_HANDLE' resource or view, which
--     are considered valid to access and act as if the descriptor were
--     bound to nothing.
--
-- If the 'PhysicalDeviceRobustness2FeaturesEXT' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
--
-- == Valid Usage
--
-- -   If @robustBufferAccess2@ is enabled then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     /must/ also be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRobustness2FeaturesEXT = PhysicalDeviceRobustness2FeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceRobustness2FeaturesEXT" "robustBufferAccess2"
    robustBufferAccess2 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRobustness2FeaturesEXT" "robustImageAccess2"
    robustImageAccess2 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRobustness2FeaturesEXT" "nullDescriptor"
    nullDescriptor :: Bool
  }
  deriving (Typeable, Eq)
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


-- | VkPhysicalDeviceRobustness2PropertiesEXT - Structure describing robust
-- buffer access properties supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceRobustness2PropertiesEXT' structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- If the 'PhysicalDeviceRobustness2PropertiesEXT' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRobustness2PropertiesEXT = PhysicalDeviceRobustness2PropertiesEXT
  { -- | @robustStorageBufferAccessSizeAlignment@ is the number of bytes that the
    -- range of a storage buffer descriptor is rounded up to when used for
    -- bounds-checking when
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    -- is enabled. This value is either 1 or 4.
    robustStorageBufferAccessSizeAlignment :: DeviceSize
  , -- | @robustUniformBufferAccessSizeAlignment@ is the number of bytes that the
    -- range of a uniform buffer descriptor is rounded up to when used for
    -- bounds-checking when
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    -- is enabled. This value is a power of two in the range [1, 256].
    robustUniformBufferAccessSizeAlignment :: DeviceSize
  }
  deriving (Typeable, Eq)
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

