{-# language CPP #-}
-- | = Name
--
-- VK_EXT_robustness2 - device extension
--
-- == VK_EXT_robustness2
--
-- [__Name String__]
--     @VK_EXT_robustness2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     287
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Liam Middlebrook
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_robustness2:%20&body=@liam-middlebrook%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-01-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Liam Middlebrook, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds stricter requirements for how out of bounds reads
-- and writes are handled. Most accesses /must/ be tightly bounds-checked,
-- out of bounds writes /must/ be discarded, out of bound reads /must/
-- return zero. Rather than allowing multiple possible (0,0,0,x) vectors,
-- the out of bounds values are treated as zero, and then missing
-- components are inserted based on the format as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-conversion-to-rgba Conversion to RGBA>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input-extraction vertex input attribute extraction>.
--
-- These additional requirements /may/ be expensive on some
-- implementations, and should only be enabled when truly necessary.
--
-- This extension also adds support for “null descriptors”, where
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE' /can/ be used instead of a
-- valid handle. Accesses to null descriptors have well-defined behavior,
-- and don’t rely on robustness.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRobustness2FeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRobustness2PropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ROBUSTNESS_2_EXTENSION_NAME'
--
-- -   'EXT_ROBUSTNESS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT'
--
-- == Issues
--
-- 1.  Why do
--     'PhysicalDeviceRobustness2PropertiesEXT'::@robustUniformBufferAccessSizeAlignment@
--     and
--     'PhysicalDeviceRobustness2PropertiesEXT'::@robustStorageBufferAccessSizeAlignment@
--     exist?
--
-- __RESOLVED__: Some implementations can’t efficiently tightly
-- bounds-check all buffer accesses. Rather, the size of the bound range is
-- padded to some power of two multiple, up to 256 bytes for uniform
-- buffers and up to 4 bytes for storage buffers, and that padded size is
-- bounds-checked. This is sufficient to implement D3D-like behavior,
-- because D3D only allows binding whole uniform buffers or ranges that are
-- a multiple of 256 bytes, and D3D raw and structured buffers only support
-- 32-bit accesses.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2019-11-01 (Jeff Bolz, Liam Middlebrook)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceRobustness2FeaturesEXT',
-- 'PhysicalDeviceRobustness2PropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_robustness2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_robustness2  ( PhysicalDeviceRobustness2FeaturesEXT(..)
                                             , PhysicalDeviceRobustness2PropertiesEXT(..)
                                             , EXT_ROBUSTNESS_2_SPEC_VERSION
                                             , pattern EXT_ROBUSTNESS_2_SPEC_VERSION
                                             , EXT_ROBUSTNESS_2_EXTENSION_NAME
                                             , pattern EXT_ROBUSTNESS_2_EXTENSION_NAME
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT))
-- | VkPhysicalDeviceRobustness2FeaturesEXT - Structure describing the
-- out-of-bounds behavior for an implementation
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
-- -   #features-robustBufferAccess2# @robustBufferAccess2@ indicates
--     whether buffer accesses are tightly bounds-checked against the range
--     of the descriptor. Uniform buffers /must/ be bounds-checked to the
--     range of the descriptor, where the range is rounded up to a multiple
--     of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-robustUniformBufferAccessSizeAlignment robustUniformBufferAccessSizeAlignment>.
--     Storage buffers /must/ be bounds-checked to the range of the
--     descriptor, where the range is rounded up to a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-robustStorageBufferAccessSizeAlignment robustStorageBufferAccessSizeAlignment>.
--     Out of bounds buffer loads will return zero values, and formatted
--     loads will have (0,0,1) values inserted for missing G, B, or A
--     components based on the format.
--
-- -   #features-robustImageAccess2# @robustImageAccess2@ indicates whether
--     image accesses are tightly bounds-checked against the dimensions of
--     the image view. Out of bounds image loads will return zero values,
--     with (0,0,1) values
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-conversion-to-rgba inserted for missing G, B, or A components>
--     based on the format.
--
-- -   #features-nullDescriptor# @nullDescriptor@ indicates whether
--     descriptors /can/ be written with a
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' resource or view, which are
--     considered valid to access and act as if the descriptor were bound
--     to nothing.
--
-- If the 'PhysicalDeviceRobustness2FeaturesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRobustness2FeaturesEXT' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage
--
-- -   #VUID-VkPhysicalDeviceRobustness2FeaturesEXT-robustBufferAccess2-04000#
--     If @robustBufferAccess2@ is enabled then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     /must/ also be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceRobustness2FeaturesEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT'
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRobustness2FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceRobustness2FeaturesEXT

instance ToCStruct PhysicalDeviceRobustness2FeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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
-- = Description
--
-- If the 'PhysicalDeviceRobustness2PropertiesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRobustness2PropertiesEXT = PhysicalDeviceRobustness2PropertiesEXT
  { -- | #limits-robustStorageBufferAccessSizeAlignment#
    -- @robustStorageBufferAccessSizeAlignment@ is the number of bytes that the
    -- range of a storage buffer descriptor is rounded up to when used for
    -- bounds-checking when
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    -- is enabled. This value is either 1 or 4.
    robustStorageBufferAccessSizeAlignment :: DeviceSize
  , -- | #limits-robustUniformBufferAccessSizeAlignment#
    -- @robustUniformBufferAccessSizeAlignment@ is the number of bytes that the
    -- range of a uniform buffer descriptor is rounded up to when used for
    -- bounds-checking when
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    -- is enabled. This value is a power of two in the range [1, 256].
    robustUniformBufferAccessSizeAlignment :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRobustness2PropertiesEXT)
#endif
deriving instance Show PhysicalDeviceRobustness2PropertiesEXT

instance ToCStruct PhysicalDeviceRobustness2PropertiesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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

