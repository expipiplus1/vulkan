{-# language CPP #-}
-- | = Name
--
-- VK_KHR_robustness2 - device extension
--
-- = VK_KHR_robustness2
--
-- [__Name String__]
--     @VK_KHR_robustness2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     613
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_robustness2] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_KHR_robustness2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-10
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
-- This extension is based on the @VK_EXT_robustness2@ extension. This
-- extension adds stricter requirements for how out of bounds reads and
-- writes are handled. Most accesses /must/ be tightly bounds-checked, out
-- of bounds writes /must/ be discarded, out of bound reads /must/ return
-- zero. Rather than allowing multiple possible (0,0,0,x) vectors, the out
-- of bounds values are treated as zero, and then missing components are
-- inserted based on the format as described in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#images-component-substitution>
-- and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fxvertex-input-extraction vertex input attribute extraction>.
--
-- These additional requirements /may/ be expensive on some
-- implementations, and should only be enabled when truly necessary.
--
-- This extension also adds support for “null descriptors”, where
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE' /can/ be used instead of a
-- valid handle. Accesses to null descriptors have well-defined behavior,
-- and do not rely on robustness.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRobustness2FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRobustness2PropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_ROBUSTNESS_2_EXTENSION_NAME'
--
-- -   'KHR_ROBUSTNESS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_KHR'
--
-- == Issues
--
-- 1.  Why do
--     'PhysicalDeviceRobustness2PropertiesKHR'::@robustUniformBufferAccessSizeAlignment@
--     and
--     'PhysicalDeviceRobustness2PropertiesKHR'::@robustStorageBufferAccessSizeAlignment@
--     exist?
--
-- __RESOLVED__: Some implementations cannot efficiently tightly
-- bounds-check all buffer accesses. Rather, the size of the bound range is
-- padded to some power of two multiple, up to 256 bytes for uniform
-- buffers and up to 4 bytes for storage buffers, and that padded size is
-- bounds-checked. This is sufficient to implement D3D-like behavior,
-- because D3D only allows binding whole uniform buffers or ranges that are
-- a multiple of 256 bytes, and D3D raw and structured buffers only support
-- 32-bit accesses.
--
-- == Version History
--
-- -   Revision 1, 2025-01-10 (Piers Daniell)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_robustness2 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_robustness2  ( PhysicalDeviceRobustness2FeaturesKHR(..)
                                             , PhysicalDeviceRobustness2PropertiesKHR(..)
                                             , KHR_ROBUSTNESS_2_SPEC_VERSION
                                             , pattern KHR_ROBUSTNESS_2_SPEC_VERSION
                                             , KHR_ROBUSTNESS_2_EXTENSION_NAME
                                             , pattern KHR_ROBUSTNESS_2_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_KHR))
-- | VkPhysicalDeviceRobustness2FeaturesKHR - Structure describing the
-- out-of-bounds behavior for an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceRobustness2FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceRobustness2FeaturesKHR', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage
--
-- -   #VUID-VkPhysicalDeviceRobustness2FeaturesKHR-robustBufferAccess2-04000#
--     If @robustBufferAccess2@ is enabled then
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     /must/ also be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceRobustness2FeaturesKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_robustness2 VK_EXT_robustness2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_robustness2 VK_KHR_robustness2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRobustness2FeaturesKHR = PhysicalDeviceRobustness2FeaturesKHR
  { -- | #features-robustBufferAccess2# @robustBufferAccess2@ enables
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-robust-buffer-access2>
    -- guarantees for shader buffer accesses.
    robustBufferAccess2 :: Bool
  , -- | #features-robustImageAccess2# @robustImageAccess2@ enables
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-robust-image-access2>
    -- guarantees for shader image accesses.
    robustImageAccess2 :: Bool
  , -- | #features-nullDescriptor# @nullDescriptor@ indicates whether descriptors
    -- /can/ be written with a 'Vulkan.Core10.APIConstants.NULL_HANDLE'
    -- resource or view, which are considered valid to access and act as if the
    -- descriptor were bound to nothing.
    nullDescriptor :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRobustness2FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceRobustness2FeaturesKHR

instance ToCStruct PhysicalDeviceRobustness2FeaturesKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRobustness2FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (robustBufferAccess2))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (robustImageAccess2))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (nullDescriptor))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRobustness2FeaturesKHR where
  peekCStruct p = do
    robustBufferAccess2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    robustImageAccess2 <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    nullDescriptor <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceRobustness2FeaturesKHR
             (bool32ToBool robustBufferAccess2)
             (bool32ToBool robustImageAccess2)
             (bool32ToBool nullDescriptor)

instance Storable PhysicalDeviceRobustness2FeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRobustness2FeaturesKHR where
  zero = PhysicalDeviceRobustness2FeaturesKHR
           zero
           zero
           zero


-- | VkPhysicalDeviceRobustness2PropertiesKHR - Structure describing robust
-- buffer access properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceRobustness2PropertiesKHR' structure is included in
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_robustness2 VK_EXT_robustness2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_robustness2 VK_KHR_robustness2>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRobustness2PropertiesKHR = PhysicalDeviceRobustness2PropertiesKHR
  { -- | #limits-robustStorageBufferAccessSizeAlignment#
    -- @robustStorageBufferAccessSizeAlignment@ is the number of bytes that the
    -- range of a storage buffer descriptor is rounded up to when used for
    -- bounds-checking when the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    -- feature is enabled. This value /must/ be either 1 or 4.
    robustStorageBufferAccessSizeAlignment :: DeviceSize
  , -- | #limits-robustUniformBufferAccessSizeAlignment#
    -- @robustUniformBufferAccessSizeAlignment@ is the number of bytes that the
    -- range of a uniform buffer descriptor is rounded up to when used for
    -- bounds-checking when the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
    -- feature is enabled. This value /must/ be a power of two in the range [1,
    -- 256].
    robustUniformBufferAccessSizeAlignment :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRobustness2PropertiesKHR)
#endif
deriving instance Show PhysicalDeviceRobustness2PropertiesKHR

instance ToCStruct PhysicalDeviceRobustness2PropertiesKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRobustness2PropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (robustStorageBufferAccessSizeAlignment)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (robustUniformBufferAccessSizeAlignment)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceRobustness2PropertiesKHR where
  peekCStruct p = do
    robustStorageBufferAccessSizeAlignment <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    robustUniformBufferAccessSizeAlignment <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ PhysicalDeviceRobustness2PropertiesKHR
             robustStorageBufferAccessSizeAlignment
             robustUniformBufferAccessSizeAlignment

instance Storable PhysicalDeviceRobustness2PropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRobustness2PropertiesKHR where
  zero = PhysicalDeviceRobustness2PropertiesKHR
           zero
           zero


type KHR_ROBUSTNESS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_ROBUSTNESS_2_SPEC_VERSION"
pattern KHR_ROBUSTNESS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_ROBUSTNESS_2_SPEC_VERSION = 1


type KHR_ROBUSTNESS_2_EXTENSION_NAME = "VK_KHR_robustness2"

-- No documentation found for TopLevel "VK_KHR_ROBUSTNESS_2_EXTENSION_NAME"
pattern KHR_ROBUSTNESS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ROBUSTNESS_2_EXTENSION_NAME = "VK_KHR_robustness2"

