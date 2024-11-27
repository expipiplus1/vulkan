{-# language CPP #-}
-- | = Name
--
-- VK_NV_extended_sparse_address_space - device extension
--
-- == VK_NV_extended_sparse_address_space
--
-- [__Name String__]
--     @VK_NV_extended_sparse_address_space@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     493
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Russell Chou
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_extended_sparse_address_space] @russellcnv%0A*Here describe the issue or question you have about the VK_NV_extended_sparse_address_space extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-10-03
--
-- [__Contributors__]
--
--     -   Russell Chou, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- Implementations may be able to support an extended address space for
-- sparse memory resources, but only for a certain set of usages.
--
-- This extension adds a query for the extended limit, and the supported
-- usages that are allowed for that limit. This limit is an increase to
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@
-- when the 'Vulkan.Core10.Handles.Image' or 'Vulkan.Core10.Handles.Buffer'
-- uses only usages that are supported.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTENDED_SPARSE_ADDRESS_SPACE_EXTENSION_NAME'
--
-- -   'NV_EXTENDED_SPARSE_ADDRESS_SPACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2023-10-03 (Russell Chou)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV',
-- 'PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_extended_sparse_address_space Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_extended_sparse_address_space  ( PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV(..)
                                                              , PhysicalDeviceExtendedSparseAddressSpacePropertiesNV(..)
                                                              , NV_EXTENDED_SPARSE_ADDRESS_SPACE_SPEC_VERSION
                                                              , pattern NV_EXTENDED_SPARSE_ADDRESS_SPACE_SPEC_VERSION
                                                              , NV_EXTENDED_SPARSE_ADDRESS_SPACE_EXTENSION_NAME
                                                              , pattern NV_EXTENDED_SPARSE_ADDRESS_SPACE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_PROPERTIES_NV))
-- | VkPhysicalDeviceExtendedSparseAddressSpaceFeaturesNV - Structure
-- describing feature to use extended sparse address space
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_extended_sparse_address_space VK_NV_extended_sparse_address_space>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV = PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV
  { -- | #features-extendedSparseAddressSpace# @extendedSparseAddressSpace@
    -- indicates that the implementation supports allowing certain usages of
    -- sparse memory resources to exceed
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@.
    -- See 'PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'.
    extendedSparseAddressSpace :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV)
#endif
deriving instance Show PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV

instance ToCStruct PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (extendedSparseAddressSpace))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV where
  peekCStruct p = do
    extendedSparseAddressSpace <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV
             (bool32ToBool extendedSparseAddressSpace)

instance Storable PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV where
  zero = PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV
           zero


-- | VkPhysicalDeviceExtendedSparseAddressSpacePropertiesNV - Structure
-- describing sparse address space limits of an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceExtendedSparseAddressSpacePropertiesNV' structure
-- is included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_extended_sparse_address_space VK_NV_extended_sparse_address_space>,
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlags',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExtendedSparseAddressSpacePropertiesNV = PhysicalDeviceExtendedSparseAddressSpacePropertiesNV
  { -- | #limits-extendedSparseAddressSpaceSize# @extendedSparseAddressSpaceSize@
    -- is the total amount of address space available, in bytes, for sparse
    -- memory resources of all usages if the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-extendedSparseAddressSpace extendedSparseAddressSpace>
    -- feature is enabled. This /must/ be greater than or equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@,
    -- and the difference in space /must/ only be used with usages allowed
    -- below. This is an upper bound on the sum of the sizes of all sparse
    -- resources, regardless of whether any memory is bound to them.
    extendedSparseAddressSpaceSize :: DeviceSize
  , -- | #limits-extendedSparseImageUsageFlags# @extendedSparseImageUsageFlags@
    -- is a bitmask of
    -- 'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits' of usages
    -- which /may/ allow an implementation to use the full
    -- @extendedSparseAddressSpaceSize@ space.
    extendedSparseImageUsageFlags :: ImageUsageFlags
  , -- | #limits-extendedSparseBufferUsageFlags# @extendedSparseBufferUsageFlags@
    -- is a bitmask of
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits' of usages
    -- which /may/ allow an implementation to use the full
    -- @extendedSparseAddressSpaceSize@ space.
    extendedSparseBufferUsageFlags :: BufferUsageFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExtendedSparseAddressSpacePropertiesNV)
#endif
deriving instance Show PhysicalDeviceExtendedSparseAddressSpacePropertiesNV

instance ToCStruct PhysicalDeviceExtendedSparseAddressSpacePropertiesNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExtendedSparseAddressSpacePropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (extendedSparseAddressSpaceSize)
    poke ((p `plusPtr` 24 :: Ptr ImageUsageFlags)) (extendedSparseImageUsageFlags)
    poke ((p `plusPtr` 28 :: Ptr BufferUsageFlags)) (extendedSparseBufferUsageFlags)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageUsageFlags)) (zero)
    poke ((p `plusPtr` 28 :: Ptr BufferUsageFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceExtendedSparseAddressSpacePropertiesNV where
  peekCStruct p = do
    extendedSparseAddressSpaceSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    extendedSparseImageUsageFlags <- peek @ImageUsageFlags ((p `plusPtr` 24 :: Ptr ImageUsageFlags))
    extendedSparseBufferUsageFlags <- peek @BufferUsageFlags ((p `plusPtr` 28 :: Ptr BufferUsageFlags))
    pure $ PhysicalDeviceExtendedSparseAddressSpacePropertiesNV
             extendedSparseAddressSpaceSize
             extendedSparseImageUsageFlags
             extendedSparseBufferUsageFlags

instance Storable PhysicalDeviceExtendedSparseAddressSpacePropertiesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExtendedSparseAddressSpacePropertiesNV where
  zero = PhysicalDeviceExtendedSparseAddressSpacePropertiesNV
           zero
           zero
           zero


type NV_EXTENDED_SPARSE_ADDRESS_SPACE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTENDED_SPARSE_ADDRESS_SPACE_SPEC_VERSION"
pattern NV_EXTENDED_SPARSE_ADDRESS_SPACE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTENDED_SPARSE_ADDRESS_SPACE_SPEC_VERSION = 1


type NV_EXTENDED_SPARSE_ADDRESS_SPACE_EXTENSION_NAME = "VK_NV_extended_sparse_address_space"

-- No documentation found for TopLevel "VK_NV_EXTENDED_SPARSE_ADDRESS_SPACE_EXTENSION_NAME"
pattern NV_EXTENDED_SPARSE_ADDRESS_SPACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTENDED_SPARSE_ADDRESS_SPACE_EXTENSION_NAME = "VK_NV_extended_sparse_address_space"

