{-# language CPP #-}
-- | = Name
--
-- VK_EXT_map_memory_placed - device extension
--
-- = VK_EXT_map_memory_placed
--
-- [__Name String__]
--     @VK_EXT_map_memory_placed@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     273
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>
--
-- [__Contact__]
--
--     -   Faith Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_map_memory_placed] @gfxstrand%0A*Here describe the issue or question you have about the VK_EXT_map_memory_placed extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_map_memory_placed.adoc VK_EXT_map_memory_placed>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Depends on apitext:VK_KHR_map_memory2
--
--     -   Interacts with apitext:VK_EXT_external_memory_host
--
-- [__Contributors__]
--
--     -   Faith Ekstrand, Collabora
--
--     -   Tobias Hector, AMD
--
--     -   James Jones, NVIDIA
--
--     -   Georg Lehmann, Valve
--
--     -   Derek Lesho, Codeweavers
--
-- == Description
--
-- This extension allows a client to request that
-- 'Vulkan.Extensions.VK_KHR_map_memory2.mapMemory2KHR' attempt to place
-- the memory map at a particular virtual address.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_map_memory2.MemoryMapInfoKHR':
--
--     -   'MemoryMapPlacedInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMapMemoryPlacedFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMapMemoryPlacedPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MAP_MEMORY_PLACED_EXTENSION_NAME'
--
-- -   'EXT_MAP_MEMORY_PLACED_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.MemoryMapFlagBits.MemoryMapFlagBits':
--
--     -   'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_map_memory2.MemoryUnmapFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_map_memory2.MEMORY_UNMAP_RESERVE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_MAP_PLACED_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 0, 2024-01-14 (Faith Ekstrand)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'MemoryMapPlacedInfoEXT', 'PhysicalDeviceMapMemoryPlacedFeaturesEXT',
-- 'PhysicalDeviceMapMemoryPlacedPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_map_memory_placed Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_map_memory_placed  ( PhysicalDeviceMapMemoryPlacedFeaturesEXT(..)
                                                   , PhysicalDeviceMapMemoryPlacedPropertiesEXT(..)
                                                   , MemoryMapPlacedInfoEXT(..)
                                                   , EXT_MAP_MEMORY_PLACED_SPEC_VERSION
                                                   , pattern EXT_MAP_MEMORY_PLACED_SPEC_VERSION
                                                   , EXT_MAP_MEMORY_PLACED_EXTENSION_NAME
                                                   , pattern EXT_MAP_MEMORY_PLACED_EXTENSION_NAME
                                                   , MemoryUnmapFlagBitsKHR(..)
                                                   , MemoryUnmapFlagsKHR
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_MAP_PLACED_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_PROPERTIES_EXT))
import Vulkan.Extensions.VK_KHR_map_memory2 (MemoryUnmapFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_map_memory2 (MemoryUnmapFlagsKHR)
-- | VkPhysicalDeviceMapMemoryPlacedFeaturesEXT - Structure describing placed
-- memory map features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceMapMemoryPlacedFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMapMemoryPlacedFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_map_memory_placed VK_EXT_map_memory_placed>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMapMemoryPlacedFeaturesEXT = PhysicalDeviceMapMemoryPlacedFeaturesEXT
  { -- | #features-memoryMapPlaced# @memoryMapPlaced@ indicates that the
    -- implementation supports placing memory maps at client-specified virtual
    -- addresses.
    memoryMapPlaced :: Bool
  , -- | #features-memoryMapRangePlaced# @memoryMapRangePlaced@ indicates that
    -- the implementation supports placing memory maps of a subrange of a
    -- memory object at client-specified virtual addresses.
    memoryMapRangePlaced :: Bool
  , -- | #features-memoryUnmapReserve# @memoryUnmapReserve@ indicates that the
    -- implementation supports leaving the memory range reserved when unmapping
    -- a memory object.
    memoryUnmapReserve :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMapMemoryPlacedFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceMapMemoryPlacedFeaturesEXT

instance ToCStruct PhysicalDeviceMapMemoryPlacedFeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMapMemoryPlacedFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (memoryMapPlaced))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (memoryMapRangePlaced))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (memoryUnmapReserve))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMapMemoryPlacedFeaturesEXT where
  peekCStruct p = do
    memoryMapPlaced <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    memoryMapRangePlaced <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    memoryUnmapReserve <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceMapMemoryPlacedFeaturesEXT
             (bool32ToBool memoryMapPlaced)
             (bool32ToBool memoryMapRangePlaced)
             (bool32ToBool memoryUnmapReserve)

instance Storable PhysicalDeviceMapMemoryPlacedFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMapMemoryPlacedFeaturesEXT where
  zero = PhysicalDeviceMapMemoryPlacedFeaturesEXT
           zero
           zero
           zero


-- | VkPhysicalDeviceMapMemoryPlacedPropertiesEXT - Structure describing the
-- alignment requirements of placed memory maps for a physical device
--
-- = Members
--
-- The members of the 'PhysicalDeviceMapMemoryPlacedPropertiesEXT'
-- structure describe the following:
--
-- = Description
--
-- If the 'PhysicalDeviceMapMemoryPlacedPropertiesEXT' structure is
-- included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_map_memory_placed VK_EXT_map_memory_placed>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMapMemoryPlacedPropertiesEXT = PhysicalDeviceMapMemoryPlacedPropertiesEXT
  { -- | #limits-minPlacedMemoryMapAlignment# @minPlacedMemoryMapAlignment@ is
    -- the minimum alignment required for memory object offsets and virtual
    -- address ranges when using placed memory mapping.
    minPlacedMemoryMapAlignment :: DeviceSize }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMapMemoryPlacedPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceMapMemoryPlacedPropertiesEXT

instance ToCStruct PhysicalDeviceMapMemoryPlacedPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMapMemoryPlacedPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (minPlacedMemoryMapAlignment)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceMapMemoryPlacedPropertiesEXT where
  peekCStruct p = do
    minPlacedMemoryMapAlignment <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ PhysicalDeviceMapMemoryPlacedPropertiesEXT
             minPlacedMemoryMapAlignment

instance Storable PhysicalDeviceMapMemoryPlacedPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMapMemoryPlacedPropertiesEXT where
  zero = PhysicalDeviceMapMemoryPlacedPropertiesEXT
           zero


-- | VkMemoryMapPlacedInfoEXT - Structure containing memory map placement
-- parameters
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryMapPlacedInfoEXT-flags-09576# If
--     'Vulkan.Extensions.VK_KHR_map_memory2.MemoryMapInfoKHR'::@flags@
--     contains
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT',
--     @pPlacedAddress@ /must/ not be @NULL@
--
-- -   #VUID-VkMemoryMapPlacedInfoEXT-pPlacedAddress-09577#
--     @pPlacedAddress@ /must/ be aligned to an integer multiple of
--     'PhysicalDeviceMapMemoryPlacedPropertiesEXT'::@minPlacedMemoryMapAlignment@
--
-- -   #VUID-VkMemoryMapPlacedInfoEXT-pPlacedAddress-09578# The address
--     range specified by @pPlacedAddress@ and
--     'Vulkan.Extensions.VK_KHR_map_memory2.MemoryMapInfoKHR'::@size@
--     /must/ not overlap any existing Vulkan memory object mapping.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryMapPlacedInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_MAP_PLACED_INFO_EXT'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_map_memory_placed VK_EXT_map_memory_placed>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryMapPlacedInfoEXT = MemoryMapPlacedInfoEXT
  { -- | @pPlacedAddress@ is the virtual address at which to place the address.
    -- If 'Vulkan.Extensions.VK_KHR_map_memory2.MemoryMapInfoKHR'::@flags@ does
    -- not contain
    -- 'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT', this
    -- value is ignored.
    placedAddress :: Ptr () }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryMapPlacedInfoEXT)
#endif
deriving instance Show MemoryMapPlacedInfoEXT

instance ToCStruct MemoryMapPlacedInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryMapPlacedInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_MAP_PLACED_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (placedAddress)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_MAP_PLACED_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct MemoryMapPlacedInfoEXT where
  peekCStruct p = do
    pPlacedAddress <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    pure $ MemoryMapPlacedInfoEXT
             pPlacedAddress

instance Storable MemoryMapPlacedInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryMapPlacedInfoEXT where
  zero = MemoryMapPlacedInfoEXT
           zero


type EXT_MAP_MEMORY_PLACED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_MAP_MEMORY_PLACED_SPEC_VERSION"
pattern EXT_MAP_MEMORY_PLACED_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_MAP_MEMORY_PLACED_SPEC_VERSION = 1


type EXT_MAP_MEMORY_PLACED_EXTENSION_NAME = "VK_EXT_map_memory_placed"

-- No documentation found for TopLevel "VK_EXT_MAP_MEMORY_PLACED_EXTENSION_NAME"
pattern EXT_MAP_MEMORY_PLACED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_MAP_MEMORY_PLACED_EXTENSION_NAME = "VK_EXT_map_memory_placed"

