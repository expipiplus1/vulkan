{-# language CPP #-}
-- | = Name
--
-- VK_EXT_memory_priority - device extension
--
-- == VK_EXT_memory_priority
--
-- [__Name String__]
--     @VK_EXT_memory_priority@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     239
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_memory_priority] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_memory_priority extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-08
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
-- == Description
--
-- This extension adds a @priority@ value specified at memory allocation
-- time. On some systems with both device-local and non-device-local memory
-- heaps, the implementation may transparently move memory from one heap to
-- another when a heap becomes full (for example, when the total memory
-- used across all processes exceeds the size of the heap). In such a case,
-- this priority value may be used to determine which allocations are more
-- likely to remain in device-local memory.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'MemoryPriorityAllocateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMemoryPriorityFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MEMORY_PRIORITY_EXTENSION_NAME'
--
-- -   'EXT_MEMORY_PRIORITY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-10-08 (Jeff Bolz)
--
--     -   Initial revision
--
-- == See Also
--
-- 'MemoryPriorityAllocateInfoEXT',
-- 'PhysicalDeviceMemoryPriorityFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_memory_priority Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_memory_priority  ( PhysicalDeviceMemoryPriorityFeaturesEXT(..)
                                                 , MemoryPriorityAllocateInfoEXT(..)
                                                 , EXT_MEMORY_PRIORITY_SPEC_VERSION
                                                 , pattern EXT_MEMORY_PRIORITY_SPEC_VERSION
                                                 , EXT_MEMORY_PRIORITY_EXTENSION_NAME
                                                 , pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME
                                                 ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT))
-- | VkPhysicalDeviceMemoryPriorityFeaturesEXT - Structure describing memory
-- priority features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMemoryPriorityFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMemoryPriorityFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_priority VK_EXT_memory_priority>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMemoryPriorityFeaturesEXT = PhysicalDeviceMemoryPriorityFeaturesEXT
  { -- | #features-memoryPriority# @memoryPriority@ indicates that the
    -- implementation supports memory priorities specified at memory allocation
    -- time via 'MemoryPriorityAllocateInfoEXT'.
    memoryPriority :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMemoryPriorityFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceMemoryPriorityFeaturesEXT

instance ToCStruct PhysicalDeviceMemoryPriorityFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMemoryPriorityFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (memoryPriority))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMemoryPriorityFeaturesEXT where
  peekCStruct p = do
    memoryPriority <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMemoryPriorityFeaturesEXT
             (bool32ToBool memoryPriority)

instance Storable PhysicalDeviceMemoryPriorityFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMemoryPriorityFeaturesEXT where
  zero = PhysicalDeviceMemoryPriorityFeaturesEXT
           zero


-- | VkMemoryPriorityAllocateInfoEXT - Specify a memory allocation priority
--
-- = Description
--
-- Memory allocations with higher priority /may/ be more likely to stay in
-- device-local memory when the system is under memory pressure.
--
-- If this structure is not included, it is as if the @priority@ value were
-- @0.5@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_priority VK_EXT_memory_priority>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryPriorityAllocateInfoEXT = MemoryPriorityAllocateInfoEXT
  { -- | @priority@ is a floating-point value between @0@ and @1@, indicating the
    -- priority of the allocation relative to other memory allocations. Larger
    -- values are higher priority. The granularity of the priorities is
    -- implementation-dependent.
    --
    -- #VUID-VkMemoryPriorityAllocateInfoEXT-priority-02602# @priority@ /must/
    -- be between @0@ and @1@, inclusive
    priority :: Float }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryPriorityAllocateInfoEXT)
#endif
deriving instance Show MemoryPriorityAllocateInfoEXT

instance ToCStruct MemoryPriorityAllocateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryPriorityAllocateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (priority))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct MemoryPriorityAllocateInfoEXT where
  peekCStruct p = do
    priority <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    pure $ MemoryPriorityAllocateInfoEXT
             (coerce @CFloat @Float priority)

instance Storable MemoryPriorityAllocateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryPriorityAllocateInfoEXT where
  zero = MemoryPriorityAllocateInfoEXT
           zero


type EXT_MEMORY_PRIORITY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_MEMORY_PRIORITY_SPEC_VERSION"
pattern EXT_MEMORY_PRIORITY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_MEMORY_PRIORITY_SPEC_VERSION = 1


type EXT_MEMORY_PRIORITY_EXTENSION_NAME = "VK_EXT_memory_priority"

-- No documentation found for TopLevel "VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME"
pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME = "VK_EXT_memory_priority"

