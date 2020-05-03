{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_memory_priority  ( PhysicalDeviceMemoryPriorityFeaturesEXT(..)
                                                 , MemoryPriorityAllocateInfoEXT(..)
                                                 , EXT_MEMORY_PRIORITY_SPEC_VERSION
                                                 , pattern EXT_MEMORY_PRIORITY_SPEC_VERSION
                                                 , EXT_MEMORY_PRIORITY_EXTENSION_NAME
                                                 , pattern EXT_MEMORY_PRIORITY_EXTENSION_NAME
                                                 ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT))
-- | VkPhysicalDeviceMemoryPriorityFeaturesEXT - Structure describing memory
-- priority features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceMemoryPriorityFeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceMemoryPriorityFeaturesEXT' structure is included
-- in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceMemoryPriorityFeaturesEXT' /can/ also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMemoryPriorityFeaturesEXT = PhysicalDeviceMemoryPriorityFeaturesEXT
  { -- | @memoryPriority@ indicates that the implementation supports memory
    -- priorities specified at memory allocation time via
    -- 'MemoryPriorityAllocateInfoEXT'.
    memoryPriority :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceMemoryPriorityFeaturesEXT

instance ToCStruct PhysicalDeviceMemoryPriorityFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryPriorityAllocateInfoEXT = MemoryPriorityAllocateInfoEXT
  { -- | @priority@ /must/ be between @0@ and @1@, inclusive
    priority :: Float }
  deriving (Typeable)
deriving instance Show MemoryPriorityAllocateInfoEXT

instance ToCStruct MemoryPriorityAllocateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
             ((\(CFloat a) -> a) priority)

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

