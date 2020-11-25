{-# language CPP #-}
-- | = Name
--
-- VK_AMD_device_coherent_memory - device extension
--
-- == VK_AMD_device_coherent_memory
--
-- [__Name String__]
--     @VK_AMD_device_coherent_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     230
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
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_AMD_device_coherent_memory:%20&body=@tobski%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-02-04
--
-- [__Contributors__]
--
--     -   Ping Fu, AMD
--
--     -   Timothy Lottes, AMD
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension adds the device coherent and device uncached memory
-- types. Any device accesses to device coherent memory are automatically
-- made visible to any other device access. Device uncached memory
-- indicates to applications that caches are disabled for a particular
-- memory type, which guarantees device coherence.
--
-- Device coherent and uncached memory are expected to have lower
-- performance for general access than non-device coherent memory, but can
-- be useful in certain scenarios; particularly so for debugging.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCoherentMemoryFeaturesAMD'
--
-- == New Enum Constants
--
-- -   'AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME'
--
-- -   'AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MemoryPropertyFlagBits':
--
--     -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--
--     -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD'
--
-- == Version History
--
-- -   Revision 1, 2019-02-04 (Tobias Hector)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PhysicalDeviceCoherentMemoryFeaturesAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_device_coherent_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_device_coherent_memory  ( PhysicalDeviceCoherentMemoryFeaturesAMD(..)
                                                        , AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION
                                                        , pattern AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION
                                                        , AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME
                                                        , pattern AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME
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
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD))
-- | VkPhysicalDeviceCoherentMemoryFeaturesAMD - Structure describing whether
-- device coherent memory can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceCoherentMemoryFeaturesAMD' structure
-- describe the following features:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCoherentMemoryFeaturesAMD = PhysicalDeviceCoherentMemoryFeaturesAMD
  { -- | #features-deviceCoherentMemory# @deviceCoherentMemory@ indicates that
    -- the implementation supports
    -- <VkMemoryPropertyFlagBits.html device coherent memory>.
    deviceCoherentMemory :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCoherentMemoryFeaturesAMD)
#endif
deriving instance Show PhysicalDeviceCoherentMemoryFeaturesAMD

instance ToCStruct PhysicalDeviceCoherentMemoryFeaturesAMD where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCoherentMemoryFeaturesAMD{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceCoherentMemory))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCoherentMemoryFeaturesAMD where
  peekCStruct p = do
    deviceCoherentMemory <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCoherentMemoryFeaturesAMD
             (bool32ToBool deviceCoherentMemory)

instance Storable PhysicalDeviceCoherentMemoryFeaturesAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCoherentMemoryFeaturesAMD where
  zero = PhysicalDeviceCoherentMemoryFeaturesAMD
           zero


type AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION"
pattern AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION = 1


type AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME = "VK_AMD_device_coherent_memory"

-- No documentation found for TopLevel "VK_AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME"
pattern AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME = "VK_AMD_device_coherent_memory"

