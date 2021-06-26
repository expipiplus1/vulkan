{-# language CPP #-}
-- | = Name
--
-- VK_KHR_zero_initialize_workgroup_memory - device extension
--
-- == VK_KHR_zero_initialize_workgroup_memory
--
-- [__Name String__]
--     @VK_KHR_zero_initialize_workgroup_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     326
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Alan Baker
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_zero_initialize_workgroup_memory:%20&body=@alan-baker%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-11-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--     None
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
--     -   Jeff Bolz, Nvidia
--
--     -   Jason Ekstrand, Intel
--
-- == Description
--
-- This extension allows the use of a null constant initializer on shader
-- Workgroup memory variables, allowing implementations to expose any
-- special hardware or instructions they may have. Zero initialization is
-- commonly used by applications running untrusted content (e.g. web
-- browsers) as way of defeating memory-scraping attacks.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME'
--
-- -   'KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-11-18 (Alan Baker)
--
--     -   Internal draft version
--
-- = See Also
--
-- 'PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_zero_initialize_workgroup_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_zero_initialize_workgroup_memory  ( PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR(..)
                                                                  , KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION
                                                                  , pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION
                                                                  , KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME
                                                                  , pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR))
-- | VkPhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR - Structure
-- describing support for zero initialization of workgroup memory by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR'
-- /can/ also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR = PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR
  { -- | #features-shaderZeroInitializeWorkgroupMemory#
    -- @shaderZeroInitializeWorkgroupMemory@ specifies whether the
    -- implementation supports initializing a variable in Workgroup storage
    -- class.
    shaderZeroInitializeWorkgroupMemory :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR

instance ToCStruct PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderZeroInitializeWorkgroupMemory))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR where
  peekCStruct p = do
    shaderZeroInitializeWorkgroupMemory <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR
             (bool32ToBool shaderZeroInitializeWorkgroupMemory)

instance Storable PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR where
  zero = PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR
           zero


type KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION"
pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION = 1


type KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME = "VK_KHR_zero_initialize_workgroup_memory"

-- No documentation found for TopLevel "VK_KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME"
pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME = "VK_KHR_zero_initialize_workgroup_memory"

