{-# language CPP #-}
-- | = Name
--
-- VK_NV_descriptor_pool_overallocation - device extension
--
-- == VK_NV_descriptor_pool_overallocation
--
-- [__Name String__]
--     @VK_NV_descriptor_pool_overallocation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     547
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_descriptor_pool_overallocation] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_NV_descriptor_pool_overallocation extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-08-30
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- There are scenarios where the application does not know ahead of time
-- how many descriptor sets it may need to allocate from a descriptor pool,
-- or how many descriptors of any of the descriptor types it may need to
-- allocate from the descriptor pool.
--
-- This extension gives applications the ability to request the
-- implementation allow more sets or descriptors to be allocated than
-- initially specified at descriptor pool creation time, subject to
-- available resources.
--
-- The
-- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_SETS_BIT_NV'
-- flag lets the application allocate more than
-- 'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo'::@maxSets@
-- descriptor sets, and the
-- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_POOLS_BIT_NV'
-- lets the application allocate more descriptors than initially specified
-- by 'Vulkan.Core10.DescriptorSet.DescriptorPoolSize'::@descriptorCount@
-- for any descriptor types.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorPoolOverallocationFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_DESCRIPTOR_POOL_OVERALLOCATION_EXTENSION_NAME'
--
-- -   'NV_DESCRIPTOR_POOL_OVERALLOCATION_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_POOLS_BIT_NV'
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_SETS_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_POOL_OVERALLOCATION_FEATURES_NV'
--
-- == Version History
--
-- -   Revision 1, 2023-08-30 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceDescriptorPoolOverallocationFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_descriptor_pool_overallocation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_descriptor_pool_overallocation  ( PhysicalDeviceDescriptorPoolOverallocationFeaturesNV(..)
                                                               , NV_DESCRIPTOR_POOL_OVERALLOCATION_SPEC_VERSION
                                                               , pattern NV_DESCRIPTOR_POOL_OVERALLOCATION_SPEC_VERSION
                                                               , NV_DESCRIPTOR_POOL_OVERALLOCATION_EXTENSION_NAME
                                                               , pattern NV_DESCRIPTOR_POOL_OVERALLOCATION_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_POOL_OVERALLOCATION_FEATURES_NV))
-- | VkPhysicalDeviceDescriptorPoolOverallocationFeaturesNV - Structure
-- describing feature to allow descriptor pool overallocation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorPoolOverallocationFeaturesNV' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDescriptorPoolOverallocationFeaturesNV' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_descriptor_pool_overallocation VK_NV_descriptor_pool_overallocation>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorPoolOverallocationFeaturesNV = PhysicalDeviceDescriptorPoolOverallocationFeaturesNV
  { -- | #features-descriptorPoolOverallocation# @descriptorPoolOverallocation@
    -- indicates that the implementation allows the application to opt into
    -- descriptor pool overallocation by creating the descriptor pool with
    -- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_SETS_BIT_NV'
    -- and\/or
    -- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_POOLS_BIT_NV'
    -- flags.
    descriptorPoolOverallocation :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorPoolOverallocationFeaturesNV)
#endif
deriving instance Show PhysicalDeviceDescriptorPoolOverallocationFeaturesNV

instance ToCStruct PhysicalDeviceDescriptorPoolOverallocationFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorPoolOverallocationFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_POOL_OVERALLOCATION_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (descriptorPoolOverallocation))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_POOL_OVERALLOCATION_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorPoolOverallocationFeaturesNV where
  peekCStruct p = do
    descriptorPoolOverallocation <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDescriptorPoolOverallocationFeaturesNV
             (bool32ToBool descriptorPoolOverallocation)

instance Storable PhysicalDeviceDescriptorPoolOverallocationFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorPoolOverallocationFeaturesNV where
  zero = PhysicalDeviceDescriptorPoolOverallocationFeaturesNV
           zero


type NV_DESCRIPTOR_POOL_OVERALLOCATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_DESCRIPTOR_POOL_OVERALLOCATION_SPEC_VERSION"
pattern NV_DESCRIPTOR_POOL_OVERALLOCATION_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DESCRIPTOR_POOL_OVERALLOCATION_SPEC_VERSION = 1


type NV_DESCRIPTOR_POOL_OVERALLOCATION_EXTENSION_NAME = "VK_NV_descriptor_pool_overallocation"

-- No documentation found for TopLevel "VK_NV_DESCRIPTOR_POOL_OVERALLOCATION_EXTENSION_NAME"
pattern NV_DESCRIPTOR_POOL_OVERALLOCATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DESCRIPTOR_POOL_OVERALLOCATION_EXTENSION_NAME = "VK_NV_descriptor_pool_overallocation"

