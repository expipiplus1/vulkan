{-# language CPP #-}
-- | = Name
--
-- VK_EXT_index_type_uint8 - device extension
--
-- = Registered Extension Number
--
-- 266
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows @uint8_t@ indices to be used with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceIndexTypeUint8FeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_INDEX_TYPE_UINT8_EXTENSION_NAME'
--
-- -   'EXT_INDEX_TYPE_UINT8_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.IndexType.IndexType':
--
--     -   'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-05-02 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceIndexTypeUint8FeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_index_type_uint8 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_index_type_uint8  ( PhysicalDeviceIndexTypeUint8FeaturesEXT(..)
                                                  , EXT_INDEX_TYPE_UINT8_SPEC_VERSION
                                                  , pattern EXT_INDEX_TYPE_UINT8_SPEC_VERSION
                                                  , EXT_INDEX_TYPE_UINT8_EXTENSION_NAME
                                                  , pattern EXT_INDEX_TYPE_UINT8_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT))
-- | VkPhysicalDeviceIndexTypeUint8FeaturesEXT - Structure describing whether
-- uint8 index type can be used
--
-- = Members
--
-- The members of the 'PhysicalDeviceIndexTypeUint8FeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceIndexTypeUint8FeaturesEXT' structure is included
-- in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceIndexTypeUint8FeaturesEXT' /can/ also be included in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceIndexTypeUint8FeaturesEXT = PhysicalDeviceIndexTypeUint8FeaturesEXT
  { -- | #features-indexTypeUint8# @indexTypeUint8@ indicates that
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT' can be used with
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
    indexTypeUint8 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceIndexTypeUint8FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceIndexTypeUint8FeaturesEXT

instance ToCStruct PhysicalDeviceIndexTypeUint8FeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceIndexTypeUint8FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (indexTypeUint8))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceIndexTypeUint8FeaturesEXT where
  peekCStruct p = do
    indexTypeUint8 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceIndexTypeUint8FeaturesEXT
             (bool32ToBool indexTypeUint8)

instance Storable PhysicalDeviceIndexTypeUint8FeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceIndexTypeUint8FeaturesEXT where
  zero = PhysicalDeviceIndexTypeUint8FeaturesEXT
           zero


type EXT_INDEX_TYPE_UINT8_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_INDEX_TYPE_UINT8_SPEC_VERSION"
pattern EXT_INDEX_TYPE_UINT8_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_INDEX_TYPE_UINT8_SPEC_VERSION = 1


type EXT_INDEX_TYPE_UINT8_EXTENSION_NAME = "VK_EXT_index_type_uint8"

-- No documentation found for TopLevel "VK_EXT_INDEX_TYPE_UINT8_EXTENSION_NAME"
pattern EXT_INDEX_TYPE_UINT8_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_INDEX_TYPE_UINT8_EXTENSION_NAME = "VK_EXT_index_type_uint8"

