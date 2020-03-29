{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_index_type_uint8  ( PhysicalDeviceIndexTypeUint8FeaturesEXT(..)
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
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT))
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
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceIndexTypeUint8FeaturesEXT' /can/ also be included in the
-- @pNext@ chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceIndexTypeUint8FeaturesEXT = PhysicalDeviceIndexTypeUint8FeaturesEXT
  { -- | @indexTypeUint8@ indicates that
    -- 'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT' can be
    -- used with
    -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
    indexTypeUint8 :: Bool }
  deriving (Typeable)
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

