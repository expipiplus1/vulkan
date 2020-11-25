{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_texel_buffer_alignment"
module Vulkan.Extensions.VK_EXT_texel_buffer_alignment  ( PhysicalDeviceTexelBufferAlignmentFeaturesEXT(..)
                                                        , PhysicalDeviceTexelBufferAlignmentPropertiesEXT(..)
                                                        , EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION
                                                        , pattern EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION
                                                        , EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME
                                                        , pattern EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME
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
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT"
data PhysicalDeviceTexelBufferAlignmentFeaturesEXT = PhysicalDeviceTexelBufferAlignmentFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceTexelBufferAlignmentFeaturesEXT" "texelBufferAlignment"
    texelBufferAlignment :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTexelBufferAlignmentFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceTexelBufferAlignmentFeaturesEXT

instance ToCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTexelBufferAlignmentFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (texelBufferAlignment))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT where
  peekCStruct p = do
    texelBufferAlignment <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceTexelBufferAlignmentFeaturesEXT
             (bool32ToBool texelBufferAlignment)


instance Storable PhysicalDeviceTexelBufferAlignmentFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTexelBufferAlignmentFeaturesEXT where
  zero = PhysicalDeviceTexelBufferAlignmentFeaturesEXT
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT"
data PhysicalDeviceTexelBufferAlignmentPropertiesEXT = PhysicalDeviceTexelBufferAlignmentPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT" "storageTexelBufferOffsetAlignmentBytes"
    storageTexelBufferOffsetAlignmentBytes :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT" "storageTexelBufferOffsetSingleTexelAlignment"
    storageTexelBufferOffsetSingleTexelAlignment :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT" "uniformTexelBufferOffsetAlignmentBytes"
    uniformTexelBufferOffsetAlignmentBytes :: DeviceSize
  , -- No documentation found for Nested "VkPhysicalDeviceTexelBufferAlignmentPropertiesEXT" "uniformTexelBufferOffsetSingleTexelAlignment"
    uniformTexelBufferOffsetSingleTexelAlignment :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTexelBufferAlignmentPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceTexelBufferAlignmentPropertiesEXT

instance ToCStruct PhysicalDeviceTexelBufferAlignmentPropertiesEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTexelBufferAlignmentPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (storageTexelBufferOffsetAlignmentBytes)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (storageTexelBufferOffsetSingleTexelAlignment))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (uniformTexelBufferOffsetAlignmentBytes)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (uniformTexelBufferOffsetSingleTexelAlignment))
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTexelBufferAlignmentPropertiesEXT where
  peekCStruct p = do
    storageTexelBufferOffsetAlignmentBytes <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    storageTexelBufferOffsetSingleTexelAlignment <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    uniformTexelBufferOffsetAlignmentBytes <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    uniformTexelBufferOffsetSingleTexelAlignment <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pure $ PhysicalDeviceTexelBufferAlignmentPropertiesEXT
             storageTexelBufferOffsetAlignmentBytes (bool32ToBool storageTexelBufferOffsetSingleTexelAlignment) uniformTexelBufferOffsetAlignmentBytes (bool32ToBool uniformTexelBufferOffsetSingleTexelAlignment)


instance Storable PhysicalDeviceTexelBufferAlignmentPropertiesEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTexelBufferAlignmentPropertiesEXT where
  zero = PhysicalDeviceTexelBufferAlignmentPropertiesEXT
           zero
           zero
           zero
           zero


type EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION"
pattern EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION = 1


type EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME = "VK_EXT_texel_buffer_alignment"

-- No documentation found for TopLevel "VK_EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME"
pattern EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME = "VK_EXT_texel_buffer_alignment"

