{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_astc_decode_mode"
module Vulkan.Extensions.VK_EXT_astc_decode_mode  ( ImageViewASTCDecodeModeEXT(..)
                                                  , PhysicalDeviceASTCDecodeFeaturesEXT(..)
                                                  , EXT_ASTC_DECODE_MODE_SPEC_VERSION
                                                  , pattern EXT_ASTC_DECODE_MODE_SPEC_VERSION
                                                  , EXT_ASTC_DECODE_MODE_EXTENSION_NAME
                                                  , pattern EXT_ASTC_DECODE_MODE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT))

-- No documentation found for TopLevel "VkImageViewASTCDecodeModeEXT"
data ImageViewASTCDecodeModeEXT = ImageViewASTCDecodeModeEXT
  { -- No documentation found for Nested "VkImageViewASTCDecodeModeEXT" "decodeMode"
    decodeMode :: Format }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageViewASTCDecodeModeEXT)
#endif
deriving instance Show ImageViewASTCDecodeModeEXT

instance ToCStruct ImageViewASTCDecodeModeEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageViewASTCDecodeModeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (decodeMode)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    f

instance FromCStruct ImageViewASTCDecodeModeEXT where
  peekCStruct p = do
    decodeMode <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    pure $ ImageViewASTCDecodeModeEXT
             decodeMode


instance Storable ImageViewASTCDecodeModeEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageViewASTCDecodeModeEXT where
  zero = ImageViewASTCDecodeModeEXT
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceASTCDecodeFeaturesEXT"
data PhysicalDeviceASTCDecodeFeaturesEXT = PhysicalDeviceASTCDecodeFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceASTCDecodeFeaturesEXT" "decodeModeSharedExponent"
    decodeModeSharedExponent :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceASTCDecodeFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceASTCDecodeFeaturesEXT

instance ToCStruct PhysicalDeviceASTCDecodeFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceASTCDecodeFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (decodeModeSharedExponent))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceASTCDecodeFeaturesEXT where
  peekCStruct p = do
    decodeModeSharedExponent <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceASTCDecodeFeaturesEXT
             (bool32ToBool decodeModeSharedExponent)


instance Storable PhysicalDeviceASTCDecodeFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceASTCDecodeFeaturesEXT where
  zero = PhysicalDeviceASTCDecodeFeaturesEXT
           zero


type EXT_ASTC_DECODE_MODE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION"
pattern EXT_ASTC_DECODE_MODE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_ASTC_DECODE_MODE_SPEC_VERSION = 1


type EXT_ASTC_DECODE_MODE_EXTENSION_NAME = "VK_EXT_astc_decode_mode"

-- No documentation found for TopLevel "VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME"
pattern EXT_ASTC_DECODE_MODE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_ASTC_DECODE_MODE_EXTENSION_NAME = "VK_EXT_astc_decode_mode"

