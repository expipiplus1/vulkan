{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_filter_cubic"
module Vulkan.Extensions.VK_EXT_filter_cubic  ( pattern FILTER_CUBIC_EXT
                                              , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT
                                              , PhysicalDeviceImageViewImageFormatInfoEXT(..)
                                              , FilterCubicImageViewImageFormatPropertiesEXT(..)
                                              , EXT_FILTER_CUBIC_SPEC_VERSION
                                              , pattern EXT_FILTER_CUBIC_SPEC_VERSION
                                              , EXT_FILTER_CUBIC_EXTENSION_NAME
                                              , pattern EXT_FILTER_CUBIC_EXTENSION_NAME
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
import Vulkan.Core10.Enums.ImageViewType (ImageViewType)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.Filter (Filter(FILTER_CUBIC_IMG))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT))
-- No documentation found for TopLevel "VK_FILTER_CUBIC_EXT"
pattern FILTER_CUBIC_EXT = FILTER_CUBIC_IMG


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT = FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG



-- No documentation found for TopLevel "VkPhysicalDeviceImageViewImageFormatInfoEXT"
data PhysicalDeviceImageViewImageFormatInfoEXT = PhysicalDeviceImageViewImageFormatInfoEXT
  { -- No documentation found for Nested "VkPhysicalDeviceImageViewImageFormatInfoEXT" "imageViewType"
    imageViewType :: ImageViewType }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceImageViewImageFormatInfoEXT)
#endif
deriving instance Show PhysicalDeviceImageViewImageFormatInfoEXT

instance ToCStruct PhysicalDeviceImageViewImageFormatInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceImageViewImageFormatInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageViewType)) (imageViewType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageViewType)) (zero)
    f

instance FromCStruct PhysicalDeviceImageViewImageFormatInfoEXT where
  peekCStruct p = do
    imageViewType <- peek @ImageViewType ((p `plusPtr` 16 :: Ptr ImageViewType))
    pure $ PhysicalDeviceImageViewImageFormatInfoEXT
             imageViewType


instance Storable PhysicalDeviceImageViewImageFormatInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceImageViewImageFormatInfoEXT where
  zero = PhysicalDeviceImageViewImageFormatInfoEXT
           zero



-- No documentation found for TopLevel "VkFilterCubicImageViewImageFormatPropertiesEXT"
data FilterCubicImageViewImageFormatPropertiesEXT = FilterCubicImageViewImageFormatPropertiesEXT
  { -- No documentation found for Nested "VkFilterCubicImageViewImageFormatPropertiesEXT" "filterCubic"
    filterCubic :: Bool
  , -- No documentation found for Nested "VkFilterCubicImageViewImageFormatPropertiesEXT" "filterCubicMinmax"
    filterCubicMinmax :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FilterCubicImageViewImageFormatPropertiesEXT)
#endif
deriving instance Show FilterCubicImageViewImageFormatPropertiesEXT

instance ToCStruct FilterCubicImageViewImageFormatPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FilterCubicImageViewImageFormatPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (filterCubic))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (filterCubicMinmax))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct FilterCubicImageViewImageFormatPropertiesEXT where
  peekCStruct p = do
    filterCubic <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    filterCubicMinmax <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ FilterCubicImageViewImageFormatPropertiesEXT
             (bool32ToBool filterCubic) (bool32ToBool filterCubicMinmax)


instance Storable FilterCubicImageViewImageFormatPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FilterCubicImageViewImageFormatPropertiesEXT where
  zero = FilterCubicImageViewImageFormatPropertiesEXT
           zero
           zero


type EXT_FILTER_CUBIC_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_EXT_FILTER_CUBIC_SPEC_VERSION"
pattern EXT_FILTER_CUBIC_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FILTER_CUBIC_SPEC_VERSION = 3


type EXT_FILTER_CUBIC_EXTENSION_NAME = "VK_EXT_filter_cubic"

-- No documentation found for TopLevel "VK_EXT_FILTER_CUBIC_EXTENSION_NAME"
pattern EXT_FILTER_CUBIC_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FILTER_CUBIC_EXTENSION_NAME = "VK_EXT_filter_cubic"

