{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_filter_cubic
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  FilterCubicImageViewImageFormatPropertiesEXT(..)
  , 
  PhysicalDeviceImageViewImageFormatInfoEXT(..)
#endif
  , pattern EXT_FILTER_CUBIC_EXTENSION_NAME
  , pattern EXT_FILTER_CUBIC_SPEC_VERSION
  , pattern FILTER_CUBIC_EXT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT
  , pattern FILTER_CUBIC_IMG
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  , pattern STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkFilter(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic
  ( pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME
  , pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.ImageView
  ( ImageViewType
  )
#endif
import Graphics.Vulkan.Core10.Sampler
  ( pattern FILTER_CUBIC_IMG
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkFilterCubicImageViewImageFormatPropertiesEXT"
data FilterCubicImageViewImageFormatPropertiesEXT = FilterCubicImageViewImageFormatPropertiesEXT
  { -- No documentation found for Nested "FilterCubicImageViewImageFormatPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FilterCubicImageViewImageFormatPropertiesEXT" "filterCubic"
  filterCubic :: Bool
  , -- No documentation found for Nested "FilterCubicImageViewImageFormatPropertiesEXT" "filterCubicMinmax"
  filterCubicMinmax :: Bool
  }
  deriving (Show, Eq)

instance Zero FilterCubicImageViewImageFormatPropertiesEXT where
  zero = FilterCubicImageViewImageFormatPropertiesEXT Nothing
                                                      False
                                                      False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceImageViewImageFormatInfoEXT"
data PhysicalDeviceImageViewImageFormatInfoEXT = PhysicalDeviceImageViewImageFormatInfoEXT
  { -- No documentation found for Nested "PhysicalDeviceImageViewImageFormatInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceImageViewImageFormatInfoEXT" "imageViewType"
  imageViewType :: ImageViewType
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceImageViewImageFormatInfoEXT where
  zero = PhysicalDeviceImageViewImageFormatInfoEXT Nothing
                                                   zero

#endif

-- No documentation found for TopLevel "VK_EXT_FILTER_CUBIC_EXTENSION_NAME"
pattern EXT_FILTER_CUBIC_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_FILTER_CUBIC_EXTENSION_NAME = VK_EXT_FILTER_CUBIC_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_FILTER_CUBIC_SPEC_VERSION"
pattern EXT_FILTER_CUBIC_SPEC_VERSION :: Integral a => a
pattern EXT_FILTER_CUBIC_SPEC_VERSION = VK_EXT_FILTER_CUBIC_SPEC_VERSION

-- No documentation found for TopLevel "FILTER_CUBIC_EXT"
pattern FILTER_CUBIC_EXT :: VkFilter
pattern FILTER_CUBIC_EXT = FILTER_CUBIC_IMG

-- No documentation found for TopLevel "FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT :: VkFormatFeatureFlagBits
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT = FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
