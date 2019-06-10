{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic
  ( VkFilterCubicImageViewImageFormatPropertiesEXT(..)
  , VkPhysicalDeviceImageViewImageFormatInfoEXT(..)
  , pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME
  , pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION
  , pattern VK_FILTER_CUBIC_EXT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  , pattern VK_FILTER_CUBIC_IMG
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageViewType(..)
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkFilter(..)
  )
import Graphics.Vulkan.C.Extensions.VK_IMG_filter_cubic
  ( pattern VK_FILTER_CUBIC_IMG
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  )


-- No documentation found for TopLevel "VkFilterCubicImageViewImageFormatPropertiesEXT"
data VkFilterCubicImageViewImageFormatPropertiesEXT = VkFilterCubicImageViewImageFormatPropertiesEXT
  { -- No documentation found for Nested "VkFilterCubicImageViewImageFormatPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFilterCubicImageViewImageFormatPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFilterCubicImageViewImageFormatPropertiesEXT" "filterCubic"
  vkFilterCubic :: VkBool32
  , -- No documentation found for Nested "VkFilterCubicImageViewImageFormatPropertiesEXT" "filterCubicMinmax"
  vkFilterCubicMinmax :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkFilterCubicImageViewImageFormatPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkFilterCubicImageViewImageFormatPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFilterCubicImageViewImageFormatPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFilterCubicImageViewImageFormatPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkFilterCubic (poked :: VkFilterCubicImageViewImageFormatPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkFilterCubicMinmax (poked :: VkFilterCubicImageViewImageFormatPropertiesEXT))

instance Zero VkFilterCubicImageViewImageFormatPropertiesEXT where
  zero = VkFilterCubicImageViewImageFormatPropertiesEXT VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
                                                        zero
                                                        zero
                                                        zero

-- No documentation found for TopLevel "VkPhysicalDeviceImageViewImageFormatInfoEXT"
data VkPhysicalDeviceImageViewImageFormatInfoEXT = VkPhysicalDeviceImageViewImageFormatInfoEXT
  { -- No documentation found for Nested "VkPhysicalDeviceImageViewImageFormatInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceImageViewImageFormatInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceImageViewImageFormatInfoEXT" "imageViewType"
  vkImageViewType :: VkImageViewType
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceImageViewImageFormatInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceImageViewImageFormatInfoEXT <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceImageViewImageFormatInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceImageViewImageFormatInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkImageViewType (poked :: VkPhysicalDeviceImageViewImageFormatInfoEXT))

instance Zero VkPhysicalDeviceImageViewImageFormatInfoEXT where
  zero = VkPhysicalDeviceImageViewImageFormatInfoEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
                                                     zero
                                                     zero

-- No documentation found for TopLevel "VK_EXT_FILTER_CUBIC_EXTENSION_NAME"
pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME = "VK_EXT_filter_cubic"

-- No documentation found for TopLevel "VK_EXT_FILTER_CUBIC_SPEC_VERSION"
pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION :: Integral a => a
pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_FILTER_CUBIC_EXT"
pattern VK_FILTER_CUBIC_EXT :: VkFilter
pattern VK_FILTER_CUBIC_EXT = VK_FILTER_CUBIC_IMG

-- No documentation found for TopLevel "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT = VkStructureType 1000170001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT = VkStructureType 1000170000
