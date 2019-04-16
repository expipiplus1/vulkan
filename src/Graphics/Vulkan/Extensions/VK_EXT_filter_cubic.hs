{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_filter_cubic
  ( withCStructFilterCubicImageViewImageFormatPropertiesEXT
  , fromCStructFilterCubicImageViewImageFormatPropertiesEXT
  , FilterCubicImageViewImageFormatPropertiesEXT(..)
  , withCStructPhysicalDeviceImageViewImageFormatInfoEXT
  , fromCStructPhysicalDeviceImageViewImageFormatInfoEXT
  , PhysicalDeviceImageViewImageFormatInfoEXT(..)
  , pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION
  , pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME
  , pattern VK_FILTER_CUBIC_EXT
  , pattern VK_FILTER_CUBIC_IMG
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic
  ( VkFilterCubicImageViewImageFormatPropertiesEXT(..)
  , VkPhysicalDeviceImageViewImageFormatInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageViewType
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic
  ( pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME
  , pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION
  , pattern VK_FILTER_CUBIC_EXT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_IMG_filter_cubic
  ( pattern VK_FILTER_CUBIC_IMG
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  )


-- No documentation found for TopLevel "FilterCubicImageViewImageFormatPropertiesEXT"
data FilterCubicImageViewImageFormatPropertiesEXT = FilterCubicImageViewImageFormatPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "FilterCubicImageViewImageFormatPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FilterCubicImageViewImageFormatPropertiesEXT" "filterCubic"
  vkFilterCubic :: Bool
  , -- No documentation found for Nested "FilterCubicImageViewImageFormatPropertiesEXT" "filterCubicMinmax"
  vkFilterCubicMinmax :: Bool
  }
  deriving (Show, Eq)
withCStructFilterCubicImageViewImageFormatPropertiesEXT :: FilterCubicImageViewImageFormatPropertiesEXT -> (VkFilterCubicImageViewImageFormatPropertiesEXT -> IO a) -> IO a
withCStructFilterCubicImageViewImageFormatPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: FilterCubicImageViewImageFormatPropertiesEXT)) (\pPNext -> cont (VkFilterCubicImageViewImageFormatPropertiesEXT VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT pPNext (boolToBool32 (vkFilterCubic (from :: FilterCubicImageViewImageFormatPropertiesEXT))) (boolToBool32 (vkFilterCubicMinmax (from :: FilterCubicImageViewImageFormatPropertiesEXT)))))
fromCStructFilterCubicImageViewImageFormatPropertiesEXT :: VkFilterCubicImageViewImageFormatPropertiesEXT -> IO FilterCubicImageViewImageFormatPropertiesEXT
fromCStructFilterCubicImageViewImageFormatPropertiesEXT c = FilterCubicImageViewImageFormatPropertiesEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFilterCubicImageViewImageFormatPropertiesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkFilterCubic (c :: VkFilterCubicImageViewImageFormatPropertiesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkFilterCubicMinmax (c :: VkFilterCubicImageViewImageFormatPropertiesEXT)))
-- No documentation found for TopLevel "PhysicalDeviceImageViewImageFormatInfoEXT"
data PhysicalDeviceImageViewImageFormatInfoEXT = PhysicalDeviceImageViewImageFormatInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceImageViewImageFormatInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceImageViewImageFormatInfoEXT" "imageViewType"
  vkImageViewType :: ImageViewType
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceImageViewImageFormatInfoEXT :: PhysicalDeviceImageViewImageFormatInfoEXT -> (VkPhysicalDeviceImageViewImageFormatInfoEXT -> IO a) -> IO a
withCStructPhysicalDeviceImageViewImageFormatInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceImageViewImageFormatInfoEXT)) (\pPNext -> cont (VkPhysicalDeviceImageViewImageFormatInfoEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT pPNext (vkImageViewType (from :: PhysicalDeviceImageViewImageFormatInfoEXT))))
fromCStructPhysicalDeviceImageViewImageFormatInfoEXT :: VkPhysicalDeviceImageViewImageFormatInfoEXT -> IO PhysicalDeviceImageViewImageFormatInfoEXT
fromCStructPhysicalDeviceImageViewImageFormatInfoEXT c = PhysicalDeviceImageViewImageFormatInfoEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceImageViewImageFormatInfoEXT)))
                                                                                                   <*> pure (vkImageViewType (c :: VkPhysicalDeviceImageViewImageFormatInfoEXT))
