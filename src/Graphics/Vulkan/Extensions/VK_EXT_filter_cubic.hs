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
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkFilter(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic
  ( VkFilterCubicImageViewImageFormatPropertiesEXT(..)
  , VkPhysicalDeviceImageViewImageFormatInfoEXT(..)
  , pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME
  , pattern VK_EXT_FILTER_CUBIC_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageViewType
  )
import Graphics.Vulkan.Core10.Sampler
  ( pattern FILTER_CUBIC_IMG
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  )



-- | VkFilterCubicImageViewImageFormatPropertiesEXT - Structure for querying
-- cubic filtering capabilities of an image view type
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic.VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT'
--
-- == Valid Usage
--
-- -   If the @pNext@ chain of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2'
--     structure contains an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic.VkFilterCubicImageViewImageFormatPropertiesEXT',
--     the @pNext@ chain of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
--     structure /must/ contain an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic.VkPhysicalDeviceImageViewImageFormatInfoEXT'
--     with an @imageViewType@ that is compatible with @imageType@.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data FilterCubicImageViewImageFormatPropertiesEXT = FilterCubicImageViewImageFormatPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "FilterCubicImageViewImageFormatPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FilterCubicImageViewImageFormatPropertiesEXT" "filterCubic"
  filterCubic :: Bool
  , -- No documentation found for Nested "FilterCubicImageViewImageFormatPropertiesEXT" "filterCubicMinmax"
  filterCubicMinmax :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkFilterCubicImageViewImageFormatPropertiesEXT' and
-- marshal a 'FilterCubicImageViewImageFormatPropertiesEXT' into it. The 'VkFilterCubicImageViewImageFormatPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructFilterCubicImageViewImageFormatPropertiesEXT :: FilterCubicImageViewImageFormatPropertiesEXT -> (VkFilterCubicImageViewImageFormatPropertiesEXT -> IO a) -> IO a
withCStructFilterCubicImageViewImageFormatPropertiesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: FilterCubicImageViewImageFormatPropertiesEXT)) (\pPNext -> cont (VkFilterCubicImageViewImageFormatPropertiesEXT VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT pPNext (boolToBool32 (filterCubic (marshalled :: FilterCubicImageViewImageFormatPropertiesEXT))) (boolToBool32 (filterCubicMinmax (marshalled :: FilterCubicImageViewImageFormatPropertiesEXT)))))

-- | A function to read a 'VkFilterCubicImageViewImageFormatPropertiesEXT' and all additional
-- structures in the pointer chain into a 'FilterCubicImageViewImageFormatPropertiesEXT'.
fromCStructFilterCubicImageViewImageFormatPropertiesEXT :: VkFilterCubicImageViewImageFormatPropertiesEXT -> IO FilterCubicImageViewImageFormatPropertiesEXT
fromCStructFilterCubicImageViewImageFormatPropertiesEXT c = FilterCubicImageViewImageFormatPropertiesEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFilterCubicImageViewImageFormatPropertiesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkFilterCubic (c :: VkFilterCubicImageViewImageFormatPropertiesEXT)))
                                                                                                         <*> pure (bool32ToBool (vkFilterCubicMinmax (c :: VkFilterCubicImageViewImageFormatPropertiesEXT)))

instance Zero FilterCubicImageViewImageFormatPropertiesEXT where
  zero = FilterCubicImageViewImageFormatPropertiesEXT Nothing
                                                      False
                                                      False



-- | VkPhysicalDeviceImageViewImageFormatInfoEXT - Structure for providing
-- image view type
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceImageViewImageFormatInfoEXT = PhysicalDeviceImageViewImageFormatInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceImageViewImageFormatInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceImageViewImageFormatInfoEXT" "imageViewType"
  imageViewType :: ImageViewType
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceImageViewImageFormatInfoEXT' and
-- marshal a 'PhysicalDeviceImageViewImageFormatInfoEXT' into it. The 'VkPhysicalDeviceImageViewImageFormatInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceImageViewImageFormatInfoEXT :: PhysicalDeviceImageViewImageFormatInfoEXT -> (VkPhysicalDeviceImageViewImageFormatInfoEXT -> IO a) -> IO a
withCStructPhysicalDeviceImageViewImageFormatInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceImageViewImageFormatInfoEXT)) (\pPNext -> cont (VkPhysicalDeviceImageViewImageFormatInfoEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT pPNext (imageViewType (marshalled :: PhysicalDeviceImageViewImageFormatInfoEXT))))

-- | A function to read a 'VkPhysicalDeviceImageViewImageFormatInfoEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceImageViewImageFormatInfoEXT'.
fromCStructPhysicalDeviceImageViewImageFormatInfoEXT :: VkPhysicalDeviceImageViewImageFormatInfoEXT -> IO PhysicalDeviceImageViewImageFormatInfoEXT
fromCStructPhysicalDeviceImageViewImageFormatInfoEXT c = PhysicalDeviceImageViewImageFormatInfoEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceImageViewImageFormatInfoEXT)))
                                                                                                   <*> pure (vkImageViewType (c :: VkPhysicalDeviceImageViewImageFormatInfoEXT))

instance Zero PhysicalDeviceImageViewImageFormatInfoEXT where
  zero = PhysicalDeviceImageViewImageFormatInfoEXT Nothing
                                                   zero


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
