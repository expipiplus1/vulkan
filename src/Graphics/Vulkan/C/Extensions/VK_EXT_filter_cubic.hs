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


-- | VkFilterCubicImageViewImageFormatPropertiesEXT - Structure for querying
-- cubic filtering capabilities of an image view type
--
-- = Description
--
-- Unresolved directive in
-- VkFilterCubicImageViewImageFormatPropertiesEXT.txt -
-- include::..\/validity\/structs\/VkFilterCubicImageViewImageFormatPropertiesEXT.txt[]
--
-- == Valid Usage
--
-- -   If the @pNext@ chain of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2'
--     structure contains an instance of
--     'VkFilterCubicImageViewImageFormatPropertiesEXT', the @pNext@ chain
--     of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2'
--     structure /must/ contain an instance of
--     'VkPhysicalDeviceImageViewImageFormatInfoEXT' with an
--     @imageViewType@ that is compatible with @imageType@.
--
-- = See Also
--
-- No cross-references are available
data VkFilterCubicImageViewImageFormatPropertiesEXT = VkFilterCubicImageViewImageFormatPropertiesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @filterCubic@ tells if image format, image type and image view type
  -- /can/ be used with cubic filtering. This field is set by the
  -- implementation. User-specified value is ignored.
  vkFilterCubic :: VkBool32
  , -- | @filterCubicMinmax@ tells if image format, image type and image view
  -- type /can/ be used with cubic filtering and minmax filtering. This field
  -- is set by the implementation. User-specified value is ignored.
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
  zero = VkFilterCubicImageViewImageFormatPropertiesEXT zero
                                                        zero
                                                        zero
                                                        zero
-- | VkPhysicalDeviceImageViewImageFormatInfoEXT - Structure for providing
-- image view type
--
-- = Description
--
-- Unresolved directive in VkPhysicalDeviceImageViewImageFormatInfoEXT.txt
-- -
-- include::..\/validity\/structs\/VkPhysicalDeviceImageViewImageFormatInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceImageViewImageFormatInfoEXT = VkPhysicalDeviceImageViewImageFormatInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @imageViewType@ is a
  -- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType' value specifying
  -- the type of the image view.
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
  zero = VkPhysicalDeviceImageViewImageFormatInfoEXT zero
                                                     zero
                                                     zero
-- No documentation found for TopLevel "VK_EXT_FILTER_CUBIC_EXTENSION_NAME"
pattern VK_EXT_FILTER_CUBIC_EXTENSION_NAME :: (Eq a ,IsString a) => a
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
