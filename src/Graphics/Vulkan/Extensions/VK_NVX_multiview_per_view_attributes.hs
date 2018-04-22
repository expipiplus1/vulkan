{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  , pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  , VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
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


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( VkSubpassDescriptionFlagBits(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX = VkStructureType 1000097000
-- | @VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX@ specifies that
-- shaders compiled for this subpass write the attributes for all views in
-- a single invocation of each vertex processing stage. All pipelines
-- compiled against a subpass that includes this bit /must/ write per-view
-- attributes to the code:*PerViewNV[] shader outputs, in addition to the
-- non-per-view (e.g. @Position@) outputs.
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX :: VkSubpassDescriptionFlagBits
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX = VkSubpassDescriptionFlagBits 0x00000001
-- | @VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX@ specifies that
-- shaders compiled for this subpass use per-view positions which only
-- differ in value in the x component. Per-view viewport mask /can/ also be
-- used.
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX :: VkSubpassDescriptionFlagBits
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX = VkSubpassDescriptionFlagBits 0x00000002
-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION"
pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION :: Integral a => a
pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME"
pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = "VK_NVX_multiview_per_view_attributes"
-- | VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX - Structure
-- describing multiview limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- @VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX@ structure
-- describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @perViewPositionAllComponents@ is @VK_TRUE@ if the implementation
--     supports per-view position values that differ in components other
--     than the X component.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX@
--
-- If the @VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX@
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "perViewPositionAllComponents"
  vkPerViewPositionAllComponents :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX <$> peek (ptr `plusPtr` 0)
                                                                     <*> peek (ptr `plusPtr` 8)
                                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX))
                *> poke (ptr `plusPtr` 16) (vkPerViewPositionAllComponents (poked :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX))
