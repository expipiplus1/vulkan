{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes
  ( VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  , pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
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
import Graphics.Vulkan.C.Core10.Pass
  ( VkSubpassDescriptionFlagBits(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX"
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

instance Zero VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  zero = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
                                                                 zero
                                                                 zero

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME"
pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = "VK_NVX_multiview_per_view_attributes"

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION"
pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION :: Integral a => a
pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX = VkStructureType 1000097000

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX"
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX :: VkSubpassDescriptionFlagBits
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX = VkSubpassDescriptionFlagBits 0x00000001

-- No documentation found for Nested "VkSubpassDescriptionFlagBits" "VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX"
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX :: VkSubpassDescriptionFlagBits
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX = VkSubpassDescriptionFlagBits 0x00000002
