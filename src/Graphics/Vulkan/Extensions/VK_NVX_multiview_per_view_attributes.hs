{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  , 
#endif
  pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  , pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  , pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes
  ( pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX"
data PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  { -- No documentation found for Nested "PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "perViewPositionAllComponents"
  perViewPositionAllComponents :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  zero = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX Nothing
                                                               False

#endif

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION :: Integral a => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
