{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes
  ( withCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  , fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  , PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  , pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
  ) where

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
import Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes
  ( VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes
  ( pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
  , pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
  )


-- No documentation found for TopLevel "PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX"
data PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "perViewPositionAllComponents"
  vkPerViewPositionAllComponents :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX -> (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX -> IO a) -> IO a
withCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)) (\pPNext -> cont (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX pPNext (boolToBool32 (vkPerViewPositionAllComponents (from :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)))))
fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX -> IO PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX c = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX <$> -- Univalued Member elided
                                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)))
                                                                                                                           <*> pure (bool32ToBool (vkPerViewPositionAllComponents (c :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)))
instance Zero PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  zero = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX Nothing
                                                               False
