{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes
  ( withCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  , fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  , PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  , pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  , pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  , pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
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
import Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes
  ( VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME
  , pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX
  , pattern SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX
  )



-- | VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX - Structure
-- describing multiview limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes.VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes.VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX" "perViewPositionAllComponents"
  perViewPositionAllComponents :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' and
-- marshal a 'PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' into it. The 'VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX -> (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX -> IO a) -> IO a
withCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)) (\pPNext -> cont (VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX pPNext (boolToBool32 (perViewPositionAllComponents (marshalled :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)))))

-- | A function to read a 'VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX'.
fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX -> IO PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX c = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX <$> -- Univalued Member elided
                                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)))
                                                                                                                           <*> pure (bool32ToBool (vkPerViewPositionAllComponents (c :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)))

instance Zero PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  zero = PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX Nothing
                                                               False


-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME = VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION"
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION :: Integral a => a
pattern NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION = VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION
