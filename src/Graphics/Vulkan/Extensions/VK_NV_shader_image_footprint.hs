{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_shader_image_footprint
  ( withCStructPhysicalDeviceShaderImageFootprintFeaturesNV
  , fromCStructPhysicalDeviceShaderImageFootprintFeaturesNV
  , PhysicalDeviceShaderImageFootprintFeaturesNV(..)
  , pattern VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
  , pattern VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
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
import Graphics.Vulkan.C.Extensions.VK_NV_shader_image_footprint
  ( VkPhysicalDeviceShaderImageFootprintFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
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
import Graphics.Vulkan.C.Extensions.VK_NV_shader_image_footprint
  ( pattern VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
  , pattern VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
  )



-- | VkPhysicalDeviceShaderImageFootprintFeaturesNV - Structure describing
-- shader image footprint features that can be supported by an
-- implementation
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-footprint Texel Footprint Evaluation>
-- for more information.
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shader_image_footprint.VkPhysicalDeviceShaderImageFootprintFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shader_image_footprint.VkPhysicalDeviceShaderImageFootprintFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in
-- VkPhysicalDeviceShaderImageFootprintFeaturesNV.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceShaderImageFootprintFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceShaderImageFootprintFeaturesNV = PhysicalDeviceShaderImageFootprintFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceShaderImageFootprintFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderImageFootprintFeaturesNV" "imageFootprint"
  imageFootprint :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceShaderImageFootprintFeaturesNV' and
-- marshal a 'PhysicalDeviceShaderImageFootprintFeaturesNV' into it. The 'VkPhysicalDeviceShaderImageFootprintFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceShaderImageFootprintFeaturesNV :: PhysicalDeviceShaderImageFootprintFeaturesNV -> (VkPhysicalDeviceShaderImageFootprintFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceShaderImageFootprintFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceShaderImageFootprintFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceShaderImageFootprintFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV pPNext (boolToBool32 (imageFootprint (marshalled :: PhysicalDeviceShaderImageFootprintFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceShaderImageFootprintFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceShaderImageFootprintFeaturesNV'.
fromCStructPhysicalDeviceShaderImageFootprintFeaturesNV :: VkPhysicalDeviceShaderImageFootprintFeaturesNV -> IO PhysicalDeviceShaderImageFootprintFeaturesNV
fromCStructPhysicalDeviceShaderImageFootprintFeaturesNV c = PhysicalDeviceShaderImageFootprintFeaturesNV <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShaderImageFootprintFeaturesNV)))
                                                                                                         <*> pure (bool32ToBool (vkImageFootprint (c :: VkPhysicalDeviceShaderImageFootprintFeaturesNV)))

instance Zero PhysicalDeviceShaderImageFootprintFeaturesNV where
  zero = PhysicalDeviceShaderImageFootprintFeaturesNV Nothing
                                                      False

