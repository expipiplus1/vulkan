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


-- No documentation found for TopLevel "PhysicalDeviceShaderImageFootprintFeaturesNV"
data PhysicalDeviceShaderImageFootprintFeaturesNV = PhysicalDeviceShaderImageFootprintFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceShaderImageFootprintFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderImageFootprintFeaturesNV" "imageFootprint"
  vkImageFootprint :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceShaderImageFootprintFeaturesNV :: PhysicalDeviceShaderImageFootprintFeaturesNV -> (VkPhysicalDeviceShaderImageFootprintFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceShaderImageFootprintFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceShaderImageFootprintFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceShaderImageFootprintFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV pPNext (boolToBool32 (vkImageFootprint (from :: PhysicalDeviceShaderImageFootprintFeaturesNV)))))
fromCStructPhysicalDeviceShaderImageFootprintFeaturesNV :: VkPhysicalDeviceShaderImageFootprintFeaturesNV -> IO PhysicalDeviceShaderImageFootprintFeaturesNV
fromCStructPhysicalDeviceShaderImageFootprintFeaturesNV c = PhysicalDeviceShaderImageFootprintFeaturesNV <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceShaderImageFootprintFeaturesNV)))
                                                                                                         <*> pure (bool32ToBool (vkImageFootprint (c :: VkPhysicalDeviceShaderImageFootprintFeaturesNV)))
instance Zero PhysicalDeviceShaderImageFootprintFeaturesNV where
  zero = PhysicalDeviceShaderImageFootprintFeaturesNV Nothing
                                                      False
