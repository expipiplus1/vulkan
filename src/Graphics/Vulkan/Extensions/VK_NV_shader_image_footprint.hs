{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_shader_image_footprint
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceShaderImageFootprintFeaturesNV(..)
  , 
#endif
  pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
  , pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_shader_image_footprint
  ( pattern VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
  , pattern VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceShaderImageFootprintFeaturesNV"
data PhysicalDeviceShaderImageFootprintFeaturesNV = PhysicalDeviceShaderImageFootprintFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceShaderImageFootprintFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceShaderImageFootprintFeaturesNV" "imageFootprint"
  imageFootprint :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceShaderImageFootprintFeaturesNV where
  zero = PhysicalDeviceShaderImageFootprintFeaturesNV Nothing
                                                      False

#endif

-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME"
pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME = VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION"
pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION :: Integral a => a
pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION = VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
