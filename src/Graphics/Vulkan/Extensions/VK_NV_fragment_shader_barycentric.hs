{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_fragment_shader_barycentric
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  , 
#endif
  pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
  , pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_shader_barycentric
  ( pattern VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
  , pattern VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV"
data PhysicalDeviceFragmentShaderBarycentricFeaturesNV = PhysicalDeviceFragmentShaderBarycentricFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceFragmentShaderBarycentricFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFragmentShaderBarycentricFeaturesNV" "fragmentShaderBarycentric"
  fragmentShaderBarycentric :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  zero = PhysicalDeviceFragmentShaderBarycentricFeaturesNV Nothing
                                                           False

#endif

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME"
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME = VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION"
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION :: Integral a => a
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION = VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
