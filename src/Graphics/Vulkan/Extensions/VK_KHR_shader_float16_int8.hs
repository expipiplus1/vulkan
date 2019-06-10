{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_float16_int8
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceFloat16Int8FeaturesKHR(..)
  , 
#endif
  pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
  , pattern KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float16_int8
  ( pattern VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
  , pattern VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceFloat16Int8FeaturesKHR"
data PhysicalDeviceFloat16Int8FeaturesKHR = PhysicalDeviceFloat16Int8FeaturesKHR
  { -- No documentation found for Nested "PhysicalDeviceFloat16Int8FeaturesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFloat16Int8FeaturesKHR" "shaderFloat16"
  shaderFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloat16Int8FeaturesKHR" "shaderInt8"
  shaderInt8 :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceFloat16Int8FeaturesKHR where
  zero = PhysicalDeviceFloat16Int8FeaturesKHR Nothing
                                              False
                                              False

#endif

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME"
pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME = VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION"
pattern KHR_SHADER_FLOAT16_INT8_SPEC_VERSION :: Integral a => a
pattern KHR_SHADER_FLOAT16_INT8_SPEC_VERSION = VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
