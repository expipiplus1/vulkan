{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_float_controls
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceFloatControlsPropertiesKHR(..)
  , 
#endif
  pattern KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  , pattern KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceFloatControlsPropertiesKHR"
data PhysicalDeviceFloatControlsPropertiesKHR = PhysicalDeviceFloatControlsPropertiesKHR
  { -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "separateDenormSettings"
  separateDenormSettings :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "separateRoundingModeSettings"
  separateRoundingModeSettings :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat16"
  shaderSignedZeroInfNanPreserveFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat32"
  shaderSignedZeroInfNanPreserveFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat64"
  shaderSignedZeroInfNanPreserveFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat16"
  shaderDenormPreserveFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat32"
  shaderDenormPreserveFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat64"
  shaderDenormPreserveFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat16"
  shaderDenormFlushToZeroFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat32"
  shaderDenormFlushToZeroFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat64"
  shaderDenormFlushToZeroFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat16"
  shaderRoundingModeRTEFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat32"
  shaderRoundingModeRTEFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat64"
  shaderRoundingModeRTEFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat16"
  shaderRoundingModeRTZFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat32"
  shaderRoundingModeRTZFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat64"
  shaderRoundingModeRTZFloat64 :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceFloatControlsPropertiesKHR where
  zero = PhysicalDeviceFloatControlsPropertiesKHR Nothing
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False
                                                  False

#endif

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME"
pattern KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME = VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION"
pattern KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION :: Integral a => a
pattern KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION = VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
