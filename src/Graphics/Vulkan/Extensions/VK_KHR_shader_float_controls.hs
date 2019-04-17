{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_float_controls
  ( withCStructPhysicalDeviceFloatControlsPropertiesKHR
  , fromCStructPhysicalDeviceFloatControlsPropertiesKHR
  , PhysicalDeviceFloatControlsPropertiesKHR(..)
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( VkPhysicalDeviceFloatControlsPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceFloatControlsPropertiesKHR"
data PhysicalDeviceFloatControlsPropertiesKHR = PhysicalDeviceFloatControlsPropertiesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "separateDenormSettings"
  vkSeparateDenormSettings :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "separateRoundingModeSettings"
  vkSeparateRoundingModeSettings :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat16"
  vkShaderSignedZeroInfNanPreserveFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat32"
  vkShaderSignedZeroInfNanPreserveFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat64"
  vkShaderSignedZeroInfNanPreserveFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat16"
  vkShaderDenormPreserveFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat32"
  vkShaderDenormPreserveFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat64"
  vkShaderDenormPreserveFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat16"
  vkShaderDenormFlushToZeroFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat32"
  vkShaderDenormFlushToZeroFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat64"
  vkShaderDenormFlushToZeroFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat16"
  vkShaderRoundingModeRTEFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat32"
  vkShaderRoundingModeRTEFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat64"
  vkShaderRoundingModeRTEFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat16"
  vkShaderRoundingModeRTZFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat32"
  vkShaderRoundingModeRTZFloat32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat64"
  vkShaderRoundingModeRTZFloat64 :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceFloatControlsPropertiesKHR :: PhysicalDeviceFloatControlsPropertiesKHR -> (VkPhysicalDeviceFloatControlsPropertiesKHR -> IO a) -> IO a
withCStructPhysicalDeviceFloatControlsPropertiesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceFloatControlsPropertiesKHR)) (\pPNext -> cont (VkPhysicalDeviceFloatControlsPropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR pPNext (boolToBool32 (vkSeparateDenormSettings (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkSeparateRoundingModeSettings (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderSignedZeroInfNanPreserveFloat16 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderSignedZeroInfNanPreserveFloat32 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderSignedZeroInfNanPreserveFloat64 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderDenormPreserveFloat16 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderDenormPreserveFloat32 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderDenormPreserveFloat64 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderDenormFlushToZeroFloat16 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderDenormFlushToZeroFloat32 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderDenormFlushToZeroFloat64 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderRoundingModeRTEFloat16 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderRoundingModeRTEFloat32 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderRoundingModeRTEFloat64 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderRoundingModeRTZFloat16 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderRoundingModeRTZFloat32 (from :: PhysicalDeviceFloatControlsPropertiesKHR))) (boolToBool32 (vkShaderRoundingModeRTZFloat64 (from :: PhysicalDeviceFloatControlsPropertiesKHR)))))
fromCStructPhysicalDeviceFloatControlsPropertiesKHR :: VkPhysicalDeviceFloatControlsPropertiesKHR -> IO PhysicalDeviceFloatControlsPropertiesKHR
fromCStructPhysicalDeviceFloatControlsPropertiesKHR c = PhysicalDeviceFloatControlsPropertiesKHR <$> -- Univalued Member elided
                                                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkSeparateDenormSettings (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkSeparateRoundingModeSettings (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderSignedZeroInfNanPreserveFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderSignedZeroInfNanPreserveFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderSignedZeroInfNanPreserveFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormPreserveFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormPreserveFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormPreserveFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormFlushToZeroFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormFlushToZeroFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderDenormFlushToZeroFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTEFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTEFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTEFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTZFloat16 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTZFloat32 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
                                                                                                 <*> pure (bool32ToBool (vkShaderRoundingModeRTZFloat64 (c :: VkPhysicalDeviceFloatControlsPropertiesKHR)))
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
