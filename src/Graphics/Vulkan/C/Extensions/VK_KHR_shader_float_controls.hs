{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( VkPhysicalDeviceFloatControlsPropertiesKHR(..)
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME
  , pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceFloatControlsPropertiesKHR"
data VkPhysicalDeviceFloatControlsPropertiesKHR = VkPhysicalDeviceFloatControlsPropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "separateDenormSettings"
  vkSeparateDenormSettings :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "separateRoundingModeSettings"
  vkSeparateRoundingModeSettings :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat16"
  vkShaderSignedZeroInfNanPreserveFloat16 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat32"
  vkShaderSignedZeroInfNanPreserveFloat32 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderSignedZeroInfNanPreserveFloat64"
  vkShaderSignedZeroInfNanPreserveFloat64 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat16"
  vkShaderDenormPreserveFloat16 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat32"
  vkShaderDenormPreserveFloat32 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormPreserveFloat64"
  vkShaderDenormPreserveFloat64 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat16"
  vkShaderDenormFlushToZeroFloat16 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat32"
  vkShaderDenormFlushToZeroFloat32 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderDenormFlushToZeroFloat64"
  vkShaderDenormFlushToZeroFloat64 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat16"
  vkShaderRoundingModeRTEFloat16 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat32"
  vkShaderRoundingModeRTEFloat32 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTEFloat64"
  vkShaderRoundingModeRTEFloat64 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat16"
  vkShaderRoundingModeRTZFloat16 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat32"
  vkShaderRoundingModeRTZFloat32 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloatControlsPropertiesKHR" "shaderRoundingModeRTZFloat64"
  vkShaderRoundingModeRTZFloat64 :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFloatControlsPropertiesKHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFloatControlsPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
                                                        <*> peek (ptr `plusPtr` 24)
                                                        <*> peek (ptr `plusPtr` 28)
                                                        <*> peek (ptr `plusPtr` 32)
                                                        <*> peek (ptr `plusPtr` 36)
                                                        <*> peek (ptr `plusPtr` 40)
                                                        <*> peek (ptr `plusPtr` 44)
                                                        <*> peek (ptr `plusPtr` 48)
                                                        <*> peek (ptr `plusPtr` 52)
                                                        <*> peek (ptr `plusPtr` 56)
                                                        <*> peek (ptr `plusPtr` 60)
                                                        <*> peek (ptr `plusPtr` 64)
                                                        <*> peek (ptr `plusPtr` 68)
                                                        <*> peek (ptr `plusPtr` 72)
                                                        <*> peek (ptr `plusPtr` 76)
                                                        <*> peek (ptr `plusPtr` 80)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkSeparateDenormSettings (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 20) (vkSeparateRoundingModeSettings (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkShaderSignedZeroInfNanPreserveFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 28) (vkShaderSignedZeroInfNanPreserveFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 32) (vkShaderSignedZeroInfNanPreserveFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 36) (vkShaderDenormPreserveFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 40) (vkShaderDenormPreserveFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 44) (vkShaderDenormPreserveFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 48) (vkShaderDenormFlushToZeroFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 52) (vkShaderDenormFlushToZeroFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 56) (vkShaderDenormFlushToZeroFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 60) (vkShaderRoundingModeRTEFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 64) (vkShaderRoundingModeRTEFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 68) (vkShaderRoundingModeRTEFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 72) (vkShaderRoundingModeRTZFloat16 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 76) (vkShaderRoundingModeRTZFloat32 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
                *> poke (ptr `plusPtr` 80) (vkShaderRoundingModeRTZFloat64 (poked :: VkPhysicalDeviceFloatControlsPropertiesKHR))
-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME"
pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SHADER_FLOAT_CONTROLS_EXTENSION_NAME = "VK_KHR_shader_float_controls"
-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION"
pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SHADER_FLOAT_CONTROLS_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR = VkStructureType 1000197000
