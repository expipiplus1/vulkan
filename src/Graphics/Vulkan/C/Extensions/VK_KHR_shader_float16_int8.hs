{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_shader_float16_int8
  ( VkPhysicalDeviceFloat16Int8FeaturesKHR(..)
  , pattern VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
  , pattern VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
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
  , Zero(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceFloat16Int8FeaturesKHR"
data VkPhysicalDeviceFloat16Int8FeaturesKHR = VkPhysicalDeviceFloat16Int8FeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceFloat16Int8FeaturesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceFloat16Int8FeaturesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceFloat16Int8FeaturesKHR" "shaderFloat16"
  vkShaderFloat16 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceFloat16Int8FeaturesKHR" "shaderInt8"
  vkShaderInt8 :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFloat16Int8FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFloat16Int8FeaturesKHR <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFloat16Int8FeaturesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFloat16Int8FeaturesKHR))
                *> poke (ptr `plusPtr` 16) (vkShaderFloat16 (poked :: VkPhysicalDeviceFloat16Int8FeaturesKHR))
                *> poke (ptr `plusPtr` 20) (vkShaderInt8 (poked :: VkPhysicalDeviceFloat16Int8FeaturesKHR))

instance Zero VkPhysicalDeviceFloat16Int8FeaturesKHR where
  zero = VkPhysicalDeviceFloat16Int8FeaturesKHR zero
                                                zero
                                                zero
                                                zero
-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME"
pattern VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME = "VK_KHR_shader_float16_int8"
-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION"
pattern VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR = VkStructureType 1000082000
