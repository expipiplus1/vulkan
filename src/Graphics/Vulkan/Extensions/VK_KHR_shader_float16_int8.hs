{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_float16_int8
  ( withCStructPhysicalDeviceFloat16Int8FeaturesKHR
  , fromCStructPhysicalDeviceFloat16Int8FeaturesKHR
  , PhysicalDeviceFloat16Int8FeaturesKHR(..)
  , pattern VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
  , pattern VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float16_int8
  ( VkPhysicalDeviceFloat16Int8FeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float16_int8
  ( pattern VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
  , pattern VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceFloat16Int8FeaturesKHR"
data PhysicalDeviceFloat16Int8FeaturesKHR = PhysicalDeviceFloat16Int8FeaturesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceFloat16Int8FeaturesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFloat16Int8FeaturesKHR" "shaderFloat16"
  vkShaderFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloat16Int8FeaturesKHR" "shaderInt8"
  vkShaderInt8 :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceFloat16Int8FeaturesKHR :: PhysicalDeviceFloat16Int8FeaturesKHR -> (VkPhysicalDeviceFloat16Int8FeaturesKHR -> IO a) -> IO a
withCStructPhysicalDeviceFloat16Int8FeaturesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceFloat16Int8FeaturesKHR)) (\pPNext -> cont (VkPhysicalDeviceFloat16Int8FeaturesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR pPNext (boolToBool32 (vkShaderFloat16 (from :: PhysicalDeviceFloat16Int8FeaturesKHR))) (boolToBool32 (vkShaderInt8 (from :: PhysicalDeviceFloat16Int8FeaturesKHR)))))
fromCStructPhysicalDeviceFloat16Int8FeaturesKHR :: VkPhysicalDeviceFloat16Int8FeaturesKHR -> IO PhysicalDeviceFloat16Int8FeaturesKHR
fromCStructPhysicalDeviceFloat16Int8FeaturesKHR c = PhysicalDeviceFloat16Int8FeaturesKHR <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFloat16Int8FeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkShaderFloat16 (c :: VkPhysicalDeviceFloat16Int8FeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkShaderInt8 (c :: VkPhysicalDeviceFloat16Int8FeaturesKHR)))
