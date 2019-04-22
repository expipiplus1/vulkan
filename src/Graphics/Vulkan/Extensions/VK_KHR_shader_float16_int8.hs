{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_float16_int8
  ( withCStructPhysicalDeviceFloat16Int8FeaturesKHR
  , fromCStructPhysicalDeviceFloat16Int8FeaturesKHR
  , PhysicalDeviceFloat16Int8FeaturesKHR(..)
  , pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
  , pattern KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
  ) where

import Data.String
  ( IsString
  )
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
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float16_int8
  ( VkPhysicalDeviceFloat16Int8FeaturesKHR(..)
  , pattern VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
  , pattern VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
  )



-- | VkPhysicalDeviceFloat16Int8FeaturesKHR - Structure describing features
-- supported by VK_KHR_shader_float16_int8
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceFloat16Int8FeaturesKHR = PhysicalDeviceFloat16Int8FeaturesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceFloat16Int8FeaturesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceFloat16Int8FeaturesKHR" "shaderFloat16"
  shaderFloat16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFloat16Int8FeaturesKHR" "shaderInt8"
  shaderInt8 :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceFloat16Int8FeaturesKHR' and
-- marshal a 'PhysicalDeviceFloat16Int8FeaturesKHR' into it. The 'VkPhysicalDeviceFloat16Int8FeaturesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceFloat16Int8FeaturesKHR :: PhysicalDeviceFloat16Int8FeaturesKHR -> (VkPhysicalDeviceFloat16Int8FeaturesKHR -> IO a) -> IO a
withCStructPhysicalDeviceFloat16Int8FeaturesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceFloat16Int8FeaturesKHR)) (\pPNext -> cont (VkPhysicalDeviceFloat16Int8FeaturesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR pPNext (boolToBool32 (shaderFloat16 (marshalled :: PhysicalDeviceFloat16Int8FeaturesKHR))) (boolToBool32 (shaderInt8 (marshalled :: PhysicalDeviceFloat16Int8FeaturesKHR)))))

-- | A function to read a 'VkPhysicalDeviceFloat16Int8FeaturesKHR' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceFloat16Int8FeaturesKHR'.
fromCStructPhysicalDeviceFloat16Int8FeaturesKHR :: VkPhysicalDeviceFloat16Int8FeaturesKHR -> IO PhysicalDeviceFloat16Int8FeaturesKHR
fromCStructPhysicalDeviceFloat16Int8FeaturesKHR c = PhysicalDeviceFloat16Int8FeaturesKHR <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceFloat16Int8FeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkShaderFloat16 (c :: VkPhysicalDeviceFloat16Int8FeaturesKHR)))
                                                                                         <*> pure (bool32ToBool (vkShaderInt8 (c :: VkPhysicalDeviceFloat16Int8FeaturesKHR)))

instance Zero PhysicalDeviceFloat16Int8FeaturesKHR where
  zero = PhysicalDeviceFloat16Int8FeaturesKHR Nothing
                                              False
                                              False


-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME"
pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME = VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION"
pattern KHR_SHADER_FLOAT16_INT8_SPEC_VERSION :: Integral a => a
pattern KHR_SHADER_FLOAT16_INT8_SPEC_VERSION = VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
