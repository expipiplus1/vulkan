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


-- | VkPhysicalDeviceFloat16Int8FeaturesKHR - Structure describing features
-- supported by VK_KHR_shader_float16_int8
--
-- = Description
--
-- Unresolved directive in VkPhysicalDeviceFloat16Int8FeaturesKHR.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceFloat16Int8FeaturesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceFloat16Int8FeaturesKHR = VkPhysicalDeviceFloat16Int8FeaturesKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @shaderFloat16@ indicates whether 16-bit floats (halfs) are supported in
  -- shader code. This also indicates whether shader modules /can/ declare
  -- the @Float16@ capability.
  vkShaderFloat16 :: VkBool32
  , -- | @shaderInt8@ indicates whether 8-bit integers (signed and unsigned) are
  -- supported in shader code. This also indicates whether shader modules
  -- /can/ declare the @Int8@ capability.
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
  zero = VkPhysicalDeviceFloat16Int8FeaturesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
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
