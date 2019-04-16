{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode
  ( VkImageViewASTCDecodeModeEXT(..)
  , VkPhysicalDeviceASTCDecodeFeaturesEXT(..)
  , pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME
  , pattern VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
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
  , VkFormat(..)
  , VkStructureType(..)
  )


-- No documentation found for TopLevel "VkImageViewASTCDecodeModeEXT"
data VkImageViewASTCDecodeModeEXT = VkImageViewASTCDecodeModeEXT
  { -- No documentation found for Nested "VkImageViewASTCDecodeModeEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageViewASTCDecodeModeEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageViewASTCDecodeModeEXT" "decodeMode"
  vkDecodeMode :: VkFormat
  }
  deriving (Eq, Show)

instance Storable VkImageViewASTCDecodeModeEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageViewASTCDecodeModeEXT <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageViewASTCDecodeModeEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageViewASTCDecodeModeEXT))
                *> poke (ptr `plusPtr` 16) (vkDecodeMode (poked :: VkImageViewASTCDecodeModeEXT))
-- No documentation found for TopLevel "VkPhysicalDeviceASTCDecodeFeaturesEXT"
data VkPhysicalDeviceASTCDecodeFeaturesEXT = VkPhysicalDeviceASTCDecodeFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceASTCDecodeFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceASTCDecodeFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceASTCDecodeFeaturesEXT" "decodeModeSharedExponent"
  vkDecodeModeSharedExponent :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceASTCDecodeFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceASTCDecodeFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceASTCDecodeFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceASTCDecodeFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkDecodeModeSharedExponent (poked :: VkPhysicalDeviceASTCDecodeFeaturesEXT))
-- No documentation found for TopLevel "VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME"
pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME = "VK_EXT_astc_decode_mode"
-- No documentation found for TopLevel "VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION"
pattern VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT"
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT = VkStructureType 1000067000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT = VkStructureType 1000067001
