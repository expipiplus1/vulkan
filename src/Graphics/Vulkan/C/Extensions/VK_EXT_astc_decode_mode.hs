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
  , Zero(..)
  )


-- | VkImageViewASTCDecodeModeEXT - Structure describing the ASTC decode mode
-- for an image view
--
-- == Valid Usage
--
-- -   @decodeMode@ /must/ be one of
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16A16_SFLOAT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_UNORM', or
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_E5B9G9R9_UFLOAT_PACK32'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-astc-decodeModeSharedExponent decodeModeSharedExponent>
--     feature is not enabled, @decodeMode@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_E5B9G9R9_UFLOAT_PACK32'
--
-- -   If @decodeMode@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_UNORM' the image
--     view /must/ not include blocks using any of the ASTC HDR modes
--
-- -   @format@ of the image view /must/ be one of
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_4x4_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_4x4_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x4_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x4_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x5_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x5_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x5_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x5_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x6_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x6_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x5_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x5_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x6_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x6_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x8_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x8_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x5_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x5_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x6_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x6_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x8_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x8_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x10_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x10_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x10_UNORM_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x10_SRGB_BLOCK',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x12_UNORM_BLOCK', or
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x12_SRGB_BLOCK'
--
-- If @format@ uses sRGB encoding then the @decodeMode@ has no effect.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT'
--
-- -   @decodeMode@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkImageViewASTCDecodeModeEXT = VkImageViewASTCDecodeModeEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @decodeMode@ is the intermediate format used to decode ASTC compressed
  -- formats.
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

instance Zero VkImageViewASTCDecodeModeEXT where
  zero = VkImageViewASTCDecodeModeEXT VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
                                      zero
                                      zero

-- | VkPhysicalDeviceASTCDecodeFeaturesEXT - Structure describing ASTC decode
-- mode features
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceASTCDecodeFeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- If the 'VkPhysicalDeviceASTCDecodeFeaturesEXT' structure is included in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'VkPhysicalDeviceASTCDecodeFeaturesEXT' /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDeviceASTCDecodeFeaturesEXT = VkPhysicalDeviceASTCDecodeFeaturesEXT
  { -- | @sType@ /must/ be
  -- 'VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT'
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceASTCDecodeFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @decodeModeSharedExponent@ indicates whether the implementation supports
  -- decoding ASTC compressed formats to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_E5B9G9R9_UFLOAT_PACK32'
  -- internal precision.
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

instance Zero VkPhysicalDeviceASTCDecodeFeaturesEXT where
  zero = VkPhysicalDeviceASTCDecodeFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
                                               zero
                                               zero

-- No documentation found for TopLevel "VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME"
pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME :: (Eq a, IsString a) => a
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
