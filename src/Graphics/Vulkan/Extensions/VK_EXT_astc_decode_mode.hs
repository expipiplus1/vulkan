{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode
  ( withCStructImageViewASTCDecodeModeEXT
  , fromCStructImageViewASTCDecodeModeEXT
  , ImageViewASTCDecodeModeEXT(..)
  , withCStructPhysicalDeviceASTCDecodeFeaturesEXT
  , fromCStructPhysicalDeviceASTCDecodeFeaturesEXT
  , PhysicalDeviceASTCDecodeFeaturesEXT(..)
  , pattern VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION
  , pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode
  ( VkImageViewASTCDecodeModeEXT(..)
  , VkPhysicalDeviceASTCDecodeFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode
  ( pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME
  , pattern VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION
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
-- Unresolved directive in VkImageViewASTCDecodeModeEXT.txt -
-- include::{generated}\/validity\/structs\/VkImageViewASTCDecodeModeEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data ImageViewASTCDecodeModeEXT = ImageViewASTCDecodeModeEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ImageViewASTCDecodeModeEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewASTCDecodeModeEXT" "decodeMode"
  decodeMode :: Format
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageViewASTCDecodeModeEXT' and
-- marshal a 'ImageViewASTCDecodeModeEXT' into it. The 'VkImageViewASTCDecodeModeEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageViewASTCDecodeModeEXT :: ImageViewASTCDecodeModeEXT -> (VkImageViewASTCDecodeModeEXT -> IO a) -> IO a
withCStructImageViewASTCDecodeModeEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImageViewASTCDecodeModeEXT)) (\pPNext -> cont (VkImageViewASTCDecodeModeEXT VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT pPNext (decodeMode (marshalled :: ImageViewASTCDecodeModeEXT))))

-- | A function to read a 'VkImageViewASTCDecodeModeEXT' and all additional
-- structures in the pointer chain into a 'ImageViewASTCDecodeModeEXT'.
fromCStructImageViewASTCDecodeModeEXT :: VkImageViewASTCDecodeModeEXT -> IO ImageViewASTCDecodeModeEXT
fromCStructImageViewASTCDecodeModeEXT c = ImageViewASTCDecodeModeEXT <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageViewASTCDecodeModeEXT)))
                                                                     <*> pure (vkDecodeMode (c :: VkImageViewASTCDecodeModeEXT))

instance Zero ImageViewASTCDecodeModeEXT where
  zero = ImageViewASTCDecodeModeEXT Nothing
                                    zero



-- | VkPhysicalDeviceASTCDecodeFeaturesEXT - Structure describing ASTC decode
-- mode features
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode.VkPhysicalDeviceASTCDecodeFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode.VkPhysicalDeviceASTCDecodeFeaturesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode.VkPhysicalDeviceASTCDecodeFeaturesEXT'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice' to enable features.
--
-- Unresolved directive in VkPhysicalDeviceASTCDecodeFeaturesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceASTCDecodeFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceASTCDecodeFeaturesEXT = PhysicalDeviceASTCDecodeFeaturesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceASTCDecodeFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceASTCDecodeFeaturesEXT" "decodeModeSharedExponent"
  decodeModeSharedExponent :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceASTCDecodeFeaturesEXT' and
-- marshal a 'PhysicalDeviceASTCDecodeFeaturesEXT' into it. The 'VkPhysicalDeviceASTCDecodeFeaturesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceASTCDecodeFeaturesEXT :: PhysicalDeviceASTCDecodeFeaturesEXT -> (VkPhysicalDeviceASTCDecodeFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceASTCDecodeFeaturesEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceASTCDecodeFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceASTCDecodeFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT pPNext (boolToBool32 (decodeModeSharedExponent (marshalled :: PhysicalDeviceASTCDecodeFeaturesEXT)))))

-- | A function to read a 'VkPhysicalDeviceASTCDecodeFeaturesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceASTCDecodeFeaturesEXT'.
fromCStructPhysicalDeviceASTCDecodeFeaturesEXT :: VkPhysicalDeviceASTCDecodeFeaturesEXT -> IO PhysicalDeviceASTCDecodeFeaturesEXT
fromCStructPhysicalDeviceASTCDecodeFeaturesEXT c = PhysicalDeviceASTCDecodeFeaturesEXT <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceASTCDecodeFeaturesEXT)))
                                                                                       <*> pure (bool32ToBool (vkDecodeModeSharedExponent (c :: VkPhysicalDeviceASTCDecodeFeaturesEXT)))

instance Zero PhysicalDeviceASTCDecodeFeaturesEXT where
  zero = PhysicalDeviceASTCDecodeFeaturesEXT Nothing
                                             False

