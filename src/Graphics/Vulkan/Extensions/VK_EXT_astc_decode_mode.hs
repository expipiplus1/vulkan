{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ImageViewASTCDecodeModeEXT(..)
  , 
  PhysicalDeviceASTCDecodeFeaturesEXT(..)
#endif
  , pattern EXT_ASTC_DECODE_MODE_EXTENSION_NAME
  , pattern EXT_ASTC_DECODE_MODE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode
  ( pattern VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME
  , pattern VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageViewASTCDecodeModeEXT"
data ImageViewASTCDecodeModeEXT = ImageViewASTCDecodeModeEXT
  { -- No documentation found for Nested "ImageViewASTCDecodeModeEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewASTCDecodeModeEXT" "decodeMode"
  decodeMode :: Format
  }
  deriving (Show, Eq)

instance Zero ImageViewASTCDecodeModeEXT where
  zero = ImageViewASTCDecodeModeEXT Nothing
                                    zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceASTCDecodeFeaturesEXT"
data PhysicalDeviceASTCDecodeFeaturesEXT = PhysicalDeviceASTCDecodeFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceASTCDecodeFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceASTCDecodeFeaturesEXT" "decodeModeSharedExponent"
  decodeModeSharedExponent :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceASTCDecodeFeaturesEXT where
  zero = PhysicalDeviceASTCDecodeFeaturesEXT Nothing
                                             False

#endif

-- No documentation found for TopLevel "VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME"
pattern EXT_ASTC_DECODE_MODE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_ASTC_DECODE_MODE_EXTENSION_NAME = VK_EXT_ASTC_DECODE_MODE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION"
pattern EXT_ASTC_DECODE_MODE_SPEC_VERSION :: Integral a => a
pattern EXT_ASTC_DECODE_MODE_SPEC_VERSION = VK_EXT_ASTC_DECODE_MODE_SPEC_VERSION
