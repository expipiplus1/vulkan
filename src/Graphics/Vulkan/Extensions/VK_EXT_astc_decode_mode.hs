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


-- No documentation found for TopLevel "ImageViewASTCDecodeModeEXT"
data ImageViewASTCDecodeModeEXT = ImageViewASTCDecodeModeEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageViewASTCDecodeModeEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewASTCDecodeModeEXT" "decodeMode"
  vkDecodeMode :: Format
  }
  deriving (Show, Eq)
withCStructImageViewASTCDecodeModeEXT :: ImageViewASTCDecodeModeEXT -> (VkImageViewASTCDecodeModeEXT -> IO a) -> IO a
withCStructImageViewASTCDecodeModeEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImageViewASTCDecodeModeEXT)) (\pPNext -> cont (VkImageViewASTCDecodeModeEXT VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT pPNext (vkDecodeMode (from :: ImageViewASTCDecodeModeEXT))))
fromCStructImageViewASTCDecodeModeEXT :: VkImageViewASTCDecodeModeEXT -> IO ImageViewASTCDecodeModeEXT
fromCStructImageViewASTCDecodeModeEXT c = ImageViewASTCDecodeModeEXT <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageViewASTCDecodeModeEXT)))
                                                                     <*> pure (vkDecodeMode (c :: VkImageViewASTCDecodeModeEXT))
-- No documentation found for TopLevel "PhysicalDeviceASTCDecodeFeaturesEXT"
data PhysicalDeviceASTCDecodeFeaturesEXT = PhysicalDeviceASTCDecodeFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceASTCDecodeFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceASTCDecodeFeaturesEXT" "decodeModeSharedExponent"
  vkDecodeModeSharedExponent :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceASTCDecodeFeaturesEXT :: PhysicalDeviceASTCDecodeFeaturesEXT -> (VkPhysicalDeviceASTCDecodeFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceASTCDecodeFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceASTCDecodeFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceASTCDecodeFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT pPNext (boolToBool32 (vkDecodeModeSharedExponent (from :: PhysicalDeviceASTCDecodeFeaturesEXT)))))
fromCStructPhysicalDeviceASTCDecodeFeaturesEXT :: VkPhysicalDeviceASTCDecodeFeaturesEXT -> IO PhysicalDeviceASTCDecodeFeaturesEXT
fromCStructPhysicalDeviceASTCDecodeFeaturesEXT c = PhysicalDeviceASTCDecodeFeaturesEXT <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceASTCDecodeFeaturesEXT)))
                                                                                       <*> pure (bool32ToBool (vkDecodeModeSharedExponent (c :: VkPhysicalDeviceASTCDecodeFeaturesEXT)))
