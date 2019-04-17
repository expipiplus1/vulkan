{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

module Graphics.Vulkan.C.Core10.Version
  ( pattern VK_MAKE_VERSION
  , _VK_VERSION_MAJOR
  , _VK_VERSION_MINOR
  , _VK_VERSION_PATCH
  , pattern VK_API_VERSION_1_0
  , pattern VK_HEADER_VERSION
  ) where

import Data.Bits
  ( (.&.)
  , (.|.)
  , shiftL
  , shiftR
  )
import Data.Word
  ( Word32
  )





-- | VK_MAKE_VERSION - Construct an API version number
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkApplicationInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance'
pattern VK_MAKE_VERSION :: Word32 -> Word32 -> Word32 -> Word32
pattern VK_MAKE_VERSION major minor patch <-
  (\v -> (_VK_VERSION_MAJOR v, _VK_VERSION_MINOR v, _VK_VERSION_PATCH v) -> (major, minor, patch))
  where VK_MAKE_VERSION major minor patch = major `shiftL` 22 .|. minor `shiftL` 12 .|. patch

-- | VK_VERSION_MAJOR - Extract API major version number
--
-- = See Also
--
-- No cross-references are available
_VK_VERSION_MAJOR :: Word32 -> Word32
_VK_VERSION_MAJOR v = v `shiftR` 22

-- | VK_VERSION_MINOR - Extract API minor version number
--
-- = See Also
--
-- No cross-references are available
_VK_VERSION_MINOR :: Word32 -> Word32
_VK_VERSION_MINOR v = v `shiftR` 12 .&. 0x3ff

-- | VK_VERSION_PATCH - Extract API patch version number
--
-- = See Also
--
-- No cross-references are available
_VK_VERSION_PATCH :: Word32 -> Word32
_VK_VERSION_PATCH v = v .&. 0xfff
-- | VK_API_VERSION_1_0 - Return API version number for Vulkan 1.0
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceProperties'
pattern VK_API_VERSION_1_0 :: Word32
pattern VK_API_VERSION_1_0 = VK_MAKE_VERSION 1 0 0
-- | VK_HEADER_VERSION - Vulkan header file version number
--
-- = See Also
--
-- No cross-references are available
pattern VK_HEADER_VERSION :: Integral a => a
pattern VK_HEADER_VERSION = 106
