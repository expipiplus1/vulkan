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





-- No documentation found for TopLevel "VK_MAKE_VERSION"
pattern VK_MAKE_VERSION :: Word32 -> Word32 -> Word32 -> Word32
pattern VK_MAKE_VERSION major minor patch <-
  (\v -> (_VK_VERSION_MAJOR v, _VK_VERSION_MINOR v, _VK_VERSION_PATCH v) -> (major, minor, patch))
  where VK_MAKE_VERSION major minor patch = major `shiftL` 22 .|. minor `shiftL` 12 .|. patch

-- No documentation found for TopLevel "VK_VERSION_MAJOR"
_VK_VERSION_MAJOR :: Word32 -> Word32
_VK_VERSION_MAJOR v = v `shiftR` 22

-- No documentation found for TopLevel "VK_VERSION_MINOR"
_VK_VERSION_MINOR :: Word32 -> Word32
_VK_VERSION_MINOR v = v `shiftR` 12 .&. 0x3ff

-- No documentation found for TopLevel "VK_VERSION_PATCH"
_VK_VERSION_PATCH :: Word32 -> Word32
_VK_VERSION_PATCH v = v .&. 0xfff

-- No documentation found for TopLevel "VK_API_VERSION_1_0"
pattern VK_API_VERSION_1_0 :: Word32
pattern VK_API_VERSION_1_0 = VK_MAKE_VERSION 1 0 0

-- No documentation found for TopLevel "VK_HEADER_VERSION"
pattern VK_HEADER_VERSION :: Integral a => a
pattern VK_HEADER_VERSION = 107
