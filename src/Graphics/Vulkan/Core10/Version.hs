{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}

module Graphics.Vulkan.Core10.Version
  ( pattern VK_HEADER_VERSION
  , pattern VK_MAKE_VERSION
  , pattern VK_API_VERSION_1_0
  , pattern VK_API_VERSION_1_1
  , _VK_VERSION_MAJOR
  , _VK_VERSION_MINOR
  , _VK_VERSION_PATCH
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





pattern VK_HEADER_VERSION :: Integral a => a
pattern VK_HEADER_VERSION = 73
pattern VK_MAKE_VERSION :: Word32 -> Word32 -> Word32 -> Word32
pattern VK_MAKE_VERSION major minor patch <-
  (\v -> (_VK_VERSION_MAJOR v, _VK_VERSION_MINOR v, _VK_VERSION_PATCH v) -> (major, minor, patch))
  where VK_MAKE_VERSION major minor patch = major `shiftL` 22 .|. minor `shiftL` 12 .|. patch

pattern VK_API_VERSION_1_0 :: Word32
pattern VK_API_VERSION_1_0 = VK_MAKE_VERSION 1 0 0

pattern VK_API_VERSION_1_1 :: Word32
pattern VK_API_VERSION_1_1 = VK_MAKE_VERSION 1 1 0

_VK_VERSION_MAJOR :: Word32 -> Word32
_VK_VERSION_MAJOR v = v `shiftR` 22

_VK_VERSION_MINOR :: Word32 -> Word32
_VK_VERSION_MINOR v = v `shiftR` 12 .&. 0x3ff

_VK_VERSION_PATCH :: Word32 -> Word32
_VK_VERSION_PATCH v = v .&. 0xfff
