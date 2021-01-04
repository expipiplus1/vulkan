{-# language CPP #-}
-- No documentation found for Chapter "Version"
module Vulkan.Version  ( pattern HEADER_VERSION
                       , pattern HEADER_VERSION_COMPLETE
                       , pattern MAKE_VERSION
                       , _VERSION_MAJOR
                       , _VERSION_MINOR
                       , _VERSION_PATCH
                       ) where

import Data.Bits ((.&.))
import Data.Bits ((.|.))
import Data.Bits (shiftL)
import Data.Bits (shiftR)
import Data.Word (Word32)

pattern HEADER_VERSION :: Word32
pattern HEADER_VERSION = 166


pattern HEADER_VERSION_COMPLETE :: Word32
pattern HEADER_VERSION_COMPLETE = MAKE_VERSION 1 2 166


pattern MAKE_VERSION :: Word32 -> Word32 -> Word32 -> Word32
pattern MAKE_VERSION major minor patch <-
  (\v -> (_VERSION_MAJOR v, _VERSION_MINOR v, _VERSION_PATCH v) -> (major, minor, patch))
  where MAKE_VERSION major minor patch = major `shiftL` 22 .|. minor `shiftL` 12 .|. patch

_VERSION_MAJOR :: Word32 -> Word32
_VERSION_MAJOR v = v `shiftR` 22

_VERSION_MINOR :: Word32 -> Word32
_VERSION_MINOR v = v `shiftR` 12 .&. 0x3ff

_VERSION_PATCH :: Word32 -> Word32
_VERSION_PATCH v = v .&. 0xfff

