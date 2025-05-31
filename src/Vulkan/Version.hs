{-# language CPP #-}
-- No documentation found for Chapter "Version"
module Vulkan.Version  ( pattern HEADER_VERSION
                       , pattern HEADER_VERSION_COMPLETE
                       , pattern MAKE_API_VERSION
                       , pattern MAKE_VERSION
                       , _VERSION_MAJOR
                       , _VERSION_MINOR
                       , _VERSION_PATCH
                       , _API_VERSION_MAJOR
                       , _API_VERSION_MINOR
                       , _API_VERSION_PATCH
                       ) where

import Data.Bits (shiftL)
import Data.Bits (shiftR)
import Data.Bits ((.&.))
import Data.Bits ((.|.))
import Data.Word (Word32)

pattern HEADER_VERSION :: Word32
pattern HEADER_VERSION = 273


pattern HEADER_VERSION_COMPLETE :: Word32
pattern HEADER_VERSION_COMPLETE = MAKE_API_VERSION 1 3 273


pattern MAKE_API_VERSION :: Word32 -> Word32 -> Word32 -> Word32
pattern MAKE_API_VERSION major minor patch <-
  (\v -> (_VERSION_MAJOR v, _VERSION_MINOR v, _VERSION_PATCH v) -> (major, minor, patch))
  where MAKE_API_VERSION major minor patch = major `shiftL` 22 .|. minor `shiftL` 12 .|. patch

{-# complete MAKE_API_VERSION #-}

{-# deprecated MAKE_VERSION "This pattern is deprecated. MAKE_API_VERSION should be used instead." #-}
pattern MAKE_VERSION :: Word32 -> Word32 -> Word32 -> Word32
pattern MAKE_VERSION major minor patch = MAKE_API_VERSION major minor patch

{-# complete MAKE_VERSION #-}

{-# deprecated _VERSION_MAJOR "This function is deprecated. _API_VERSION_MAJOR should be used instead." #-}
_VERSION_MAJOR :: Word32 -> Word32
_VERSION_MAJOR = _API_VERSION_MAJOR

{-# deprecated _VERSION_MINOR "This function is deprecated. _API_VERSION_MINOR should be used instead." #-}
_VERSION_MINOR :: Word32 -> Word32
_VERSION_MINOR = _API_VERSION_MINOR

{-# deprecated _VERSION_PATCH "This function is deprecated. _API_VERSION_PATCH should be used instead." #-}
_VERSION_PATCH :: Word32 -> Word32
_VERSION_PATCH = _API_VERSION_PATCH

_API_VERSION_MAJOR :: Word32 -> Word32
_API_VERSION_MAJOR v = v `shiftR` 22

_API_VERSION_MINOR :: Word32 -> Word32
_API_VERSION_MINOR v = v `shiftR` 12 .&. 0x3ff

_API_VERSION_PATCH :: Word32 -> Word32
_API_VERSION_PATCH v = v .&. 0xfff

