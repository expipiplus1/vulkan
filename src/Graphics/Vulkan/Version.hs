
module Graphics.Vulkan.Version where

import Data.Word( Word32(..)
                )
import Data.Bits( shiftR
                , shiftL
                , (.|.)
                , (.&.)
                )

vkApiVersion :: Word32
vkApiVersion  = vkMakeVersion 1 0 3

vkVersionMinor :: Word32 -> Word32
vkVersionMinor version = (.&.) (shiftR version 12) 1023

vkVersionPatch :: Word32 -> Word32
vkVersionPatch version = (.&.) version 4095

vkMakeVersion :: Word32 -> Word32 -> Word32 -> Word32
vkMakeVersion major minor patch = (.|.) ((.|.) (shiftL major 22) (shiftL minor 12)) patch

vkVersionMajor :: Word32 -> Word32
vkVersionMajor version = shiftR version 22

