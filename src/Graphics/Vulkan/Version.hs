
module Graphics.Vulkan.Version where

import Data.Word( Word32
                )
import Data.Bits( shiftR
                , shiftL
                , (.|.)
                , (.&.)
                )

vkVersionMinor :: Word32 -> Word32
vkVersionMinor version = (.&.) (shiftR version 12) 1023

vkHeaderVersion :: Word32
vkHeaderVersion  = 16

vkVersionPatch :: Word32 -> Word32
vkVersionPatch version = (.&.) version 4095

vkApiVersion10 :: Word32
vkApiVersion10  = vkMakeVersion 1 0 0

vkMakeVersion :: Word32 -> Word32 -> Word32 -> Word32
vkMakeVersion major minor patch = (.|.) ((.|.) (shiftL major 22) (shiftL minor 12)) patch

vkVersionMajor :: Word32 -> Word32
vkVersionMajor version = shiftR version 22

