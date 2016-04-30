
module Graphics.Vulkan.Version where

import Data.Word( Word32(..)
                )
import Data.Bits( shiftR
                , shiftL
                , (.|.)
                , (.&.)
                )

apiVersion :: Word32
apiVersion  = makeVersion 1 0 3

versionMinor :: Word32 -> Word32
versionMinor version = (.&.) (shiftR version 12) 1023

versionPatch :: Word32 -> Word32
versionPatch version = (.&.) version 4095

makeVersion :: Word32 -> Word32 -> Word32 -> Word32
makeVersion major minor patch = (.|.) ((.|.) (shiftL major 22) (shiftL minor 12)) patch

versionMajor :: Word32 -> Word32
versionMajor version = shiftR version 22

