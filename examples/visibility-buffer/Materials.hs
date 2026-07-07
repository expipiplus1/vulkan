{-| The scene's material table — the single source of truth for surface shading.

A small CPU-defined set of PBR-lite materials uploaded to an SSBO, like "Lights". The
resolve indexes it by instanceId: cave cubes share the grey materials
(@hash % greyCount@), the knot's two instances pick @knotBase + kinst@.
-}
module Materials
  ( Material (..)
  , materials
  , count
  , greyCount
  , knotBase
  , bufferBytes
  , upload
  ) where

import Control.Monad (zipWithM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (poke)
import Geomancy.Vec4 (vec4)
import UnliftIO.Foreign (allocaBytes)
import qualified Vulkan.Core10 as Vk

import qualified Pipeline.Shade as Shade

-- | A PBR-lite material: linear albedo, metalness, roughness.
data Material = Material
  { albedo :: (Float, Float, Float)
  , metalness :: Float
  , roughness :: Float
  }

{- | The material list.

'greyCount' matte grey rock materials (the cubes share these), then the knot's
terracotta (kinst 0) and gold (kinst 1).
-}
materials :: [Material]
materials =
  [ Material (0.30, 0.30, 0.32) 0.0 0.90
  , Material (0.40, 0.39, 0.38) 0.0 0.85
  , Material (0.50, 0.49, 0.47) 0.0 0.90
  , Material (0.58, 0.56, 0.53) 0.0 0.82
  , Material (0.66, 0.64, 0.61) 0.0 0.88
  , Material (0.90, 0.52, 0.42) 0.0 0.55 -- terracotta (knot inst 0)
  , Material (1.00, 0.80, 0.34) 1.0 0.25 -- gold (knot inst 1)
  ]

-- | Grey (cube) material count; cubes index @hash % greyCount@.
greyCount :: Word32
greyCount = 5

-- | Index of the first knot material (kinst adds to this).
knotBase :: Word32
knotBase = 5

count :: Word32
count = fromIntegral (length materials)

-- | Bytes for the SSBO: two @vec4@ (albedo+pad, metal/rough+pad) per material.
bufferBytes :: Vk.DeviceSize
bufferBytes = fromIntegral (length materials) * 32

-- | Fill the material SSBO via 'Vk.cmdUpdateBuffer'.
upload :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> m ()
upload cb buffer =
  liftIO $ allocaBytes bytes $ \p -> do
    zipWithM_ (pokeMat p) [0 ..] materials
    Vk.cmdUpdateBuffer cb buffer 0 (fromIntegral bytes) p
  where
    bytes = length materials * 32
    pokeMat :: Ptr () -> Int -> Material -> IO ()
    pokeMat p i (Material (r, g, b) metal rough) =
      poke (castPtr (p `plusPtr` (i * 32))) Shade.Material{Shade.albedo = vec4 r g b 0, Shade.pbr = vec4 metal rough 0 0}
