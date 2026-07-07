{-| The scene's light list — the single source of truth for the glowstones.

The bake, the resolve, and the glowstone draw all read the same GPU buffer
('upload'), so the visible blocks and the shading lights can't drift apart.
-}
module Lights
  ( Light (..)
  , lights
  , count
  , orbIndex
  , orbLight
  , orbRadius
  , orbPosition
  , bufferBytes
  , upload
  , updateOrb
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

{- | A glowstone.

An emissive block (world position + half-size) that also lights the scene (linear
colour + intensity).
-}
data Light = Light
  { position :: (Float, Float, Float)
  , halfSize :: Float
  , color :: (Float, Float, Float)
  , intensity :: Float
  }

{- | The scene's lights.

Four static glowstones at the cardinal directions, plus the dynamic orb (last,
'orbIndex') — a moving light drawn as a glowing sphere.
-}
lights :: [Light]
lights =
  [ Light (0.02, 0.30, -0.05) 0.05 (1.0, 0.85, 0.55) 0.06
  , Light (0.02, -0.30, -0.05) 0.05 (0.55, 0.75, 1.0) 0.06
  , Light (-0.30, 0.02, -0.05) 0.05 (1.0, 0.65, 0.45) 0.06
  , Light (0.30, 0.02, -0.05) 0.05 (0.6, 1.0, 0.75) 0.06
  , orbLight 0
  ]

-- | The orb light at time @t@: 'orbPosition' moves it; radius/colour/intensity fixed.
orbLight :: Float -> Light
orbLight t = Light (orbPosition t) orbRadius (0.7, 0.9, 1.0) 0.08

count :: Word32
count = fromIntegral (length lights)

-- | Index of the dynamic orb light (the last entry).
orbIndex :: Word32
orbIndex = count - 1

-- | The orb's visible sphere radius (world).
orbRadius :: Float
orbRadius = 1 / 128

{- | The orb's world position at time @t@ (seconds).

An XY orbit (facing the camera) at a radius that clears the knot's silhouette, so the
orb never interpenetrates it; a gentle z-bob keeps it near the chamber mid-plane.
Phased so t=0 is upper-right.
-}
orbPosition :: Float -> (Float, Float, Float)
orbPosition t = (0.17 * cos a, 0.17 * sin a, 0.05 * sin (a * 1.7))
  where
    a = t + 0.6

-- | Bytes for the SSBO: two @vec4@ (@posHalf@, @colInt@) per light.
bufferBytes :: Vk.DeviceSize
bufferBytes = fromIntegral (length lights) * 32

{- | Fill the lights SSBO via 'Vk.cmdUpdateBuffer'.

@Light { vec4 posHalf; vec4 colInt; }[]@ — the block half-size rides in @posHalf.w@,
intensity in @colInt.w@.
-}
upload :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> m ()
upload cb buffer =
  liftIO $ allocaBytes bytes $ \p -> do
    zipWithM_ (\i l -> pokeLight (p `plusPtr` (i * 32)) l) [0 ..] lights
    Vk.cmdUpdateBuffer cb buffer 0 (fromIntegral bytes) p
  where
    bytes = length lights * 32

{- | Rewrite just the orb's SSBO entry (32 bytes at 'orbIndex') for time @t@.

A per-frame companion to 'upload' that moves the light without re-touching the rest.
-}
updateOrb :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Float -> m ()
updateOrb cb buffer t =
  liftIO $ allocaBytes 32 $ \p -> do
    pokeLight p (orbLight t)
    Vk.cmdUpdateBuffer cb buffer (fromIntegral orbIndex * 32) 32 p

-- | Poke one light into the reflected 'Shade.Light' std430 layout (posHalf, colInt).
pokeLight :: Ptr () -> Light -> IO ()
pokeLight p (Light (px, py, pz) hs (cr, cg, cbl) inten) =
  poke (castPtr p) Shade.Light{Shade.posHalf = vec4 px py pz hs, Shade.colInt = vec4 cr cg cbl inten}
