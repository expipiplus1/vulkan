{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| The scene's lights — the single source of truth for the glowstones and the orbs.

The bake, the resolve, and the glowstone draw all read the same GPU buffer
('upload'), so the visible blocks and the shading lights can't drift apart.

The SSBO is ordered @glowstones ++ orbs@: the static prefix is baked once, the orb
suffix (from 'orbBase') is rewritten every frame.
-}
module Scene.Lights
  ( Shade.Light (..)
  , position
  , glowstones
  , Orb (..)
  , orbs
  , orbBase
  , orbCount
  , orbLight
  , orbLights
  , lights
  , count
  , slots
  , bufferBytes
  , upload
  , updateOrbs
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word32)
import Foreign.Storable (sizeOf)
import Geomancy (Vec3, vec3, withVec4)
import qualified Geomancy.Vec4 as Vec4
import qualified Vulkan.Core10 as Vk

import qualified Pipeline.Shade as Shade
import qualified Scene.Cave as Cave
import qualified Upload

-- | A light's world position (its @posHalf.xyz@).
position :: Shade.Light -> Vec3
position (Shade.Light posHalf _colInt) = withVec4 posHalf \x y z _half -> vec3 x y z

{- | The static glowstones: one 2 m block at the centre of each backroom.

At a room's centre a lamp has no rock beside it, so its nearest lit surface is the whole
wall and nothing hot-spots. Nothing else reaches a backroom — 40 m of falloff through a
3.5 m hall leaves the stage's orb below the ambient floor there — so without these the
backrooms would be black rather than dim.

@colInt.w@ is radiant intensity: the resolve divides it by the squared distance, so it
scales with the square of any change to the scene's scale.
-}
glowstones :: [Shade.Light]
glowstones = zipWith stone Cave.sideCentres hues
  where
    stone centre hue = Shade.Light (Vec4.fromVec3 centre glowstoneHalf) (Vec4.fromVec3 hue glowstoneIntensity)
    hues =
      [ vec3 1.00 0.65 0.45 -- +X ember
      , vec3 0.55 0.75 1.00 -- -X ice
      , vec3 0.60 1.00 0.75 -- +Y moss
      , vec3 0.85 0.55 1.00 -- -Y orchid
      , vec3 1.00 0.85 0.55 -- +Z amber
      , vec3 0.45 1.00 0.95 -- -Z teal
      ]

-- | Half-edge of a glowstone block: a 2 m block, at its backroom's centre.
glowstoneHalf :: Float
glowstoneHalf = 0.25

{- | Radiant intensity of a glowstone.

Chosen so a backroom meters just past the auto-exposure ceiling: lit enough to read,
dark enough that walking in from the stage costs the viewer a real adaptation.
-}
glowstoneIntensity :: Float
glowstoneIntensity = 8

{- | A dynamic orb: an animated light drawn as a glowing sphere.

The stage's key light. Orbits in XY (facing the camera) at 'orbit' radius, with a gentle
z-'bob' keeping it near the chamber mid-plane. 'phase' is its angle at @t = 0@.
-}
data Orb = Orb
  { phase :: Float
  , speed :: Float
  -- ^ Radians per second.
  , orbit :: Float
  {- ^ Orbit radius (m), out near the chamber wall: the near rock floods and the knot
  throws a shadow clear across the far side, both sweeping as the orb goes round.
  -}
  , bob :: Float
  -- ^ Z amplitude (m).
  , size :: Float
  -- ^ Visible sphere radius (m).
  , color :: Vec3
  , intensity :: Float
  -- ^ Radiant intensity, as for the 'glowstones'.
  }

{- | The scene's dynamic orbs — any number, including none.

Each takes an object slot, a lights-SSBO entry and its own shadow-cube slice, all
refreshed per frame — so the orb's shadows are the only ones that move.
-}
orbs :: [Orb]
orbs =
  [ Orb{phase = 0.6, speed = 1, orbit = 8, bob = 1.5, size = 0.25, color = vec3 0.7 0.9 1.0, intensity = 20}
  ]

-- | The orb's world position at time @t@ (seconds).
orbPosition :: Orb -> Float -> Vec3
orbPosition o t = vec3 (o.orbit * cos a) (o.orbit * sin a) (o.bob * sin (a * 1.7))
  where
    a = o.phase + o.speed * t

-- | The orb's light at time @t@: 'orbPosition' moves it; the rest is fixed.
orbLight :: Orb -> Float -> Shade.Light
orbLight o t = Shade.Light (Vec4.fromVec3 (orbPosition o t) o.size) (Vec4.fromVec3 o.color o.intensity)

orbLights :: Float -> [Shade.Light]
orbLights t = map (`orbLight` t) orbs

-- | Every light at time @t@: the glowstones, then the orbs (from 'orbBase').
lights :: Float -> [Shade.Light]
lights t = glowstones <> orbLights t

-- | Index of the first orb, in the lights SSBO and in the shadow-cube slices alike.
orbBase :: Word32
orbBase = fromIntegral (length glowstones)

orbCount :: Word32
orbCount = fromIntegral (length orbs)

count :: Word32
count = orbBase + orbCount

{- | Slots reserved for lights, at least one.

Vulkan forbids zero-sized buffers and zero-layer images, so an unlit scene still
reserves a placeholder slot. Nothing reads it: the resolve loops to @lightCount@
('count'), not to the bound array's length.
-}
slots :: Word32
slots = max 1 count

-- | The SSBO stride: one @Light { vec4 posHalf; vec4 colInt; }@.
lightBytes :: Int
lightBytes = sizeOf (undefined :: Shade.Light)

-- | Bytes for the SSBO: two @vec4@ (@posHalf@, @colInt@) per light.
bufferBytes :: Vk.DeviceSize
bufferBytes = fromIntegral (fromIntegral slots * lightBytes)

{- | Fill the lights SSBO for time @t@.

@Light { vec4 posHalf; vec4 colInt; }[]@ — the block half-size rides in @posHalf.w@,
intensity in @colInt.w@.
-}
upload :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Float -> m ()
upload cb buffer t = Upload.slice cb buffer 0 (lights t)

{- | Rewrite just the orbs' SSBO entries (from 'orbBase') for time @t@.

A per-frame companion to 'upload' that moves the orbs without re-touching the glowstones.
-}
updateOrbs :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Float -> m ()
updateOrbs cb buffer t = Upload.slice cb buffer orbBase (orbLights t)
