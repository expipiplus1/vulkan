{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Orbit camera.

Spherical coordinates about 'target'. 'initial' is the single viewpoint
both the windowed viewer and the headless render start from, so the PNG matches the
window on launch. Arrows orbit and @-@/@+@ dolly ('update'); @.@ prints the current
parameters ('dump') to copy back into 'initial'. 'viewProjFor' is the projection
every pass pushes.
-}
module Scene.Camera
  ( Orbit (..)
  , initial
  , outside
  , eye
  , update
  , dump
  , near
  , viewFor
  , viewProjFor
  , projScales
  ) where

import Data.IORef (IORef, readIORef, writeIORef)
import qualified Geomancy.Mat4 as Mat4
import Geomancy.Transform (unTransform)
import Geomancy.Vec3 (Vec3, vec3, withVec3)
import qualified Geomancy.Vulkan.Projection as Projection
import qualified Geomancy.Vulkan.View as View
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Core10 as Vk

-- | The point the camera looks at and orbits.
target :: Vec3
target = vec3 0 0 0

fov, near :: Float
fov = 70 * pi / 180
near = 0.05

{- | The camera view-projection for @eye@ at @extent@ (geomancy, reverse-Z).

Near maps to 1, infinite far to 0 — depth clears to 0 and the test is GREATER.
-}
viewProjFor :: Vec3 -> Vk.Extent2D -> Mat4.Mat4
viewProjFor eyePos (Vk.Extent2D w h) = Mat4.matrixProduct (unTransform proj) (viewFor eyePos)
  where
    proj = Projection.reverseDepthRH fov near (fromIntegral w) (fromIntegral h)

-- | The world-to-view half of 'viewProjFor'.
viewFor :: Vec3 -> Mat4.Mat4
viewFor eyePos = unTransform (View.lookAtRH eyePos target (vec3 0 1 0))

{- | The projection's @(sx, sy)@ diagonal: @ndc.xy = (sx, sy) * view.xy / view.z@.

With 'near' over the pyramid depth this inverts the projection, so the AO
gather reconstructs view-space positions without a matrix inverse.
-}
projScales :: Vk.Extent2D -> (Float, Float)
projScales (Vk.Extent2D w h) = (sy * fromIntegral h / fromIntegral w, sy)
  where
    sy = recip (tan (fov / 2))

data Orbit = Orbit
  { azimuth :: Float
  , elevation :: Float
  , distance :: Float
  -- ^ Metres from 'target'.
  }
  deriving (Eq, Ord, Show)

-- | The launch viewpoint shared by both drivers.
initial :: Orbit
initial =
  Orbit
    { azimuth = 1.474
    , elevation = 0.12951519
    , distance = 9
    }

{- | Outside the cave, looking in.

Ambient-lit rock with the side chambers glaring through the few gaps the dense rock
leaves — a dark mean over a huge dynamic range, the case that pins auto-exposure to
its ceiling. Closer than 'Cave.caveRadius' would put the eye back inside the rock.
-}
outside :: Orbit
outside = initial{distance = 80}

-- | Eye position for an orbit state.
eye :: Orbit -> Vec3
eye o =
  withVec3 target \tx ty tz ->
    vec3 (tx + o.distance * ce * ca) (ty + o.distance * se) (tz + o.distance * ce * sa)
  where
    ca = cos o.azimuth
    sa = sin o.azimuth
    ce = cos o.elevation
    se = sin o.elevation

{- | Advance the orbit from held keys over @dt@ seconds.

Arrows orbit (left/right azimuth, up/down elevation), @-@/@+@ dolly. Elevation is
clamped shy of the poles; distance to a sane range.
-}
update :: GLFW.Window -> Float -> IORef Orbit -> IO Orbit
update window dt ref = do
  o <- readIORef ref
  l <- held GLFW.Key'Left
  r <- held GLFW.Key'Right
  u <- held GLFW.Key'Up
  d <- held GLFW.Key'Down
  outward <- (||) <$> held GLFW.Key'Minus <*> held GLFW.Key'PadSubtract
  inward <- (||) <$> held GLFW.Key'Equal <*> held GLFW.Key'PadAdd
  let
    rot = 1.6 * dt
    axis neg pos = (if pos then rot else 0) - (if neg then rot else 0)
    o' =
      Orbit
        { azimuth = o.azimuth + axis l r
        , elevation = clamp (-1.45) 1.45 (o.elevation + axis d u)
        , distance = clamp 0.5 150.0 (o.distance * exp (0.9 * dt * (bit outward - bit inward)))
        }
  writeIORef ref o'
  pure o'
  where
    held k = (== GLFW.KeyState'Pressed) <$> GLFW.getKey window k
    clamp lo hi = max lo . min hi
    bit b = if b then 1 else 0

-- | Print the current parameters (the @.@ key handler); 'show' matches 'initial''s syntax.
dump :: Orbit -> IO ()
dump o = putStrLn ("orbit " <> show o)
