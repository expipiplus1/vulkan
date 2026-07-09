{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Orbit camera.

Spherical coordinates about 'Scene.cameraTarget'. 'initial' is the single viewpoint
both the windowed viewer and the headless render start from, so the PNG matches the
window on launch. Arrows orbit and @-@/@+@ dolly ('update'); @.@ prints the current
parameters ('dump') to copy back into 'initial'.
-}
module Scene.Camera
  ( Orbit (..)
  , initial
  , outside
  , eye
  , update
  , dump
  ) where

import Data.IORef (IORef, readIORef, writeIORef)
import Geomancy.Vec3 (Vec3, vec3, withVec3)
import qualified Graphics.UI.GLFW as GLFW

import qualified Scene

-- | Orbit camera: spherical coordinates about 'Scene.cameraTarget'.
data Orbit = Orbit
  { azimuth :: Float
  , elevation :: Float
  , distance :: Float
  -- ^ Metres from 'Scene.cameraTarget'.
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
  withVec3 Scene.cameraTarget \tx ty tz ->
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
