{-# LANGUAGE OverloadedRecordDot #-}

{-| The Box3D side of the example: the rigid-body world mirroring the render
scene, the kicks that keep it lively (explosions, popcorn tosses and their
schedule), and camera-ray picking.
-}
module Physics
  ( Ball
  , buildWorld
  , stepWorld
  , settleWorld
  , destroyWorld
  , readScene
  , applyMoveEvents
  , popcorn
  , explode
  , pickSphere

    -- * Kick scheduling and the beacon pulse
  , KickTimer
  , newKickTimer
  , tickKick
  , fireKick
  , kickEnvelope
  , beaconize
  ) where

import qualified Box3D.Body as Body
import qualified Box3D.Events as Events
import Box3D.Id (BodyId, WorldId)
import qualified Box3D.MathTypes as B3
import qualified Box3D.Shape as Shape
import qualified Box3D.Types as B3
import Box3D.UserData (ptrToUserIndex, userIndexToPtr)
import qualified Box3D.World as World
import Control.Monad (replicateM_, when)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Foreign.Marshal.Utils (toBool)
import Geomancy.Vec3 (pattern WithVec3)
import qualified Geomancy.Vec3 as V3
import Geomancy.Vec4 (vec4, pattern WithVec4)
import Say (sayErrString)
import System.Random (StdGen, uniformR)
import Text.Printf (printf)

import Pathtracer (Camera (..), Sphere (..))
import Scene (isBeacon, materialKind, materialName)

{- | A dynamic sphere: its rigid body and its render record (the physics step
only rewrites @centerRadius@; albedo and material ride along unchanged).
-}
data Ball = Ball
  { body :: BodyId
  , template :: Sphere
  }

{- | Build the Box3D world mirroring the render scene: the giant ground sphere
becomes a static body, every other sphere a dynamic one dropped from its
(already elevated) render position. Each body's user data carries its index
into the @Scene@ SSBO, so 'Events.bodyMoveEvents' can address the sphere it
moves (the ground sits at index 0, the balls follow).
-}
buildWorld :: Sphere -> [Sphere] -> IO (WorldId, V.Vector Ball)
buildWorld ground balls = do
  wd <- B3.defaultWorldDef
  world <- World.create wd{B3.worldDefGravity = B3.Vec3 0 (-10) 0}

  let WithVec4 gx gy gz gr = ground.centerRadius
  gbd <- B3.defaultBodyDef
  groundBody <- Body.create world gbd{B3.bodyDefPosition = B3.Vec3 gx gy gz}
  gsd <- B3.defaultShapeDef
  _ <-
    Shape.createSphere
      groundBody
      gsd{B3.shapeDefBaseMaterial = surfaceMaterial gsd 0.6 0.4}
      (B3.Sphere B3.vec3Zero gr)

  bodies <- V.fromList <$> traverse (uncurry (makeBall world)) (zip [1 ..] balls)
  pure (world, bodies)
  where
    -- Rolling resistance is what keeps the balls from diffusing out of frame
    -- over a minute of animation; the damping below tames residual drift.
    surfaceMaterial sd friction restitution =
      (B3.shapeDefBaseMaterial sd)
        { B3.surfaceMaterialFriction = friction
        , B3.surfaceMaterialRestitution = restitution
        , B3.surfaceMaterialRollingResistance = 0.05
        }
    makeBall world sceneIndex s = do
      let WithVec4 cx cy cz r = s.centerRadius
      bd <- B3.defaultBodyDef
      b <-
        Body.create
          world
          bd
            { B3.bodyDefType = if isBeacon s then B3.StaticBody else B3.DynamicBody
            , B3.bodyDefPosition = B3.Vec3 cx cy cz
            , B3.bodyDefUserData = userIndexToPtr sceneIndex
            , B3.bodyDefLinearDamping = 0.2
            , B3.bodyDefAngularDamping = 0.5
            }
      sd <- B3.defaultShapeDef
      _ <-
        Shape.createSphere
          b
          sd
            { B3.shapeDefBaseMaterial = surfaceMaterial sd 0.4 (restitutionFor s)
            , B3.shapeDefDensity = densityFor s
            }
          (B3.Sphere B3.vec3Zero r)
      pure Ball{body = b, template = s}

-- | Bouncier the shinier: lambertian < metal < glass; glowies are lively too.
restitutionFor :: Sphere -> Float
restitutionFor s = case materialKind s of
  1 -> 0.75
  2 -> 0.85
  3 -> 0.8
  _ -> 0.55

{- | Realistic-ish densities (kg/m³) by render material: iron for the metals,
glass for the dielectrics, plastics for the matte ones. Combined with the
varied radii, per-area kicks now sort the pile: light plastic flies, heavy
iron barely scoots and sinks to the bottom.
-}
densityFor :: Sphere -> Float
densityFor s = case materialKind s of
  1 -> 7870 -- iron
  2 -> 2500 -- glass
  3 -> 1180 -- polycarbonate lamp shell
  _ -> 950 -- polyethylene

{- | Apply one step's move events to the scene: each event addresses its
sphere by the user-data index and rewrites the centre, keeping the radius.
Bodies that slept through the step (or fell asleep) simply keep their entry.
-}
applyMoveEvents :: WorldId -> VS.Vector Sphere -> IO (VS.Vector Sphere)
applyMoveEvents world scene = do
  moves <- Events.bodyMoveEvents world
  let update ev =
        let
          i = ptrToUserIndex (B3.bodyMoveEventUserData ev)
          B3.Transform (B3.Vec3 x y z) _rotation = B3.bodyMoveEventTransform ev
          WithVec4 _ _ _ r = (scene VS.! i).centerRadius
        in
          (i, (scene VS.! i){centerRadius = vec4 x y z r})
  pure (scene VS.// map update (VS.toList moves))

-- | Advance the world by one frame's delta (4 solver substeps).
stepWorld :: WorldId -> Float -> IO ()
stepWorld world dt = World.step world dt 4

{- | Advance the world by virtual seconds without rendering (fixed 60 Hz
steps), letting the drop settle before the first frame.
-}
settleWorld :: WorldId -> Float -> IO ()
settleWorld world seconds =
  replicateM_ (ceiling (max 0 seconds * 60)) (World.step world (1 / 60) 4)

destroyWorld :: WorldId -> IO ()
destroyWorld = World.destroy

-- | The render record for a ball at its current physical position.
ballSphere :: Ball -> IO Sphere
ballSphere ball = do
  B3.Vec3 x y z <- Body.getPosition ball.body
  let WithVec4 _ _ _ r = ball.template.centerRadius
  pure ball.template{centerRadius = vec4 x y z r}

{- | Read every ball's position into a fresh scene vector. Needed after
'settleWorld': move events only cover the latest step, and settled bodies
are asleep with nothing to report.
-}
readScene :: Sphere -> V.Vector Ball -> IO (VS.Vector Sphere)
readScene ground balls = do
  moved <- traverse ballSphere (V.toList balls)
  pure (VS.fromList (ground : moved))

{- | Toss a random quarter of the small balls: mostly up, drifting back toward
the scene centre so the pile stays in frame. The impulse (@--pop-impulse@) is
per projected area (like the explosion), so a light plastic ball flies while
an equal-sized iron one barely hops — repeated pops stratify the pile by
density.
-}
popcorn :: Float -> IORef StdGen -> V.Vector Ball -> IO ()
popcorn popImpulse genRef balls = do
  sayErrString "pop!"
  for_ balls $ \ball -> do
    let WithVec4 _ _ _ r = ball.template.centerRadius
    when (r < 0.5) $ do
      lucky <- rand genRef (0, 1)
      when (lucky < 0.25) $ do
        B3.Vec3 x _ z <- Body.getPosition ball.body
        up <- rand genRef (0.66, 1.33)
        jitter <- rand genRef (0, 0.1)
        let
          kick = popImpulse * pi * r * r
          d = max 0.5 (sqrt (x * x + z * z))
          -- Spring-like centring: strays get pushed home, the pile only jitters.
          toCentre = (min 0.7 (0.08 * d) + jitter) * kick
        Body.applyLinearImpulseToCenter
          ball.body
          (B3.Vec3 (negate x / d * toCentre) (up * kick) (negate z / d * toCentre))
          True

rand :: IORef StdGen -> (Float, Float) -> IO Float
rand ref range = atomicModifyIORef' ref $ \g ->
  let (a, g') = uniformR range g in (g', a)

{- | Kick the pile from below the scene centre with @--boom-impulse@ per
projected area, at full strength within @--boom-radius@.
-}
explode :: Float -> Float -> WorldId -> IO ()
explode boomImpulse boomRadius world = do
  sayErrString "boom!"
  ed <- B3.defaultExplosionDef
  World.explode
    world
    ed
      { -- Epicentre below the ground surface: with it inside a shape the push
        -- direction is centroid-minus-epicentre, so an epicentre placed above
        -- the resting balls' centres shoves them into the ground instead.
        B3.explosionDefPosition = B3.Vec3 0 (-0.5) 0
      , B3.explosionDefRadius = boomRadius
      , B3.explosionDefFalloff = 4
      , -- Default shape density is 1000 kg/m³ (water), so the per-area
        -- impulse must be SI-sized; see --boom-impulse.
        B3.explosionDefImpulsePerArea = boomImpulse
      , -- The default def is all-zeros, and a zero mask matches no shapes at
        -- all — an explosion built straight from it is a silent no-op.
        B3.explosionDefMaskBits = maxBound
      }

{- | Camera-ray picking: cast a Box3D ray ('World.castRayClosest') through a
clicked pixel, print what it hit, and return the hit sphere's scene index.

The ray is generated exactly as the shader generates its primary rays, so the
physics pick agrees with the pixels. Box3D rays are finite (origin plus
translation), so the direction is scaled well past the far edge of the scene.
-}
pickSphere :: WorldId -> VS.Vector Sphere -> Camera -> Float -> Float -> IO (Maybe Int)
pickSphere world scene cam nx ny = do
  let
    dir = cam.lowerLeft + cam.horizontal V3.^* nx + cam.vertical V3.^* (1 - ny) - cam.origin
    WithVec3 ox oy oz = cam.origin
    WithVec3 tx ty tz = V3.normalize dir V3.^* rayReach
  filt <- B3.defaultQueryFilter
  hit <- World.castRayClosest world (B3.Vec3 ox oy oz) (B3.Vec3 tx ty tz) filt
  if not (toBool (B3.rayResultHit hit))
    then Nothing <$ sayErrString "pick: nothing (sky)"
    else do
      body <- Shape.getBody (B3.rayResultShapeId hit)
      i <- ptrToUserIndex <$> Body.getUserData body
      if i < 0 || i >= VS.length scene
        then Nothing <$ sayErrString ("pick: foreign body, user index " <> show i)
        else do
          mass <- Body.getMass body
          density <- Shape.getDensity (B3.rayResultShapeId hit)
          B3.Vec3 vx vy vz <- Body.getLinearVelocity body
          let
            s = scene VS.! i
            WithVec4 cx cy cz r = s.centerRadius
            speed = sqrt (vx * vx + vy * vy + vz * vz)
          sayErrString $
            printf
              "pick: #%d %s r=%.2fm ρ=%.0f m=%.1fkg at (%.2f, %.2f, %.2f) v=%.2fm/s, %.1fm away"
              i
              (materialName i s)
              r
              density
              mass
              cx
              cy
              cz
              speed
              (B3.rayResultFraction hit * rayReach)
          pure (Just i)
  where
    rayReach = 500 :: Float

--------------------------------------------------------------------------------
-- Kick scheduling and the beacon pulse
--------------------------------------------------------------------------------

{- | One kick kind's schedule: the next timed deadline and the last firing,
both in elapsed seconds. The beacon's pulse envelope reads both.
-}
data KickTimer = KickTimer
  { nextAt :: IORef Float
  , lastAt :: IORef Float
  }

newKickTimer :: Float -> IO KickTimer
newKickTimer period =
  KickTimer
    <$> newIORef (if period > 0 then period else 1 / 0)
    <*> newIORef (-1 / 0)

-- | Fire the action once the timed deadline passes, then schedule the next.
tickKick :: KickTimer -> Float -> Float -> IO () -> IO ()
tickKick timer period t action = when (period > 0) $ do
  next <- readIORef timer.nextAt
  when (t >= next) $ do
    writeIORef timer.nextAt (next + period)
    fireKick timer t action

-- | Fire the action now (the click path), recording the time for the fade.
fireKick :: KickTimer -> Float -> IO () -> IO ()
fireKick timer t action = writeIORef timer.lastAt t >> action

{- | The beacon envelope at elapsed time @t@: a sharp 0.2 s attack peaking
exactly at a scheduled kick (anticipation is only possible for timed ones),
then — no decay, no sustain — a 0.8 s release after any kick. Attack and
release sum to one second, so @--*-every 1@ stacks pulses back-to-back
without interleaving.
-}
kickEnvelope :: KickTimer -> Float -> IO Float
kickEnvelope timer t = do
  next <- readIORef timer.nextAt
  lastFired <- readIORef timer.lastAt
  let
    attack = 1 - (next - t) / 0.2
    release = 1 - (t - lastFired) / 0.8
  pure (min 1 (max 0 (max attack release)))

{- | Layer the kick pulses onto the beacon (scene index 1): red for the
explosion, blue for the popcorn, over its green base — material type 4
carries the emission RGB in @material.yzw@. The whole scene buffer is
re-uploaded every frame anyway (physics moves everything), so the pulse
simply rides along.
-}
beaconize :: Float -> Float -> VS.Vector Sphere -> VS.Vector Sphere
beaconize boomEnv popEnv scene =
  scene VS.// [(1, s{material = vec4 4 (4 * boomEnv) 0 (4 * popEnv)})]
  where
    s = scene VS.! 1
