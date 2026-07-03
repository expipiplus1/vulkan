{-# LANGUAGE OverloadedRecordDot #-}

{-| The procedural sphere scene (the @pathtrace-reflect@ arrangement with a
beacon and night-time glowies), the camera under its day/night sky, and the
BVH built over the spheres on the host — linked and traversed on the GPU by
device address.
-}
module Scene
  ( buildCamera
  , elevatedScene
  , isBeacon
  , materialKind
  , materialName
  , sphereAabb
  , buildBvh
  , flattenBvh
  , toBvhNode
  , BvhFlat
  ) where

import Control.Monad.Trans.State (State, evalState, runState, state)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Word (Word32, Word64)
import Geomancy.Vec3 (Vec3, emap2, vec3, pattern WithVec3)
import qualified Geomancy.Vec3 as V3
import Geomancy.Vec4 (Vec4, fromVec3, vec4, pattern WithVec4)
import System.Random (StdGen, mkStdGen, uniformR)

import Vulkan.Utils.SpirV.DeviceAddress (DeviceAddress (..))

import Pathtracer (BvhNode (..), Camera (..), Sphere (..))

--------------------------------------------------------------------------------
-- Camera and the day/night clock
--------------------------------------------------------------------------------

-- | Build the reflected 'Camera' for a simple look-at camera at an hour of day.
buildCamera :: Float -> Float -> Vec3 -> Vec3 -> Vec3 -> Float -> Camera
buildCamera aspect vfovDeg lookFrom lookAt vup hours =
  Camera
    { origin = lookFrom
    , lowerLeft = lowerLeft
    , horizontal = horizontal
    , vertical = vertical
    , sunGlow = fromVec3 (sunDirAt hours) (glowAt hours)
    }
  where
    theta = vfovDeg * pi / 180
    viewportH = 2 * tan (theta / 2)
    viewportW = aspect * viewportH
    w = V3.normalize (lookFrom - lookAt)
    u = V3.normalize (V3.cross vup w)
    v = V3.cross w u
    horizontal = u V3.^* viewportW
    vertical = v V3.^* viewportH
    lowerLeft = lookFrom - (horizontal V3.^* 0.5) - (vertical V3.^* 0.5) - w

{- | Sun direction for an hour of the 24-hour day: rises at 6, peaks at 12
(elevation ~63°), sets at 18; the azimuth sweeps a full turn per day.
-}
sunDirAt :: Float -> Vec3
sunDirAt hours = vec3 (cos el * cos az) (sin el) (cos el * sin az)
  where
    el = 1.1 * sin ((hours - 6) / 12 * pi)
    az = hours / 24 * 2 * pi

{- | The emissive ramp fed to the shader: 0 in daylight, 1 once the sun is
properly down. Mirrors the sky's own day factor so the glowies take over
right as the ambient light fades.
-}
glowAt :: Float -> Float
glowAt hours = case sunDirAt hours of
  WithVec3 _ sunH _ -> 1 - smoothstep (-0.18) 0.05 sunH

smoothstep :: Float -> Float -> Float -> Float
smoothstep lo hi x = t * t * (3 - 2 * t)
  where
    t = min 1 (max 0 ((x - lo) / (hi - lo)))

--------------------------------------------------------------------------------
-- Spheres
--------------------------------------------------------------------------------

-- | The material type tag (the shader's @material.x@).
materialKind :: Sphere -> Int
materialKind s = case s.material of
  WithVec4 kind _ _ _ -> round kind

-- | The pulsing kick indicator at the scene centre (material type 4).
isBeacon :: Sphere -> Bool
isBeacon s = materialKind s == 4

-- | Human name for a picked sphere; scene index 0 is the ground.
materialName :: Int -> Sphere -> String
materialName 0 _ = "the ground"
materialName _ s = case materialKind s of
  1 -> "metal"
  2 -> "glass"
  3 -> "glowy"
  4 -> "the beacon"
  _ -> "plastic"

{- | The scene of @n@ random spheres (seeded), split into the (static) ground
and the dynamic balls, each lifted to a random drop height.
-}
elevatedScene :: Int -> Word32 -> (Sphere, [Sphere])
elevatedScene n seed = case buildScene n seed of
  g : rest ->
    (g, evalState (traverse elevate rest) (mkStdGen (fromIntegral seed + 7)))
  [] -> error "buildScene: empty"

{- | Lift a sphere to a random drop height, keeping its ground position.
The beacon stays put: it is locked in place, not dropped.
-}
elevate :: Sphere -> State StdGen Sphere
elevate s
  | isBeacon s = pure s
  | otherwise = do
      h <- state (uniformR (1.5, 8))
      let WithVec4 x y z r = s.centerRadius
      pure s{centerRadius = vec4 x (y + h) z r}

{- | The full scene: ground, three feature spheres, and @n@ random small ones
laid out on a grid (seeded). The pathtrace-reflect arrangement, except the
matte feature sphere sits locked at the centre as the kick-indicating beacon
(green base, emission layered by 'Physics.beaconize' — its scene index 1 is
what 'Physics.beaconize' relies on) and the glass one takes its old spot. The
ground is head of the list.
-}
buildScene :: Int -> Word32 -> [Sphere]
buildScene n seed = ground : features ++ small
  where
    ground = lambertian (vec3 0 (-1000) 0) 1000 (vec3 0.5 0.5 0.5)
    features =
      [ mkSphere (vec3 0 1 0) 1 (fromVec3 (vec3 0 0.75 0) 1) (vec4 4 0 0 0)
      , dielectric (vec3 (-4) 1 0) 1 1.5
      , metal (vec3 4 1 0) 1 (vec3 0.7 0.6 0.5) 0
      ]
    small = evalState (smallSpheres n) (mkStdGen (fromIntegral seed))

-- | Up to @n@ small spheres scattered over a grid, as in @pathtrace-reflect@.
smallSpheres :: Int -> State StdGen [Sphere]
smallSpheres n = catMaybes <$> mapM gen (take n grid)
  where
    side = ceiling (sqrt (fromIntegral (max 1 n) :: Double)) :: Int
    lo = negate (side `div` 2)
    grid = [(a, b) | a <- [lo .. lo + side - 1], b <- [lo .. lo + side - 1]]
    gen (a, b) = do
      rx <- rF (0, 0.9)
      rz <- rF (0, 0.9)
      r <- rF (0.12, 0.3)
      let
        center = vec3 (fromIntegral a + rx) r (fromIntegral b + rz)
        d = center - vec3 4 r 0
      if V3.dot d d <= 0.9 * 0.9
        then pure Nothing
        else do
          choose <- rF (0, 1)
          fmap Just $
            if choose < (0.62 :: Float)
              then do
                c1 <- rF (0, 1)
                c2 <- rF (0, 1)
                c3 <- rF (0, 1)
                pure $ lambertian center r (vec3 (c1 * c1) (c2 * c2) (c3 * c3))
              else
                if choose < 0.78
                  then do
                    -- A glowy: saturated colour (brightest channel pinned to
                    -- 1), lighting up as the shader ramps 'glow' at night.
                    c1 <- rF (0.2, 1)
                    c2 <- rF (0.2, 1)
                    c3 <- rF (0.2, 1)
                    strength <- rF (6, 16)
                    let m = max c1 (max c2 c3)
                    pure $ emissive center r (vec3 (c1 / m) (c2 / m) (c3 / m)) strength
                  else
                    if choose < 0.95
                      then do
                        c1 <- rF (0.5, 1)
                        c2 <- rF (0.5, 1)
                        c3 <- rF (0.5, 1)
                        fz <- rF (0, 0.5)
                        pure $ metal center r (vec3 c1 c2 c3) fz
                      else pure $ dielectric center r 1.5
    rF range = state (uniformR range)

-- Sphere constructors (material encoding matches the shader).

mkSphere :: Vec3 -> Float -> Vec4 -> Vec4 -> Sphere
mkSphere center r = Sphere (fromVec3 center r)

lambertian :: Vec3 -> Float -> Vec3 -> Sphere
lambertian c r albedo = mkSphere c r (fromVec3 albedo 1) (vec4 0 0 0 0)

metal :: Vec3 -> Float -> Vec3 -> Float -> Sphere
metal c r albedo fuzz = mkSphere c r (fromVec3 albedo 1) (vec4 1 fuzz 0 0)

dielectric :: Vec3 -> Float -> Float -> Sphere
dielectric c r ior = mkSphere c r (vec4 1 1 1 1) (vec4 2 0 ior 0)

emissive :: Vec3 -> Float -> Vec3 -> Float -> Sphere
emissive c r colour strength = mkSphere c r (fromVec3 colour 1) (vec4 3 strength 0 0)

--------------------------------------------------------------------------------
-- BVH (rebuilt on the host every frame; traversed on the GPU by device address)
--------------------------------------------------------------------------------

-- | An axis-aligned bounding box (min, max corners).
data Aabb = Aabb !Vec3 !Vec3

-- | Bound a sphere from its @centerRadius@ (xyz centre, w radius).
sphereAabb :: Sphere -> Aabb
sphereAabb sphere = case sphere.centerRadius of
  WithVec4 cx cy cz r ->
    Aabb (vec3 (cx - r) (cy - r) (cz - r)) (vec3 (cx + r) (cy + r) (cz + r))

aabbUnion :: Aabb -> Aabb -> Aabb
aabbUnion (Aabb amin amax) (Aabb bmin bmax) =
  Aabb (emap2 min amin bmin) (emap2 max amax bmax)

-- | A binary BVH over sphere indices; each leaf bounds one sphere.
data Bvh = BvhLeaf Int Aabb | BvhSplit Aabb Bvh Bvh

-- | Build a BVH by recursively median-splitting the longest axis.
buildBvh :: [(Int, Aabb)] -> Bvh
buildBvh [] = error "buildBvh: empty scene"
buildBvh [(i, bb)] = BvhLeaf i bb
buildBvh xs = BvhSplit bb (buildBvh l) (buildBvh r)
  where
    bb = foldr1 aabbUnion (map snd xs)
    Aabb lo hi = bb
    axis = longestAxis (hi - lo)
    (l, r) = splitAt (length xs `div` 2) (sortOn (axisKey axis . snd) xs)

longestAxis :: Vec3 -> Int
longestAxis (WithVec3 x y z)
  | x >= y && x >= z = 0
  | y >= z = 1
  | otherwise = 2

-- | Twice the box centre on an axis (the constant factor is irrelevant to sorting).
axisKey :: Int -> Aabb -> Float
axisKey axis (Aabb (WithVec3 mnx mny mnz) (WithVec3 mxx mxy mxz)) = case axis of
  0 -> mnx + mxx
  1 -> mny + mxy
  _ -> mnz + mxz

{- | A flattened node: bounds, child array indices (-1 if none), and the leaf's
sphere index (-1 for an internal node).
-}
data BvhFlat = BvhFlat Vec3 Vec3 Int Int Int

-- | Flatten the tree to an array in pre-order so the root lands at index 0.
flattenBvh :: Bvh -> [BvhFlat]
flattenBvh tree = map snd (sortOn fst nodes)
  where
    (_, (_, nodes)) = runState (go tree) (0 :: Int, [])
    fresh = state (\(n, acc) -> (n, (n + 1, acc)))
    emit i f = state (\(n, acc) -> ((), (n, (i, f) : acc)))
    go (BvhLeaf i (Aabb mn mx)) = do
      idx <- fresh
      emit idx (BvhFlat mn mx (-1) (-1) i)
      pure idx
    go (BvhSplit (Aabb mn mx) l r) = do
      idx <- fresh
      li <- go l
      ri <- go r
      emit idx (BvhFlat mn mx li ri (-1))
      pure idx

{- | Realise a flattened node as the reflected 'BvhNode' record, resolving child
indices to device addresses within the node buffer (base + index * stride).
-}
toBvhNode :: Word64 -> Int -> BvhFlat -> BvhNode
toBvhNode base stride (BvhFlat mn mx li ri si) =
  BvhNode
    { boundsMin = fromVec3 mn 0
    , boundsMax = fromVec3 mx 0
    , left = childAddr li
    , right = childAddr ri
    , sphereIndex = fromIntegral si
    }
  where
    childAddr i
      | i < 0 = DeviceAddress 0
      | otherwise = DeviceAddress (base + fromIntegral (i * stride))
