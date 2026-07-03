{-# LANGUAGE OverloadedRecordDot #-}

{-| The procedural sphere scene (a "Ray Tracing in One Weekend" arrangement)
and the BVH built over it on the host — linked and traversed on the GPU by
device address.
-}
module Scene
  ( buildCamera
  , buildScene
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

{- | Build the reflected 'Camera' for a simple look-at camera with unit focal
length (no defocus blur).
-}
buildCamera :: Float -> Float -> Vec3 -> Vec3 -> Vec3 -> Camera
buildCamera aspect vfovDeg lookFrom lookAt vup =
  Camera
    { origin = lookFrom
    , lowerLeft = lowerLeft
    , horizontal = horizontal
    , vertical = vertical
    , skyTop = vec3 0.5 0.7 1.0
    , skyBottom = vec3 1.0 1.0 1.0
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

{- | The full scene: ground, three feature spheres, and @count@ random small
ones laid out on a grid (from @seed@).
-}
buildScene :: Int -> Word32 -> [Sphere]
buildScene count seed = ground : features ++ small
  where
    ground = lambertian (vec3 0 (-1000) 0) 1000 (vec3 0.5 0.5 0.5)
    features =
      [ dielectric (vec3 0 1 0) 1 1.5
      , lambertian (vec3 (-4) 1 0) 1 (vec3 0.4 0.2 0.1)
      , metal (vec3 4 1 0) 1 (vec3 0.7 0.6 0.5) 0
      ]
    small = evalState (smallSpheres count) (mkStdGen (fromIntegral seed))

{- | Up to @n@ small spheres scattered over a grid, skipping any too close to the
feature spheres. Materials are randomly diffuse\/metal\/glass.
-}
smallSpheres :: Int -> State StdGen [Sphere]
smallSpheres n = catMaybes <$> mapM gen (take n grid)
  where
    -- A square field of unit cells centred on the origin, sized to hold @n@
    -- spheres so they expand toward the horizon as the count grows. (@n = 484@
    -- reproduces the classic @[-11 .. 10]^2@ "Ray Tracing in One Weekend" grid.)
    side = ceiling (sqrt (fromIntegral (max 1 n) :: Double)) :: Int
    lo = negate (side `div` 2)
    grid = [(a, b) | a <- [lo .. lo + side - 1], b <- [lo .. lo + side - 1]]
    gen (a, b) = do
      rx <- rF (0, 0.9)
      rz <- rF (0, 0.9)
      let
        center = vec3 (fromIntegral a + rx) 0.2 (fromIntegral b + rz)
        d = center - vec3 4 0.2 0
      -- Skip spheres too close to the metal feature sphere (compare squared).
      if V3.dot d d <= 0.9 * 0.9
        then pure Nothing
        else do
          choose <- rF (0, 1)
          fmap Just $
            if choose < (0.8 :: Float)
              then do
                c1 <- rF (0, 1)
                c2 <- rF (0, 1)
                c3 <- rF (0, 1)
                pure $ lambertian center 0.2 (vec3 (c1 * c1) (c2 * c2) (c3 * c3))
              else
                if choose < 0.95
                  then do
                    c1 <- rF (0.5, 1)
                    c2 <- rF (0.5, 1)
                    c3 <- rF (0.5, 1)
                    fz <- rF (0, 0.5)
                    pure $ metal center 0.2 (vec3 c1 c2 c3) fz
                  else pure $ dielectric center 0.2 1.5
    rF range = state (uniformR range)

-- Sphere constructors (material encoding matches the shader). The center is a
-- 'Vec3' and the albedo a 'Vec3'; @centerRadius@ packs the radius into w.

mkSphere :: Vec3 -> Float -> Vec4 -> Vec4 -> Sphere
mkSphere center r = Sphere (fromVec3 center r)

lambertian :: Vec3 -> Float -> Vec3 -> Sphere
lambertian c r albedo = mkSphere c r (fromVec3 albedo 1) (vec4 0 0 0 0)

metal :: Vec3 -> Float -> Vec3 -> Float -> Sphere
metal c r albedo fuzz = mkSphere c r (fromVec3 albedo 1) (vec4 1 fuzz 0 0)

dielectric :: Vec3 -> Float -> Float -> Sphere
dielectric c r ior = mkSphere c r (vec4 1 1 1 1) (vec4 2 0 ior 0)

--------------------------------------------------------------------------------
-- BVH (built on the host; linked and traversed on the GPU by device address)
--------------------------------------------------------------------------------

-- | An axis-aligned bounding box (min, max corners).
data Aabb = Aabb !Vec3 !Vec3

-- | Bound a sphere from its reflected @centerRadius@ (xyz centre, w radius).
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

{- | Flatten the tree to an array in pre-order so the root lands at index 0;
children are referenced by array index, later resolved to device addresses.
-}
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
