{-# LANGUAGE OverloadedRecordDot #-}

{-| The object table + indirect draw commands for the unified mesh renderer.

One object-table SSBO holds every drawn thing @{transform, emissive, meshId,
materialId, flags}@, indexed by @gl_InstanceIndex@. It is partitioned per mesh with
CPU-known objects at fixed leading slots and the GPU generator appending after a known
base ('caveBase'), so every draw command's @firstInstance@ is static:

@
[ glowstones (fixed) | cave cubes (GPU) | (unused) ][ knot i0 i1 ][ orb ]
  0            G-1     G                    K-1          K   K+1     O
@

The indirect buffer holds five 'Vk.DrawIndirectCommand's: @mainCube mainKnot
mainSphere@ (the camera pass draws these three) then @occCube occKnot@ (the shadow
pass draws these two, @occCube@ skipping the non-occluder glowstones; the orb sphere
is not an occluder). The gen bumps the two cube @instanceCount@s; every other field
is CPU-static.
-}
module Objects
  ( Layout (..)
  , layout
  , objectBufferBytes
  , indirectBytes
  , mainDrawOffset
  , occluderDrawOffset
  , drawStride
  , mainDrawCount
  , occluderDrawCount
  , uploadDrawCommands
  , uploadStaticObjects
  , writeOrbObject
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word32)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke, sizeOf)
import Geomancy.Transform (rotateY, scale, translateV, unTransform)
import Geomancy.Vec3 (vec3)
import Geomancy.Vec4 (vec4)
import UnliftIO.Foreign (allocaBytes)
import qualified Vulkan.Core10 as Vk

import qualified Lights
import qualified Materials
import qualified Meshes
import Pipeline.Mesh (Object (..))

-- | Number of CPU-known glowstone cubes (every light except the dynamic orb).
glowstoneCount :: Word32
glowstoneCount = Lights.orbIndex

knotObjectCount :: Word32
knotObjectCount = 2

-- | The object-table partitioning for a given cave-cube capacity.
data Layout = Layout
  { caveBase :: Word32
  -- ^ First cave-cube slot (the generator's atomic counter starts here).
  , knotBase :: Word32
  -- ^ First knot-object slot.
  , orbBase :: Word32
  -- ^ The (single) dynamic orb sphere slot.
  , total :: Word32
  }

layout :: Word32 -> Layout
layout maxCubes =
  Layout
    { caveBase = glowstoneCount
    , knotBase = glowstoneCount + maxCubes
    , orbBase = glowstoneCount + maxCubes + knotObjectCount
    , total = glowstoneCount + maxCubes + knotObjectCount + 1
    }

objectStride :: Int
objectStride = sizeOf (undefined :: Object)

objectBufferBytes :: Layout -> Vk.DeviceSize
objectBufferBytes l = fromIntegral l.total * fromIntegral objectStride

-- Five commands: mainCube, mainKnot, mainSphere (camera), occCube, occKnot (shadow).
drawStride :: Word32
drawStride = fromIntegral (sizeOf (undefined :: Vk.DrawIndirectCommand))

mainDrawCount, occluderDrawCount :: Word32
mainDrawCount = 3
occluderDrawCount = 2

mainDrawOffset :: Vk.DeviceSize
mainDrawOffset = 0

occluderDrawOffset :: Vk.DeviceSize
occluderDrawOffset = fromIntegral mainDrawCount * fromIntegral drawStride

indirectBytes :: Vk.DeviceSize
indirectBytes = fromIntegral (mainDrawCount + occluderDrawCount) * fromIntegral drawStride

{- | Initialise the five draw commands.

The cube @instanceCount@s (main starts at 'glowstoneCount', occluder at 0) are the
generator's atomic counters; the rest are static.
-}
uploadDrawCommands :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Layout -> m ()
uploadDrawCommands cb buffer l =
  liftIO $ allocaBytes (fromIntegral indirectBytes) $ \p -> do
    pokeArray
      (castPtr p)
      -- vertexCount, instanceCount, firstVertex, firstInstance
      [ Vk.DrawIndirectCommand Meshes.cubeVertexCount glowstoneCount 0 0 -- mainCube (+ cave cubes)
      , Vk.DrawIndirectCommand Meshes.knotVertexCount knotObjectCount 0 l.knotBase -- mainKnot
      , Vk.DrawIndirectCommand Meshes.sphereVertexCount 1 0 l.orbBase -- mainSphere (orb)
      , Vk.DrawIndirectCommand Meshes.cubeVertexCount 0 0 l.caveBase -- occCube (cave cubes only)
      , Vk.DrawIndirectCommand Meshes.knotVertexCount knotObjectCount 0 l.knotBase -- occKnot
      ]
    Vk.cmdUpdateBuffer cb buffer 0 indirectBytes p

{- | Write the CPU-known objects.

The glowstone cubes (emissive, @[0..G)@) and the two knot instances (at 'knotBase').
-}
uploadStaticObjects :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Layout -> m ()
uploadStaticObjects cb buffer l = liftIO $ do
  allocaBytes (glow * objectStride) $ \p -> do
    pokeArray (castPtr p) glowstones
    Vk.cmdUpdateBuffer cb buffer 0 (fromIntegral (glow * objectStride)) p
  allocaBytes (2 * objectStride) $ \p -> do
    pokeArray (castPtr p) knots
    Vk.cmdUpdateBuffer cb buffer (fromIntegral l.knotBase * fromIntegral objectStride) (fromIntegral (2 * objectStride)) p
  where
    glow = fromIntegral glowstoneCount
    glowstones =
      [ Object
          { transform = unTransform (translateV (vec3 px py pz) <> scale hs)
          , emissive = vec4 (r * 4) (g * 4) (b * 4) 0
          , meshId = Meshes.cube
          , materialId = 0
          , flags = 0
          , pad = 0
          }
      | Lights.Light (px, py, pz) hs (r, g, b) _ <- take glow Lights.lights
      ]
    knots =
      [ Object
          { transform = unTransform (if k == 0 then scale 1 else rotateY (pi / 2))
          , emissive = vec4 0 0 0 0
          , meshId = Meshes.knot
          , materialId = Materials.knotBase + k
          , flags = 0
          , pad = 0
          }
      | k <- [0, 1]
      ]

{- | (Re)write the dynamic orb sphere object at 'orbBase' for time @t@.

An emissive sphere placed by 'Lights.orbPosition' / 'Lights.orbRadius'.
-}
writeOrbObject :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Layout -> Float -> m ()
writeOrbObject cb buffer l t =
  liftIO $ allocaBytes objectStride $ \p -> do
    poke (castPtr p) (orbObject t)
    Vk.cmdUpdateBuffer cb buffer (fromIntegral l.orbBase * fromIntegral objectStride) (fromIntegral objectStride) p

orbObject :: Float -> Object
orbObject t =
  Object
    { transform = unTransform (translateV (vec3 px py pz) <> scale Lights.orbRadius)
    , emissive = vec4 (r * 5) (g * 5) (b * 5) 0
    , meshId = Meshes.sphere
    , materialId = 0
    , flags = 0
    , pad = 0
    }
  where
    (px, py, pz) = Lights.orbPosition t
    Lights.Light _ _ (r, g, b) _ = Lights.orbLight 0
