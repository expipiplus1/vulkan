{-# LANGUAGE OverloadedRecordDot #-}

{-| The object table + indirect draw commands for the unified mesh renderer.

One object-table SSBO holds every drawn thing @{transform, emissive, meshId,
materialId, flags}@, indexed by @gl_InstanceIndex@. It is partitioned per mesh with
CPU-known objects at fixed leading slots and the GPU generator appending after a known
base ('caveBase'), so every draw command's @firstInstance@ is static:

@
[ glowstones (fixed) | cave cubes (GPU) | (unused) ][ knot i0 i1 ][ orbs.. ]
  0            G-1     G                    K-1          K   K+1     O
@

The indirect buffer holds five 'Vk.DrawIndirectCommand's: @mainCube mainKnot
mainSphere@ (the camera pass draws these three) then @occCube occKnot@ (the shadow
pass draws these two, @occCube@ skipping the non-occluder glowstones; the orb spheres
are not occluders). The gen bumps the two cube @instanceCount@s; every other field
is CPU-static.

The vertex shaders don't index the table by @gl_InstanceIndex@ directly: each draw
family goes through an instance remap (camera @visMain@, occluder @visOcc@) holding
one object id per drawn instance. The never-culled slots are identity
('uploadStaticRemap', and the gen seeds its cubes likewise); the per-frame cull
compacts the cave range ("Pipeline.Cull").
-}
module Scene.Objects
  ( Layout (..)
  , layout
  , objectBufferBytes
  , indirectBytes
  , remapBytes
  , mainDrawOffset
  , mainCubeCountOffset
  , occluderDrawOffset
  , occCubeCountOffset
  , drawStride
  , mainDrawCount
  , occluderDrawCount
  , uploadDrawCommands
  , uploadStaticObjects
  , uploadStaticRemap
  , writeOrbObjects
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word32)
import Foreign.Storable (sizeOf)
import Geomancy (vec4, withVec4)
import Geomancy.Transform (rotateY, scale, translate, unTransform)
import qualified Vulkan.Core10 as Vk

import Pipeline.Mesh (Object (..))
import qualified Scene.Lights as Lights
import qualified Scene.Materials as Materials
import qualified Scene.Meshes as Meshes
import qualified Upload

-- | Number of CPU-known glowstone cubes (one per static light).
glowstoneCount :: Word32
glowstoneCount = fromIntegral (length Lights.glowstones)

knotObjectCount :: Word32
knotObjectCount = 2

-- | The object-table partitioning for a given cave-cube capacity.
data Layout = Layout
  { caveBase :: Word32
  -- ^ First cave-cube slot (the generator's atomic counter starts here).
  , knotBase :: Word32
  -- ^ First knot-object slot.
  , orbBase :: Word32
  -- ^ First dynamic orb sphere slot ('Lights.orbCount' of them).
  , total :: Word32
  }

layout :: Word32 -> Layout
layout maxCubes =
  Layout
    { caveBase = glowstoneCount
    , knotBase = glowstoneCount + maxCubes
    , orbBase = glowstoneCount + maxCubes + knotObjectCount
    , total = glowstoneCount + maxCubes + knotObjectCount + Lights.orbCount
    }

objectStride :: Int
objectStride = sizeOf (undefined :: Object)

objectBufferBytes :: Layout -> Vk.DeviceSize
objectBufferBytes l = fromIntegral l.total * fromIntegral objectStride

drawStride :: Word32
drawStride = fromIntegral (sizeOf (undefined :: Vk.DrawIndirectCommand))

mainDrawCount, occluderDrawCount :: Word32
mainDrawCount = 3
occluderDrawCount = 2

mainDrawOffset :: Vk.DeviceSize
mainDrawOffset = 0

occluderDrawOffset :: Vk.DeviceSize
occluderDrawOffset = fromIntegral mainDrawCount * fromIntegral drawStride

{- | Byte offsets of the two cube @instanceCount@s — the gen's and the cull's counters.

The shaders hardcode them as word indices (@cmd[1]@, @cmd[13]@ in
"Pipeline.Voxels.Gen" and "Pipeline.Cull.Shader"); keep all three in step.
-}
mainCubeCountOffset, occCubeCountOffset :: Vk.DeviceSize
mainCubeCountOffset = mainDrawOffset + 4
occCubeCountOffset = occluderDrawOffset + 4

indirectBytes :: Vk.DeviceSize
indirectBytes = fromIntegral (mainDrawCount + occluderDrawCount) * fromIntegral drawStride

-- | Bytes for an instance remap: one @uint@ object id per drawable slot.
remapBytes :: Layout -> Vk.DeviceSize
remapBytes l = fromIntegral l.total * 4

{- | Initialise the five draw commands.

The cube @instanceCount@s (main starts at 'glowstoneCount', occluder at 0) are
append counters — the generator's at bake, then the cull's each frame
("Pipeline.Cull" resets and refills them); the rest are static.
-}
uploadDrawCommands :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Layout -> m ()
uploadDrawCommands cb buffer l = Upload.slice cb buffer 0 commands
  where
    -- vertexCount, instanceCount, firstVertex, firstInstance
    commands =
      [ Vk.DrawIndirectCommand Meshes.cubeVertexCount glowstoneCount 0 0 -- mainCube (+ cave cubes)
      , Vk.DrawIndirectCommand Meshes.knotVertexCount knotObjectCount 0 l.knotBase -- mainKnot
      , Vk.DrawIndirectCommand Meshes.sphereVertexCount Lights.orbCount 0 l.orbBase -- mainSphere (orbs)
      , Vk.DrawIndirectCommand Meshes.cubeVertexCount 0 0 l.caveBase -- occCube (cave cubes only)
      , Vk.DrawIndirectCommand Meshes.knotVertexCount knotObjectCount 0 l.knotBase -- occKnot
      ]

{- | Write the CPU-known objects.

The glowstone cubes (emissive, @[0..G)@) and the two knot instances (at 'knotBase').
-}
uploadStaticObjects :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Layout -> m ()
uploadStaticObjects cb buffer l = do
  Upload.slice cb buffer 0 glowstones
  Upload.slice cb buffer l.knotBase knots
  where
    glowstones = do
      Lights.Light pos color <- Lights.glowstones
      pure
        Object
          { transform = unTransform $ withVec4 pos \x y z hs -> translate x y z <> scale hs
          , emissive = color * vec4 4 4 4 0
          , meshId = Meshes.cube
          , materialId = 0
          , flags = 0
          , pad = 0
          }
    knots = do
      k <- [0, 1]
      pure
        Object
          { transform = unTransform $ if k == 0 then mempty else rotateY (pi / 2)
          , emissive = vec4 0 0 0 0
          , meshId = Meshes.knot
          , materialId = Materials.knotBase + k
          , flags = 0
          , pad = 0
          }

{- | Seed a remap's never-culled entries with identity.

The glowstones, knots and orbs always draw as themselves; the cave range is seeded
by the generator and rewritten per frame by the cull.
-}
uploadStaticRemap :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Layout -> m ()
uploadStaticRemap cb buffer l = do
  when (l.caveBase > 0) $ Upload.slice cb buffer 0 [0 .. l.caveBase - 1 :: Word32]
  Upload.slice cb buffer l.knotBase [l.knotBase .. l.total - 1]

{- | (Re)write the dynamic orb spheres (from 'orbBase') for time @t@.

Emissive spheres placed by 'Lights.orbLight'.
-}
writeOrbObjects :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Layout -> Float -> m ()
writeOrbObjects cb buffer l t = Upload.slice cb buffer l.orbBase (map (`orbObject` t) Lights.orbs)

orbObject :: Lights.Orb -> Float -> Object
orbObject o t =
  Object
    { transform = unTransform $ withVec4 pos \x y z size -> translate x y z <> scale size
    , emissive = color * vec4 5 5 5 0
    , meshId = Meshes.sphere
    , materialId = 0
    , flags = 0
    , pad = 0
    }
  where
    Lights.Light pos color = Lights.orbLight o t
