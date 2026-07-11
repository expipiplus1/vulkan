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

The indirect buffer holds @mainCube mainKnot mainSphere@ (the camera pass draws
these three), @occCube occKnot@ (the full occluder set the static bake draws,
@occCube@ skipping the non-occluder glowstones; the orb spheres are not
occluders), then one @orbOccCube orbOccKnot@ pair per orb — the per-frame
@shadow.orbs@ refresh draws orb @i@'s pair, whose cube range the cull compacts
to that orb's reach alone. The gen bumps the main and full-occ cube
@instanceCount@s once; the cull resets and refills the per-orb ones each frame.

The vertex shaders don't index the table by @gl_InstanceIndex@ directly: each draw
family goes through an instance remap (camera @visMain@, occluder @visOcc@) holding
one object id per drawn instance. The never-culled slots are identity
('uploadStaticRemap', and the gen seeds its cubes likewise); the per-orb ranges
sit past 'remapBytes' in @visOcc@, 'orbOccCap' entries each.
-}
module Scene.Objects
  ( Layout (..)
  , layout
  , objectBufferBytes
  , indirectBytes
  , remapBytes
  , occRemapBytes
  , orbOccCap
  , mainDrawOffset
  , mainCubeCountOffset
  , occluderDrawOffset
  , orbOccDrawOffset
  , orbOccCountOffsets
  , orbOccCountWord0
  , orbOccCountWordStride
  , drawStride
  , mainDrawCount
  , occluderDrawCount
  , uploadDrawCommands
  , uploadStaticObjects
  , uploadStaticRemap
  , seedOrbOccRemap
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

-- | Byte offset of orb @i@'s @[orbOccCube, orbOccKnot]@ pair.
orbOccDrawOffset :: Word32 -> Vk.DeviceSize
orbOccDrawOffset i = occluderDrawOffset + fromIntegral (occluderDrawCount + 2 * i) * fromIntegral drawStride

{- | Byte offset of the camera cube draw's @instanceCount@ — the gen's, then the cull's counter.

"Pipeline.Voxels.Gen" hardcodes it (and the full occluder cube's, at
@occluderDrawOffset + 4@) as word indices @cmd[1]@ / @cmd[13]@; keep them in step.
-}
mainCubeCountOffset :: Vk.DeviceSize
mainCubeCountOffset = mainDrawOffset + 4

-- | Byte offsets of the per-orb occluder-cube @instanceCount@s (the cull's counters).
orbOccCountOffsets :: [Vk.DeviceSize]
orbOccCountOffsets = [orbOccDrawOffset i + 4 | i <- [0 .. Lights.orbCount - 1]]

{- | Orb @o@'s counter as a raw-word index: @orbOccCountWord0 + orbOccCountWordStride * o@.

Spliced into "Pipeline.Cull.Shader"'s @cmd[]@ access, so the command layout has
one owner.
-}
orbOccCountWord0, orbOccCountWordStride :: Word32
orbOccCountWord0 = fromIntegral (orbOccDrawOffset 0 + 4) `div` 4
orbOccCountWordStride = 2 * drawStride `div` 4

indirectBytes :: Vk.DeviceSize
indirectBytes = fromIntegral (mainDrawCount + occluderDrawCount + 2 * Lights.orbCount) * fromIntegral drawStride

-- | Bytes for an instance remap: one @uint@ object id per drawable slot.
remapBytes :: Layout -> Vk.DeviceSize
remapBytes l = fromIntegral l.total * 4

{- | Entries in one orb's occluder range: everything a reach sphere can plausibly
hold, at a quarter of the cave capacity. The cull drops appends past it (losing
those cubes' shadows for that orb); the count still runs over, so the draw's tail
reads the 'seedOrbOccRemap' filler — duplicates of a real cube, not garbage.
-}
orbOccCap :: Layout -> Word32
orbOccCap l = (l.knotBase - l.caveBase) `div` 4

-- | 'remapBytes' plus the per-orb occluder ranges ('orbOccCap' entries each).
occRemapBytes :: Layout -> Vk.DeviceSize
occRemapBytes l = remapBytes l + fromIntegral (Lights.orbCount * orbOccCap l) * 4

{- | Initialise the draw commands.

The cube @instanceCount@s (main starts at 'glowstoneCount', the occluders at 0)
are append counters — the generator's at bake, then the cull's each frame
("Pipeline.Cull" resets and refills the per-orb ones); the rest are static.
-}
uploadDrawCommands :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Layout -> m ()
uploadDrawCommands cb buffer l = Upload.slice cb buffer 0 commands
  where
    -- vertexCount, instanceCount, firstVertex, firstInstance
    occKnot = Vk.DrawIndirectCommand Meshes.knotVertexCount knotObjectCount 0 l.knotBase
    commands =
      [ Vk.DrawIndirectCommand Meshes.cubeVertexCount glowstoneCount 0 0 -- mainCube (+ cave cubes)
      , Vk.DrawIndirectCommand Meshes.knotVertexCount knotObjectCount 0 l.knotBase -- mainKnot
      , Vk.DrawIndirectCommand Meshes.sphereVertexCount Lights.orbCount 0 l.orbBase -- mainSphere (orbs)
      , Vk.DrawIndirectCommand Meshes.cubeVertexCount 0 0 l.caveBase -- occCube (cave cubes only)
      , occKnot
      ]
        <> concat
          [ [Vk.DrawIndirectCommand Meshes.cubeVertexCount 0 0 (l.total + i * orbOccCap l), occKnot] -- orb i's pair
          | i <- [0 .. Lights.orbCount - 1]
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

{- | Fill the per-orb occluder ranges with the first cave slot.

The filler is only ever drawn on range overflow (see 'orbOccCap'), and a
duplicate of a real occluder shadows idempotently.
-}
seedOrbOccRemap :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Layout -> m ()
seedOrbOccRemap cb buffer l =
  when (Lights.orbCount > 0) $
    Vk.cmdFillBuffer cb buffer (remapBytes l) (occRemapBytes l - remapBytes l) l.caveBase

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
