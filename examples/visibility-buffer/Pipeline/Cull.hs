{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The per-frame cull pipeline.

Wraps the "Pipeline.Cull.Shader" compute pass: 'reset' rewinds the camera and
per-orb occluder cube draws, 'record' refills them (and their instance remaps)
with the cave cubes that pass this frame's tests. Two graph passes at the top of the frame
("Scene"); the barriers between the fills, the dispatch and the draws that
consume the refill are the graph tracker's.
-}
module Pipeline.Cull
  ( allocatePipeline
  , CullBuffers (..)
  , allocateSet
  , Params (..)
  , reset
  , record
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Word (Word32)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)

import qualified Pipeline.Cull.Shader as Shader
import qualified Scene.Objects as Objects

-- Generate the @Params@ cull push-constant record.
reflectShaderTypesBytes Shader.code

allocatePipeline :: Vk.Device -> ResourceT IO Pipeline
allocatePipeline dev = allocateCompute dev () Shader.code

-- | The buffers the cull reads and refills.
data CullBuffers = CullBuffers
  { objects :: Vk.Buffer
  -- ^ the shared object table (read: cave-cube bounds from the transforms).
  , indirect :: Vk.Buffer
  -- ^ the draw commands ("Scene.Objects"); the cube @instanceCount@s are the counters.
  , visMain :: Vk.Buffer
  -- ^ the camera instance remap, refilled from the frustum test.
  , visOcc :: Vk.Buffer
  -- ^ the occluder instance remap, its per-orb ranges refilled from the reach tests.
  , lights :: Vk.Buffer
  -- ^ the lights SSBO (read: each orb's centre and reach).
  }

-- | The set: 'CullBuffers' at bindings 0-3 and 5, the depth pyramid sampled at 4.
allocateSet :: Vk.Device -> Pipeline -> CullBuffers -> Vk.Sampler -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateSet dev cull bufs sampler hizView = do
  set <- Pipeline.allocateSet dev cull 0
  Vk.updateDescriptorSets
    dev
    [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.objects
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.indirect
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visMain
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visOcc
    , combinedImageSamplerWrite set 4 sampler hizView Vk.IMAGE_LAYOUT_GENERAL
    , bufferWrite set 5 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.lights
    ]
    []
  pure set

{- | Reset the cube draws.

@mainCube.instanceCount@ rewinds to the glowstone prefix (= @caveBase@), each
orb's occluder cube to empty. The full occluder set (@occCube@) keeps the
generator's count — only the bake draws it.
-}
reset :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Word32 -> m ()
reset cb indirect caveBase = do
  Vk.cmdFillBuffer cb indirect Objects.mainCubeCountOffset 4 caveBase
  forM_ Objects.orbOccCountOffsets \off -> Vk.cmdFillBuffer cb indirect off 4 0

-- | Record the cull dispatch, refilling the reset draws from this frame's tests.
record :: (MonadIO m) => Pipeline -> Vk.DescriptorSet -> Params -> Vk.CommandBuffer -> m ()
record cull set params cb = liftIO do
  Pipeline.bind cb cull
  Pipeline.push cb cull params
  Pipeline.bindSet cb cull 0 set
  Vk.cmdDispatch cb ((params.caveCount + 255) `div` 256) 1 1
