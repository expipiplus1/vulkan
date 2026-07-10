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

Wraps the "Pipeline.Cull.Shader" compute pass: 'record' resets the two cube
draw commands and refills them (and their instance remaps) with the cave cubes
that pass this frame's tests. Recorded at the top of the frame's command buffer,
before anything that draws.
-}
module Pipeline.Cull
  ( Pipeline (..)
  , allocatePipeline
  , CullBuffers (..)
  , allocateSet
  , Params (..)
  , record
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.Bits ((.|.))
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as MemoryBarrier (MemoryBarrier (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite)
import Vulkan.Utils.SpirV.Descriptors (pushConstantsSize)
import Vulkan.Utils.SpirV.Pipeline (allocateComputePipeline, allocateReflectedLayout, singleSetLayout)
import qualified Vulkan.Utils.SpirV.Pipeline
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Cull.Shader as Shader
import qualified Scene.Objects as Objects

-- Generate the @Params@ cull push-constant record.
reflectShaderTypesBytes Shader.code

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , layout :: Vk.PipelineLayout
  , setLayout :: Vk.DescriptorSetLayout
  , pushSize :: Word32
  -- ^ Reflected @Params@ range size (< the std430 'Storable' size, which trailing-pads).
  }

allocatePipeline :: Vk.Device -> ResourceT IO Pipeline
allocatePipeline dev = do
  reflected <- reflectBytes Shader.code
  (_, reflectedLayout) <- allocateReflectedLayout dev [reflected]
  setLayout <- singleSetLayout reflectedLayout
  (_, pipeline) <- allocateComputePipeline dev reflectedLayout () (reflected, Shader.code)
  let pushSize = pushConstantsSize reflected
  pure Pipeline{pipeline, layout = reflectedLayout.pipelineLayout, setLayout, pushSize}

-- | The buffers the cull reads and refills.
data CullBuffers = CullBuffers
  { objects :: Vk.Buffer
  -- ^ the shared object table (read: cave-cube bounds from the transforms).
  , indirect :: Vk.Buffer
  -- ^ the draw commands ("Scene.Objects"); the two cube @instanceCount@s are the counters.
  , visMain :: Vk.Buffer
  -- ^ the camera instance remap, refilled from the frustum test.
  , visOcc :: Vk.Buffer
  -- ^ the occluder instance remap, refilled from the orb-reach test.
  }

-- | The set: 'CullBuffers' at bindings 0-3, the depth pyramid sampled at 4.
allocateSet :: Vk.Device -> Pipeline -> CullBuffers -> Vk.Sampler -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateSet dev cull bufs sampler hizView = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 4
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
            ]
        }
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [cull.setLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.objects
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.indirect
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visMain
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visOcc
    , combinedImageSamplerWrite set 4 sampler hizView Vk.IMAGE_LAYOUT_GENERAL
    ]
    []
  pure set

{- | Record the per-frame cull into @cb@.

Resets the cube draws (@mainCube@ to the glowstone prefix, @occCube@ to empty),
then refills them from the tests. Bracketed by barriers against the previous
frame's draws (they read the commands and remaps the reset overwrites) and this
frame's (they consume the refill).
-}
record :: (MonadIO m) => Pipeline -> Vk.DescriptorSet -> Vk.Buffer -> Params -> Vk.CommandBuffer -> m ()
record cull set indirect params cb = liftIO do
  Vk.cmdPipelineBarrier
    cb
    (Vk.PIPELINE_STAGE_DRAW_INDIRECT_BIT .|. Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT .|. Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT)
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    zero
    [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_SHADER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT} :: Vk.MemoryBarrier]
    []
    []
  -- mainCube.instanceCount ← the glowstone prefix (= caveBase); occCube's ← 0.
  Vk.cmdFillBuffer cb indirect Objects.mainCubeCountOffset 4 params.caveBase
  Vk.cmdFillBuffer cb indirect Objects.occCubeCountOffset 4 0
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
    zero
    [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT .|. Vk.ACCESS_SHADER_WRITE_BIT} :: Vk.MemoryBarrier]
    []
    []
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE cull.pipeline
  with params \p ->
    Vk.cmdPushConstants cb cull.layout Vk.SHADER_STAGE_COMPUTE_BIT 0 cull.pushSize (castPtr p)
  Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE cull.layout 0 [set] []
  Vk.cmdDispatch cb ((params.caveCount + 255) `div` 256) 1 1
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
    (Vk.PIPELINE_STAGE_DRAW_INDIRECT_BIT .|. Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT)
    zero
    [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_SHADER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_INDIRECT_COMMAND_READ_BIT .|. Vk.ACCESS_SHADER_READ_BIT} :: Vk.MemoryBarrier]
    []
    []
