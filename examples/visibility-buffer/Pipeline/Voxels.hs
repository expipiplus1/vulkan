{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The voxel-cave generator pipeline.

Wraps the "Pipeline.Voxels.Gen" compute pass. 'recordGenerate' records the single
pass into a command buffer the caller submits once; descriptor-set layout and
push-constant range are reflected from the shader.
-}
module Pipeline.Voxels
  ( allocateGenerator
  , GenBuffers (..)
  , allocateGenSets
  , Params (..)
  , recordGenerate
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Bits ((.|.))
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as MemoryBarrier (MemoryBarrier (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Voxels.Gen as Gen

-- Generate the @Params@ generation push-constant record.
reflectShaderTypesBytes Gen.code

-- | The surface-shell gen pipeline (the whole generator).
allocateGenerator :: Vk.Device -> ResourceT IO Pipeline
allocateGenerator dev = allocateCompute dev () Gen.code

-- | The device buffers the generator appends into.
data GenBuffers = GenBuffers
  { objects :: Vk.Buffer
  -- ^ the shared object table; the gen appends cube objects after 'Objects.caveBase'.
  , indirect :: Vk.Buffer
  -- ^ the draw commands ("Objects"); the gen bumps the two cube @instanceCount@s.
  , visMain :: Vk.Buffer
  -- ^ the camera instance remap, identity-seeded per emitted cube ("Pipeline.Cull").
  , visOcc :: Vk.Buffer
  -- ^ the occluder instance remap, as 'visMain'.
  }

allocateGenSets :: Vk.Device -> Pipeline -> GenBuffers -> ResourceT IO Vk.DescriptorSet
allocateGenSets dev gen bufs = do
  set <- Pipeline.allocateSet dev gen 0
  Vk.updateDescriptorSets
    dev
    [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.objects
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.indirect
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visMain
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER bufs.visOcc
    ]
    []
  pure set

{- | Record generation into @cb@.

Barriers the uploaded draw commands + object table into the gen stage, then
evaluate-and-appends the surface shell. The draw commands + static objects are
uploaded by the caller ("Objects") before this; the caller submits @cb@ once and
waits.
-}
recordGenerate :: (MonadIO m) => Pipeline -> Vk.DescriptorSet -> Params -> Vk.CommandBuffer -> m ()
recordGenerate gen set params cb = do
  -- Make the uploaded draw commands + static objects visible to the gen's atomic
  -- append (it read-modify-writes the two cube instanceCounts).
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
    zero
    [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT .|. Vk.ACCESS_SHADER_WRITE_BIT} :: Vk.MemoryBarrier]
    []
    []
  Pipeline.bind cb gen
  Pipeline.push cb gen params
  Pipeline.bindSet cb gen 0 set
  Vk.cmdDispatch cb groups groups groups
  where
    groups = (params.gridN + 3) `div` 4
