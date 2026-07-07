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
  ( ComputePipeline (..)
  , allocateGenerator
  , GenBuffers (..)
  , allocateGenSets
  , Params (..)
  , recordGenerate
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.Bits ((.|.))
import qualified Data.Vector as V
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Foreign.Storable (sizeOf)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as MemoryBarrier (MemoryBarrier (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Shader (shaderModuleStage)
import Vulkan.Utils.SpirV.Descriptors (pushConstantRanges, singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import Data.ByteString (ByteString)
import qualified Pipeline.Voxels.Gen as Gen

-- Generate the @Params@ generation push-constant record.
reflectShaderTypesBytes Gen.code

-- | A compute pipeline plus the layouts reflected for it.
data ComputePipeline = ComputePipeline
  { pipeline :: Vk.Pipeline
  , layout :: Vk.PipelineLayout
  , setLayout :: Vk.DescriptorSetLayout
  }

-- | The surface-shell gen pipeline (the whole generator).
allocateGenerator :: Vk.Device -> ResourceT IO ComputePipeline
allocateGenerator dev = buildCompute dev Gen.code

buildCompute :: Vk.Device -> ByteString -> ResourceT IO ComputePipeline
buildCompute dev code = do
  reflected <- reflectBytes code
  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, setLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate
  (_, layout) <-
    Vk.withPipelineLayout
      dev
      zero{Vk.setLayouts = [setLayout], Vk.pushConstantRanges = V.fromList (pushConstantRanges reflected)}
      Nothing
      allocate
  (_, stage) <- shaderModuleStage dev Vk.SHADER_STAGE_COMPUTE_BIT Nothing code
  let createInfo = zero{Vk.layout = layout, Vk.stage = stage} :: Vk.ComputePipelineCreateInfo '[]
  (_, (_, pipelines)) <- Vk.withComputePipelines dev zero [SomeStruct createInfo] Nothing allocate
  pure ComputePipeline{pipeline = V.head pipelines, layout, setLayout}

-- | The two device buffers the generator appends into.
data GenBuffers = GenBuffers
  { objects :: Vk.Buffer
  -- ^ the shared object table; the gen appends cube objects after 'Objects.caveBase'.
  , indirect :: Vk.Buffer
  -- ^ the four draw commands ("Objects"); the gen bumps the two cube @instanceCount@s.
  }

allocateGenSets :: Vk.Device -> ComputePipeline -> GenBuffers -> ResourceT IO Vk.DescriptorSet
allocateGenSets dev gen bufs = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero{Vk.maxSets = 1, Vk.poolSizes = [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 2]}
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [gen.setLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ ssbo set 0 bufs.objects
    , ssbo set 1 bufs.indirect
    ]
    []
  pure set
  where
    ssbo set binding buffer =
      SomeStruct
        zero
          { Vk.dstSet = set
          , Vk.dstBinding = binding
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
          , Vk.descriptorCount = 1
          , Vk.bufferInfo = [Vk.DescriptorBufferInfo buffer 0 Vk.WHOLE_SIZE]
          }

{- | Record generation into @cb@.

Barriers the uploaded draw commands + object table into the gen stage, then
evaluate-and-appends the surface shell. The draw commands + static objects are
uploaded by the caller ("Objects") before this; the caller submits @cb@ once and
waits.
-}
recordGenerate :: (MonadIO m) => ComputePipeline -> Vk.DescriptorSet -> Params -> Vk.CommandBuffer -> m ()
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
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE gen.pipeline
  liftIO $ with params \p ->
    Vk.cmdPushConstants cb gen.layout Vk.SHADER_STAGE_COMPUTE_BIT 0 (fromIntegral (sizeOf params)) (castPtr p)
  Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE gen.layout 0 [set] []
  Vk.cmdDispatch cb groups groups groups
  where
    groups = (params.gridN + 3) `div` 4
