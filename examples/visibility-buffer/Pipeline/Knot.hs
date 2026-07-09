{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| The torus-knot mesh generator.

A compute pipeline that builds a tube mesh into the shared vertex SSBO (at
'Meshes.knotBase') once at load time; the knot is then drawn like every other mesh by
"Pipeline.Mesh" (two object-table instances). The gen push-constant range is
reflected from the shader bytecode.
-}
module Pipeline.Knot
  ( Knot (..)
  , allocateKnot
  , vertexCount
  , allocateKnotSet
  , GenParams (..)
  , recordGenerate
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Foreign.Storable (sizeOf)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite)
import Vulkan.Utils.SpirV.Pipeline (allocateComputePipeline, allocateReflectedLayout, singleSetLayout)
import qualified Vulkan.Utils.SpirV.Pipeline
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Knot.Gen as Gen

-- Generate the @Params@ generation push-constant record (segments, ring, tube, scale, base).
reflectShaderTypesBytes Gen.code

-- | Tube-mesh topology (must match the loops in "Pipeline.Knot.Gen").
segments, ring :: Word32
segments = 200
ring = 20

-- | Non-indexed triangle-soup vertex count: @segments * ring@ quads × 6.
vertexCount :: Word32
vertexCount = segments * ring * 6

-- | The mesh-gen compute pipeline (writes the shared vertex SSBO).
data Knot = Knot
  { pipeline :: Vk.Pipeline
  , layout :: Vk.PipelineLayout
  , setLayout :: Vk.DescriptorSetLayout
  }

allocateKnot :: Vk.Device -> ResourceT IO Knot
allocateKnot dev = buildCompute dev Gen.code

buildCompute :: Vk.Device -> ByteString -> ResourceT IO Knot
buildCompute dev code = do
  reflected <- reflectBytes code
  (_, reflectedLayout) <- allocateReflectedLayout dev [reflected]
  setLayout <- singleSetLayout reflectedLayout
  (_, pipeline) <- allocateComputePipeline dev reflectedLayout () (reflected, code)
  pure Knot{pipeline, layout = reflectedLayout.pipelineLayout, setLayout}

-- | A set binding the shared vertex SSBO the gen writes into.
allocateKnotSet :: Vk.Device -> Knot -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateKnotSet dev knot vertices = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero{Vk.maxSets = 1, Vk.poolSizes = [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 1]}
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [knot.setLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets dev [bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER vertices] []
  pure set

-- | Mesh-gen parameters, pushed to the compute stage.
data GenParams = GenParams
  { tubeR :: Float
  , scale :: Float
  }

-- | Record the tube-mesh generation into @cb@ (writes from vertex @base@).
recordGenerate :: (MonadIO m) => Knot -> Vk.DescriptorSet -> Word32 -> GenParams -> Vk.CommandBuffer -> m ()
recordGenerate knot genSet base params cb = do
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE knot.pipeline
  let pc = Params{segments = segments, ring = ring, tubeR = params.tubeR, scale = params.scale, base = base}
  liftIO $ with pc \p ->
    Vk.cmdPushConstants cb knot.layout Vk.SHADER_STAGE_COMPUTE_BIT 0 (fromIntegral (sizeOf pc)) (castPtr p)
  Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE knot.layout 0 [genSet] []
  Vk.cmdDispatch cb ((segments * ring + 63) `div` 64) 1 1
