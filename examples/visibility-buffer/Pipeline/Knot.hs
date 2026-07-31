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
  ( allocateKnot
  , vertexCount
  , allocateKnotSet
  , GenParams (..)
  , recordGenerate
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Word (Word32)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)

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
allocateKnot :: Vk.Device -> ResourceT IO Pipeline
allocateKnot dev = allocateCompute dev () Gen.code

-- | A set binding the shared vertex SSBO the gen writes into.
allocateKnotSet :: Vk.Device -> Pipeline -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateKnotSet dev knot vertices = do
  set <- Pipeline.allocateSet dev knot 0
  Vk.updateDescriptorSets dev [bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER vertices] []
  pure set

-- | Mesh-gen parameters, pushed to the compute stage.
data GenParams = GenParams
  { tubeR :: Float
  , scale :: Float
  }

-- | Record the tube-mesh generation into @cb@ (writes from vertex @base@).
recordGenerate :: (MonadIO m) => Pipeline -> Vk.DescriptorSet -> Word32 -> GenParams -> Vk.CommandBuffer -> m ()
recordGenerate knot genSet base params cb = do
  Pipeline.bind cb knot
  Pipeline.push cb knot Params{segments = segments, ring = ring, tubeR = params.tubeR, scale = params.scale, base = base}
  Pipeline.bindSet cb knot 0 genSet
  Vk.cmdDispatch cb ((segments * ring + 63) `div` 64) 1 1
