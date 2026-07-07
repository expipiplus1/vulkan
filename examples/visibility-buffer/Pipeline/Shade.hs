{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The shade (deferred resolve) compute pipeline.

Maps visibility-buffer ids to HDR colour ("Pipeline.Shade.Shader"). The set-0 layout
is reflected from 'Shade.Shader.code' via 'singleDescriptorSetLayoutInfo', so it
can't drift from the shader. Callers allocate one descriptor set per render target
with 'allocateDescriptorSet'.
-}
module Pipeline.Shade
  ( Pipeline (..)
  , Camera (..)
  , Light (..)
  , Vertex (..)
  , Material (..)
  , allocatePipeline
  , allocateDescriptorSet
  , workgroup
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Data.Vector as V
import Data.Word (Word32)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite, imageWrite)
import Vulkan.Utils.Shader (shaderModuleStage)
import Vulkan.Utils.SpirV.Descriptors (pushConstantRanges, singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Shade.Shader as Shader

-- Generate geomancy-backed records (with std140/std430 'Storable') for the shader's
-- blocks — notably the @Camera@ push constant — from the same SPIR-V the runtime loads.
reflectShaderTypesBytes Shader.code

-- | Workgroup size on each axis (matches @local_size_x\/y@ in the shader).
workgroup :: Word32
workgroup = 8

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  , descriptorSetLayout :: Vk.DescriptorSetLayout
  , cameraPushSize :: Word32
  {- ^ Reflected @Camera@ push-constant range size (< the std430 'Storable' size,
  which trailing-pads to 16) — push exactly this many bytes to satisfy the layout.
  -}
  }

-- | The pipeline; its set 0 layout reflected from 'Shader.code'.
allocatePipeline :: Vk.Device -> ResourceT IO Pipeline
allocatePipeline dev = do
  reflected <- reflectBytes Shader.code
  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate
  let ranges = pushConstantRanges reflected
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero{Vk.setLayouts = [descriptorSetLayout], Vk.pushConstantRanges = V.fromList ranges}
      Nothing
      allocate
  (_, stage) <- shaderModuleStage dev Vk.SHADER_STAGE_COMPUTE_BIT Nothing Shader.code
  let createInfo = zero{Vk.layout = pipelineLayout, Vk.stage = stage} :: Vk.ComputePipelineCreateInfo '[]
  (_, (_, [pipeline])) <- Vk.withComputePipelines dev zero [SomeStruct createInfo] Nothing allocate
  let cameraPushSize = maximum (0 : [r.offset + r.size | r <- ranges])
  pure Pipeline{pipeline, pipelineLayout, descriptorSetLayout, cameraPushSize}

{- | A descriptor set for the resolve.

Visibility (0) and colour (1) storage images, the shared vertex SSBO (2, DAIS), the
lights SSBO (3), the EVSM shadow cube-array sampler (4), the object table SSBO (5),
the material table SSBO (6), and the mesh table SSBO (7).
-}
allocateDescriptorSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.ImageView -> Vk.Buffer -> Vk.Buffer -> Vk.Sampler -> Vk.ImageView -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateDescriptorSet dev pl visView colorView verts lights sampler shadowCube objects materials meshes = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE 2
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 5
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
            ]
        }
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.descriptorSetLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL visView
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL colorView
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER verts
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER lights
    , combinedImageSamplerWrite set 4 sampler shadowCube Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    , bufferWrite set 5 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER objects
    , bufferWrite set 6 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER materials
    , bufferWrite set 7 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER meshes
    ]
    []
  pure set
