{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The unified mesh visibility pipeline.

One raster pipeline that draws every base mesh via vertex-pulling + a per-object
transform (see "Pipeline.Mesh.Shader"). Interface types (the vertex, mesh-entry and
object SSBO layouts, the camera push range) are reflected from the shader bytecode;
'composes' proves the vertex↔fragment composition at compile time. 'Object' is the
CPU-pokeable object-table record.
-}
module Pipeline.Mesh
  ( Pipeline (..)
  , composes
  , allocatePipeline
  , allocateSet
  , Object (..)
  , Vertex (..)
  , MeshEntry (..)
  , Camera (..)
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Data.Vector as V
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.SpirV.Descriptors (pushConstantRanges, singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.Stage (CompatibleResources, MatchInterface, reflectStageSigBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Mesh.Shader as Shader

-- Reflect the SSBO/push records (Object, Vertex, MeshEntry, Camera).
reflectShaderTypesBytes Shader.vertCode

reflectStageSigBytes "VertSig" Shader.vertCode
reflectStageSigBytes "FragSig" Shader.fragCode

-- | The vertex↔fragment composition, proved at compile time.
composes :: (MatchInterface VertSig FragSig, CompatibleResources VertSig FragSig) => Bool
composes = True

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  , descriptorSetLayout :: Vk.DescriptorSetLayout
  }

-- | The pipeline drawing into @visFormat@ (+ @depthFormat@ depth test).
allocatePipeline :: Vk.Device -> Vk.Format -> Vk.Format -> ResourceT IO Pipeline
allocatePipeline dev visFormat depthFormat = do
  reflected <- reflectBytes Shader.vertCode
  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero{Vk.setLayouts = [descriptorSetLayout], Vk.pushConstantRanges = V.fromList (pushConstantRanges reflected)}
      Nothing
      allocate
  (_, pipeline) <-
    Dynamic.allocatePipelineFromShaders
      dev
      zero{Dynamic.colorFormats = [visFormat], Dynamic.depthFormat = Just depthFormat, Dynamic.layout = Just pipelineLayout}
      ()
      [(Vk.SHADER_STAGE_VERTEX_BIT, Shader.vertCode), (Vk.SHADER_STAGE_FRAGMENT_BIT, Shader.fragCode)]
  pure Pipeline{pipeline, pipelineLayout, descriptorSetLayout}

-- | A set binding the vertex (0), mesh-table (1) and object-table (2) SSBOs.
allocateSet :: Vk.Device -> Pipeline -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl verts meshes objects = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero{Vk.maxSets = 1, Vk.poolSizes = [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 3]}
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.descriptorSetLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER verts
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER meshes
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER objects
    ]
    []
  pure set
