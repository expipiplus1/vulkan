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
  ( composes
  , allocatePipeline
  , allocateSet
  , Object (..)
  , Vertex (..)
  , MeshEntry (..)
  , Camera (..)
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateGraphicsPipeline, allocateReflectedLayout)
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

-- | The pipeline drawing into @visFormat@ (+ @depthFormat@ depth test).
allocatePipeline :: Vk.Device -> Vk.Format -> Vk.Format -> ResourceT IO Pipeline
allocatePipeline dev visFormat depthFormat = do
  vertModule <- reflectBytes Shader.vertCode
  fragModule <- reflectBytes Shader.fragCode
  layout <- allocateReflectedLayout dev [vertModule, fragModule]
  allocateGraphicsPipeline
    dev
    layout
    zero{Dynamic.colorFormats = [visFormat], Dynamic.depthFormat = Just depthFormat}
    ()
    [(vertModule, Shader.vertCode), (fragModule, Shader.fragCode)]

-- | A set binding the vertex (0), mesh-table (1), object-table (2) and instance-remap (3) SSBOs.
allocateSet :: Vk.Device -> Pipeline -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl verts meshes objects visible = do
  set <- Pipeline.allocateSet dev pl 0
  Vk.updateDescriptorSets
    dev
    [ bufferWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER verts
    , bufferWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER meshes
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER objects
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER visible
    ]
    []
  pure set
