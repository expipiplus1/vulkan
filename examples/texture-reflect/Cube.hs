{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The cube pipeline: a spinning cube sampling the offscreen image through a
@sampler2D@ at set 1, binding 0.

Its @position@ + @uv@ vertex attributes are described from reflection
("Vulkan.Utils.SpirV.VertexInput"); the per-stage signatures prove the
vertex\/fragment composition at compile time ('composes'). The @Globals@
record its shaders share with the offscreen pass is generated once, in "Tri".
-}
module Cube
  ( composes
  , Pipeline (..)
  , allocatePipeline
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import qualified Vulkan.Core10 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Zero (zero)

import Data.SpirV.Reflect.FFI (loadBytes)
import Vulkan.Utils.SpirV.Stage (CompatibleResources, MatchInterface, reflectStageSigBytes)
import Vulkan.Utils.SpirV.VertexInput (vertexInputState)

import qualified Cube.Shader as Shader

reflectStageSigBytes "VertSig" Shader.vertCode
reflectStageSigBytes "FragSig" Shader.fragCode

-- | This pipeline's vertex↔fragment composition, proved at compile time.
composes :: (MatchInterface VertSig FragSig, CompatibleResources VertSig FragSig) => Bool
composes = True

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  }

{- | Colour + depth, vertex attributes from reflection. Set 0 is the layout
shared with the offscreen pipeline; set 1 holds this pipeline's sampler.
-}
allocatePipeline
  :: Vk.Device
  -> Vk.Format
  -> Vk.Format
  -> Vk.DescriptorSetLayout
  -> Vk.DescriptorSetLayout
  -> ResourceT IO Pipeline
allocatePipeline dev colorFormat depthFormat set0Layout set1Layout = do
  vertModule <- loadBytes Shader.vertCode
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero{PipelineLayoutCreateInfo.setLayouts = [set0Layout, set1Layout]}
      Nothing
      allocate
  (_, pipeline) <-
    Dynamic.allocatePipelineFromShaders
      dev
      zero
        { Dynamic.colorFormats = [colorFormat]
        , Dynamic.depthFormat = Just depthFormat
        , Dynamic.vertexInput = vertexInputState vertModule
        , Dynamic.layout = Just pipelineLayout
        }
      ()
      [(Vk.SHADER_STAGE_VERTEX_BIT, Shader.vertCode), (Vk.SHADER_STAGE_FRAGMENT_BIT, Shader.fragCode)]
  pure Pipeline{pipeline = pipeline, pipelineLayout = pipelineLayout}
