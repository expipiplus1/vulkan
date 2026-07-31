{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The mesh pipeline family: one vertex shader, two pipelines.

Reflection of 'Shader.vertCode' \/ 'Shader.fragCode' provides the @Scene@ UBO
record, the @Vertex@ SSBO element record, and a stage signature for each
shader. The fragment stage's compatibility with the vertex stage — matching
@out@\/@in@ interface and shared use of the @Scene@ descriptor — is verified
__at compile time__ by 'MatchInterface' \/ 'CompatibleResources' (see
'pipelineComposes'); the merged pipeline layout (Scene visible to both stages,
the Mesh SSBO to the vertex stage) and each pipeline come from
'allocateReflectedLayout' \/ 'allocateGraphicsPipeline'.
-}
module Mesh
  ( Scene (..)
  , Vertex (..)
  , pipelineComposes
  , Pipelines (..)
  , allocatePipelines
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import qualified Geomancy
import Graphics.Gl.Block (Std140 (..), Std430 (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Zero (zero)

import Data.SpirV.Reflect.FFI (loadBytes)
import Vulkan.Utils.Pipeline (Pipeline)
import Vulkan.Utils.SpirV.Pipeline (allocateGraphicsPipeline, allocateReflectedLayout)
import Vulkan.Utils.SpirV.Stage (CompatibleResources, MatchInterface, reflectStageSigBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)

import qualified Mesh.Shader as Shader

-- Reflection at compile time: the @Scene@ UBO record and the @Vertex@ SSBO
-- element record, plus a stage signature for each shader.
reflectShaderTypesBytes Shader.vertCode
reflectStageSigBytes "VertSig" Shader.vertCode
reflectStageSigBytes "FragSig" Shader.fragCode

-- Compile-time proof that the fragment stage composes with the vertex stage:
-- matching interface and compatible shared resources. Evaluating it forces GHC to
-- discharge those constraints.
pipelineComposes :: (MatchInterface VertSig FragSig, CompatibleResources VertSig FragSig) => Bool
pipelineComposes = True

data Pipelines = Pipelines
  { depthOnly :: Pipeline
  , depthColor :: Pipeline
  }

{- | Two pipelines from the same vertex shader and the same reflected layout:
depth-only (no colour attachment) and depth+colour. Vertex input is empty —
geometry comes from the SSBO via @gl_VertexIndex@.
-}
allocatePipelines :: Vk.Device -> Vk.Format -> Vk.Format -> ResourceT IO Pipelines
allocatePipelines dev colorFormat depthFormat = do
  vertModule <- loadBytes Shader.vertCode
  fragModule <- loadBytes Shader.fragCode

  -- Descriptor set layouts + pipeline layout from reflection, merged across both
  -- stages: Scene visible to vertex AND fragment (stage-flag union), the Mesh SSBO
  -- to the vertex stage. One layout, shared by both pipelines.
  layout <- allocateReflectedLayout dev [vertModule, fragModule]

  depthOnly <-
    allocateGraphicsPipeline
      dev
      layout
      zero{Dynamic.depthFormat = Just depthFormat}
      ()
      [(vertModule, Shader.vertCode)]
  depthColor <-
    allocateGraphicsPipeline
      dev
      layout
      zero
        { Dynamic.colorFormats = [colorFormat]
        , Dynamic.depthFormat = Just depthFormat
        }
      ()
      [(vertModule, Shader.vertCode), (fragModule, Shader.fragCode)]

  pure Pipelines{depthOnly = depthOnly, depthColor = depthColor}
