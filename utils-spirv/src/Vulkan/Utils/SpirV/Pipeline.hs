{-| Build "Vulkan.Utils.Pipeline" bundles from reflected SPIR-V.

'allocateReflectedLayout' merges a /family/ of shaders into one verified
'Pipeline.Layout' — bindings and push ranges merged across every module, stage
flags OR-ed, shared block layouts cross-checked — shared by every pipeline built
against it. 'allocateGraphicsPipeline' then folds that layout and the vertex
stage's reflected vertex input into "Vulkan.Utils.DynamicRendering";
'allocateComputePipeline' is the single-stage compute sibling, and
'allocateCompute' collapses the whole family-of-one case into one call.

The bundles carry their set infos and push ranges, so descriptor sets and pushes
('Pipeline.allocateSet', 'Pipeline.push') track the shaders with nothing
hand-counted.
-}
module Vulkan.Utils.SpirV.Pipeline
  ( allocateReflectedLayout
  , allocateGraphicsPipeline
  , allocateComputePipeline
  , allocateCompute
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, allocate, release)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Vector qualified as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.Core10 qualified as Vk
import Vulkan.Zero (zero)

import Data.SpirV.Reflect.Module (Module)

import Vulkan.Utils.DynamicRendering qualified as Dynamic
import Vulkan.Utils.Pipeline (Layout, Pipeline (..))
import Vulkan.Utils.Pipeline qualified as Pipeline
import Vulkan.Utils.Pipeline.Specialization (Specialization)
import Vulkan.Utils.Shader (shaderModuleStage)
import Vulkan.Utils.SpirV.Descriptors (mergedDescriptorSetLayoutInfos, mergedPushConstantRanges, moduleStageFlags)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.Specialization (withSpecializationInfo)
import Vulkan.Utils.SpirV.VertexInput (vertexInputState)

{- | Build the 'Pipeline.Layout' for a family of pipelines from their reflected
modules. Bindings and push-constant ranges are merged across every module —
stage flags OR-ed, shared block layouts cross-checked (see
'mergedDescriptorSetLayoutInfos' / 'mergedPushConstantRanges') — and a conflict
'fail's in @m@.

Pass every distinct shader the family uses, so the one layout stays compatible
with each pipeline built against it. The layout is owned by @m@'s
'Control.Monad.Trans.Resource.ResourceT' and must outlive the pipelines.
-}
allocateReflectedLayout
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> [Module]
  -> m Layout
allocateReflectedLayout dev modules = do
  setInfos <- orFail (mergedDescriptorSetLayoutInfos modules)
  pushRanges <- orFail (mergedPushConstantRanges modules)
  sets <-
    traverse
      (\(setNo, info) -> (,) setNo <$> Pipeline.allocateSetLayout dev info)
      setInfos
  Pipeline.allocateLayout dev sets pushRanges
  where
    orFail = either fail pure

{- | Build one pipeline of a family against a shared 'Layout', folding in both
that layout and the vertex stage's reflected vertex input.

Each stage is its reflected 'Module' paired with the SPIR-V to compile; the stage
flag is taken from the module. This fills in the config's 'Dynamic.layout' and
'Dynamic.vertexInput' (from the vertex stage's reflection), overwriting any
values set on them; set the per-variant 'Dynamic.colorFormats' \/
'Dynamic.depthFormat' \/ 'Dynamic.dynamicStates' and vary @spec@ for
specialization. For custom vertex input, drive "Vulkan.Utils.DynamicRendering"
directly and bundle the 'Pipeline' by hand.
-}
allocateGraphicsPipeline
  :: (MonadResource m, MonadUnliftIO m, MonadFail m, Specialization spec)
  => Vk.Device
  -> Layout
  -> Dynamic.PipelineConfig
  -> spec
  -- ^ Specialization shared by every stage; @()@ for none.
  -> [(Module, ByteString)]
  -- ^ Each stage's reflected module and the SPIR-V to compile.
  -> m Pipeline
allocateGraphicsPipeline dev layout config spec stages = do
  (_, pipeline) <-
    Dynamic.allocatePipelineFromShaders
      dev
      config
        { Dynamic.layout = Just layout.pipelineLayout
        , Dynamic.vertexInput = maybe zero vertexInputState vertexModule
        }
      spec
      [(moduleStageFlags m, spv) | (m, spv) <- stages]
  pure Pipeline{pipeline, bindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS, layout}
  where
    vertexModule = fst <$> find (\(m, _) -> moduleStageFlags m == Vk.SHADER_STAGE_VERTEX_BIT) stages

{- | The compute sibling of 'allocateGraphicsPipeline': one compute stage built
against a shared 'Layout'.

Specialization constants are packed against the module's reflected
@constant_id@s (see "Vulkan.Utils.SpirV.Specialization"); pass @()@ for none.
-}
allocateComputePipeline
  :: (MonadResource m, MonadUnliftIO m, MonadFail m, Specialization spec)
  => Vk.Device
  -> Layout
  -> spec
  -> (Module, ByteString)
  -- ^ The compute stage's reflected module and the SPIR-V to compile.
  -> m Pipeline
allocateComputePipeline dev layout spec (m, spv) = do
  unless (moduleStageFlags m == Vk.SHADER_STAGE_COMPUTE_BIT) $
    fail "allocateComputePipeline: the module is not a compute shader"
  withSpecializationInfo m spec $ \specInfo -> do
    (stageKey, stage) <- shaderModuleStage dev Vk.SHADER_STAGE_COMPUTE_BIT specInfo spv
    let createInfo = zero{Vk.layout = layout.pipelineLayout, Vk.stage = stage} :: Vk.ComputePipelineCreateInfo '[]
    (_, (_, pipelines)) <- Vk.withComputePipelines dev zero (V.singleton (SomeStruct createInfo)) Nothing allocate
    release stageKey
    pure Pipeline{pipeline = V.head pipelines, bindPoint = Vk.PIPELINE_BIND_POINT_COMPUTE, layout}

{- | Reflect → layout → pipeline, in one call.

The family-of-one case: the shader's SPIR-V is reflected here and the layout is
its alone. For pipelines sharing a layout across shaders, compose
'allocateReflectedLayout' with 'allocateComputePipeline'.
-}
allocateCompute
  :: (MonadResource m, MonadUnliftIO m, MonadFail m, Specialization spec)
  => Vk.Device
  -> spec
  -> ByteString
  -- ^ The compute stage's SPIR-V.
  -> m Pipeline
allocateCompute dev spec code = do
  reflected <- reflectBytes code
  layout <- allocateReflectedLayout dev [reflected]
  allocateComputePipeline dev layout spec (reflected, code)
