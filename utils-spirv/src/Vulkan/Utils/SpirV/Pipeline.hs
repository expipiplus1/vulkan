{-# LANGUAGE NoFieldSelectors #-}

{-| Assemble a dynamic-rendering graphics pipeline from reflected SPIR-V: one
verified pipeline layout, merged across a /family/ of shaders, shared by every
pipeline built against it — each folding in its own reflected vertex input.

Build the layout once from all the family's distinct modules
('allocateReflectedLayout'), so pipelines that differ only by specialization,
attachment formats or dynamic state share one 'Vk.PipelineLayout' and bind the
same descriptor sets. 'allocateGraphicsPipeline' then folds that layout and the
vertex stage's reflected vertex input into "Vulkan.Utils.DynamicRendering",
removing the by-hand wiring of merged set layouts + vertex input.
-}
module Vulkan.Utils.SpirV.Pipeline
  ( ReflectedLayout (..)
  , allocateReflectedLayout
  , allocateGraphicsPipeline
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Vector qualified as V
import Data.Word (Word32)
import Vulkan.Core10 qualified as Vk
import Vulkan.Zero (zero)

import Data.SpirV.Reflect.Module (Module)

import Vulkan.Utils.DynamicRendering qualified as Dynamic
import Vulkan.Utils.Pipeline.Specialization (Specialization)
import Vulkan.Utils.SpirV.Descriptors (mergedDescriptorSetLayoutInfos, mergedPushConstantRanges, moduleStageFlags)
import Vulkan.Utils.SpirV.VertexInput (vertexInputState)

{- | A materialized, verified pipeline layout reflected from a family of shaders:
the 'Vk.PipelineLayout' to bind against, and the per-set 'Vk.DescriptorSetLayout's
(keyed by set number) for allocating the descriptor sets.
-}
data ReflectedLayout = ReflectedLayout
  { pipelineLayout :: Vk.PipelineLayout
  , setLayouts :: [(Word32, Vk.DescriptorSetLayout)]
  }

{- | Build the descriptor-set layouts and pipeline layout for a family of pipelines
from their reflected modules. Bindings and push-constant ranges are merged across
every module — stage flags OR-ed, shared block layouts cross-checked (see
'mergedDescriptorSetLayoutInfos' / 'mergedPushConstantRanges') — and a conflict
'fail's in @m@.

Pass every distinct shader the family uses, so the one layout stays compatible
with each pipeline built against it ('allocateGraphicsPipeline'). The layout (and the
set layouts) are owned by @m@'s 'Control.Monad.Trans.Resource.ResourceT' and must
outlive the pipelines.
-}
allocateReflectedLayout
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> [Module]
  -> m (ReleaseKey, ReflectedLayout)
allocateReflectedLayout dev modules = do
  setInfos <- orFail (mergedDescriptorSetLayoutInfos modules)
  pushRanges <- orFail (mergedPushConstantRanges modules)
  setLayouts <-
    traverse
      ( \(setNo, info) -> do
          (_, layout) <- Vk.withDescriptorSetLayout dev info Nothing allocate
          pure (setNo, layout)
      )
      setInfos
  (key, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero
        { Vk.setLayouts = V.fromList (map snd setLayouts)
        , Vk.pushConstantRanges = V.fromList pushRanges
        }
      Nothing
      allocate
  pure (key, ReflectedLayout{pipelineLayout, setLayouts})
  where
    orFail = either fail pure

{- | Build one pipeline of a family against a shared 'ReflectedLayout', folding in
both that layout and the vertex stage's reflected vertex input.

Each stage is its reflected 'Module' paired with the SPIR-V to compile; the stage
flag is taken from the module. This fills in the config's 'Dynamic.layout' (from the
'ReflectedLayout') and 'Dynamic.vertexInput' (from the vertex stage's reflection),
overwriting any values set on them; set the per-variant 'Dynamic.colorFormats' \/
'Dynamic.depthFormat' \/ 'Dynamic.dynamicStates' and vary @spec@ for specialization.
For custom vertex input, drive "Vulkan.Utils.DynamicRendering" directly.
-}
allocateGraphicsPipeline
  :: (MonadResource m, MonadUnliftIO m, MonadFail m, Specialization spec)
  => Vk.Device
  -> ReflectedLayout
  -> Dynamic.PipelineConfig
  -> spec
  -- ^ Specialization shared by every stage; @()@ for none.
  -> [(Module, ByteString)]
  -- ^ Each stage's reflected module and the SPIR-V to compile.
  -> m (ReleaseKey, Vk.Pipeline)
allocateGraphicsPipeline dev reflected config spec stages =
  Dynamic.allocatePipelineFromShaders
    dev
    config
      { Dynamic.layout = Just reflected.pipelineLayout
      , Dynamic.vertexInput = maybe zero vertexInputState vertexModule
      }
    spec
    [(moduleStageFlags m, spv) | (m, spv) <- stages]
  where
    vertexModule = fst <$> find (\(m, _) -> moduleStageFlags m == Vk.SHADER_STAGE_VERTEX_BIT) stages
