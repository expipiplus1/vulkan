{-# LANGUAGE OverloadedLists #-}

{-| Shared graphics-pipeline construction behind the two rendering paths,
"Vulkan.Utils.RenderPass" and "Vulkan.Utils.DynamicRendering". Not meant for
direct use — import one of those modules instead.

The only difference between the paths is whether the pipeline references a
'Vk.RenderPass' or carries a @PipelineRenderingCreateInfo@ in its pNext chain,
so everything else (the vanilla rasterizer/blend/dynamic-state config, the
transient empty layout, the shader-module lifetime) lives here once.
-}
module Vulkan.Utils.Pipeline.Internal
  ( basePipelineCreateInfo
  , buildColorPipeline
  , withCompiledStages
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate, release)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Pipeline.Specialization (Specialization, withSpecialization)
import Vulkan.Utils.Shader (shaderModuleStage)
import Vulkan.Zero (zero)

{- | The shared body of the vanilla graphics pipeline: the given @dynamicStates@,
@colorAttachmentCount@ identical non-blended color attachments, an optional
depth-stencil state, and empty vertex input. The static values left in the
create-info (cull mode, topology, …) are the baked defaults for any state not
listed dynamic; states that are listed dynamic ignore them, so callers MUST emit
the matching @cmdSet*@ before drawing.

The colour and depth shape MUST match the attachments the pipeline renders to —
the render pass (render-pass path) or the @PipelineRenderingCreateInfo@ formats
(dynamic-rendering path):

  * @colorAttachmentCount == 0@ omits @colorBlendState@ entirely (a depth-only
    pipeline); otherwise one RGBA, non-blended attachment per colour target.
  * @depth@ adds a zeroed @depthStencilState@ — present (non-NULL) is required
    whenever a depth attachment is used; the actual test config is dynamic, so a
    zeroed struct is correct.

Pass @Just@ the target render pass (render-pass path), or @Nothing@ and attach
a @PipelineRenderingCreateInfo@ to the returned struct's pNext chain
(dynamic-rendering path).
-}
basePipelineCreateInfo
  :: Vk.PipelineLayout
  -> Maybe Vk.RenderPass
  -> Int
  -- ^ Colour attachment count (blend attachments); @0@ for depth-only.
  -> Bool
  -- ^ Whether a depth attachment is present.
  -> Vk.PipelineVertexInputStateCreateInfo '[]
  -- ^ Vertex input (bindings + attributes); @zero@ for none.
  -> Vector Vk.DynamicState
  -> Vector (SomeStruct Vk.PipelineShaderStageCreateInfo)
  -> Vk.GraphicsPipelineCreateInfo '[]
basePipelineCreateInfo pipelineLayout renderPass colorAttachmentCount depth vertexInput dynamicStates' stages =
  zero
    { Vk.stages = stages
    , Vk.vertexInputState = Just (SomeStruct vertexInput)
    , Vk.inputAssemblyState =
        Just
          zero
            { Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
            , Vk.primitiveRestartEnable = False
            }
    , Vk.viewportState =
        Just $
          SomeStruct
            zero
              { -- The counts MUST be zero when the matching @*_WITH_COUNT@ state
                -- is dynamic (set then via @cmdSetViewportWithCount@); otherwise
                -- the static count stands (plain @VIEWPORT@/@SCISSOR@ only swap
                -- the values). VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-03379/03380.
                Vk.viewportCount =
                  if Vk.DYNAMIC_STATE_VIEWPORT_WITH_COUNT `V.elem` dynamicStates' then 0 else 1
              , Vk.scissorCount =
                  if Vk.DYNAMIC_STATE_SCISSOR_WITH_COUNT `V.elem` dynamicStates' then 0 else 1
              }
    , Vk.rasterizationState =
        Just $
          SomeStruct
            zero
              { Vk.depthClampEnable = False
              , Vk.rasterizerDiscardEnable = False
              , Vk.lineWidth = 1
              , Vk.polygonMode = Vk.POLYGON_MODE_FILL
              , Vk.cullMode = Vk.CULL_MODE_NONE
              , Vk.frontFace = Vk.FRONT_FACE_COUNTER_CLOCKWISE
              , Vk.depthBiasEnable = False
              }
    , Vk.multisampleState =
        Just $
          SomeStruct
            zero
              { Vk.sampleShadingEnable = False
              , Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
              , Vk.minSampleShading = 1
              , Vk.sampleMask = [maxBound]
              }
    , Vk.depthStencilState =
        if depth then Just zero else Nothing
    , Vk.colorBlendState =
        if colorAttachmentCount == 0
          then Nothing
          else
            Just $
              SomeStruct
                zero
                  { Vk.logicOpEnable = False
                  , Vk.attachments = V.replicate colorAttachmentCount colorBlendAttachment
                  }
    , Vk.dynamicState = Just zero{Vk.dynamicStates = dynamicStates'}
    , Vk.layout = pipelineLayout
    , Vk.renderPass = fromMaybe Vk.NULL_HANDLE renderPass
    , Vk.subpass = 0
    , Vk.basePipelineHandle = zero
    }
  where
    colorBlendAttachment :: Vk.PipelineColorBlendAttachmentState
    colorBlendAttachment =
      zero
        { Vk.colorWriteMask =
            Vk.COLOR_COMPONENT_R_BIT
              .|. Vk.COLOR_COMPONENT_G_BIT
              .|. Vk.COLOR_COMPONENT_B_BIT
              .|. Vk.COLOR_COMPONENT_A_BIT
        , Vk.blendEnable = False
        }

{- | Build a single graphics pipeline from the given create-info builder. With
'Nothing', a transient empty pipeline layout is allocated and freed after the
build (the historical behaviour: no descriptor sets, no push constants); with
@Just layout@, the caller's layout is used and remains owned (and kept alive)
by the caller. The returned 'ReleaseKey' frees the pipeline.
-}
buildColorPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Maybe Vk.PipelineLayout
  -> (Vk.PipelineLayout -> SomeStruct Vk.GraphicsPipelineCreateInfo)
  -> m (ReleaseKey, Vk.Pipeline)
buildColorPipeline dev layout mkCreateInfo = case layout of
  Just pipelineLayout -> build pipelineLayout
  Nothing -> do
    (layoutKey, pipelineLayout) <- Vk.withPipelineLayout dev zero Nothing allocate
    built <- build pipelineLayout
    release layoutKey
    pure built
  where
    build pipelineLayout = do
      (key, (_, [pipeline])) <-
        Vk.withGraphicsPipelines dev zero [mkCreateInfo pipelineLayout] Nothing allocate
      pure (key, pipeline)

{- | Compile each @(stage, SPIR-V)@ pair into a shader module, run the
continuation with the resulting stages, then release the now-redundant
module handles. Shader modules are only needed during pipeline creation, so
the continuation typically returns the built pipeline.
-}
withCompiledStages
  :: (MonadResource m, MonadUnliftIO m, Specialization spec)
  => Vk.Device
  -> spec
  -> [(Vk.ShaderStageFlagBits, ByteString)]
  -> (Vector (SomeStruct Vk.PipelineShaderStageCreateInfo) -> m a)
  -> m a
withCompiledStages dev spec shaders k =
  withSpecialization spec $ \specializationInfo -> do
    compiled <-
      traverse
        (\(stage, code) -> shaderModuleStage dev stage specializationInfo code)
        shaders
    let (keys, stages) = unzip compiled
    result <- k (V.fromList stages)
    traverse_ release keys
    pure result
