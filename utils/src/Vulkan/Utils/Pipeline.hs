{-# LANGUAGE OverloadedLists #-}

{-| Vanilla vertex+fragment graphics pipeline useful for "draw a thing into
a color attachment" examples and prototypes.

Bakes dynamic viewport + scissor, so callers must emit @cmdSetViewport@ /
@cmdSetScissor@ at draw time. The pipeline layout is empty (no descriptor
sets, no push constants).
-}
module Vulkan.Utils.Pipeline
  ( createColorPipeline
  , createColorPipelineFromShaders
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate, release)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Shader (shaderStage)
import Vulkan.Zero (zero)

createColorPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> Vector (SomeStruct Vk.PipelineShaderStageCreateInfo)
  -> m (ReleaseKey, Vk.Pipeline)
createColorPipeline dev renderPass stages = do
  (layoutKey, pipelineLayout) <- Vk.withPipelineLayout dev zero Nothing allocate
  let
    pipelineCreateInfo :: Vk.GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { Vk.stages = stages
        , Vk.vertexInputState = Just zero
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
                  { Vk.viewportCount = 1
                  , Vk.scissorCount = 1
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
                  , Vk.frontFace = Vk.FRONT_FACE_CLOCKWISE
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
        , Vk.depthStencilState = Nothing
        , Vk.colorBlendState =
            Just $
              SomeStruct
                zero
                  { Vk.logicOpEnable = False
                  , Vk.attachments =
                      [ zero
                          { Vk.colorWriteMask =
                              Vk.COLOR_COMPONENT_R_BIT
                                .|. Vk.COLOR_COMPONENT_G_BIT
                                .|. Vk.COLOR_COMPONENT_B_BIT
                                .|. Vk.COLOR_COMPONENT_A_BIT
                          , Vk.blendEnable = False
                          }
                      ]
                  }
        , Vk.dynamicState =
            Just
              zero
                { Vk.dynamicStates =
                    [ Vk.DYNAMIC_STATE_VIEWPORT
                    , Vk.DYNAMIC_STATE_SCISSOR
                    ]
                }
        , Vk.layout = pipelineLayout
        , Vk.renderPass = renderPass
        , Vk.subpass = 0
        , Vk.basePipelineHandle = zero
        }
  (key, (_, [graphicsPipeline])) <-
    Vk.withGraphicsPipelines
      dev
      zero
      [SomeStruct pipelineCreateInfo]
      Nothing
      allocate
  release layoutKey
  pure (key, graphicsPipeline)

{- | Compile each @(stage, SPIR-V)@ pair into a shader module, build a vanilla
'createColorPipeline' with those stages, then release the now-redundant
shader-module handles. The returned 'ReleaseKey' frees the pipeline.
-}
createColorPipelineFromShaders
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> [(Vk.ShaderStageFlagBits, ByteString)]
  -> m (ReleaseKey, Vk.Pipeline)
createColorPipelineFromShaders dev renderPass shaders = do
  compiled <- traverse (uncurry (shaderStage dev)) shaders
  let (keys, stages) = unzip compiled
  (key, pipeline) <- createColorPipeline dev renderPass (V.fromList stages)
  traverse_ release keys
  pure (key, pipeline)
