{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pipeline
  ( createPipeline
  ) where

import Control.Monad.Trans.Resource
import Data.Bits
import Data.Foldable (traverse_)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.ShaderQQ.HLSL.Shaderc (frag, vert)
import Vulkan.Zero (zero)

-- | The most vanilla rendering pipeline; draws three vertices.
createPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> m (ReleaseKey, Vk.Pipeline)
createPipeline dev renderPass = do
  (shaderKeys, shaderStages) <- V.unzip <$> createShaders dev
  (layoutKey, pipelineLayout) <- Vk.withPipelineLayout dev zero Nothing allocate
  let
    pipelineCreateInfo :: Vk.GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { Vk.stages = shaderStages
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
  traverse_ release shaderKeys
  pure (key, graphicsPipeline)

createShaders
  :: (MonadResource m)
  => Vk.Device
  -> m (V.Vector (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo))
createShaders dev = do
  (fragKey, fragModule) <- Vk.withShaderModule dev zero{Vk.code = fragCode} Nothing allocate
  (vertKey, vertModule) <- Vk.withShaderModule dev zero{Vk.code = vertCode} Nothing allocate
  let
    vertShaderStageCreateInfo =
      zero
        { Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
        , Vk.module' = vertModule
        , Vk.name = "main"
        }
    fragShaderStageCreateInfo =
      zero
        { Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
        , Vk.module' = fragModule
        , Vk.name = "main"
        }
  pure
    [ (vertKey, SomeStruct vertShaderStageCreateInfo)
    , (fragKey, SomeStruct fragShaderStageCreateInfo)
    ]
  where
    vertCode =
      [vert|
        const static float2 positions[3] = {
          {0.0, -0.5},
          {0.5, 0.5},
          {-0.5, 0.5}
        };

        const static float3 colors[3] = {
          {1.0, 1.0, 0.0},
          {0.0, 1.0, 1.0},
          {1.0, 0.0, 1.0}
        };

        struct VSOutput
        {
          float4 pos : SV_POSITION;
          [[vk::location(0)]] float3 col;
        };

        VSOutput main(const uint i : SV_VertexID)
        {
          VSOutput output;
          output.pos = float4(positions[i], 0, 1.0);
          output.col = colors[i];
          return output;
        }
      |]
    fragCode =
      [frag|
        float4 main([[vk::location(0)]] const float3 col) : SV_TARGET
        {
            return float4(col, 1);
        }
      |]
