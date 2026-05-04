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
import Vulkan.CStruct.Extends
import Vulkan.Core10 as Vk hiding
  ( withBuffer
  , withImage
  )
import Vulkan.Utils.ShaderQQ.HLSL.Shaderc
  ( frag
  , vert
  )
import Vulkan.Zero

-- | The most vanilla rendering pipeline; draws three vertices.
createPipeline
  :: (MonadResource m, MonadFail m)
  => Device
  -> RenderPass
  -> m (ReleaseKey, Pipeline)
createPipeline dev renderPass = do
  (shaderKeys, shaderStages) <- V.unzip <$> createShaders dev
  (layoutKey, pipelineLayout) <- withPipelineLayout dev zero Nothing allocate
  let
    pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo =
      zero
        { stages = shaderStages
        , vertexInputState = Just zero
        , inputAssemblyState =
            Just
              zero
                { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                , primitiveRestartEnable = False
                }
        , viewportState =
            Just $
              SomeStruct zero{viewportCount = 1, scissorCount = 1}
        , rasterizationState =
            Just . SomeStruct $
              zero
                { depthClampEnable = False
                , rasterizerDiscardEnable = False
                , lineWidth = 1
                , polygonMode = POLYGON_MODE_FILL
                , cullMode = CULL_MODE_NONE
                , frontFace = FRONT_FACE_CLOCKWISE
                , depthBiasEnable = False
                }
        , multisampleState =
            Just . SomeStruct $
              zero
                { sampleShadingEnable = False
                , rasterizationSamples = SAMPLE_COUNT_1_BIT
                , minSampleShading = 1
                , sampleMask = [maxBound]
                }
        , depthStencilState = Nothing
        , colorBlendState =
            Just . SomeStruct $
              zero
                { logicOpEnable = False
                , attachments =
                    [ zero
                        { colorWriteMask =
                            COLOR_COMPONENT_R_BIT
                              .|. COLOR_COMPONENT_G_BIT
                              .|. COLOR_COMPONENT_B_BIT
                              .|. COLOR_COMPONENT_A_BIT
                        , blendEnable = False
                        }
                    ]
                }
        , dynamicState =
            Just
              zero
                { dynamicStates =
                    [ DYNAMIC_STATE_VIEWPORT
                    , DYNAMIC_STATE_SCISSOR
                    ]
                }
        , layout = pipelineLayout
        , renderPass = renderPass
        , subpass = 0
        , basePipelineHandle = zero
        }
  (key, (_, ~[graphicsPipeline])) <-
    withGraphicsPipelines
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
  => Device
  -> m (V.Vector (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo))
createShaders dev = do
  let
    fragCode =
      [frag|
        float4 main([[vk::location(0)]] const float3 col) : SV_TARGET
        {
            return float4(col, 1);
        }
      |]
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
  (fragKey, fragModule) <- withShaderModule dev zero{code = fragCode} Nothing allocate
  (vertKey, vertModule) <- withShaderModule dev zero{code = vertCode} Nothing allocate
  let
    vertShaderStageCreateInfo =
      zero
        { stage = SHADER_STAGE_VERTEX_BIT
        , module' = vertModule
        , name = "main"
        }
    fragShaderStageCreateInfo =
      zero
        { stage = SHADER_STAGE_FRAGMENT_BIT
        , module' = fragModule
        , name = "main"
        }
  pure
    [ (vertKey, SomeStruct vertShaderStageCreateInfo)
    , (fragKey, SomeStruct fragShaderStageCreateInfo)
    ]
