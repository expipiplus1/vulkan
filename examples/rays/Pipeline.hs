{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Pipeline
  ( createPipeline
  , Pipeline.createRenderPass
  ) where

import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Foldable                  ( traverse_ )
import qualified Data.Vector                   as V
import           MonadVulkan
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Utils.ShaderQQ.Shaderc
import           Vulkan.Zero

-- Create the most vanilla rendering pipeline
createPipeline :: RenderPass -> V (ReleaseKey, Pipeline)
createPipeline renderPass = do
  (shaderKeys, shaderStages  ) <- V.unzip <$> createShaders
  (layoutKey , pipelineLayout) <- withPipelineLayout' zero
  let
    pipelineCreateInfo :: GraphicsPipelineCreateInfo '[]
    pipelineCreateInfo = zero
      { stages             = shaderStages
      , vertexInputState   = Just zero
      , inputAssemblyState = Just zero
                               { topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                               , primitiveRestartEnable = False
                               }
      , viewportState      = Just
        $ SomeStruct zero { viewportCount = 1, scissorCount = 1 }
      , rasterizationState = SomeStruct $ zero
                               { depthClampEnable        = False
                               , rasterizerDiscardEnable = False
                               , lineWidth               = 1
                               , polygonMode             = POLYGON_MODE_FILL
                               , cullMode                = CULL_MODE_NONE
                               , frontFace               = FRONT_FACE_CLOCKWISE
                               , depthBiasEnable         = False
                               }
      , multisampleState   = Just . SomeStruct $ zero
                               { sampleShadingEnable  = False
                               , rasterizationSamples = SAMPLE_COUNT_1_BIT
                               , minSampleShading     = 1
                               , sampleMask           = [maxBound]
                               }
      , depthStencilState  = Nothing
      , colorBlendState    = Just . SomeStruct $ zero
                               { logicOpEnable = False
                               , attachments   = [ zero
                                                     { colorWriteMask =
                                                       COLOR_COMPONENT_R_BIT
                                                       .|. COLOR_COMPONENT_G_BIT
                                                       .|. COLOR_COMPONENT_B_BIT
                                                       .|. COLOR_COMPONENT_A_BIT
                                                     , blendEnable    = False
                                                     }
                                                 ]
                               }
      , dynamicState       = Just zero
                               { dynamicStates = [ DYNAMIC_STATE_VIEWPORT
                                                 , DYNAMIC_STATE_SCISSOR
                                                 ]
                               }
      , layout             = pipelineLayout
      , renderPass         = renderPass
      , subpass            = 0
      , basePipelineHandle = zero
      }
  (key, (_, ~[graphicsPipeline])) <- withGraphicsPipelines'
    zero
    [SomeStruct pipelineCreateInfo]
  release layoutKey
  traverse_ release shaderKeys
  pure (key, graphicsPipeline)

-- | Create a renderpass with a single subpass
createRenderPass :: Format -> V (ReleaseKey, RenderPass)
createRenderPass imageFormat = do
  let
    attachmentDescription :: AttachmentDescription
    attachmentDescription = zero
      { format         = imageFormat
      , samples        = SAMPLE_COUNT_1_BIT
      , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
      , storeOp        = ATTACHMENT_STORE_OP_STORE
      , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
      , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
      , initialLayout  = IMAGE_LAYOUT_UNDEFINED
      , finalLayout    = IMAGE_LAYOUT_PRESENT_SRC_KHR
      }
    subpass :: SubpassDescription
    subpass = zero
      { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
      , colorAttachments  =
        [ zero { attachment = 0
               , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
               }
        ]
      }
    subpassDependency :: SubpassDependency
    subpassDependency = zero
      { srcSubpass    = SUBPASS_EXTERNAL
      , dstSubpass    = 0
      , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      , srcAccessMask = zero
      , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT
                          .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
      }
  withRenderPass' zero { attachments  = [attachmentDescription]
                       , subpasses    = [subpass]
                       , dependencies = [subpassDependency]
                       }

-- | Create a vertex and fragment shader which render a colored triangle
createShaders
  :: V (V.Vector (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo))
createShaders = do
  let fragCode = [frag|
        float4 main([[vk::location(0)]] const float3 col) : SV_TARGET
        {
            return float4(col, 1);
        }
      |]
      vertCode = [vert|
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
  (fragKey, fragModule) <- withShaderModule' zero { code = fragCode }
  (vertKey, vertModule) <- withShaderModule' zero { code = vertCode }
  let vertShaderStageCreateInfo = zero { stage   = SHADER_STAGE_VERTEX_BIT
                                       , module' = vertModule
                                       , name    = "main"
                                       }
      fragShaderStageCreateInfo = zero { stage   = SHADER_STAGE_FRAGMENT_BIT
                                       , module' = fragModule
                                       , name    = "main"
                                       }
  pure
    [ (vertKey, SomeStruct vertShaderStageCreateInfo)
    , (fragKey, SomeStruct fragShaderStageCreateInfo)
    ]
