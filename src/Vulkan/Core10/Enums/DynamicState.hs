{-# language CPP #-}
-- No documentation found for Chapter "DynamicState"
module Vulkan.Core10.Enums.DynamicState  (DynamicState( DYNAMIC_STATE_VIEWPORT
                                                      , DYNAMIC_STATE_SCISSOR
                                                      , DYNAMIC_STATE_LINE_WIDTH
                                                      , DYNAMIC_STATE_DEPTH_BIAS
                                                      , DYNAMIC_STATE_BLEND_CONSTANTS
                                                      , DYNAMIC_STATE_DEPTH_BOUNDS
                                                      , DYNAMIC_STATE_STENCIL_COMPARE_MASK
                                                      , DYNAMIC_STATE_STENCIL_WRITE_MASK
                                                      , DYNAMIC_STATE_STENCIL_REFERENCE
                                                      , DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT
                                                      , DYNAMIC_STATE_LOGIC_OP_EXT
                                                      , DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT
                                                      , DYNAMIC_STATE_VERTEX_INPUT_EXT
                                                      , DYNAMIC_STATE_LINE_STIPPLE_EXT
                                                      , DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR
                                                      , DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
                                                      , DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
                                                      , DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
                                                      , DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR
                                                      , DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
                                                      , DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
                                                      , DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
                                                      , DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE
                                                      , DYNAMIC_STATE_DEPTH_BIAS_ENABLE
                                                      , DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE
                                                      , DYNAMIC_STATE_STENCIL_OP
                                                      , DYNAMIC_STATE_STENCIL_TEST_ENABLE
                                                      , DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE
                                                      , DYNAMIC_STATE_DEPTH_COMPARE_OP
                                                      , DYNAMIC_STATE_DEPTH_WRITE_ENABLE
                                                      , DYNAMIC_STATE_DEPTH_TEST_ENABLE
                                                      , DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE
                                                      , DYNAMIC_STATE_SCISSOR_WITH_COUNT
                                                      , DYNAMIC_STATE_VIEWPORT_WITH_COUNT
                                                      , DYNAMIC_STATE_PRIMITIVE_TOPOLOGY
                                                      , DYNAMIC_STATE_FRONT_FACE
                                                      , DYNAMIC_STATE_CULL_MODE
                                                      , ..
                                                      )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkDynamicState - Indicate which dynamic state is taken from dynamic
-- state commands
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'
newtype DynamicState = DynamicState Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DYNAMIC_STATE_VIEWPORT' specifies that the @pViewports@ state in
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo' will be ignored
-- and /must/ be set dynamically with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' before any drawing
-- commands. The number of viewports used by a pipeline is still specified
-- by the @viewportCount@ member of
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'.
pattern DYNAMIC_STATE_VIEWPORT                            = DynamicState 0
-- | 'DYNAMIC_STATE_SCISSOR' specifies that the @pScissors@ state in
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo' will be ignored
-- and /must/ be set dynamically with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetScissor' before any drawing
-- commands. The number of scissor rectangles used by a pipeline is still
-- specified by the @scissorCount@ member of
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'.
pattern DYNAMIC_STATE_SCISSOR                             = DynamicState 1
-- | 'DYNAMIC_STATE_LINE_WIDTH' specifies that the @lineWidth@ state in
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' before any drawing
-- commands that generate line primitives for the rasterizer.
pattern DYNAMIC_STATE_LINE_WIDTH                          = DynamicState 2
-- | 'DYNAMIC_STATE_DEPTH_BIAS' specifies that the @depthBiasConstantFactor@,
-- @depthBiasClamp@ and @depthBiasSlopeFactor@ states in
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBias' before any draws
-- are performed with @depthBiasEnable@ in
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE'.
pattern DYNAMIC_STATE_DEPTH_BIAS                          = DynamicState 3
-- | 'DYNAMIC_STATE_BLEND_CONSTANTS' specifies that the @blendConstants@
-- state in 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo' will
-- be ignored and /must/ be set dynamically with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetBlendConstants' before any
-- draws are performed with a pipeline state with
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState' member
-- @blendEnable@ set to 'Vulkan.Core10.FundamentalTypes.TRUE' and any of
-- the blend functions using a constant blend color.
pattern DYNAMIC_STATE_BLEND_CONSTANTS                     = DynamicState 4
-- | 'DYNAMIC_STATE_DEPTH_BOUNDS' specifies that the @minDepthBounds@ and
-- @maxDepthBounds@ states of
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBounds' before any draws
-- are performed with a pipeline state with
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' member
-- @depthBoundsTestEnable@ set to 'Vulkan.Core10.FundamentalTypes.TRUE'.
pattern DYNAMIC_STATE_DEPTH_BOUNDS                        = DynamicState 5
-- | 'DYNAMIC_STATE_STENCIL_COMPARE_MASK' specifies that the @compareMask@
-- state in 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- for both @front@ and @back@ will be ignored and /must/ be set
-- dynamically with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilCompareMask' before
-- any draws are performed with a pipeline state with
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' member
-- @stencilTestEnable@ set to 'Vulkan.Core10.FundamentalTypes.TRUE'
pattern DYNAMIC_STATE_STENCIL_COMPARE_MASK                = DynamicState 6
-- | 'DYNAMIC_STATE_STENCIL_WRITE_MASK' specifies that the @writeMask@ state
-- in 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' for both
-- @front@ and @back@ will be ignored and /must/ be set dynamically with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask' before any
-- draws are performed with a pipeline state with
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' member
-- @stencilTestEnable@ set to 'Vulkan.Core10.FundamentalTypes.TRUE'
pattern DYNAMIC_STATE_STENCIL_WRITE_MASK                  = DynamicState 7
-- | 'DYNAMIC_STATE_STENCIL_REFERENCE' specifies that the @reference@ state
-- in 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' for both
-- @front@ and @back@ will be ignored and /must/ be set dynamically with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilReference' before any
-- draws are performed with a pipeline state with
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' member
-- @stencilTestEnable@ set to 'Vulkan.Core10.FundamentalTypes.TRUE'
pattern DYNAMIC_STATE_STENCIL_REFERENCE                   = DynamicState 8
-- | 'DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT' specifies that the
-- @pColorWriteEnables@ state in
-- 'Vulkan.Extensions.VK_EXT_color_write_enable.PipelineColorWriteCreateInfoEXT'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
-- before any draw call.
pattern DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT              = DynamicState 1000381000
-- | 'DYNAMIC_STATE_LOGIC_OP_EXT' specifies that the @logicOp@ state in
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT'
-- before any drawing commands.
pattern DYNAMIC_STATE_LOGIC_OP_EXT                        = DynamicState 1000377003
-- | 'DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT' specifies that the
-- @patchControlPoints@ state in
-- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT'
-- before any drawing commands.
pattern DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT            = DynamicState 1000377000
-- | 'DYNAMIC_STATE_VERTEX_INPUT_EXT' specifies that the @pVertexInputState@
-- state will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
-- before any drawing commands
pattern DYNAMIC_STATE_VERTEX_INPUT_EXT                    = DynamicState 1000352000
-- | 'DYNAMIC_STATE_LINE_STIPPLE_EXT' specifies that the @lineStippleFactor@
-- and @lineStipplePattern@ state in
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.cmdSetLineStippleEXT'
-- before any draws are performed with a pipeline state with
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
-- member @stippledLineEnable@ set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE'.
pattern DYNAMIC_STATE_LINE_STIPPLE_EXT                    = DynamicState 1000259000
-- | 'DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR' specifies that state in
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'
-- and
-- 'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR'
-- or
-- 'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.cmdSetFragmentShadingRateEnumNV'
-- before any drawing commands.
pattern DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR           = DynamicState 1000226000
-- | 'DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV' specifies that the
-- @pExclusiveScissors@ state in
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV'
-- before any drawing commands. The number of exclusive scissor rectangles
-- used by a pipeline is still specified by the @exclusiveScissorCount@
-- member of
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'.
pattern DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV                = DynamicState 1000205001
-- | 'DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV' specifies that the
-- coarse sample order state in
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportCoarseSampleOrderStateCreateInfoNV'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetCoarseSampleOrderNV'
-- before any drawing commands.
pattern DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV     = DynamicState 1000164006
-- | 'DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV' specifies that the
-- @pShadingRatePalettes@ state in
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
-- before any drawing commands.
pattern DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV    = DynamicState 1000164004
-- | 'DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR' specifies that the
-- default stack size computation for the pipeline will be ignored and
-- /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdSetRayTracingPipelineStackSizeKHR'
-- before any ray tracing calls are performed.
pattern DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR = DynamicState 1000347000
-- | 'DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT' specifies that the
-- @sampleLocationsInfo@ state in
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
-- before any draw or clear commands. Enabling custom sample locations is
-- still indicated by the @sampleLocationsEnable@ member of
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'.
pattern DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT                = DynamicState 1000143000
-- | 'DYNAMIC_STATE_DISCARD_RECTANGLE_EXT' specifies that the
-- @pDiscardRectangles@ state in
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT'
-- before any draw or clear commands. The
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.DiscardRectangleModeEXT'
-- and the number of active discard rectangles is still specified by the
-- @discardRectangleMode@ and @discardRectangleCount@ members of
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'.
pattern DYNAMIC_STATE_DISCARD_RECTANGLE_EXT               = DynamicState 1000099000
-- | 'DYNAMIC_STATE_VIEWPORT_W_SCALING_NV' specifies that the
-- @pViewportWScalings@ state in
-- 'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
-- before any draws are performed with a pipeline state with
-- 'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
-- member @viewportScalingEnable@ set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE'
pattern DYNAMIC_STATE_VIEWPORT_W_SCALING_NV               = DynamicState 1000087000
-- | 'DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE' specifies that the
-- @primitiveRestartEnable@ state in
-- 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnable'
-- before any drawing commands.
pattern DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE            = DynamicState 1000377004
-- | 'DYNAMIC_STATE_DEPTH_BIAS_ENABLE' specifies that the @depthBiasEnable@
-- state in 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable'
-- before any drawing commands.
pattern DYNAMIC_STATE_DEPTH_BIAS_ENABLE                   = DynamicState 1000377002
-- | 'DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE' specifies that the
-- @rasterizerDiscardEnable@ state in
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
-- before any drawing commands.
pattern DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE           = DynamicState 1000377001
-- | 'DYNAMIC_STATE_STENCIL_OP' specifies that the @failOp@, @passOp@,
-- @depthFailOp@, and @compareOp@ states in
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' for both
-- @front@ and @back@ will be ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilOp'
-- before any draws are performed with a pipeline state with
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' member
-- @stencilTestEnable@ set to 'Vulkan.Core10.FundamentalTypes.TRUE'
pattern DYNAMIC_STATE_STENCIL_OP                          = DynamicState 1000267011
-- | 'DYNAMIC_STATE_STENCIL_TEST_ENABLE' specifies that the
-- @stencilTestEnable@ state in
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetStencilTestEnable'
-- before any draw call.
pattern DYNAMIC_STATE_STENCIL_TEST_ENABLE                 = DynamicState 1000267010
-- | 'DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE' specifies that the
-- @depthBoundsTestEnable@ state in
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnable'
-- before any draw call.
pattern DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE            = DynamicState 1000267009
-- | 'DYNAMIC_STATE_DEPTH_COMPARE_OP' specifies that the @depthCompareOp@
-- state in 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthCompareOp'
-- before any draw call.
pattern DYNAMIC_STATE_DEPTH_COMPARE_OP                    = DynamicState 1000267008
-- | 'DYNAMIC_STATE_DEPTH_WRITE_ENABLE' specifies that the @depthWriteEnable@
-- state in 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnable'
-- before any draw call.
pattern DYNAMIC_STATE_DEPTH_WRITE_ENABLE                  = DynamicState 1000267007
-- | 'DYNAMIC_STATE_DEPTH_TEST_ENABLE' specifies that the @depthTestEnable@
-- state in 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetDepthTestEnable'
-- before any draw call.
pattern DYNAMIC_STATE_DEPTH_TEST_ENABLE                   = DynamicState 1000267006
-- | 'DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE' specifies that the @stride@
-- state in 'Vulkan.Core10.Pipeline.VertexInputBindingDescription' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2'
-- before any draw call.
pattern DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE         = DynamicState 1000267005
-- | 'DYNAMIC_STATE_SCISSOR_WITH_COUNT' specifies that the @scissorCount@ and
-- @pScissors@ state in
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo' will be ignored
-- and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
-- before any draw call.
pattern DYNAMIC_STATE_SCISSOR_WITH_COUNT                  = DynamicState 1000267004
-- | 'DYNAMIC_STATE_VIEWPORT_WITH_COUNT' specifies that the @viewportCount@
-- and @pViewports@ state in
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo' will be ignored
-- and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
-- before any draw call.
pattern DYNAMIC_STATE_VIEWPORT_WITH_COUNT                 = DynamicState 1000267003
-- | 'DYNAMIC_STATE_PRIMITIVE_TOPOLOGY' specifies that the @topology@ state
-- in 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo' only
-- specifies the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>,
-- and the specific topology order and adjacency /must/ be set dynamically
-- with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
-- before any drawing commands.
pattern DYNAMIC_STATE_PRIMITIVE_TOPOLOGY                  = DynamicState 1000267002
-- | 'DYNAMIC_STATE_FRONT_FACE' specifies that the @frontFace@ state in
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetFrontFace'
-- before any drawing commands.
pattern DYNAMIC_STATE_FRONT_FACE                          = DynamicState 1000267001
-- | 'DYNAMIC_STATE_CULL_MODE' specifies that the @cullMode@ state in
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo' will be
-- ignored and /must/ be set dynamically with
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetCullMode'
-- before any drawing commands.
pattern DYNAMIC_STATE_CULL_MODE                           = DynamicState 1000267000
{-# complete DYNAMIC_STATE_VIEWPORT,
             DYNAMIC_STATE_SCISSOR,
             DYNAMIC_STATE_LINE_WIDTH,
             DYNAMIC_STATE_DEPTH_BIAS,
             DYNAMIC_STATE_BLEND_CONSTANTS,
             DYNAMIC_STATE_DEPTH_BOUNDS,
             DYNAMIC_STATE_STENCIL_COMPARE_MASK,
             DYNAMIC_STATE_STENCIL_WRITE_MASK,
             DYNAMIC_STATE_STENCIL_REFERENCE,
             DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT,
             DYNAMIC_STATE_LOGIC_OP_EXT,
             DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT,
             DYNAMIC_STATE_VERTEX_INPUT_EXT,
             DYNAMIC_STATE_LINE_STIPPLE_EXT,
             DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR,
             DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV,
             DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV,
             DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV,
             DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR,
             DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT,
             DYNAMIC_STATE_DISCARD_RECTANGLE_EXT,
             DYNAMIC_STATE_VIEWPORT_W_SCALING_NV,
             DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE,
             DYNAMIC_STATE_DEPTH_BIAS_ENABLE,
             DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE,
             DYNAMIC_STATE_STENCIL_OP,
             DYNAMIC_STATE_STENCIL_TEST_ENABLE,
             DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE,
             DYNAMIC_STATE_DEPTH_COMPARE_OP,
             DYNAMIC_STATE_DEPTH_WRITE_ENABLE,
             DYNAMIC_STATE_DEPTH_TEST_ENABLE,
             DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE,
             DYNAMIC_STATE_SCISSOR_WITH_COUNT,
             DYNAMIC_STATE_VIEWPORT_WITH_COUNT,
             DYNAMIC_STATE_PRIMITIVE_TOPOLOGY,
             DYNAMIC_STATE_FRONT_FACE,
             DYNAMIC_STATE_CULL_MODE :: DynamicState #-}

conNameDynamicState :: String
conNameDynamicState = "DynamicState"

enumPrefixDynamicState :: String
enumPrefixDynamicState = "DYNAMIC_STATE_"

showTableDynamicState :: [(DynamicState, String)]
showTableDynamicState =
  [ (DYNAMIC_STATE_VIEWPORT                           , "VIEWPORT")
  , (DYNAMIC_STATE_SCISSOR                            , "SCISSOR")
  , (DYNAMIC_STATE_LINE_WIDTH                         , "LINE_WIDTH")
  , (DYNAMIC_STATE_DEPTH_BIAS                         , "DEPTH_BIAS")
  , (DYNAMIC_STATE_BLEND_CONSTANTS                    , "BLEND_CONSTANTS")
  , (DYNAMIC_STATE_DEPTH_BOUNDS                       , "DEPTH_BOUNDS")
  , (DYNAMIC_STATE_STENCIL_COMPARE_MASK               , "STENCIL_COMPARE_MASK")
  , (DYNAMIC_STATE_STENCIL_WRITE_MASK                 , "STENCIL_WRITE_MASK")
  , (DYNAMIC_STATE_STENCIL_REFERENCE                  , "STENCIL_REFERENCE")
  , (DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT             , "COLOR_WRITE_ENABLE_EXT")
  , (DYNAMIC_STATE_LOGIC_OP_EXT                       , "LOGIC_OP_EXT")
  , (DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT           , "PATCH_CONTROL_POINTS_EXT")
  , (DYNAMIC_STATE_VERTEX_INPUT_EXT                   , "VERTEX_INPUT_EXT")
  , (DYNAMIC_STATE_LINE_STIPPLE_EXT                   , "LINE_STIPPLE_EXT")
  , (DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR          , "FRAGMENT_SHADING_RATE_KHR")
  , (DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV               , "EXCLUSIVE_SCISSOR_NV")
  , (DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV    , "VIEWPORT_COARSE_SAMPLE_ORDER_NV")
  , (DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV   , "VIEWPORT_SHADING_RATE_PALETTE_NV")
  , (DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR, "RAY_TRACING_PIPELINE_STACK_SIZE_KHR")
  , (DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT               , "SAMPLE_LOCATIONS_EXT")
  , (DYNAMIC_STATE_DISCARD_RECTANGLE_EXT              , "DISCARD_RECTANGLE_EXT")
  , (DYNAMIC_STATE_VIEWPORT_W_SCALING_NV              , "VIEWPORT_W_SCALING_NV")
  , (DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE           , "PRIMITIVE_RESTART_ENABLE")
  , (DYNAMIC_STATE_DEPTH_BIAS_ENABLE                  , "DEPTH_BIAS_ENABLE")
  , (DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE          , "RASTERIZER_DISCARD_ENABLE")
  , (DYNAMIC_STATE_STENCIL_OP                         , "STENCIL_OP")
  , (DYNAMIC_STATE_STENCIL_TEST_ENABLE                , "STENCIL_TEST_ENABLE")
  , (DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE           , "DEPTH_BOUNDS_TEST_ENABLE")
  , (DYNAMIC_STATE_DEPTH_COMPARE_OP                   , "DEPTH_COMPARE_OP")
  , (DYNAMIC_STATE_DEPTH_WRITE_ENABLE                 , "DEPTH_WRITE_ENABLE")
  , (DYNAMIC_STATE_DEPTH_TEST_ENABLE                  , "DEPTH_TEST_ENABLE")
  , (DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE        , "VERTEX_INPUT_BINDING_STRIDE")
  , (DYNAMIC_STATE_SCISSOR_WITH_COUNT                 , "SCISSOR_WITH_COUNT")
  , (DYNAMIC_STATE_VIEWPORT_WITH_COUNT                , "VIEWPORT_WITH_COUNT")
  , (DYNAMIC_STATE_PRIMITIVE_TOPOLOGY                 , "PRIMITIVE_TOPOLOGY")
  , (DYNAMIC_STATE_FRONT_FACE                         , "FRONT_FACE")
  , (DYNAMIC_STATE_CULL_MODE                          , "CULL_MODE")
  ]

instance Show DynamicState where
  showsPrec = enumShowsPrec enumPrefixDynamicState
                            showTableDynamicState
                            conNameDynamicState
                            (\(DynamicState x) -> x)
                            (showsPrec 11)

instance Read DynamicState where
  readPrec = enumReadPrec enumPrefixDynamicState showTableDynamicState conNameDynamicState DynamicState

