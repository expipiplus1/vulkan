{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.DynamicState  (DynamicState( DYNAMIC_STATE_VIEWPORT
                                                               , DYNAMIC_STATE_SCISSOR
                                                               , DYNAMIC_STATE_LINE_WIDTH
                                                               , DYNAMIC_STATE_DEPTH_BIAS
                                                               , DYNAMIC_STATE_BLEND_CONSTANTS
                                                               , DYNAMIC_STATE_DEPTH_BOUNDS
                                                               , DYNAMIC_STATE_STENCIL_COMPARE_MASK
                                                               , DYNAMIC_STATE_STENCIL_WRITE_MASK
                                                               , DYNAMIC_STATE_STENCIL_REFERENCE
                                                               , DYNAMIC_STATE_LINE_STIPPLE_EXT
                                                               , DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
                                                               , DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
                                                               , DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
                                                               , DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
                                                               , DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
                                                               , DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
                                                               , ..
                                                               )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Zero (Zero)
-- | VkDynamicState - Indicate which dynamic state is taken from dynamic
-- state commands
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'
newtype DynamicState = DynamicState Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DYNAMIC_STATE_VIEWPORT' specifies that the @pViewports@ state in
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo' will
-- be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetViewport' before any
-- draw commands. The number of viewports used by a pipeline is still
-- specified by the @viewportCount@ member of
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'.
pattern DYNAMIC_STATE_VIEWPORT = DynamicState 0
-- | 'DYNAMIC_STATE_SCISSOR' specifies that the @pScissors@ state in
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo' will
-- be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetScissor' before any
-- draw commands. The number of scissor rectangles used by a pipeline is
-- still specified by the @scissorCount@ member of
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'.
pattern DYNAMIC_STATE_SCISSOR = DynamicState 1
-- | 'DYNAMIC_STATE_LINE_WIDTH' specifies that the @lineWidth@ state in
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth' before
-- any draw commands that generate line primitives for the rasterizer.
pattern DYNAMIC_STATE_LINE_WIDTH = DynamicState 2
-- | 'DYNAMIC_STATE_DEPTH_BIAS' specifies that the @depthBiasConstantFactor@,
-- @depthBiasClamp@ and @depthBiasSlopeFactor@ states in
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBias' before
-- any draws are performed with @depthBiasEnable@ in
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
-- set to 'Graphics.Vulkan.Core10.BaseType.TRUE'.
pattern DYNAMIC_STATE_DEPTH_BIAS = DynamicState 3
-- | 'DYNAMIC_STATE_BLEND_CONSTANTS' specifies that the @blendConstants@
-- state in
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo' will
-- be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetBlendConstants'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'
-- member @blendEnable@ set to 'Graphics.Vulkan.Core10.BaseType.TRUE' and
-- any of the blend functions using a constant blend color.
pattern DYNAMIC_STATE_BLEND_CONSTANTS = DynamicState 4
-- | 'DYNAMIC_STATE_DEPTH_BOUNDS' specifies that the @minDepthBounds@ and
-- @maxDepthBounds@ states of
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBounds' before
-- any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- member @depthBoundsTestEnable@ set to
-- 'Graphics.Vulkan.Core10.BaseType.TRUE'.
pattern DYNAMIC_STATE_DEPTH_BOUNDS = DynamicState 5
-- | 'DYNAMIC_STATE_STENCIL_COMPARE_MASK' specifies that the @compareMask@
-- state in
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- for both @front@ and @back@ will be ignored and /must/ be set
-- dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetStencilCompareMask'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- member @stencilTestEnable@ set to 'Graphics.Vulkan.Core10.BaseType.TRUE'
pattern DYNAMIC_STATE_STENCIL_COMPARE_MASK = DynamicState 6
-- | 'DYNAMIC_STATE_STENCIL_WRITE_MASK' specifies that the @writeMask@ state
-- in 'Graphics.Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- for both @front@ and @back@ will be ignored and /must/ be set
-- dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- member @stencilTestEnable@ set to 'Graphics.Vulkan.Core10.BaseType.TRUE'
pattern DYNAMIC_STATE_STENCIL_WRITE_MASK = DynamicState 7
-- | 'DYNAMIC_STATE_STENCIL_REFERENCE' specifies that the @reference@ state
-- in 'Graphics.Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- for both @front@ and @back@ will be ignored and /must/ be set
-- dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetStencilReference'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- member @stencilTestEnable@ set to 'Graphics.Vulkan.Core10.BaseType.TRUE'
pattern DYNAMIC_STATE_STENCIL_REFERENCE = DynamicState 8
-- | 'DYNAMIC_STATE_LINE_STIPPLE_EXT' specifies that the @lineStippleFactor@
-- and @lineStipplePattern@ state in
-- 'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.cmdSetLineStippleEXT'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
-- member @stippledLineEnable@ set to
-- 'Graphics.Vulkan.Core10.BaseType.TRUE'.
pattern DYNAMIC_STATE_LINE_STIPPLE_EXT = DynamicState 1000259000
-- | 'DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV' specifies that the
-- @pExclusiveScissors@ state in
-- 'Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV'
-- before any draw commands. The number of exclusive scissor rectangles
-- used by a pipeline is still specified by the @exclusiveScissorCount@
-- member of
-- 'Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'.
pattern DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV = DynamicState 1000205001
-- | 'DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV' specifies that the
-- coarse sample order state in
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportCoarseSampleOrderStateCreateInfoNV'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetCoarseSampleOrderNV'
-- before any draw commands.
pattern DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV = DynamicState 1000164006
-- | 'DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV' specifies that the
-- @pShadingRatePalettes@ state in
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
-- before any draw commands.
pattern DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV = DynamicState 1000164004
-- | 'DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT' specifies that the
-- @sampleLocationsInfo@ state in
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
-- before any draw or clear commands. Enabling custom sample locations is
-- still indicated by the @sampleLocationsEnable@ member of
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'.
pattern DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT = DynamicState 1000143000
-- | 'DYNAMIC_STATE_DISCARD_RECTANGLE_EXT' specifies that the
-- @pDiscardRectangles@ state in
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT'
-- before any draw or clear commands. The
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.DiscardRectangleModeEXT'
-- and the number of active discard rectangles is still specified by the
-- @discardRectangleMode@ and @discardRectangleCount@ members of
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'.
pattern DYNAMIC_STATE_DISCARD_RECTANGLE_EXT = DynamicState 1000099000
-- | 'DYNAMIC_STATE_VIEWPORT_W_SCALING_NV' specifies that the
-- @pViewportScalings@ state in
-- 'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
-- member @viewportScalingEnable@ set to
-- 'Graphics.Vulkan.Core10.BaseType.TRUE'
pattern DYNAMIC_STATE_VIEWPORT_W_SCALING_NV = DynamicState 1000087000
{-# complete DYNAMIC_STATE_VIEWPORT,
             DYNAMIC_STATE_SCISSOR,
             DYNAMIC_STATE_LINE_WIDTH,
             DYNAMIC_STATE_DEPTH_BIAS,
             DYNAMIC_STATE_BLEND_CONSTANTS,
             DYNAMIC_STATE_DEPTH_BOUNDS,
             DYNAMIC_STATE_STENCIL_COMPARE_MASK,
             DYNAMIC_STATE_STENCIL_WRITE_MASK,
             DYNAMIC_STATE_STENCIL_REFERENCE,
             DYNAMIC_STATE_LINE_STIPPLE_EXT,
             DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV,
             DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV,
             DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV,
             DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT,
             DYNAMIC_STATE_DISCARD_RECTANGLE_EXT,
             DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: DynamicState #-}

instance Show DynamicState where
  showsPrec p = \case
    DYNAMIC_STATE_VIEWPORT -> showString "DYNAMIC_STATE_VIEWPORT"
    DYNAMIC_STATE_SCISSOR -> showString "DYNAMIC_STATE_SCISSOR"
    DYNAMIC_STATE_LINE_WIDTH -> showString "DYNAMIC_STATE_LINE_WIDTH"
    DYNAMIC_STATE_DEPTH_BIAS -> showString "DYNAMIC_STATE_DEPTH_BIAS"
    DYNAMIC_STATE_BLEND_CONSTANTS -> showString "DYNAMIC_STATE_BLEND_CONSTANTS"
    DYNAMIC_STATE_DEPTH_BOUNDS -> showString "DYNAMIC_STATE_DEPTH_BOUNDS"
    DYNAMIC_STATE_STENCIL_COMPARE_MASK -> showString "DYNAMIC_STATE_STENCIL_COMPARE_MASK"
    DYNAMIC_STATE_STENCIL_WRITE_MASK -> showString "DYNAMIC_STATE_STENCIL_WRITE_MASK"
    DYNAMIC_STATE_STENCIL_REFERENCE -> showString "DYNAMIC_STATE_STENCIL_REFERENCE"
    DYNAMIC_STATE_LINE_STIPPLE_EXT -> showString "DYNAMIC_STATE_LINE_STIPPLE_EXT"
    DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV -> showString "DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV"
    DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV -> showString "DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV"
    DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV -> showString "DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV"
    DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT -> showString "DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT"
    DYNAMIC_STATE_DISCARD_RECTANGLE_EXT -> showString "DYNAMIC_STATE_DISCARD_RECTANGLE_EXT"
    DYNAMIC_STATE_VIEWPORT_W_SCALING_NV -> showString "DYNAMIC_STATE_VIEWPORT_W_SCALING_NV"
    DynamicState x -> showParen (p >= 11) (showString "DynamicState " . showsPrec 11 x)

instance Read DynamicState where
  readPrec = parens (choose [("DYNAMIC_STATE_VIEWPORT", pure DYNAMIC_STATE_VIEWPORT)
                            , ("DYNAMIC_STATE_SCISSOR", pure DYNAMIC_STATE_SCISSOR)
                            , ("DYNAMIC_STATE_LINE_WIDTH", pure DYNAMIC_STATE_LINE_WIDTH)
                            , ("DYNAMIC_STATE_DEPTH_BIAS", pure DYNAMIC_STATE_DEPTH_BIAS)
                            , ("DYNAMIC_STATE_BLEND_CONSTANTS", pure DYNAMIC_STATE_BLEND_CONSTANTS)
                            , ("DYNAMIC_STATE_DEPTH_BOUNDS", pure DYNAMIC_STATE_DEPTH_BOUNDS)
                            , ("DYNAMIC_STATE_STENCIL_COMPARE_MASK", pure DYNAMIC_STATE_STENCIL_COMPARE_MASK)
                            , ("DYNAMIC_STATE_STENCIL_WRITE_MASK", pure DYNAMIC_STATE_STENCIL_WRITE_MASK)
                            , ("DYNAMIC_STATE_STENCIL_REFERENCE", pure DYNAMIC_STATE_STENCIL_REFERENCE)
                            , ("DYNAMIC_STATE_LINE_STIPPLE_EXT", pure DYNAMIC_STATE_LINE_STIPPLE_EXT)
                            , ("DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV", pure DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV)
                            , ("DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV", pure DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV)
                            , ("DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV", pure DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV)
                            , ("DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT", pure DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT)
                            , ("DYNAMIC_STATE_DISCARD_RECTANGLE_EXT", pure DYNAMIC_STATE_DISCARD_RECTANGLE_EXT)
                            , ("DYNAMIC_STATE_VIEWPORT_W_SCALING_NV", pure DYNAMIC_STATE_VIEWPORT_W_SCALING_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "DynamicState")
                       v <- step readPrec
                       pure (DynamicState v)))

