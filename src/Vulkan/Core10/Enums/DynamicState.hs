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
                                                      , DYNAMIC_STATE_STENCIL_OP_EXT
                                                      , DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT
                                                      , DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT
                                                      , DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT
                                                      , DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT
                                                      , DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT
                                                      , DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT
                                                      , DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT
                                                      , DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT
                                                      , DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT
                                                      , DYNAMIC_STATE_FRONT_FACE_EXT
                                                      , DYNAMIC_STATE_CULL_MODE_EXT
                                                      , DYNAMIC_STATE_LINE_STIPPLE_EXT
                                                      , DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR
                                                      , DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
                                                      , DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
                                                      , DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
                                                      , DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR
                                                      , DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
                                                      , DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
                                                      , DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
                                                      , ..
                                                      )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkDynamicState"
newtype DynamicState = DynamicState Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT"
pattern DYNAMIC_STATE_VIEWPORT                            = DynamicState 0
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_SCISSOR"
pattern DYNAMIC_STATE_SCISSOR                             = DynamicState 1
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_LINE_WIDTH"
pattern DYNAMIC_STATE_LINE_WIDTH                          = DynamicState 2
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DEPTH_BIAS"
pattern DYNAMIC_STATE_DEPTH_BIAS                          = DynamicState 3
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_BLEND_CONSTANTS"
pattern DYNAMIC_STATE_BLEND_CONSTANTS                     = DynamicState 4
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DEPTH_BOUNDS"
pattern DYNAMIC_STATE_DEPTH_BOUNDS                        = DynamicState 5
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK"
pattern DYNAMIC_STATE_STENCIL_COMPARE_MASK                = DynamicState 6
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_STENCIL_WRITE_MASK"
pattern DYNAMIC_STATE_STENCIL_WRITE_MASK                  = DynamicState 7
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_STENCIL_REFERENCE"
pattern DYNAMIC_STATE_STENCIL_REFERENCE                   = DynamicState 8
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_STENCIL_OP_EXT"
pattern DYNAMIC_STATE_STENCIL_OP_EXT                      = DynamicState 1000267011
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT"
pattern DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT             = DynamicState 1000267010
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT"
pattern DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT        = DynamicState 1000267009
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT"
pattern DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT                = DynamicState 1000267008
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT"
pattern DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT              = DynamicState 1000267007
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT"
pattern DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT               = DynamicState 1000267006
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT"
pattern DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT     = DynamicState 1000267005
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT"
pattern DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT              = DynamicState 1000267004
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT"
pattern DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT             = DynamicState 1000267003
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT"
pattern DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT              = DynamicState 1000267002
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_FRONT_FACE_EXT"
pattern DYNAMIC_STATE_FRONT_FACE_EXT                      = DynamicState 1000267001
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_CULL_MODE_EXT"
pattern DYNAMIC_STATE_CULL_MODE_EXT                       = DynamicState 1000267000
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_LINE_STIPPLE_EXT"
pattern DYNAMIC_STATE_LINE_STIPPLE_EXT                    = DynamicState 1000259000
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR"
pattern DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR           = DynamicState 1000226000
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV"
pattern DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV                = DynamicState 1000205001
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV"
pattern DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV     = DynamicState 1000164006
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV"
pattern DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV    = DynamicState 1000164004
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR"
pattern DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR = DynamicState 1000347000
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT"
pattern DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT                = DynamicState 1000143000
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT"
pattern DYNAMIC_STATE_DISCARD_RECTANGLE_EXT               = DynamicState 1000099000
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV"
pattern DYNAMIC_STATE_VIEWPORT_W_SCALING_NV               = DynamicState 1000087000
{-# complete DYNAMIC_STATE_VIEWPORT,
             DYNAMIC_STATE_SCISSOR,
             DYNAMIC_STATE_LINE_WIDTH,
             DYNAMIC_STATE_DEPTH_BIAS,
             DYNAMIC_STATE_BLEND_CONSTANTS,
             DYNAMIC_STATE_DEPTH_BOUNDS,
             DYNAMIC_STATE_STENCIL_COMPARE_MASK,
             DYNAMIC_STATE_STENCIL_WRITE_MASK,
             DYNAMIC_STATE_STENCIL_REFERENCE,
             DYNAMIC_STATE_STENCIL_OP_EXT,
             DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT,
             DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT,
             DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT,
             DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT,
             DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT,
             DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT,
             DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT,
             DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT,
             DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT,
             DYNAMIC_STATE_FRONT_FACE_EXT,
             DYNAMIC_STATE_CULL_MODE_EXT,
             DYNAMIC_STATE_LINE_STIPPLE_EXT,
             DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR,
             DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV,
             DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV,
             DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV,
             DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR,
             DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT,
             DYNAMIC_STATE_DISCARD_RECTANGLE_EXT,
             DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: DynamicState #-}

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
  , (DYNAMIC_STATE_STENCIL_OP_EXT                     , "STENCIL_OP_EXT")
  , (DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT            , "STENCIL_TEST_ENABLE_EXT")
  , (DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT       , "DEPTH_BOUNDS_TEST_ENABLE_EXT")
  , (DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT               , "DEPTH_COMPARE_OP_EXT")
  , (DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT             , "DEPTH_WRITE_ENABLE_EXT")
  , (DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT              , "DEPTH_TEST_ENABLE_EXT")
  , (DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT    , "VERTEX_INPUT_BINDING_STRIDE_EXT")
  , (DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT             , "SCISSOR_WITH_COUNT_EXT")
  , (DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT            , "VIEWPORT_WITH_COUNT_EXT")
  , (DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT             , "PRIMITIVE_TOPOLOGY_EXT")
  , (DYNAMIC_STATE_FRONT_FACE_EXT                     , "FRONT_FACE_EXT")
  , (DYNAMIC_STATE_CULL_MODE_EXT                      , "CULL_MODE_EXT")
  , (DYNAMIC_STATE_LINE_STIPPLE_EXT                   , "LINE_STIPPLE_EXT")
  , (DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR          , "FRAGMENT_SHADING_RATE_KHR")
  , (DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV               , "EXCLUSIVE_SCISSOR_NV")
  , (DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV    , "VIEWPORT_COARSE_SAMPLE_ORDER_NV")
  , (DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV   , "VIEWPORT_SHADING_RATE_PALETTE_NV")
  , (DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR, "RAY_TRACING_PIPELINE_STACK_SIZE_KHR")
  , (DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT               , "SAMPLE_LOCATIONS_EXT")
  , (DYNAMIC_STATE_DISCARD_RECTANGLE_EXT              , "DISCARD_RECTANGLE_EXT")
  , (DYNAMIC_STATE_VIEWPORT_W_SCALING_NV              , "VIEWPORT_W_SCALING_NV")
  ]


instance Show DynamicState where
showsPrec =
  enumShowsPrec enumPrefixDynamicState showTableDynamicState conNameDynamicState (\(DynamicState x) -> x) (showsPrec 11)


instance Read DynamicState where
  readPrec = enumReadPrec enumPrefixDynamicState showTableDynamicState conNameDynamicState DynamicState

