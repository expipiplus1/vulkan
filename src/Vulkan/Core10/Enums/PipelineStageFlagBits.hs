{-# language CPP #-}
-- No documentation found for Chapter "PipelineStageFlagBits"
module Vulkan.Core10.Enums.PipelineStageFlagBits  ( PipelineStageFlags
                                                  , PipelineStageFlagBits( PIPELINE_STAGE_TOP_OF_PIPE_BIT
                                                                         , PIPELINE_STAGE_DRAW_INDIRECT_BIT
                                                                         , PIPELINE_STAGE_VERTEX_INPUT_BIT
                                                                         , PIPELINE_STAGE_VERTEX_SHADER_BIT
                                                                         , PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
                                                                         , PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
                                                                         , PIPELINE_STAGE_GEOMETRY_SHADER_BIT
                                                                         , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
                                                                         , PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
                                                                         , PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
                                                                         , PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                         , PIPELINE_STAGE_COMPUTE_SHADER_BIT
                                                                         , PIPELINE_STAGE_TRANSFER_BIT
                                                                         , PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
                                                                         , PIPELINE_STAGE_HOST_BIT
                                                                         , PIPELINE_STAGE_ALL_GRAPHICS_BIT
                                                                         , PIPELINE_STAGE_ALL_COMMANDS_BIT
                                                                         , PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV
                                                                         , PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
                                                                         , PIPELINE_STAGE_MESH_SHADER_BIT_NV
                                                                         , PIPELINE_STAGE_TASK_SHADER_BIT_NV
                                                                         , PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
                                                                         , PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
                                                                         , PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR
                                                                         , PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
                                                                         , PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
                                                                         , ..
                                                                         )
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type PipelineStageFlags = PipelineStageFlagBits

-- No documentation found for TopLevel "VkPipelineStageFlagBits"
newtype PipelineStageFlagBits = PipelineStageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT"
pattern PIPELINE_STAGE_TOP_OF_PIPE_BIT                      = PipelineStageFlagBits 0x00000001
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT"
pattern PIPELINE_STAGE_DRAW_INDIRECT_BIT                    = PipelineStageFlagBits 0x00000002
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_VERTEX_INPUT_BIT"
pattern PIPELINE_STAGE_VERTEX_INPUT_BIT                     = PipelineStageFlagBits 0x00000004
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_VERTEX_SHADER_BIT"
pattern PIPELINE_STAGE_VERTEX_SHADER_BIT                    = PipelineStageFlagBits 0x00000008
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
pattern PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT      = PipelineStageFlagBits 0x00000010
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
pattern PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT   = PipelineStageFlagBits 0x00000020
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
pattern PIPELINE_STAGE_GEOMETRY_SHADER_BIT                  = PipelineStageFlagBits 0x00000040
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
pattern PIPELINE_STAGE_FRAGMENT_SHADER_BIT                  = PipelineStageFlagBits 0x00000080
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
pattern PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT             = PipelineStageFlagBits 0x00000100
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
pattern PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT              = PipelineStageFlagBits 0x00000200
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
pattern PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT          = PipelineStageFlagBits 0x00000400
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT"
pattern PIPELINE_STAGE_COMPUTE_SHADER_BIT                   = PipelineStageFlagBits 0x00000800
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TRANSFER_BIT"
pattern PIPELINE_STAGE_TRANSFER_BIT                         = PipelineStageFlagBits 0x00001000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
pattern PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT                   = PipelineStageFlagBits 0x00002000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_HOST_BIT"
pattern PIPELINE_STAGE_HOST_BIT                             = PipelineStageFlagBits 0x00004000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT"
pattern PIPELINE_STAGE_ALL_GRAPHICS_BIT                     = PipelineStageFlagBits 0x00008000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ALL_COMMANDS_BIT"
pattern PIPELINE_STAGE_ALL_COMMANDS_BIT                     = PipelineStageFlagBits 0x00010000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV"
pattern PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV            = PipelineStageFlagBits 0x00020000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT     = PipelineStageFlagBits 0x00800000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV"
pattern PIPELINE_STAGE_MESH_SHADER_BIT_NV                   = PipelineStageFlagBits 0x00100000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV"
pattern PIPELINE_STAGE_TASK_SHADER_BIT_NV                   = PipelineStageFlagBits 0x00080000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV"
pattern PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV            = PipelineStageFlagBits 0x00400000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR"
pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR           = PipelineStageFlagBits 0x00200000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR"
pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR = PipelineStageFlagBits 0x02000000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT        = PipelineStageFlagBits 0x00040000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT"
pattern PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT           = PipelineStageFlagBits 0x01000000

conNamePipelineStageFlagBits :: String
conNamePipelineStageFlagBits = "PipelineStageFlagBits"

enumPrefixPipelineStageFlagBits :: String
enumPrefixPipelineStageFlagBits = "PIPELINE_STAGE_"

showTablePipelineStageFlagBits :: [(PipelineStageFlagBits, String)]
showTablePipelineStageFlagBits =
  [ (PIPELINE_STAGE_TOP_OF_PIPE_BIT                     , "TOP_OF_PIPE_BIT")
  , (PIPELINE_STAGE_DRAW_INDIRECT_BIT                   , "DRAW_INDIRECT_BIT")
  , (PIPELINE_STAGE_VERTEX_INPUT_BIT                    , "VERTEX_INPUT_BIT")
  , (PIPELINE_STAGE_VERTEX_SHADER_BIT                   , "VERTEX_SHADER_BIT")
  , (PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT     , "TESSELLATION_CONTROL_SHADER_BIT")
  , (PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT  , "TESSELLATION_EVALUATION_SHADER_BIT")
  , (PIPELINE_STAGE_GEOMETRY_SHADER_BIT                 , "GEOMETRY_SHADER_BIT")
  , (PIPELINE_STAGE_FRAGMENT_SHADER_BIT                 , "FRAGMENT_SHADER_BIT")
  , (PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT            , "EARLY_FRAGMENT_TESTS_BIT")
  , (PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT             , "LATE_FRAGMENT_TESTS_BIT")
  , (PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT         , "COLOR_ATTACHMENT_OUTPUT_BIT")
  , (PIPELINE_STAGE_COMPUTE_SHADER_BIT                  , "COMPUTE_SHADER_BIT")
  , (PIPELINE_STAGE_TRANSFER_BIT                        , "TRANSFER_BIT")
  , (PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT                  , "BOTTOM_OF_PIPE_BIT")
  , (PIPELINE_STAGE_HOST_BIT                            , "HOST_BIT")
  , (PIPELINE_STAGE_ALL_GRAPHICS_BIT                    , "ALL_GRAPHICS_BIT")
  , (PIPELINE_STAGE_ALL_COMMANDS_BIT                    , "ALL_COMMANDS_BIT")
  , (PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV           , "COMMAND_PREPROCESS_BIT_NV")
  , (PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT    , "FRAGMENT_DENSITY_PROCESS_BIT_EXT")
  , (PIPELINE_STAGE_MESH_SHADER_BIT_NV                  , "MESH_SHADER_BIT_NV")
  , (PIPELINE_STAGE_TASK_SHADER_BIT_NV                  , "TASK_SHADER_BIT_NV")
  , (PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV           , "SHADING_RATE_IMAGE_BIT_NV")
  , (PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR          , "RAY_TRACING_SHADER_BIT_KHR")
  , (PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR, "ACCELERATION_STRUCTURE_BUILD_BIT_KHR")
  , (PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT       , "CONDITIONAL_RENDERING_BIT_EXT")
  , (PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT          , "TRANSFORM_FEEDBACK_BIT_EXT")
  ]


instance Show PipelineStageFlagBits where
showsPrec = enumShowsPrec enumPrefixPipelineStageFlagBits
                          showTablePipelineStageFlagBits
                          conNamePipelineStageFlagBits
                          (\(PipelineStageFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PipelineStageFlagBits where
  readPrec = enumReadPrec enumPrefixPipelineStageFlagBits
                          showTablePipelineStageFlagBits
                          conNamePipelineStageFlagBits
                          PipelineStageFlagBits

