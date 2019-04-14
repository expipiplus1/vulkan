{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Pipeline
  ( VkBlendFactor(..)
  , pattern VK_BLEND_FACTOR_ZERO
  , pattern VK_BLEND_FACTOR_ONE
  , pattern VK_BLEND_FACTOR_SRC_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR
  , pattern VK_BLEND_FACTOR_DST_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR
  , pattern VK_BLEND_FACTOR_SRC_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
  , pattern VK_BLEND_FACTOR_DST_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA
  , pattern VK_BLEND_FACTOR_CONSTANT_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR
  , pattern VK_BLEND_FACTOR_CONSTANT_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA
  , pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE
  , pattern VK_BLEND_FACTOR_SRC1_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR
  , pattern VK_BLEND_FACTOR_SRC1_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
  , VkBlendOp(..)
  , pattern VK_BLEND_OP_ADD
  , pattern VK_BLEND_OP_SUBTRACT
  , pattern VK_BLEND_OP_REVERSE_SUBTRACT
  , pattern VK_BLEND_OP_MIN
  , pattern VK_BLEND_OP_MAX
  , VkColorComponentFlagBits(..)
  , pattern VK_COLOR_COMPONENT_R_BIT
  , pattern VK_COLOR_COMPONENT_G_BIT
  , pattern VK_COLOR_COMPONENT_B_BIT
  , pattern VK_COLOR_COMPONENT_A_BIT
  , VkColorComponentFlags
  , VkCompareOp(..)
  , pattern VK_COMPARE_OP_NEVER
  , pattern VK_COMPARE_OP_LESS
  , pattern VK_COMPARE_OP_EQUAL
  , pattern VK_COMPARE_OP_LESS_OR_EQUAL
  , pattern VK_COMPARE_OP_GREATER
  , pattern VK_COMPARE_OP_NOT_EQUAL
  , pattern VK_COMPARE_OP_GREATER_OR_EQUAL
  , pattern VK_COMPARE_OP_ALWAYS
  , VkComputePipelineCreateInfo(..)
  , VkCullModeFlagBits(..)
  , pattern VK_CULL_MODE_FRONT_BIT
  , pattern VK_CULL_MODE_BACK_BIT
  , pattern VK_CULL_MODE_NONE
  , pattern VK_CULL_MODE_FRONT_AND_BACK
  , VkCullModeFlags
  , VkDynamicState(..)
  , pattern VK_DYNAMIC_STATE_VIEWPORT
  , pattern VK_DYNAMIC_STATE_SCISSOR
  , pattern VK_DYNAMIC_STATE_LINE_WIDTH
  , pattern VK_DYNAMIC_STATE_DEPTH_BIAS
  , pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS
  , pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS
  , pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK
  , pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK
  , pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE
  , VkExtent2D(..)
  , VkFrontFace(..)
  , pattern VK_FRONT_FACE_COUNTER_CLOCKWISE
  , pattern VK_FRONT_FACE_CLOCKWISE
  , VkGraphicsPipelineCreateInfo(..)
  , VkLogicOp(..)
  , pattern VK_LOGIC_OP_CLEAR
  , pattern VK_LOGIC_OP_AND
  , pattern VK_LOGIC_OP_AND_REVERSE
  , pattern VK_LOGIC_OP_COPY
  , pattern VK_LOGIC_OP_AND_INVERTED
  , pattern VK_LOGIC_OP_NO_OP
  , pattern VK_LOGIC_OP_XOR
  , pattern VK_LOGIC_OP_OR
  , pattern VK_LOGIC_OP_NOR
  , pattern VK_LOGIC_OP_EQUIVALENT
  , pattern VK_LOGIC_OP_INVERT
  , pattern VK_LOGIC_OP_OR_REVERSE
  , pattern VK_LOGIC_OP_COPY_INVERTED
  , pattern VK_LOGIC_OP_OR_INVERTED
  , pattern VK_LOGIC_OP_NAND
  , pattern VK_LOGIC_OP_SET
  , VkOffset2D(..)
  , VkPipeline
  , VkPipelineColorBlendAttachmentState(..)
  , VkPipelineColorBlendStateCreateFlags(..)
  , VkPipelineColorBlendStateCreateInfo(..)
  , VkPipelineCreateFlagBits(..)
  , pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
  , pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
  , pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT
  , VkPipelineCreateFlags
  , VkPipelineDepthStencilStateCreateFlags(..)
  , VkPipelineDepthStencilStateCreateInfo(..)
  , VkPipelineDynamicStateCreateFlags(..)
  , VkPipelineDynamicStateCreateInfo(..)
  , VkPipelineInputAssemblyStateCreateFlags(..)
  , VkPipelineInputAssemblyStateCreateInfo(..)
  , VkPipelineLayout
  , VkPipelineMultisampleStateCreateFlags(..)
  , VkPipelineMultisampleStateCreateInfo(..)
  , VkPipelineRasterizationStateCreateFlags(..)
  , VkPipelineRasterizationStateCreateInfo(..)
  , VkPipelineShaderStageCreateFlags(..)
  , VkPipelineShaderStageCreateInfo(..)
  , VkPipelineTessellationStateCreateFlags(..)
  , VkPipelineTessellationStateCreateInfo(..)
  , VkPipelineVertexInputStateCreateFlags(..)
  , VkPipelineVertexInputStateCreateInfo(..)
  , VkPipelineViewportStateCreateFlags(..)
  , VkPipelineViewportStateCreateInfo(..)
  , VkPolygonMode(..)
  , pattern VK_POLYGON_MODE_FILL
  , pattern VK_POLYGON_MODE_LINE
  , pattern VK_POLYGON_MODE_POINT
  , VkPrimitiveTopology(..)
  , pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
  , VkRect2D(..)
  , VkRenderPass
  , VkSampleMask
  , VkShaderStageFlagBits(..)
  , pattern VK_SHADER_STAGE_VERTEX_BIT
  , pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
  , pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
  , pattern VK_SHADER_STAGE_GEOMETRY_BIT
  , pattern VK_SHADER_STAGE_FRAGMENT_BIT
  , pattern VK_SHADER_STAGE_COMPUTE_BIT
  , pattern VK_SHADER_STAGE_ALL_GRAPHICS
  , pattern VK_SHADER_STAGE_ALL
  , VkSpecializationInfo(..)
  , VkSpecializationMapEntry(..)
  , VkStencilOp(..)
  , pattern VK_STENCIL_OP_KEEP
  , pattern VK_STENCIL_OP_ZERO
  , pattern VK_STENCIL_OP_REPLACE
  , pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP
  , pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP
  , pattern VK_STENCIL_OP_INVERT
  , pattern VK_STENCIL_OP_INCREMENT_AND_WRAP
  , pattern VK_STENCIL_OP_DECREMENT_AND_WRAP
  , VkStencilOpState(..)
  , VkVertexInputAttributeDescription(..)
  , VkVertexInputBindingDescription(..)
  , VkVertexInputRate(..)
  , pattern VK_VERTEX_INPUT_RATE_VERTEX
  , pattern VK_VERTEX_INPUT_RATE_INSTANCE
  , VkViewport(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateComputePipelines
#endif
  , FN_vkCreateComputePipelines
  , PFN_vkCreateComputePipelines
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateGraphicsPipelines
#endif
  , FN_vkCreateGraphicsPipelines
  , PFN_vkCreateGraphicsPipelines
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyPipeline
#endif
  , FN_vkDestroyPipeline
  , PFN_vkDestroyPipeline
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkSampleCountFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCache
  )
import Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModule
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkBlendFactor

-- No documentation found for TopLevel "VkBlendFactor"
newtype VkBlendFactor = VkBlendFactor Int32
  deriving (Eq, Ord, Storable)

instance Show VkBlendFactor where
  showsPrec _ VK_BLEND_FACTOR_ZERO = showString "VK_BLEND_FACTOR_ZERO"
  showsPrec _ VK_BLEND_FACTOR_ONE = showString "VK_BLEND_FACTOR_ONE"
  showsPrec _ VK_BLEND_FACTOR_SRC_COLOR = showString "VK_BLEND_FACTOR_SRC_COLOR"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR"
  showsPrec _ VK_BLEND_FACTOR_DST_COLOR = showString "VK_BLEND_FACTOR_DST_COLOR"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = showString "VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR"
  showsPrec _ VK_BLEND_FACTOR_SRC_ALPHA = showString "VK_BLEND_FACTOR_SRC_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_DST_ALPHA = showString "VK_BLEND_FACTOR_DST_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = showString "VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_CONSTANT_COLOR = showString "VK_BLEND_FACTOR_CONSTANT_COLOR"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = showString "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR"
  showsPrec _ VK_BLEND_FACTOR_CONSTANT_ALPHA = showString "VK_BLEND_FACTOR_CONSTANT_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = showString "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_SRC_ALPHA_SATURATE = showString "VK_BLEND_FACTOR_SRC_ALPHA_SATURATE"
  showsPrec _ VK_BLEND_FACTOR_SRC1_COLOR = showString "VK_BLEND_FACTOR_SRC1_COLOR"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR"
  showsPrec _ VK_BLEND_FACTOR_SRC1_ALPHA = showString "VK_BLEND_FACTOR_SRC1_ALPHA"
  showsPrec _ VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = showString "VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA"
  showsPrec p (VkBlendFactor x) = showParen (p >= 11) (showString "VkBlendFactor " . showsPrec 11 x)

instance Read VkBlendFactor where
  readPrec = parens ( choose [ ("VK_BLEND_FACTOR_ZERO",                     pure VK_BLEND_FACTOR_ZERO)
                             , ("VK_BLEND_FACTOR_ONE",                      pure VK_BLEND_FACTOR_ONE)
                             , ("VK_BLEND_FACTOR_SRC_COLOR",                pure VK_BLEND_FACTOR_SRC_COLOR)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR",      pure VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR)
                             , ("VK_BLEND_FACTOR_DST_COLOR",                pure VK_BLEND_FACTOR_DST_COLOR)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR",      pure VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR)
                             , ("VK_BLEND_FACTOR_SRC_ALPHA",                pure VK_BLEND_FACTOR_SRC_ALPHA)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA",      pure VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA)
                             , ("VK_BLEND_FACTOR_DST_ALPHA",                pure VK_BLEND_FACTOR_DST_ALPHA)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA",      pure VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA)
                             , ("VK_BLEND_FACTOR_CONSTANT_COLOR",           pure VK_BLEND_FACTOR_CONSTANT_COLOR)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR", pure VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR)
                             , ("VK_BLEND_FACTOR_CONSTANT_ALPHA",           pure VK_BLEND_FACTOR_CONSTANT_ALPHA)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA", pure VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA)
                             , ("VK_BLEND_FACTOR_SRC_ALPHA_SATURATE",       pure VK_BLEND_FACTOR_SRC_ALPHA_SATURATE)
                             , ("VK_BLEND_FACTOR_SRC1_COLOR",               pure VK_BLEND_FACTOR_SRC1_COLOR)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR",     pure VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR)
                             , ("VK_BLEND_FACTOR_SRC1_ALPHA",               pure VK_BLEND_FACTOR_SRC1_ALPHA)
                             , ("VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA",     pure VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBlendFactor")
                        v <- step readPrec
                        pure (VkBlendFactor v)
                        )
                    )

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ZERO"
pattern VK_BLEND_FACTOR_ZERO :: VkBlendFactor
pattern VK_BLEND_FACTOR_ZERO = VkBlendFactor 0

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE"
pattern VK_BLEND_FACTOR_ONE :: VkBlendFactor
pattern VK_BLEND_FACTOR_ONE = VkBlendFactor 1

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC_COLOR"
pattern VK_BLEND_FACTOR_SRC_COLOR :: VkBlendFactor
pattern VK_BLEND_FACTOR_SRC_COLOR = VkBlendFactor 2

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR"
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR :: VkBlendFactor
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR = VkBlendFactor 3

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_DST_COLOR"
pattern VK_BLEND_FACTOR_DST_COLOR :: VkBlendFactor
pattern VK_BLEND_FACTOR_DST_COLOR = VkBlendFactor 4

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR"
pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR :: VkBlendFactor
pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR = VkBlendFactor 5

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC_ALPHA"
pattern VK_BLEND_FACTOR_SRC_ALPHA :: VkBlendFactor
pattern VK_BLEND_FACTOR_SRC_ALPHA = VkBlendFactor 6

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA"
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA :: VkBlendFactor
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = VkBlendFactor 7

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_DST_ALPHA"
pattern VK_BLEND_FACTOR_DST_ALPHA :: VkBlendFactor
pattern VK_BLEND_FACTOR_DST_ALPHA = VkBlendFactor 8

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA"
pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA :: VkBlendFactor
pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA = VkBlendFactor 9

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_CONSTANT_COLOR"
pattern VK_BLEND_FACTOR_CONSTANT_COLOR :: VkBlendFactor
pattern VK_BLEND_FACTOR_CONSTANT_COLOR = VkBlendFactor 10

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR"
pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR :: VkBlendFactor
pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = VkBlendFactor 11

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_CONSTANT_ALPHA"
pattern VK_BLEND_FACTOR_CONSTANT_ALPHA :: VkBlendFactor
pattern VK_BLEND_FACTOR_CONSTANT_ALPHA = VkBlendFactor 12

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA"
pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA :: VkBlendFactor
pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = VkBlendFactor 13

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC_ALPHA_SATURATE"
pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE :: VkBlendFactor
pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE = VkBlendFactor 14

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC1_COLOR"
pattern VK_BLEND_FACTOR_SRC1_COLOR :: VkBlendFactor
pattern VK_BLEND_FACTOR_SRC1_COLOR = VkBlendFactor 15

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR"
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR :: VkBlendFactor
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = VkBlendFactor 16

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_SRC1_ALPHA"
pattern VK_BLEND_FACTOR_SRC1_ALPHA :: VkBlendFactor
pattern VK_BLEND_FACTOR_SRC1_ALPHA = VkBlendFactor 17

-- No documentation found for Nested "VkBlendFactor" "VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA"
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA :: VkBlendFactor
pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = VkBlendFactor 18
-- ** VkBlendOp

-- No documentation found for TopLevel "VkBlendOp"
newtype VkBlendOp = VkBlendOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkBlendOp where
  showsPrec _ VK_BLEND_OP_ADD = showString "VK_BLEND_OP_ADD"
  showsPrec _ VK_BLEND_OP_SUBTRACT = showString "VK_BLEND_OP_SUBTRACT"
  showsPrec _ VK_BLEND_OP_REVERSE_SUBTRACT = showString "VK_BLEND_OP_REVERSE_SUBTRACT"
  showsPrec _ VK_BLEND_OP_MIN = showString "VK_BLEND_OP_MIN"
  showsPrec _ VK_BLEND_OP_MAX = showString "VK_BLEND_OP_MAX"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkBlendOp 1000148000) = showString "VK_BLEND_OP_ZERO_EXT"
  showsPrec _ (VkBlendOp 1000148001) = showString "VK_BLEND_OP_SRC_EXT"
  showsPrec _ (VkBlendOp 1000148002) = showString "VK_BLEND_OP_DST_EXT"
  showsPrec _ (VkBlendOp 1000148003) = showString "VK_BLEND_OP_SRC_OVER_EXT"
  showsPrec _ (VkBlendOp 1000148004) = showString "VK_BLEND_OP_DST_OVER_EXT"
  showsPrec _ (VkBlendOp 1000148005) = showString "VK_BLEND_OP_SRC_IN_EXT"
  showsPrec _ (VkBlendOp 1000148006) = showString "VK_BLEND_OP_DST_IN_EXT"
  showsPrec _ (VkBlendOp 1000148007) = showString "VK_BLEND_OP_SRC_OUT_EXT"
  showsPrec _ (VkBlendOp 1000148008) = showString "VK_BLEND_OP_DST_OUT_EXT"
  showsPrec _ (VkBlendOp 1000148009) = showString "VK_BLEND_OP_SRC_ATOP_EXT"
  showsPrec _ (VkBlendOp 1000148010) = showString "VK_BLEND_OP_DST_ATOP_EXT"
  showsPrec _ (VkBlendOp 1000148011) = showString "VK_BLEND_OP_XOR_EXT"
  showsPrec _ (VkBlendOp 1000148012) = showString "VK_BLEND_OP_MULTIPLY_EXT"
  showsPrec _ (VkBlendOp 1000148013) = showString "VK_BLEND_OP_SCREEN_EXT"
  showsPrec _ (VkBlendOp 1000148014) = showString "VK_BLEND_OP_OVERLAY_EXT"
  showsPrec _ (VkBlendOp 1000148015) = showString "VK_BLEND_OP_DARKEN_EXT"
  showsPrec _ (VkBlendOp 1000148016) = showString "VK_BLEND_OP_LIGHTEN_EXT"
  showsPrec _ (VkBlendOp 1000148017) = showString "VK_BLEND_OP_COLORDODGE_EXT"
  showsPrec _ (VkBlendOp 1000148018) = showString "VK_BLEND_OP_COLORBURN_EXT"
  showsPrec _ (VkBlendOp 1000148019) = showString "VK_BLEND_OP_HARDLIGHT_EXT"
  showsPrec _ (VkBlendOp 1000148020) = showString "VK_BLEND_OP_SOFTLIGHT_EXT"
  showsPrec _ (VkBlendOp 1000148021) = showString "VK_BLEND_OP_DIFFERENCE_EXT"
  showsPrec _ (VkBlendOp 1000148022) = showString "VK_BLEND_OP_EXCLUSION_EXT"
  showsPrec _ (VkBlendOp 1000148023) = showString "VK_BLEND_OP_INVERT_EXT"
  showsPrec _ (VkBlendOp 1000148024) = showString "VK_BLEND_OP_INVERT_RGB_EXT"
  showsPrec _ (VkBlendOp 1000148025) = showString "VK_BLEND_OP_LINEARDODGE_EXT"
  showsPrec _ (VkBlendOp 1000148026) = showString "VK_BLEND_OP_LINEARBURN_EXT"
  showsPrec _ (VkBlendOp 1000148027) = showString "VK_BLEND_OP_VIVIDLIGHT_EXT"
  showsPrec _ (VkBlendOp 1000148028) = showString "VK_BLEND_OP_LINEARLIGHT_EXT"
  showsPrec _ (VkBlendOp 1000148029) = showString "VK_BLEND_OP_PINLIGHT_EXT"
  showsPrec _ (VkBlendOp 1000148030) = showString "VK_BLEND_OP_HARDMIX_EXT"
  showsPrec _ (VkBlendOp 1000148031) = showString "VK_BLEND_OP_HSL_HUE_EXT"
  showsPrec _ (VkBlendOp 1000148032) = showString "VK_BLEND_OP_HSL_SATURATION_EXT"
  showsPrec _ (VkBlendOp 1000148033) = showString "VK_BLEND_OP_HSL_COLOR_EXT"
  showsPrec _ (VkBlendOp 1000148034) = showString "VK_BLEND_OP_HSL_LUMINOSITY_EXT"
  showsPrec _ (VkBlendOp 1000148035) = showString "VK_BLEND_OP_PLUS_EXT"
  showsPrec _ (VkBlendOp 1000148036) = showString "VK_BLEND_OP_PLUS_CLAMPED_EXT"
  showsPrec _ (VkBlendOp 1000148037) = showString "VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT"
  showsPrec _ (VkBlendOp 1000148038) = showString "VK_BLEND_OP_PLUS_DARKER_EXT"
  showsPrec _ (VkBlendOp 1000148039) = showString "VK_BLEND_OP_MINUS_EXT"
  showsPrec _ (VkBlendOp 1000148040) = showString "VK_BLEND_OP_MINUS_CLAMPED_EXT"
  showsPrec _ (VkBlendOp 1000148041) = showString "VK_BLEND_OP_CONTRAST_EXT"
  showsPrec _ (VkBlendOp 1000148042) = showString "VK_BLEND_OP_INVERT_OVG_EXT"
  showsPrec _ (VkBlendOp 1000148043) = showString "VK_BLEND_OP_RED_EXT"
  showsPrec _ (VkBlendOp 1000148044) = showString "VK_BLEND_OP_GREEN_EXT"
  showsPrec _ (VkBlendOp 1000148045) = showString "VK_BLEND_OP_BLUE_EXT"
  showsPrec p (VkBlendOp x) = showParen (p >= 11) (showString "VkBlendOp " . showsPrec 11 x)

instance Read VkBlendOp where
  readPrec = parens ( choose [ ("VK_BLEND_OP_ADD",              pure VK_BLEND_OP_ADD)
                             , ("VK_BLEND_OP_SUBTRACT",         pure VK_BLEND_OP_SUBTRACT)
                             , ("VK_BLEND_OP_REVERSE_SUBTRACT", pure VK_BLEND_OP_REVERSE_SUBTRACT)
                             , ("VK_BLEND_OP_MIN",              pure VK_BLEND_OP_MIN)
                             , ("VK_BLEND_OP_MAX",              pure VK_BLEND_OP_MAX)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_BLEND_OP_ZERO_EXT",               pure (VkBlendOp 1000148000))
                             , ("VK_BLEND_OP_SRC_EXT",                pure (VkBlendOp 1000148001))
                             , ("VK_BLEND_OP_DST_EXT",                pure (VkBlendOp 1000148002))
                             , ("VK_BLEND_OP_SRC_OVER_EXT",           pure (VkBlendOp 1000148003))
                             , ("VK_BLEND_OP_DST_OVER_EXT",           pure (VkBlendOp 1000148004))
                             , ("VK_BLEND_OP_SRC_IN_EXT",             pure (VkBlendOp 1000148005))
                             , ("VK_BLEND_OP_DST_IN_EXT",             pure (VkBlendOp 1000148006))
                             , ("VK_BLEND_OP_SRC_OUT_EXT",            pure (VkBlendOp 1000148007))
                             , ("VK_BLEND_OP_DST_OUT_EXT",            pure (VkBlendOp 1000148008))
                             , ("VK_BLEND_OP_SRC_ATOP_EXT",           pure (VkBlendOp 1000148009))
                             , ("VK_BLEND_OP_DST_ATOP_EXT",           pure (VkBlendOp 1000148010))
                             , ("VK_BLEND_OP_XOR_EXT",                pure (VkBlendOp 1000148011))
                             , ("VK_BLEND_OP_MULTIPLY_EXT",           pure (VkBlendOp 1000148012))
                             , ("VK_BLEND_OP_SCREEN_EXT",             pure (VkBlendOp 1000148013))
                             , ("VK_BLEND_OP_OVERLAY_EXT",            pure (VkBlendOp 1000148014))
                             , ("VK_BLEND_OP_DARKEN_EXT",             pure (VkBlendOp 1000148015))
                             , ("VK_BLEND_OP_LIGHTEN_EXT",            pure (VkBlendOp 1000148016))
                             , ("VK_BLEND_OP_COLORDODGE_EXT",         pure (VkBlendOp 1000148017))
                             , ("VK_BLEND_OP_COLORBURN_EXT",          pure (VkBlendOp 1000148018))
                             , ("VK_BLEND_OP_HARDLIGHT_EXT",          pure (VkBlendOp 1000148019))
                             , ("VK_BLEND_OP_SOFTLIGHT_EXT",          pure (VkBlendOp 1000148020))
                             , ("VK_BLEND_OP_DIFFERENCE_EXT",         pure (VkBlendOp 1000148021))
                             , ("VK_BLEND_OP_EXCLUSION_EXT",          pure (VkBlendOp 1000148022))
                             , ("VK_BLEND_OP_INVERT_EXT",             pure (VkBlendOp 1000148023))
                             , ("VK_BLEND_OP_INVERT_RGB_EXT",         pure (VkBlendOp 1000148024))
                             , ("VK_BLEND_OP_LINEARDODGE_EXT",        pure (VkBlendOp 1000148025))
                             , ("VK_BLEND_OP_LINEARBURN_EXT",         pure (VkBlendOp 1000148026))
                             , ("VK_BLEND_OP_VIVIDLIGHT_EXT",         pure (VkBlendOp 1000148027))
                             , ("VK_BLEND_OP_LINEARLIGHT_EXT",        pure (VkBlendOp 1000148028))
                             , ("VK_BLEND_OP_PINLIGHT_EXT",           pure (VkBlendOp 1000148029))
                             , ("VK_BLEND_OP_HARDMIX_EXT",            pure (VkBlendOp 1000148030))
                             , ("VK_BLEND_OP_HSL_HUE_EXT",            pure (VkBlendOp 1000148031))
                             , ("VK_BLEND_OP_HSL_SATURATION_EXT",     pure (VkBlendOp 1000148032))
                             , ("VK_BLEND_OP_HSL_COLOR_EXT",          pure (VkBlendOp 1000148033))
                             , ("VK_BLEND_OP_HSL_LUMINOSITY_EXT",     pure (VkBlendOp 1000148034))
                             , ("VK_BLEND_OP_PLUS_EXT",               pure (VkBlendOp 1000148035))
                             , ("VK_BLEND_OP_PLUS_CLAMPED_EXT",       pure (VkBlendOp 1000148036))
                             , ("VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT", pure (VkBlendOp 1000148037))
                             , ("VK_BLEND_OP_PLUS_DARKER_EXT",        pure (VkBlendOp 1000148038))
                             , ("VK_BLEND_OP_MINUS_EXT",              pure (VkBlendOp 1000148039))
                             , ("VK_BLEND_OP_MINUS_CLAMPED_EXT",      pure (VkBlendOp 1000148040))
                             , ("VK_BLEND_OP_CONTRAST_EXT",           pure (VkBlendOp 1000148041))
                             , ("VK_BLEND_OP_INVERT_OVG_EXT",         pure (VkBlendOp 1000148042))
                             , ("VK_BLEND_OP_RED_EXT",                pure (VkBlendOp 1000148043))
                             , ("VK_BLEND_OP_GREEN_EXT",              pure (VkBlendOp 1000148044))
                             , ("VK_BLEND_OP_BLUE_EXT",               pure (VkBlendOp 1000148045))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBlendOp")
                        v <- step readPrec
                        pure (VkBlendOp v)
                        )
                    )

-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_ADD"
pattern VK_BLEND_OP_ADD :: VkBlendOp
pattern VK_BLEND_OP_ADD = VkBlendOp 0

-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SUBTRACT"
pattern VK_BLEND_OP_SUBTRACT :: VkBlendOp
pattern VK_BLEND_OP_SUBTRACT = VkBlendOp 1

-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_REVERSE_SUBTRACT"
pattern VK_BLEND_OP_REVERSE_SUBTRACT :: VkBlendOp
pattern VK_BLEND_OP_REVERSE_SUBTRACT = VkBlendOp 2

-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MIN"
pattern VK_BLEND_OP_MIN :: VkBlendOp
pattern VK_BLEND_OP_MIN = VkBlendOp 3

-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MAX"
pattern VK_BLEND_OP_MAX :: VkBlendOp
pattern VK_BLEND_OP_MAX = VkBlendOp 4
-- ** VkColorComponentFlagBits

-- No documentation found for TopLevel "VkColorComponentFlagBits"
newtype VkColorComponentFlagBits = VkColorComponentFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkColorComponentFlagBits where
  showsPrec _ VK_COLOR_COMPONENT_R_BIT = showString "VK_COLOR_COMPONENT_R_BIT"
  showsPrec _ VK_COLOR_COMPONENT_G_BIT = showString "VK_COLOR_COMPONENT_G_BIT"
  showsPrec _ VK_COLOR_COMPONENT_B_BIT = showString "VK_COLOR_COMPONENT_B_BIT"
  showsPrec _ VK_COLOR_COMPONENT_A_BIT = showString "VK_COLOR_COMPONENT_A_BIT"
  showsPrec p (VkColorComponentFlagBits x) = showParen (p >= 11) (showString "VkColorComponentFlagBits " . showsPrec 11 x)

instance Read VkColorComponentFlagBits where
  readPrec = parens ( choose [ ("VK_COLOR_COMPONENT_R_BIT", pure VK_COLOR_COMPONENT_R_BIT)
                             , ("VK_COLOR_COMPONENT_G_BIT", pure VK_COLOR_COMPONENT_G_BIT)
                             , ("VK_COLOR_COMPONENT_B_BIT", pure VK_COLOR_COMPONENT_B_BIT)
                             , ("VK_COLOR_COMPONENT_A_BIT", pure VK_COLOR_COMPONENT_A_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkColorComponentFlagBits")
                        v <- step readPrec
                        pure (VkColorComponentFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkColorComponentFlagBits" "VK_COLOR_COMPONENT_R_BIT"
pattern VK_COLOR_COMPONENT_R_BIT :: VkColorComponentFlagBits
pattern VK_COLOR_COMPONENT_R_BIT = VkColorComponentFlagBits 0x00000001

-- No documentation found for Nested "VkColorComponentFlagBits" "VK_COLOR_COMPONENT_G_BIT"
pattern VK_COLOR_COMPONENT_G_BIT :: VkColorComponentFlagBits
pattern VK_COLOR_COMPONENT_G_BIT = VkColorComponentFlagBits 0x00000002

-- No documentation found for Nested "VkColorComponentFlagBits" "VK_COLOR_COMPONENT_B_BIT"
pattern VK_COLOR_COMPONENT_B_BIT :: VkColorComponentFlagBits
pattern VK_COLOR_COMPONENT_B_BIT = VkColorComponentFlagBits 0x00000004

-- No documentation found for Nested "VkColorComponentFlagBits" "VK_COLOR_COMPONENT_A_BIT"
pattern VK_COLOR_COMPONENT_A_BIT :: VkColorComponentFlagBits
pattern VK_COLOR_COMPONENT_A_BIT = VkColorComponentFlagBits 0x00000008
-- No documentation found for TopLevel "VkColorComponentFlags"
type VkColorComponentFlags = VkColorComponentFlagBits
-- ** VkCompareOp

-- No documentation found for TopLevel "VkCompareOp"
newtype VkCompareOp = VkCompareOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkCompareOp where
  showsPrec _ VK_COMPARE_OP_NEVER = showString "VK_COMPARE_OP_NEVER"
  showsPrec _ VK_COMPARE_OP_LESS = showString "VK_COMPARE_OP_LESS"
  showsPrec _ VK_COMPARE_OP_EQUAL = showString "VK_COMPARE_OP_EQUAL"
  showsPrec _ VK_COMPARE_OP_LESS_OR_EQUAL = showString "VK_COMPARE_OP_LESS_OR_EQUAL"
  showsPrec _ VK_COMPARE_OP_GREATER = showString "VK_COMPARE_OP_GREATER"
  showsPrec _ VK_COMPARE_OP_NOT_EQUAL = showString "VK_COMPARE_OP_NOT_EQUAL"
  showsPrec _ VK_COMPARE_OP_GREATER_OR_EQUAL = showString "VK_COMPARE_OP_GREATER_OR_EQUAL"
  showsPrec _ VK_COMPARE_OP_ALWAYS = showString "VK_COMPARE_OP_ALWAYS"
  showsPrec p (VkCompareOp x) = showParen (p >= 11) (showString "VkCompareOp " . showsPrec 11 x)

instance Read VkCompareOp where
  readPrec = parens ( choose [ ("VK_COMPARE_OP_NEVER",            pure VK_COMPARE_OP_NEVER)
                             , ("VK_COMPARE_OP_LESS",             pure VK_COMPARE_OP_LESS)
                             , ("VK_COMPARE_OP_EQUAL",            pure VK_COMPARE_OP_EQUAL)
                             , ("VK_COMPARE_OP_LESS_OR_EQUAL",    pure VK_COMPARE_OP_LESS_OR_EQUAL)
                             , ("VK_COMPARE_OP_GREATER",          pure VK_COMPARE_OP_GREATER)
                             , ("VK_COMPARE_OP_NOT_EQUAL",        pure VK_COMPARE_OP_NOT_EQUAL)
                             , ("VK_COMPARE_OP_GREATER_OR_EQUAL", pure VK_COMPARE_OP_GREATER_OR_EQUAL)
                             , ("VK_COMPARE_OP_ALWAYS",           pure VK_COMPARE_OP_ALWAYS)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCompareOp")
                        v <- step readPrec
                        pure (VkCompareOp v)
                        )
                    )

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_NEVER"
pattern VK_COMPARE_OP_NEVER :: VkCompareOp
pattern VK_COMPARE_OP_NEVER = VkCompareOp 0

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_LESS"
pattern VK_COMPARE_OP_LESS :: VkCompareOp
pattern VK_COMPARE_OP_LESS = VkCompareOp 1

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_EQUAL"
pattern VK_COMPARE_OP_EQUAL :: VkCompareOp
pattern VK_COMPARE_OP_EQUAL = VkCompareOp 2

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_LESS_OR_EQUAL"
pattern VK_COMPARE_OP_LESS_OR_EQUAL :: VkCompareOp
pattern VK_COMPARE_OP_LESS_OR_EQUAL = VkCompareOp 3

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_GREATER"
pattern VK_COMPARE_OP_GREATER :: VkCompareOp
pattern VK_COMPARE_OP_GREATER = VkCompareOp 4

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_NOT_EQUAL"
pattern VK_COMPARE_OP_NOT_EQUAL :: VkCompareOp
pattern VK_COMPARE_OP_NOT_EQUAL = VkCompareOp 5

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_GREATER_OR_EQUAL"
pattern VK_COMPARE_OP_GREATER_OR_EQUAL :: VkCompareOp
pattern VK_COMPARE_OP_GREATER_OR_EQUAL = VkCompareOp 6

-- No documentation found for Nested "VkCompareOp" "VK_COMPARE_OP_ALWAYS"
pattern VK_COMPARE_OP_ALWAYS :: VkCompareOp
pattern VK_COMPARE_OP_ALWAYS = VkCompareOp 7
-- No documentation found for TopLevel "VkComputePipelineCreateInfo"
data VkComputePipelineCreateInfo = VkComputePipelineCreateInfo
  { -- No documentation found for Nested "VkComputePipelineCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "flags"
  vkFlags :: VkPipelineCreateFlags
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "stage"
  vkStage :: VkPipelineShaderStageCreateInfo
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "layout"
  vkLayout :: VkPipelineLayout
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "basePipelineHandle"
  vkBasePipelineHandle :: VkPipeline
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "basePipelineIndex"
  vkBasePipelineIndex :: Int32
  }
  deriving (Eq, Show)

instance Storable VkComputePipelineCreateInfo where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkComputePipelineCreateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 72)
                                         <*> peek (ptr `plusPtr` 80)
                                         <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkStage (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkLayout (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 80) (vkBasePipelineHandle (poked :: VkComputePipelineCreateInfo))
                *> poke (ptr `plusPtr` 88) (vkBasePipelineIndex (poked :: VkComputePipelineCreateInfo))
-- ** VkCullModeFlagBits

-- No documentation found for TopLevel "VkCullModeFlagBits"
newtype VkCullModeFlagBits = VkCullModeFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkCullModeFlagBits where
  showsPrec _ VK_CULL_MODE_FRONT_BIT = showString "VK_CULL_MODE_FRONT_BIT"
  showsPrec _ VK_CULL_MODE_BACK_BIT = showString "VK_CULL_MODE_BACK_BIT"
  showsPrec _ VK_CULL_MODE_NONE = showString "VK_CULL_MODE_NONE"
  showsPrec _ VK_CULL_MODE_FRONT_AND_BACK = showString "VK_CULL_MODE_FRONT_AND_BACK"
  showsPrec p (VkCullModeFlagBits x) = showParen (p >= 11) (showString "VkCullModeFlagBits " . showsPrec 11 x)

instance Read VkCullModeFlagBits where
  readPrec = parens ( choose [ ("VK_CULL_MODE_FRONT_BIT",      pure VK_CULL_MODE_FRONT_BIT)
                             , ("VK_CULL_MODE_BACK_BIT",       pure VK_CULL_MODE_BACK_BIT)
                             , ("VK_CULL_MODE_NONE",           pure VK_CULL_MODE_NONE)
                             , ("VK_CULL_MODE_FRONT_AND_BACK", pure VK_CULL_MODE_FRONT_AND_BACK)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCullModeFlagBits")
                        v <- step readPrec
                        pure (VkCullModeFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkCullModeFlagBits" "VK_CULL_MODE_FRONT_BIT"
pattern VK_CULL_MODE_FRONT_BIT :: VkCullModeFlagBits
pattern VK_CULL_MODE_FRONT_BIT = VkCullModeFlagBits 0x00000001

-- No documentation found for Nested "VkCullModeFlagBits" "VK_CULL_MODE_BACK_BIT"
pattern VK_CULL_MODE_BACK_BIT :: VkCullModeFlagBits
pattern VK_CULL_MODE_BACK_BIT = VkCullModeFlagBits 0x00000002

-- No documentation found for Nested "VkCullModeFlagBits" "VK_CULL_MODE_NONE"
pattern VK_CULL_MODE_NONE :: VkCullModeFlagBits
pattern VK_CULL_MODE_NONE = VkCullModeFlagBits 0x00000000

-- No documentation found for Nested "VkCullModeFlagBits" "VK_CULL_MODE_FRONT_AND_BACK"
pattern VK_CULL_MODE_FRONT_AND_BACK :: VkCullModeFlagBits
pattern VK_CULL_MODE_FRONT_AND_BACK = VkCullModeFlagBits 0x00000003
-- No documentation found for TopLevel "VkCullModeFlags"
type VkCullModeFlags = VkCullModeFlagBits
-- ** VkDynamicState

-- No documentation found for TopLevel "VkDynamicState"
newtype VkDynamicState = VkDynamicState Int32
  deriving (Eq, Ord, Storable)

instance Show VkDynamicState where
  showsPrec _ VK_DYNAMIC_STATE_VIEWPORT = showString "VK_DYNAMIC_STATE_VIEWPORT"
  showsPrec _ VK_DYNAMIC_STATE_SCISSOR = showString "VK_DYNAMIC_STATE_SCISSOR"
  showsPrec _ VK_DYNAMIC_STATE_LINE_WIDTH = showString "VK_DYNAMIC_STATE_LINE_WIDTH"
  showsPrec _ VK_DYNAMIC_STATE_DEPTH_BIAS = showString "VK_DYNAMIC_STATE_DEPTH_BIAS"
  showsPrec _ VK_DYNAMIC_STATE_BLEND_CONSTANTS = showString "VK_DYNAMIC_STATE_BLEND_CONSTANTS"
  showsPrec _ VK_DYNAMIC_STATE_DEPTH_BOUNDS = showString "VK_DYNAMIC_STATE_DEPTH_BOUNDS"
  showsPrec _ VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = showString "VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK"
  showsPrec _ VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = showString "VK_DYNAMIC_STATE_STENCIL_WRITE_MASK"
  showsPrec _ VK_DYNAMIC_STATE_STENCIL_REFERENCE = showString "VK_DYNAMIC_STATE_STENCIL_REFERENCE"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDynamicState 1000087000) = showString "VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV"
  showsPrec _ (VkDynamicState 1000099000) = showString "VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT"
  showsPrec _ (VkDynamicState 1000143000) = showString "VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT"
  showsPrec p (VkDynamicState x) = showParen (p >= 11) (showString "VkDynamicState " . showsPrec 11 x)

instance Read VkDynamicState where
  readPrec = parens ( choose [ ("VK_DYNAMIC_STATE_VIEWPORT",             pure VK_DYNAMIC_STATE_VIEWPORT)
                             , ("VK_DYNAMIC_STATE_SCISSOR",              pure VK_DYNAMIC_STATE_SCISSOR)
                             , ("VK_DYNAMIC_STATE_LINE_WIDTH",           pure VK_DYNAMIC_STATE_LINE_WIDTH)
                             , ("VK_DYNAMIC_STATE_DEPTH_BIAS",           pure VK_DYNAMIC_STATE_DEPTH_BIAS)
                             , ("VK_DYNAMIC_STATE_BLEND_CONSTANTS",      pure VK_DYNAMIC_STATE_BLEND_CONSTANTS)
                             , ("VK_DYNAMIC_STATE_DEPTH_BOUNDS",         pure VK_DYNAMIC_STATE_DEPTH_BOUNDS)
                             , ("VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK", pure VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK)
                             , ("VK_DYNAMIC_STATE_STENCIL_WRITE_MASK",   pure VK_DYNAMIC_STATE_STENCIL_WRITE_MASK)
                             , ("VK_DYNAMIC_STATE_STENCIL_REFERENCE",    pure VK_DYNAMIC_STATE_STENCIL_REFERENCE)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV", pure (VkDynamicState 1000087000))
                             , ("VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT", pure (VkDynamicState 1000099000))
                             , ("VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT",  pure (VkDynamicState 1000143000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDynamicState")
                        v <- step readPrec
                        pure (VkDynamicState v)
                        )
                    )

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_VIEWPORT"
pattern VK_DYNAMIC_STATE_VIEWPORT :: VkDynamicState
pattern VK_DYNAMIC_STATE_VIEWPORT = VkDynamicState 0

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_SCISSOR"
pattern VK_DYNAMIC_STATE_SCISSOR :: VkDynamicState
pattern VK_DYNAMIC_STATE_SCISSOR = VkDynamicState 1

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_LINE_WIDTH"
pattern VK_DYNAMIC_STATE_LINE_WIDTH :: VkDynamicState
pattern VK_DYNAMIC_STATE_LINE_WIDTH = VkDynamicState 2

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DEPTH_BIAS"
pattern VK_DYNAMIC_STATE_DEPTH_BIAS :: VkDynamicState
pattern VK_DYNAMIC_STATE_DEPTH_BIAS = VkDynamicState 3

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_BLEND_CONSTANTS"
pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS :: VkDynamicState
pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS = VkDynamicState 4

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_DEPTH_BOUNDS"
pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS :: VkDynamicState
pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS = VkDynamicState 5

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK"
pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK :: VkDynamicState
pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = VkDynamicState 6

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_STENCIL_WRITE_MASK"
pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK :: VkDynamicState
pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = VkDynamicState 7

-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_STENCIL_REFERENCE"
pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE :: VkDynamicState
pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE = VkDynamicState 8
-- No documentation found for TopLevel "VkExtent2D"
data VkExtent2D = VkExtent2D
  { -- No documentation found for Nested "VkExtent2D" "width"
  vkWidth :: Word32
  , -- No documentation found for Nested "VkExtent2D" "height"
  vkHeight :: Word32
  }
  deriving (Eq, Show)

instance Storable VkExtent2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkExtent2D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkWidth (poked :: VkExtent2D))
                *> poke (ptr `plusPtr` 4) (vkHeight (poked :: VkExtent2D))
-- ** VkFrontFace

-- No documentation found for TopLevel "VkFrontFace"
newtype VkFrontFace = VkFrontFace Int32
  deriving (Eq, Ord, Storable)

instance Show VkFrontFace where
  showsPrec _ VK_FRONT_FACE_COUNTER_CLOCKWISE = showString "VK_FRONT_FACE_COUNTER_CLOCKWISE"
  showsPrec _ VK_FRONT_FACE_CLOCKWISE = showString "VK_FRONT_FACE_CLOCKWISE"
  showsPrec p (VkFrontFace x) = showParen (p >= 11) (showString "VkFrontFace " . showsPrec 11 x)

instance Read VkFrontFace where
  readPrec = parens ( choose [ ("VK_FRONT_FACE_COUNTER_CLOCKWISE", pure VK_FRONT_FACE_COUNTER_CLOCKWISE)
                             , ("VK_FRONT_FACE_CLOCKWISE",         pure VK_FRONT_FACE_CLOCKWISE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFrontFace")
                        v <- step readPrec
                        pure (VkFrontFace v)
                        )
                    )

-- No documentation found for Nested "VkFrontFace" "VK_FRONT_FACE_COUNTER_CLOCKWISE"
pattern VK_FRONT_FACE_COUNTER_CLOCKWISE :: VkFrontFace
pattern VK_FRONT_FACE_COUNTER_CLOCKWISE = VkFrontFace 0

-- No documentation found for Nested "VkFrontFace" "VK_FRONT_FACE_CLOCKWISE"
pattern VK_FRONT_FACE_CLOCKWISE :: VkFrontFace
pattern VK_FRONT_FACE_CLOCKWISE = VkFrontFace 1
-- No documentation found for TopLevel "VkGraphicsPipelineCreateInfo"
data VkGraphicsPipelineCreateInfo = VkGraphicsPipelineCreateInfo
  { -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "flags"
  vkFlags :: VkPipelineCreateFlags
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "stageCount"
  vkStageCount :: Word32
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pStages"
  vkPStages :: Ptr VkPipelineShaderStageCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pVertexInputState"
  vkPVertexInputState :: Ptr VkPipelineVertexInputStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pInputAssemblyState"
  vkPInputAssemblyState :: Ptr VkPipelineInputAssemblyStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pTessellationState"
  vkPTessellationState :: Ptr VkPipelineTessellationStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pViewportState"
  vkPViewportState :: Ptr VkPipelineViewportStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pRasterizationState"
  vkPRasterizationState :: Ptr VkPipelineRasterizationStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pMultisampleState"
  vkPMultisampleState :: Ptr VkPipelineMultisampleStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pDepthStencilState"
  vkPDepthStencilState :: Ptr VkPipelineDepthStencilStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pColorBlendState"
  vkPColorBlendState :: Ptr VkPipelineColorBlendStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pDynamicState"
  vkPDynamicState :: Ptr VkPipelineDynamicStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "layout"
  vkLayout :: VkPipelineLayout
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "renderPass"
  vkRenderPass :: VkRenderPass
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "subpass"
  vkSubpass :: Word32
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "basePipelineHandle"
  vkBasePipelineHandle :: VkPipeline
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "basePipelineIndex"
  vkBasePipelineIndex :: Int32
  }
  deriving (Eq, Show)

instance Storable VkGraphicsPipelineCreateInfo where
  sizeOf ~_ = 144
  alignment ~_ = 8
  peek ptr = VkGraphicsPipelineCreateInfo <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 20)
                                          <*> peek (ptr `plusPtr` 24)
                                          <*> peek (ptr `plusPtr` 32)
                                          <*> peek (ptr `plusPtr` 40)
                                          <*> peek (ptr `plusPtr` 48)
                                          <*> peek (ptr `plusPtr` 56)
                                          <*> peek (ptr `plusPtr` 64)
                                          <*> peek (ptr `plusPtr` 72)
                                          <*> peek (ptr `plusPtr` 80)
                                          <*> peek (ptr `plusPtr` 88)
                                          <*> peek (ptr `plusPtr` 96)
                                          <*> peek (ptr `plusPtr` 104)
                                          <*> peek (ptr `plusPtr` 112)
                                          <*> peek (ptr `plusPtr` 120)
                                          <*> peek (ptr `plusPtr` 128)
                                          <*> peek (ptr `plusPtr` 136)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkStageCount (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPStages (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPVertexInputState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPInputAssemblyState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkPTessellationState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPViewportState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 64) (vkPRasterizationState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 72) (vkPMultisampleState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 80) (vkPDepthStencilState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 88) (vkPColorBlendState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 96) (vkPDynamicState (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 104) (vkLayout (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 112) (vkRenderPass (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 120) (vkSubpass (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 128) (vkBasePipelineHandle (poked :: VkGraphicsPipelineCreateInfo))
                *> poke (ptr `plusPtr` 136) (vkBasePipelineIndex (poked :: VkGraphicsPipelineCreateInfo))
-- ** VkLogicOp

-- No documentation found for TopLevel "VkLogicOp"
newtype VkLogicOp = VkLogicOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkLogicOp where
  showsPrec _ VK_LOGIC_OP_CLEAR = showString "VK_LOGIC_OP_CLEAR"
  showsPrec _ VK_LOGIC_OP_AND = showString "VK_LOGIC_OP_AND"
  showsPrec _ VK_LOGIC_OP_AND_REVERSE = showString "VK_LOGIC_OP_AND_REVERSE"
  showsPrec _ VK_LOGIC_OP_COPY = showString "VK_LOGIC_OP_COPY"
  showsPrec _ VK_LOGIC_OP_AND_INVERTED = showString "VK_LOGIC_OP_AND_INVERTED"
  showsPrec _ VK_LOGIC_OP_NO_OP = showString "VK_LOGIC_OP_NO_OP"
  showsPrec _ VK_LOGIC_OP_XOR = showString "VK_LOGIC_OP_XOR"
  showsPrec _ VK_LOGIC_OP_OR = showString "VK_LOGIC_OP_OR"
  showsPrec _ VK_LOGIC_OP_NOR = showString "VK_LOGIC_OP_NOR"
  showsPrec _ VK_LOGIC_OP_EQUIVALENT = showString "VK_LOGIC_OP_EQUIVALENT"
  showsPrec _ VK_LOGIC_OP_INVERT = showString "VK_LOGIC_OP_INVERT"
  showsPrec _ VK_LOGIC_OP_OR_REVERSE = showString "VK_LOGIC_OP_OR_REVERSE"
  showsPrec _ VK_LOGIC_OP_COPY_INVERTED = showString "VK_LOGIC_OP_COPY_INVERTED"
  showsPrec _ VK_LOGIC_OP_OR_INVERTED = showString "VK_LOGIC_OP_OR_INVERTED"
  showsPrec _ VK_LOGIC_OP_NAND = showString "VK_LOGIC_OP_NAND"
  showsPrec _ VK_LOGIC_OP_SET = showString "VK_LOGIC_OP_SET"
  showsPrec p (VkLogicOp x) = showParen (p >= 11) (showString "VkLogicOp " . showsPrec 11 x)

instance Read VkLogicOp where
  readPrec = parens ( choose [ ("VK_LOGIC_OP_CLEAR",         pure VK_LOGIC_OP_CLEAR)
                             , ("VK_LOGIC_OP_AND",           pure VK_LOGIC_OP_AND)
                             , ("VK_LOGIC_OP_AND_REVERSE",   pure VK_LOGIC_OP_AND_REVERSE)
                             , ("VK_LOGIC_OP_COPY",          pure VK_LOGIC_OP_COPY)
                             , ("VK_LOGIC_OP_AND_INVERTED",  pure VK_LOGIC_OP_AND_INVERTED)
                             , ("VK_LOGIC_OP_NO_OP",         pure VK_LOGIC_OP_NO_OP)
                             , ("VK_LOGIC_OP_XOR",           pure VK_LOGIC_OP_XOR)
                             , ("VK_LOGIC_OP_OR",            pure VK_LOGIC_OP_OR)
                             , ("VK_LOGIC_OP_NOR",           pure VK_LOGIC_OP_NOR)
                             , ("VK_LOGIC_OP_EQUIVALENT",    pure VK_LOGIC_OP_EQUIVALENT)
                             , ("VK_LOGIC_OP_INVERT",        pure VK_LOGIC_OP_INVERT)
                             , ("VK_LOGIC_OP_OR_REVERSE",    pure VK_LOGIC_OP_OR_REVERSE)
                             , ("VK_LOGIC_OP_COPY_INVERTED", pure VK_LOGIC_OP_COPY_INVERTED)
                             , ("VK_LOGIC_OP_OR_INVERTED",   pure VK_LOGIC_OP_OR_INVERTED)
                             , ("VK_LOGIC_OP_NAND",          pure VK_LOGIC_OP_NAND)
                             , ("VK_LOGIC_OP_SET",           pure VK_LOGIC_OP_SET)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkLogicOp")
                        v <- step readPrec
                        pure (VkLogicOp v)
                        )
                    )

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_CLEAR"
pattern VK_LOGIC_OP_CLEAR :: VkLogicOp
pattern VK_LOGIC_OP_CLEAR = VkLogicOp 0

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_AND"
pattern VK_LOGIC_OP_AND :: VkLogicOp
pattern VK_LOGIC_OP_AND = VkLogicOp 1

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_AND_REVERSE"
pattern VK_LOGIC_OP_AND_REVERSE :: VkLogicOp
pattern VK_LOGIC_OP_AND_REVERSE = VkLogicOp 2

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_COPY"
pattern VK_LOGIC_OP_COPY :: VkLogicOp
pattern VK_LOGIC_OP_COPY = VkLogicOp 3

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_AND_INVERTED"
pattern VK_LOGIC_OP_AND_INVERTED :: VkLogicOp
pattern VK_LOGIC_OP_AND_INVERTED = VkLogicOp 4

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_NO_OP"
pattern VK_LOGIC_OP_NO_OP :: VkLogicOp
pattern VK_LOGIC_OP_NO_OP = VkLogicOp 5

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_XOR"
pattern VK_LOGIC_OP_XOR :: VkLogicOp
pattern VK_LOGIC_OP_XOR = VkLogicOp 6

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_OR"
pattern VK_LOGIC_OP_OR :: VkLogicOp
pattern VK_LOGIC_OP_OR = VkLogicOp 7

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_NOR"
pattern VK_LOGIC_OP_NOR :: VkLogicOp
pattern VK_LOGIC_OP_NOR = VkLogicOp 8

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_EQUIVALENT"
pattern VK_LOGIC_OP_EQUIVALENT :: VkLogicOp
pattern VK_LOGIC_OP_EQUIVALENT = VkLogicOp 9

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_INVERT"
pattern VK_LOGIC_OP_INVERT :: VkLogicOp
pattern VK_LOGIC_OP_INVERT = VkLogicOp 10

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_OR_REVERSE"
pattern VK_LOGIC_OP_OR_REVERSE :: VkLogicOp
pattern VK_LOGIC_OP_OR_REVERSE = VkLogicOp 11

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_COPY_INVERTED"
pattern VK_LOGIC_OP_COPY_INVERTED :: VkLogicOp
pattern VK_LOGIC_OP_COPY_INVERTED = VkLogicOp 12

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_OR_INVERTED"
pattern VK_LOGIC_OP_OR_INVERTED :: VkLogicOp
pattern VK_LOGIC_OP_OR_INVERTED = VkLogicOp 13

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_NAND"
pattern VK_LOGIC_OP_NAND :: VkLogicOp
pattern VK_LOGIC_OP_NAND = VkLogicOp 14

-- No documentation found for Nested "VkLogicOp" "VK_LOGIC_OP_SET"
pattern VK_LOGIC_OP_SET :: VkLogicOp
pattern VK_LOGIC_OP_SET = VkLogicOp 15
-- No documentation found for TopLevel "VkOffset2D"
data VkOffset2D = VkOffset2D
  { -- No documentation found for Nested "VkOffset2D" "x"
  vkX :: Int32
  , -- No documentation found for Nested "VkOffset2D" "y"
  vkY :: Int32
  }
  deriving (Eq, Show)

instance Storable VkOffset2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkOffset2D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkOffset2D))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkOffset2D))
-- | Dummy data to tag the 'Ptr' with
data VkPipeline_T
-- No documentation found for TopLevel "VkPipeline"
type VkPipeline = Ptr VkPipeline_T
-- No documentation found for TopLevel "VkPipelineColorBlendAttachmentState"
data VkPipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState
  { -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "blendEnable"
  vkBlendEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "srcColorBlendFactor"
  vkSrcColorBlendFactor :: VkBlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "dstColorBlendFactor"
  vkDstColorBlendFactor :: VkBlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "colorBlendOp"
  vkColorBlendOp :: VkBlendOp
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "srcAlphaBlendFactor"
  vkSrcAlphaBlendFactor :: VkBlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "dstAlphaBlendFactor"
  vkDstAlphaBlendFactor :: VkBlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "alphaBlendOp"
  vkAlphaBlendOp :: VkBlendOp
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "colorWriteMask"
  vkColorWriteMask :: VkColorComponentFlags
  }
  deriving (Eq, Show)

instance Storable VkPipelineColorBlendAttachmentState where
  sizeOf ~_ = 32
  alignment ~_ = 4
  peek ptr = VkPipelineColorBlendAttachmentState <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 4)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 12)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBlendEnable (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 4) (vkSrcColorBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 8) (vkDstColorBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 12) (vkColorBlendOp (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 16) (vkSrcAlphaBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 20) (vkDstAlphaBlendFactor (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 24) (vkAlphaBlendOp (poked :: VkPipelineColorBlendAttachmentState))
                *> poke (ptr `plusPtr` 28) (vkColorWriteMask (poked :: VkPipelineColorBlendAttachmentState))
-- ** VkPipelineColorBlendStateCreateFlags

-- No documentation found for TopLevel "VkPipelineColorBlendStateCreateFlags"
newtype VkPipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineColorBlendStateCreateFlags where
  
  showsPrec p (VkPipelineColorBlendStateCreateFlags x) = showParen (p >= 11) (showString "VkPipelineColorBlendStateCreateFlags " . showsPrec 11 x)

instance Read VkPipelineColorBlendStateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineColorBlendStateCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineColorBlendStateCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineColorBlendStateCreateInfo"
data VkPipelineColorBlendStateCreateInfo = VkPipelineColorBlendStateCreateInfo
  { -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "flags"
  vkFlags :: VkPipelineColorBlendStateCreateFlags
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "logicOpEnable"
  vkLogicOpEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "logicOp"
  vkLogicOp :: VkLogicOp
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "attachmentCount"
  vkAttachmentCount :: Word32
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "pAttachments"
  vkPAttachments :: Ptr VkPipelineColorBlendAttachmentState
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "blendConstants"
  vkBlendConstants :: Vector 4 CFloat
  }
  deriving (Eq, Show)

instance Storable VkPipelineColorBlendStateCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkPipelineColorBlendStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
                                                 <*> peek (ptr `plusPtr` 32)
                                                 <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkLogicOpEnable (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkLogicOp (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkAttachmentCount (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPAttachments (poked :: VkPipelineColorBlendStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkBlendConstants (poked :: VkPipelineColorBlendStateCreateInfo))
-- ** VkPipelineCreateFlagBits

-- No documentation found for TopLevel "VkPipelineCreateFlagBits"
newtype VkPipelineCreateFlagBits = VkPipelineCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineCreateFlagBits where
  showsPrec _ VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = showString "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
  showsPrec _ VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = showString "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
  showsPrec _ VK_PIPELINE_CREATE_DERIVATIVE_BIT = showString "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkPipelineCreateFlagBits 0x00000008) = showString "VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
  showsPrec _ (VkPipelineCreateFlagBits 0x00000010) = showString "VK_PIPELINE_CREATE_DISPATCH_BASE"
  showsPrec p (VkPipelineCreateFlagBits x) = showParen (p >= 11) (showString "VkPipelineCreateFlagBits " . showsPrec 11 x)

instance Read VkPipelineCreateFlagBits where
  readPrec = parens ( choose [ ("VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT", pure VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT)
                             , ("VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT",    pure VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT)
                             , ("VK_PIPELINE_CREATE_DERIVATIVE_BIT",           pure VK_PIPELINE_CREATE_DERIVATIVE_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT", pure (VkPipelineCreateFlagBits 0x00000008))
                             , ("VK_PIPELINE_CREATE_DISPATCH_BASE",                    pure (VkPipelineCreateFlagBits 0x00000010))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineCreateFlagBits")
                        v <- step readPrec
                        pure (VkPipelineCreateFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = VkPipelineCreateFlagBits 0x00000001

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = VkPipelineCreateFlagBits 0x00000002

-- No documentation found for Nested "VkPipelineCreateFlagBits" "VK_PIPELINE_CREATE_DERIVATIVE_BIT"
pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT = VkPipelineCreateFlagBits 0x00000004
-- No documentation found for TopLevel "VkPipelineCreateFlags"
type VkPipelineCreateFlags = VkPipelineCreateFlagBits
-- ** VkPipelineDepthStencilStateCreateFlags

-- No documentation found for TopLevel "VkPipelineDepthStencilStateCreateFlags"
newtype VkPipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineDepthStencilStateCreateFlags where
  
  showsPrec p (VkPipelineDepthStencilStateCreateFlags x) = showParen (p >= 11) (showString "VkPipelineDepthStencilStateCreateFlags " . showsPrec 11 x)

instance Read VkPipelineDepthStencilStateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineDepthStencilStateCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineDepthStencilStateCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineDepthStencilStateCreateInfo"
data VkPipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo
  { -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "flags"
  vkFlags :: VkPipelineDepthStencilStateCreateFlags
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "depthTestEnable"
  vkDepthTestEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "depthWriteEnable"
  vkDepthWriteEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "depthCompareOp"
  vkDepthCompareOp :: VkCompareOp
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "depthBoundsTestEnable"
  vkDepthBoundsTestEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "stencilTestEnable"
  vkStencilTestEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "front"
  vkFront :: VkStencilOpState
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "back"
  vkBack :: VkStencilOpState
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "minDepthBounds"
  vkMinDepthBounds :: CFloat
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "maxDepthBounds"
  vkMaxDepthBounds :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkPipelineDepthStencilStateCreateInfo where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek ptr = VkPipelineDepthStencilStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
                                                   <*> peek (ptr `plusPtr` 24)
                                                   <*> peek (ptr `plusPtr` 28)
                                                   <*> peek (ptr `plusPtr` 32)
                                                   <*> peek (ptr `plusPtr` 36)
                                                   <*> peek (ptr `plusPtr` 40)
                                                   <*> peek (ptr `plusPtr` 68)
                                                   <*> peek (ptr `plusPtr` 96)
                                                   <*> peek (ptr `plusPtr` 100)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDepthTestEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkDepthWriteEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkDepthCompareOp (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkDepthBoundsTestEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkStencilTestEnable (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkFront (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 68) (vkBack (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 96) (vkMinDepthBounds (poked :: VkPipelineDepthStencilStateCreateInfo))
                *> poke (ptr `plusPtr` 100) (vkMaxDepthBounds (poked :: VkPipelineDepthStencilStateCreateInfo))
-- ** VkPipelineDynamicStateCreateFlags

-- No documentation found for TopLevel "VkPipelineDynamicStateCreateFlags"
newtype VkPipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineDynamicStateCreateFlags where
  
  showsPrec p (VkPipelineDynamicStateCreateFlags x) = showParen (p >= 11) (showString "VkPipelineDynamicStateCreateFlags " . showsPrec 11 x)

instance Read VkPipelineDynamicStateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineDynamicStateCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineDynamicStateCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineDynamicStateCreateInfo"
data VkPipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo
  { -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "flags"
  vkFlags :: VkPipelineDynamicStateCreateFlags
  , -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "dynamicStateCount"
  vkDynamicStateCount :: Word32
  , -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "pDynamicStates"
  vkPDynamicStates :: Ptr VkDynamicState
  }
  deriving (Eq, Show)

instance Storable VkPipelineDynamicStateCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineDynamicStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDynamicStateCount (poked :: VkPipelineDynamicStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPDynamicStates (poked :: VkPipelineDynamicStateCreateInfo))
-- ** VkPipelineInputAssemblyStateCreateFlags

-- No documentation found for TopLevel "VkPipelineInputAssemblyStateCreateFlags"
newtype VkPipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineInputAssemblyStateCreateFlags where
  
  showsPrec p (VkPipelineInputAssemblyStateCreateFlags x) = showParen (p >= 11) (showString "VkPipelineInputAssemblyStateCreateFlags " . showsPrec 11 x)

instance Read VkPipelineInputAssemblyStateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineInputAssemblyStateCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineInputAssemblyStateCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineInputAssemblyStateCreateInfo"
data VkPipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo
  { -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "flags"
  vkFlags :: VkPipelineInputAssemblyStateCreateFlags
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "topology"
  vkTopology :: VkPrimitiveTopology
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "primitiveRestartEnable"
  vkPrimitiveRestartEnable :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPipelineInputAssemblyStateCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineInputAssemblyStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkTopology (poked :: VkPipelineInputAssemblyStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPrimitiveRestartEnable (poked :: VkPipelineInputAssemblyStateCreateInfo))
-- | Dummy data to tag the 'Ptr' with
data VkPipelineLayout_T
-- No documentation found for TopLevel "VkPipelineLayout"
type VkPipelineLayout = Ptr VkPipelineLayout_T
-- ** VkPipelineMultisampleStateCreateFlags

-- No documentation found for TopLevel "VkPipelineMultisampleStateCreateFlags"
newtype VkPipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineMultisampleStateCreateFlags where
  
  showsPrec p (VkPipelineMultisampleStateCreateFlags x) = showParen (p >= 11) (showString "VkPipelineMultisampleStateCreateFlags " . showsPrec 11 x)

instance Read VkPipelineMultisampleStateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineMultisampleStateCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineMultisampleStateCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineMultisampleStateCreateInfo"
data VkPipelineMultisampleStateCreateInfo = VkPipelineMultisampleStateCreateInfo
  { -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "flags"
  vkFlags :: VkPipelineMultisampleStateCreateFlags
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "rasterizationSamples"
  vkRasterizationSamples :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "sampleShadingEnable"
  vkSampleShadingEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "minSampleShading"
  vkMinSampleShading :: CFloat
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "pSampleMask"
  vkPSampleMask :: Ptr VkSampleMask
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "alphaToCoverageEnable"
  vkAlphaToCoverageEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "alphaToOneEnable"
  vkAlphaToOneEnable :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPipelineMultisampleStateCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineMultisampleStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 28)
                                                  <*> peek (ptr `plusPtr` 32)
                                                  <*> peek (ptr `plusPtr` 40)
                                                  <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkRasterizationSamples (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkSampleShadingEnable (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkMinSampleShading (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPSampleMask (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkAlphaToCoverageEnable (poked :: VkPipelineMultisampleStateCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkAlphaToOneEnable (poked :: VkPipelineMultisampleStateCreateInfo))
-- ** VkPipelineRasterizationStateCreateFlags

-- No documentation found for TopLevel "VkPipelineRasterizationStateCreateFlags"
newtype VkPipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineRasterizationStateCreateFlags where
  
  showsPrec p (VkPipelineRasterizationStateCreateFlags x) = showParen (p >= 11) (showString "VkPipelineRasterizationStateCreateFlags " . showsPrec 11 x)

instance Read VkPipelineRasterizationStateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineRasterizationStateCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineRasterizationStateCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineRasterizationStateCreateInfo"
data VkPipelineRasterizationStateCreateInfo = VkPipelineRasterizationStateCreateInfo
  { -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "flags"
  vkFlags :: VkPipelineRasterizationStateCreateFlags
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthClampEnable"
  vkDepthClampEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "rasterizerDiscardEnable"
  vkRasterizerDiscardEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "polygonMode"
  vkPolygonMode :: VkPolygonMode
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "cullMode"
  vkCullMode :: VkCullModeFlags
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "frontFace"
  vkFrontFace :: VkFrontFace
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthBiasEnable"
  vkDepthBiasEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthBiasConstantFactor"
  vkDepthBiasConstantFactor :: CFloat
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthBiasClamp"
  vkDepthBiasClamp :: CFloat
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthBiasSlopeFactor"
  vkDepthBiasSlopeFactor :: CFloat
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "lineWidth"
  vkLineWidth :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkPipelineRasterizationStateCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPipelineRasterizationStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
                                                    <*> peek (ptr `plusPtr` 28)
                                                    <*> peek (ptr `plusPtr` 32)
                                                    <*> peek (ptr `plusPtr` 36)
                                                    <*> peek (ptr `plusPtr` 40)
                                                    <*> peek (ptr `plusPtr` 44)
                                                    <*> peek (ptr `plusPtr` 48)
                                                    <*> peek (ptr `plusPtr` 52)
                                                    <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkDepthClampEnable (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkRasterizerDiscardEnable (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkPolygonMode (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkCullMode (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 36) (vkFrontFace (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkDepthBiasEnable (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 44) (vkDepthBiasConstantFactor (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkDepthBiasClamp (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkDepthBiasSlopeFactor (poked :: VkPipelineRasterizationStateCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkLineWidth (poked :: VkPipelineRasterizationStateCreateInfo))
-- ** VkPipelineShaderStageCreateFlags

-- No documentation found for TopLevel "VkPipelineShaderStageCreateFlags"
newtype VkPipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineShaderStageCreateFlags where
  
  showsPrec p (VkPipelineShaderStageCreateFlags x) = showParen (p >= 11) (showString "VkPipelineShaderStageCreateFlags " . showsPrec 11 x)

instance Read VkPipelineShaderStageCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineShaderStageCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineShaderStageCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineShaderStageCreateInfo"
data VkPipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo
  { -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "flags"
  vkFlags :: VkPipelineShaderStageCreateFlags
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "stage"
  vkStage :: VkShaderStageFlagBits
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "module"
  vkModule :: VkShaderModule
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "pName"
  vkPName :: Ptr CChar
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "pSpecializationInfo"
  vkPSpecializationInfo :: Ptr VkSpecializationInfo
  }
  deriving (Eq, Show)

instance Storable VkPipelineShaderStageCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineShaderStageCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkStage (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkModule (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPName (poked :: VkPipelineShaderStageCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPSpecializationInfo (poked :: VkPipelineShaderStageCreateInfo))
-- ** VkPipelineTessellationStateCreateFlags

-- No documentation found for TopLevel "VkPipelineTessellationStateCreateFlags"
newtype VkPipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineTessellationStateCreateFlags where
  
  showsPrec p (VkPipelineTessellationStateCreateFlags x) = showParen (p >= 11) (showString "VkPipelineTessellationStateCreateFlags " . showsPrec 11 x)

instance Read VkPipelineTessellationStateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineTessellationStateCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineTessellationStateCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineTessellationStateCreateInfo"
data VkPipelineTessellationStateCreateInfo = VkPipelineTessellationStateCreateInfo
  { -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "flags"
  vkFlags :: VkPipelineTessellationStateCreateFlags
  , -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "patchControlPoints"
  vkPatchControlPoints :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPipelineTessellationStateCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPipelineTessellationStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
                                                   <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineTessellationStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkPatchControlPoints (poked :: VkPipelineTessellationStateCreateInfo))
-- ** VkPipelineVertexInputStateCreateFlags

-- No documentation found for TopLevel "VkPipelineVertexInputStateCreateFlags"
newtype VkPipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineVertexInputStateCreateFlags where
  
  showsPrec p (VkPipelineVertexInputStateCreateFlags x) = showParen (p >= 11) (showString "VkPipelineVertexInputStateCreateFlags " . showsPrec 11 x)

instance Read VkPipelineVertexInputStateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineVertexInputStateCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineVertexInputStateCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineVertexInputStateCreateInfo"
data VkPipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo
  { -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "flags"
  vkFlags :: VkPipelineVertexInputStateCreateFlags
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "vertexBindingDescriptionCount"
  vkVertexBindingDescriptionCount :: Word32
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "pVertexBindingDescriptions"
  vkPVertexBindingDescriptions :: Ptr VkVertexInputBindingDescription
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "vertexAttributeDescriptionCount"
  vkVertexAttributeDescriptionCount :: Word32
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "pVertexAttributeDescriptions"
  vkPVertexAttributeDescriptions :: Ptr VkVertexInputAttributeDescription
  }
  deriving (Eq, Show)

instance Storable VkPipelineVertexInputStateCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineVertexInputStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 32)
                                                  <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkVertexBindingDescriptionCount (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPVertexBindingDescriptions (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkVertexAttributeDescriptionCount (poked :: VkPipelineVertexInputStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPVertexAttributeDescriptions (poked :: VkPipelineVertexInputStateCreateInfo))
-- ** VkPipelineViewportStateCreateFlags

-- No documentation found for TopLevel "VkPipelineViewportStateCreateFlags"
newtype VkPipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkPipelineViewportStateCreateFlags where
  
  showsPrec p (VkPipelineViewportStateCreateFlags x) = showParen (p >= 11) (showString "VkPipelineViewportStateCreateFlags " . showsPrec 11 x)

instance Read VkPipelineViewportStateCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineViewportStateCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineViewportStateCreateFlags v)
                        )
                    )


-- No documentation found for TopLevel "VkPipelineViewportStateCreateInfo"
data VkPipelineViewportStateCreateInfo = VkPipelineViewportStateCreateInfo
  { -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "flags"
  vkFlags :: VkPipelineViewportStateCreateFlags
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "viewportCount"
  vkViewportCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "pViewports"
  vkPViewports :: Ptr VkViewport
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "scissorCount"
  vkScissorCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "pScissors"
  vkPScissors :: Ptr VkRect2D
  }
  deriving (Eq, Show)

instance Storable VkPipelineViewportStateCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineViewportStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 20)
                                               <*> peek (ptr `plusPtr` 24)
                                               <*> peek (ptr `plusPtr` 32)
                                               <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkViewportCount (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPViewports (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkScissorCount (poked :: VkPipelineViewportStateCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPScissors (poked :: VkPipelineViewportStateCreateInfo))
-- ** VkPolygonMode

-- No documentation found for TopLevel "VkPolygonMode"
newtype VkPolygonMode = VkPolygonMode Int32
  deriving (Eq, Ord, Storable)

instance Show VkPolygonMode where
  showsPrec _ VK_POLYGON_MODE_FILL = showString "VK_POLYGON_MODE_FILL"
  showsPrec _ VK_POLYGON_MODE_LINE = showString "VK_POLYGON_MODE_LINE"
  showsPrec _ VK_POLYGON_MODE_POINT = showString "VK_POLYGON_MODE_POINT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkPolygonMode 1000153000) = showString "VK_POLYGON_MODE_FILL_RECTANGLE_NV"
  showsPrec p (VkPolygonMode x) = showParen (p >= 11) (showString "VkPolygonMode " . showsPrec 11 x)

instance Read VkPolygonMode where
  readPrec = parens ( choose [ ("VK_POLYGON_MODE_FILL",  pure VK_POLYGON_MODE_FILL)
                             , ("VK_POLYGON_MODE_LINE",  pure VK_POLYGON_MODE_LINE)
                             , ("VK_POLYGON_MODE_POINT", pure VK_POLYGON_MODE_POINT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_POLYGON_MODE_FILL_RECTANGLE_NV", pure (VkPolygonMode 1000153000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPolygonMode")
                        v <- step readPrec
                        pure (VkPolygonMode v)
                        )
                    )

-- No documentation found for Nested "VkPolygonMode" "VK_POLYGON_MODE_FILL"
pattern VK_POLYGON_MODE_FILL :: VkPolygonMode
pattern VK_POLYGON_MODE_FILL = VkPolygonMode 0

-- No documentation found for Nested "VkPolygonMode" "VK_POLYGON_MODE_LINE"
pattern VK_POLYGON_MODE_LINE :: VkPolygonMode
pattern VK_POLYGON_MODE_LINE = VkPolygonMode 1

-- No documentation found for Nested "VkPolygonMode" "VK_POLYGON_MODE_POINT"
pattern VK_POLYGON_MODE_POINT :: VkPolygonMode
pattern VK_POLYGON_MODE_POINT = VkPolygonMode 2
-- ** VkPrimitiveTopology

-- No documentation found for TopLevel "VkPrimitiveTopology"
newtype VkPrimitiveTopology = VkPrimitiveTopology Int32
  deriving (Eq, Ord, Storable)

instance Show VkPrimitiveTopology where
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_POINT_LIST = showString "VK_PRIMITIVE_TOPOLOGY_POINT_LIST"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_LIST = showString "VK_PRIMITIVE_TOPOLOGY_LINE_LIST"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = showString "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY = showString "VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY = showString "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = showString "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY"
  showsPrec _ VK_PRIMITIVE_TOPOLOGY_PATCH_LIST = showString "VK_PRIMITIVE_TOPOLOGY_PATCH_LIST"
  showsPrec p (VkPrimitiveTopology x) = showParen (p >= 11) (showString "VkPrimitiveTopology " . showsPrec 11 x)

instance Read VkPrimitiveTopology where
  readPrec = parens ( choose [ ("VK_PRIMITIVE_TOPOLOGY_POINT_LIST",                    pure VK_PRIMITIVE_TOPOLOGY_POINT_LIST)
                             , ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST",                     pure VK_PRIMITIVE_TOPOLOGY_LINE_LIST)
                             , ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP",                    pure VK_PRIMITIVE_TOPOLOGY_LINE_STRIP)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST",                 pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP",                pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN",                  pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN)
                             , ("VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY",      pure VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY)
                             , ("VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY",     pure VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY",  pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY)
                             , ("VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY", pure VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY)
                             , ("VK_PRIMITIVE_TOPOLOGY_PATCH_LIST",                    pure VK_PRIMITIVE_TOPOLOGY_PATCH_LIST)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPrimitiveTopology")
                        v <- step readPrec
                        pure (VkPrimitiveTopology v)
                        )
                    )

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_POINT_LIST"
pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST = VkPrimitiveTopology 0

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_LINE_LIST"
pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST = VkPrimitiveTopology 1

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP"
pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP = VkPrimitiveTopology 2

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST"
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = VkPrimitiveTopology 3

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP"
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = VkPrimitiveTopology 4

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN"
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = VkPrimitiveTopology 5

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY"
pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY = VkPrimitiveTopology 6

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY"
pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY = VkPrimitiveTopology 7

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY"
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY = VkPrimitiveTopology 8

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY"
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = VkPrimitiveTopology 9

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_PATCH_LIST"
pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST :: VkPrimitiveTopology
pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST = VkPrimitiveTopology 10
-- No documentation found for TopLevel "VkRect2D"
data VkRect2D = VkRect2D
  { -- No documentation found for Nested "VkRect2D" "offset"
  vkOffset :: VkOffset2D
  , -- No documentation found for Nested "VkRect2D" "extent"
  vkExtent :: VkExtent2D
  }
  deriving (Eq, Show)

instance Storable VkRect2D where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkRect2D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkOffset (poked :: VkRect2D))
                *> poke (ptr `plusPtr` 8) (vkExtent (poked :: VkRect2D))
-- | Dummy data to tag the 'Ptr' with
data VkRenderPass_T
-- No documentation found for TopLevel "VkRenderPass"
type VkRenderPass = Ptr VkRenderPass_T
-- No documentation found for TopLevel "VkSampleMask"
type VkSampleMask = Word32
-- ** VkShaderStageFlagBits

-- No documentation found for TopLevel "VkShaderStageFlagBits"
newtype VkShaderStageFlagBits = VkShaderStageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkShaderStageFlagBits where
  showsPrec _ VK_SHADER_STAGE_VERTEX_BIT = showString "VK_SHADER_STAGE_VERTEX_BIT"
  showsPrec _ VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = showString "VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT"
  showsPrec _ VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = showString "VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT"
  showsPrec _ VK_SHADER_STAGE_GEOMETRY_BIT = showString "VK_SHADER_STAGE_GEOMETRY_BIT"
  showsPrec _ VK_SHADER_STAGE_FRAGMENT_BIT = showString "VK_SHADER_STAGE_FRAGMENT_BIT"
  showsPrec _ VK_SHADER_STAGE_COMPUTE_BIT = showString "VK_SHADER_STAGE_COMPUTE_BIT"
  showsPrec _ VK_SHADER_STAGE_ALL_GRAPHICS = showString "VK_SHADER_STAGE_ALL_GRAPHICS"
  showsPrec _ VK_SHADER_STAGE_ALL = showString "VK_SHADER_STAGE_ALL"
  showsPrec p (VkShaderStageFlagBits x) = showParen (p >= 11) (showString "VkShaderStageFlagBits " . showsPrec 11 x)

instance Read VkShaderStageFlagBits where
  readPrec = parens ( choose [ ("VK_SHADER_STAGE_VERTEX_BIT",                  pure VK_SHADER_STAGE_VERTEX_BIT)
                             , ("VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT",    pure VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT)
                             , ("VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT", pure VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT)
                             , ("VK_SHADER_STAGE_GEOMETRY_BIT",                pure VK_SHADER_STAGE_GEOMETRY_BIT)
                             , ("VK_SHADER_STAGE_FRAGMENT_BIT",                pure VK_SHADER_STAGE_FRAGMENT_BIT)
                             , ("VK_SHADER_STAGE_COMPUTE_BIT",                 pure VK_SHADER_STAGE_COMPUTE_BIT)
                             , ("VK_SHADER_STAGE_ALL_GRAPHICS",                pure VK_SHADER_STAGE_ALL_GRAPHICS)
                             , ("VK_SHADER_STAGE_ALL",                         pure VK_SHADER_STAGE_ALL)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkShaderStageFlagBits")
                        v <- step readPrec
                        pure (VkShaderStageFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_VERTEX_BIT"
pattern VK_SHADER_STAGE_VERTEX_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_VERTEX_BIT = VkShaderStageFlagBits 0x00000001

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT"
pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = VkShaderStageFlagBits 0x00000002

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT"
pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = VkShaderStageFlagBits 0x00000004

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_GEOMETRY_BIT"
pattern VK_SHADER_STAGE_GEOMETRY_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_GEOMETRY_BIT = VkShaderStageFlagBits 0x00000008

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_FRAGMENT_BIT"
pattern VK_SHADER_STAGE_FRAGMENT_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_FRAGMENT_BIT = VkShaderStageFlagBits 0x00000010

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_COMPUTE_BIT"
pattern VK_SHADER_STAGE_COMPUTE_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_COMPUTE_BIT = VkShaderStageFlagBits 0x00000020

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_ALL_GRAPHICS"
pattern VK_SHADER_STAGE_ALL_GRAPHICS :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_ALL_GRAPHICS = VkShaderStageFlagBits 0x0000001f

-- No documentation found for Nested "VkShaderStageFlagBits" "VK_SHADER_STAGE_ALL"
pattern VK_SHADER_STAGE_ALL :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_ALL = VkShaderStageFlagBits 0x7fffffff
-- No documentation found for TopLevel "VkSpecializationInfo"
data VkSpecializationInfo = VkSpecializationInfo
  { -- No documentation found for Nested "VkSpecializationInfo" "mapEntryCount"
  vkMapEntryCount :: Word32
  , -- No documentation found for Nested "VkSpecializationInfo" "pMapEntries"
  vkPMapEntries :: Ptr VkSpecializationMapEntry
  , -- No documentation found for Nested "VkSpecializationInfo" "dataSize"
  vkDataSize :: CSize
  , -- No documentation found for Nested "VkSpecializationInfo" "pData"
  vkPData :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkSpecializationInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkSpecializationInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMapEntryCount (poked :: VkSpecializationInfo))
                *> poke (ptr `plusPtr` 8) (vkPMapEntries (poked :: VkSpecializationInfo))
                *> poke (ptr `plusPtr` 16) (vkDataSize (poked :: VkSpecializationInfo))
                *> poke (ptr `plusPtr` 24) (vkPData (poked :: VkSpecializationInfo))
-- No documentation found for TopLevel "VkSpecializationMapEntry"
data VkSpecializationMapEntry = VkSpecializationMapEntry
  { -- No documentation found for Nested "VkSpecializationMapEntry" "constantID"
  vkConstantID :: Word32
  , -- No documentation found for Nested "VkSpecializationMapEntry" "offset"
  vkOffset :: Word32
  , -- No documentation found for Nested "VkSpecializationMapEntry" "size"
  vkSize :: CSize
  }
  deriving (Eq, Show)

instance Storable VkSpecializationMapEntry where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkSpecializationMapEntry <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkConstantID (poked :: VkSpecializationMapEntry))
                *> poke (ptr `plusPtr` 4) (vkOffset (poked :: VkSpecializationMapEntry))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkSpecializationMapEntry))
-- ** VkStencilOp

-- No documentation found for TopLevel "VkStencilOp"
newtype VkStencilOp = VkStencilOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkStencilOp where
  showsPrec _ VK_STENCIL_OP_KEEP = showString "VK_STENCIL_OP_KEEP"
  showsPrec _ VK_STENCIL_OP_ZERO = showString "VK_STENCIL_OP_ZERO"
  showsPrec _ VK_STENCIL_OP_REPLACE = showString "VK_STENCIL_OP_REPLACE"
  showsPrec _ VK_STENCIL_OP_INCREMENT_AND_CLAMP = showString "VK_STENCIL_OP_INCREMENT_AND_CLAMP"
  showsPrec _ VK_STENCIL_OP_DECREMENT_AND_CLAMP = showString "VK_STENCIL_OP_DECREMENT_AND_CLAMP"
  showsPrec _ VK_STENCIL_OP_INVERT = showString "VK_STENCIL_OP_INVERT"
  showsPrec _ VK_STENCIL_OP_INCREMENT_AND_WRAP = showString "VK_STENCIL_OP_INCREMENT_AND_WRAP"
  showsPrec _ VK_STENCIL_OP_DECREMENT_AND_WRAP = showString "VK_STENCIL_OP_DECREMENT_AND_WRAP"
  showsPrec p (VkStencilOp x) = showParen (p >= 11) (showString "VkStencilOp " . showsPrec 11 x)

instance Read VkStencilOp where
  readPrec = parens ( choose [ ("VK_STENCIL_OP_KEEP",                pure VK_STENCIL_OP_KEEP)
                             , ("VK_STENCIL_OP_ZERO",                pure VK_STENCIL_OP_ZERO)
                             , ("VK_STENCIL_OP_REPLACE",             pure VK_STENCIL_OP_REPLACE)
                             , ("VK_STENCIL_OP_INCREMENT_AND_CLAMP", pure VK_STENCIL_OP_INCREMENT_AND_CLAMP)
                             , ("VK_STENCIL_OP_DECREMENT_AND_CLAMP", pure VK_STENCIL_OP_DECREMENT_AND_CLAMP)
                             , ("VK_STENCIL_OP_INVERT",              pure VK_STENCIL_OP_INVERT)
                             , ("VK_STENCIL_OP_INCREMENT_AND_WRAP",  pure VK_STENCIL_OP_INCREMENT_AND_WRAP)
                             , ("VK_STENCIL_OP_DECREMENT_AND_WRAP",  pure VK_STENCIL_OP_DECREMENT_AND_WRAP)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStencilOp")
                        v <- step readPrec
                        pure (VkStencilOp v)
                        )
                    )

-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_KEEP"
pattern VK_STENCIL_OP_KEEP :: VkStencilOp
pattern VK_STENCIL_OP_KEEP = VkStencilOp 0

-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_ZERO"
pattern VK_STENCIL_OP_ZERO :: VkStencilOp
pattern VK_STENCIL_OP_ZERO = VkStencilOp 1

-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_REPLACE"
pattern VK_STENCIL_OP_REPLACE :: VkStencilOp
pattern VK_STENCIL_OP_REPLACE = VkStencilOp 2

-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_INCREMENT_AND_CLAMP"
pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP :: VkStencilOp
pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP = VkStencilOp 3

-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_DECREMENT_AND_CLAMP"
pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP :: VkStencilOp
pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP = VkStencilOp 4

-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_INVERT"
pattern VK_STENCIL_OP_INVERT :: VkStencilOp
pattern VK_STENCIL_OP_INVERT = VkStencilOp 5

-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_INCREMENT_AND_WRAP"
pattern VK_STENCIL_OP_INCREMENT_AND_WRAP :: VkStencilOp
pattern VK_STENCIL_OP_INCREMENT_AND_WRAP = VkStencilOp 6

-- No documentation found for Nested "VkStencilOp" "VK_STENCIL_OP_DECREMENT_AND_WRAP"
pattern VK_STENCIL_OP_DECREMENT_AND_WRAP :: VkStencilOp
pattern VK_STENCIL_OP_DECREMENT_AND_WRAP = VkStencilOp 7
-- No documentation found for TopLevel "VkStencilOpState"
data VkStencilOpState = VkStencilOpState
  { -- No documentation found for Nested "VkStencilOpState" "failOp"
  vkFailOp :: VkStencilOp
  , -- No documentation found for Nested "VkStencilOpState" "passOp"
  vkPassOp :: VkStencilOp
  , -- No documentation found for Nested "VkStencilOpState" "depthFailOp"
  vkDepthFailOp :: VkStencilOp
  , -- No documentation found for Nested "VkStencilOpState" "compareOp"
  vkCompareOp :: VkCompareOp
  , -- No documentation found for Nested "VkStencilOpState" "compareMask"
  vkCompareMask :: Word32
  , -- No documentation found for Nested "VkStencilOpState" "writeMask"
  vkWriteMask :: Word32
  , -- No documentation found for Nested "VkStencilOpState" "reference"
  vkReference :: Word32
  }
  deriving (Eq, Show)

instance Storable VkStencilOpState where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek ptr = VkStencilOpState <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 4)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 12)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 20)
                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFailOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 4) (vkPassOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 8) (vkDepthFailOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 12) (vkCompareOp (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 16) (vkCompareMask (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 20) (vkWriteMask (poked :: VkStencilOpState))
                *> poke (ptr `plusPtr` 24) (vkReference (poked :: VkStencilOpState))
-- No documentation found for TopLevel "VkVertexInputAttributeDescription"
data VkVertexInputAttributeDescription = VkVertexInputAttributeDescription
  { -- No documentation found for Nested "VkVertexInputAttributeDescription" "location"
  vkLocation :: Word32
  , -- No documentation found for Nested "VkVertexInputAttributeDescription" "binding"
  vkBinding :: Word32
  , -- No documentation found for Nested "VkVertexInputAttributeDescription" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkVertexInputAttributeDescription" "offset"
  vkOffset :: Word32
  }
  deriving (Eq, Show)

instance Storable VkVertexInputAttributeDescription where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkVertexInputAttributeDescription <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 4)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLocation (poked :: VkVertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 4) (vkBinding (poked :: VkVertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 8) (vkFormat (poked :: VkVertexInputAttributeDescription))
                *> poke (ptr `plusPtr` 12) (vkOffset (poked :: VkVertexInputAttributeDescription))
-- No documentation found for TopLevel "VkVertexInputBindingDescription"
data VkVertexInputBindingDescription = VkVertexInputBindingDescription
  { -- No documentation found for Nested "VkVertexInputBindingDescription" "binding"
  vkBinding :: Word32
  , -- No documentation found for Nested "VkVertexInputBindingDescription" "stride"
  vkStride :: Word32
  , -- No documentation found for Nested "VkVertexInputBindingDescription" "inputRate"
  vkInputRate :: VkVertexInputRate
  }
  deriving (Eq, Show)

instance Storable VkVertexInputBindingDescription where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkVertexInputBindingDescription <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 4)
                                             <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBinding (poked :: VkVertexInputBindingDescription))
                *> poke (ptr `plusPtr` 4) (vkStride (poked :: VkVertexInputBindingDescription))
                *> poke (ptr `plusPtr` 8) (vkInputRate (poked :: VkVertexInputBindingDescription))
-- ** VkVertexInputRate

-- No documentation found for TopLevel "VkVertexInputRate"
newtype VkVertexInputRate = VkVertexInputRate Int32
  deriving (Eq, Ord, Storable)

instance Show VkVertexInputRate where
  showsPrec _ VK_VERTEX_INPUT_RATE_VERTEX = showString "VK_VERTEX_INPUT_RATE_VERTEX"
  showsPrec _ VK_VERTEX_INPUT_RATE_INSTANCE = showString "VK_VERTEX_INPUT_RATE_INSTANCE"
  showsPrec p (VkVertexInputRate x) = showParen (p >= 11) (showString "VkVertexInputRate " . showsPrec 11 x)

instance Read VkVertexInputRate where
  readPrec = parens ( choose [ ("VK_VERTEX_INPUT_RATE_VERTEX",   pure VK_VERTEX_INPUT_RATE_VERTEX)
                             , ("VK_VERTEX_INPUT_RATE_INSTANCE", pure VK_VERTEX_INPUT_RATE_INSTANCE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkVertexInputRate")
                        v <- step readPrec
                        pure (VkVertexInputRate v)
                        )
                    )

-- No documentation found for Nested "VkVertexInputRate" "VK_VERTEX_INPUT_RATE_VERTEX"
pattern VK_VERTEX_INPUT_RATE_VERTEX :: VkVertexInputRate
pattern VK_VERTEX_INPUT_RATE_VERTEX = VkVertexInputRate 0

-- No documentation found for Nested "VkVertexInputRate" "VK_VERTEX_INPUT_RATE_INSTANCE"
pattern VK_VERTEX_INPUT_RATE_INSTANCE :: VkVertexInputRate
pattern VK_VERTEX_INPUT_RATE_INSTANCE = VkVertexInputRate 1
-- No documentation found for TopLevel "VkViewport"
data VkViewport = VkViewport
  { -- No documentation found for Nested "VkViewport" "x"
  vkX :: CFloat
  , -- No documentation found for Nested "VkViewport" "y"
  vkY :: CFloat
  , -- No documentation found for Nested "VkViewport" "width"
  vkWidth :: CFloat
  , -- No documentation found for Nested "VkViewport" "height"
  vkHeight :: CFloat
  , -- No documentation found for Nested "VkViewport" "minDepth"
  vkMinDepth :: CFloat
  , -- No documentation found for Nested "VkViewport" "maxDepth"
  vkMaxDepth :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkViewport where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkViewport <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
                        <*> peek (ptr `plusPtr` 12)
                        <*> peek (ptr `plusPtr` 16)
                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkViewport))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkViewport))
                *> poke (ptr `plusPtr` 8) (vkWidth (poked :: VkViewport))
                *> poke (ptr `plusPtr` 12) (vkHeight (poked :: VkViewport))
                *> poke (ptr `plusPtr` 16) (vkMinDepth (poked :: VkViewport))
                *> poke (ptr `plusPtr` 20) (vkMaxDepth (poked :: VkViewport))
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreateComputePipelines"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateComputePipelines" vkCreateComputePipelines :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkComputePipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult

#endif
type FN_vkCreateComputePipelines = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkComputePipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
type PFN_vkCreateComputePipelines = FunPtr FN_vkCreateComputePipelines
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCreateGraphicsPipelines"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateGraphicsPipelines" vkCreateGraphicsPipelines :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkGraphicsPipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult

#endif
type FN_vkCreateGraphicsPipelines = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkGraphicsPipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
type PFN_vkCreateGraphicsPipelines = FunPtr FN_vkCreateGraphicsPipelines
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkDestroyPipeline"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyPipeline" vkDestroyPipeline :: ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyPipeline = ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyPipeline = FunPtr FN_vkDestroyPipeline
