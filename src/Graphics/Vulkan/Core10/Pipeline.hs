{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Pipeline
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
  , VkCompareOp(..)
  , pattern VK_COMPARE_OP_NEVER
  , pattern VK_COMPARE_OP_LESS
  , pattern VK_COMPARE_OP_EQUAL
  , pattern VK_COMPARE_OP_LESS_OR_EQUAL
  , pattern VK_COMPARE_OP_GREATER
  , pattern VK_COMPARE_OP_NOT_EQUAL
  , pattern VK_COMPARE_OP_GREATER_OR_EQUAL
  , pattern VK_COMPARE_OP_ALWAYS
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
  , VkPolygonMode(..)
  , pattern VK_POLYGON_MODE_FILL
  , pattern VK_POLYGON_MODE_LINE
  , pattern VK_POLYGON_MODE_POINT
  , VkFrontFace(..)
  , pattern VK_FRONT_FACE_COUNTER_CLOCKWISE
  , pattern VK_FRONT_FACE_CLOCKWISE
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
  , VkStencilOp(..)
  , pattern VK_STENCIL_OP_KEEP
  , pattern VK_STENCIL_OP_ZERO
  , pattern VK_STENCIL_OP_REPLACE
  , pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP
  , pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP
  , pattern VK_STENCIL_OP_INVERT
  , pattern VK_STENCIL_OP_INCREMENT_AND_WRAP
  , pattern VK_STENCIL_OP_DECREMENT_AND_WRAP
  , VkVertexInputRate(..)
  , pattern VK_VERTEX_INPUT_RATE_VERTEX
  , pattern VK_VERTEX_INPUT_RATE_INSTANCE
  , VkPipelineDepthStencilStateCreateFlags(..)
  , VkPipelineDynamicStateCreateFlags(..)
  , VkPipelineColorBlendStateCreateFlags(..)
  , VkPipelineMultisampleStateCreateFlags(..)
  , VkPipelineRasterizationStateCreateFlags(..)
  , VkPipelineViewportStateCreateFlags(..)
  , VkPipelineTessellationStateCreateFlags(..)
  , VkPipelineInputAssemblyStateCreateFlags(..)
  , VkPipelineVertexInputStateCreateFlags(..)
  , VkPipelineShaderStageCreateFlags(..)
  , VkShaderStageFlagBits(..)
  , pattern VK_SHADER_STAGE_VERTEX_BIT
  , pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
  , pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
  , pattern VK_SHADER_STAGE_GEOMETRY_BIT
  , pattern VK_SHADER_STAGE_FRAGMENT_BIT
  , pattern VK_SHADER_STAGE_COMPUTE_BIT
  , pattern VK_SHADER_STAGE_ALL_GRAPHICS
  , pattern VK_SHADER_STAGE_ALL
  , VkPipelineCreateFlagBits(..)
  , pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
  , pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
  , pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT
  , VkColorComponentFlagBits(..)
  , pattern VK_COLOR_COMPONENT_R_BIT
  , pattern VK_COLOR_COMPONENT_G_BIT
  , pattern VK_COLOR_COMPONENT_B_BIT
  , pattern VK_COLOR_COMPONENT_A_BIT
  , VkCullModeFlagBits(..)
  , pattern VK_CULL_MODE_FRONT_BIT
  , pattern VK_CULL_MODE_BACK_BIT
  , pattern VK_CULL_MODE_NONE
  , pattern VK_CULL_MODE_FRONT_AND_BACK
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
  , vkCreateGraphicsPipelines
  , vkCreateComputePipelines
  , vkDestroyPipeline
  , VkOffset2D(..)
  , VkExtent2D(..)
  , VkViewport(..)
  , VkRect2D(..)
  , VkSpecializationMapEntry(..)
  , VkSpecializationInfo(..)
  , VkPipelineShaderStageCreateInfo(..)
  , VkComputePipelineCreateInfo(..)
  , VkVertexInputBindingDescription(..)
  , VkVertexInputAttributeDescription(..)
  , VkPipelineVertexInputStateCreateInfo(..)
  , VkPipelineInputAssemblyStateCreateInfo(..)
  , VkPipelineTessellationStateCreateInfo(..)
  , VkPipelineViewportStateCreateInfo(..)
  , VkPipelineRasterizationStateCreateInfo(..)
  , VkPipelineMultisampleStateCreateInfo(..)
  , VkPipelineColorBlendAttachmentState(..)
  , VkPipelineColorBlendStateCreateInfo(..)
  , VkPipelineDynamicStateCreateInfo(..)
  , VkStencilOpState(..)
  , VkPipelineDepthStencilStateCreateInfo(..)
  , VkGraphicsPipelineCreateInfo(..)
  , VkPipelineCreateFlags
  , VkColorComponentFlags
  , VkCullModeFlags
  , VkSampleMask
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
  ( CFloat(..)
  , CChar(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkSampleCountFlagBits(..)
  , VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.PipelineCache
  ( VkPipelineCache
  )
import Graphics.Vulkan.Core10.Shader
  ( VkShaderModule
  )


-- ** VkBlendFactor

-- | VkBlendFactor - Framebuffer blending factors
--
-- = Description
-- #_description#
--
-- The semantics of each enum value is described in the table below:
--
-- > +-----------------------------------------+-------------------+--------+
-- > | VkBlendFactor                           | RGB Blend Factors | Alpha  |
-- > |                                         | (Sr,Sg,Sb) or     | Blend  |
-- > |                                         | (Dr,Dg,Db)        | Factor |
-- > |                                         |                   | (Sa or |
-- > |                                         |                   | Da)    |
-- > +=========================================+===================+========+
-- > | @VK_BLEND_FACTOR_ZERO@                  | (0,0,0)           | 0      |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_ONE@                   | (1,1,1)           | 1      |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_SRC_COLOR@             | (Rs0,Gs0,Bs0)     | As0    |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR@   | (1-Rs0,1-Gs0,1-Bs | 1-As0  |
-- > |                                         | 0)                |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_DST_COLOR@             | (Rd,Gd,Bd)        | Ad     |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR@   | (1-Rd,1-Gd,1-Bd)  | 1-Ad   |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_SRC_ALPHA@             | (As0,As0,As0)     | As0    |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA@   | (1-As0,1-As0,1-As | 1-As0  |
-- > |                                         | 0)                |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_DST_ALPHA@             | (Ad,Ad,Ad)        | Ad     |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA@   | (1-Ad,1-Ad,1-Ad)  | 1-Ad   |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_CONSTANT_COLOR@        | (Rc,Gc,Bc)        | Ac     |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COL | (1-Rc,1-Gc,1-Bc)  | 1-Ac   |
-- > | OR@                                     |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_CONSTANT_ALPHA@        | (Ac,Ac,Ac)        | Ac     |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALP | (1-Ac,1-Ac,1-Ac)  | 1-Ac   |
-- > | HA@                                     |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_SRC_ALPHA_SATURATE@    | (f,f,f); f =      | 1      |
-- > |                                         | min(As0,1-Ad)     |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_SRC1_COLOR@            | (Rs1,Gs1,Bs1)     | As1    |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@  | (1-Rs1,1-Gs1,1-Bs | 1-As1  |
-- > |                                         | 1)                |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_SRC1_ALPHA@            | (As1,As1,As1)     | As1    |
-- > +-----------------------------------------+-------------------+--------+
-- > | @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@  | (1-As1,1-As1,1-As | 1-As1  |
-- > |                                         | 1)                |        |
-- > +-----------------------------------------+-------------------+--------+
-- >
-- > Blend Factors
--
-- In this table, the following conventions are used:
--
-- -   Rs0,Gs0,Bs0 and As0 represent the first source color R, G, B, and A
--     components, respectively, for the fragment output location
--     corresponding to the color attachment being blended.
--
-- -   Rs1,Gs1,Bs1 and As1 represent the second source color R, G, B, and A
--     components, respectively, used in dual source blending modes, for
--     the fragment output location corresponding to the color attachment
--     being blended.
--
-- -   Rd,Gd,Bd and Ad represent the R, G, B, and A components of the
--     destination color. That is, the color currently in the corresponding
--     color attachment for this fragment\/sample.
--
-- -   Rc,Gc,Bc and Ac represent the blend constant R, G, B, and A
--     components, respectively.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineColorBlendAttachmentState'
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

-- | VkBlendOp - Framebuffer blending operations
--
-- = Description
-- #_description#
--
-- The semantics of each basic blend operations is described in the table
-- below:
--
-- > +-------------------------------+--------------------+----------------+
-- > | VkBlendOp                     | RGB Components     | Alpha          |
-- > |                               |                    | Component      |
-- > +===============================+====================+================+
-- > | @VK_BLEND_OP_ADD@             | R = Rs0 × Sr + Rd  | A = As0 × Sa + |
-- > |                               | × Dr               | Ad × Da        |
-- > |                               | G = Gs0 × Sg + Gd  |                |
-- > |                               | × Dg               |                |
-- > |                               | B = Bs0 × Sb + Bd  |                |
-- > |                               | × Db               |                |
-- > +-------------------------------+--------------------+----------------+
-- > | @VK_BLEND_OP_SUBTRACT@        | R = Rs0 × Sr - Rd  | A = As0 × Sa - |
-- > |                               | × Dr               | Ad × Da        |
-- > |                               | G = Gs0 × Sg - Gd  |                |
-- > |                               | × Dg               |                |
-- > |                               | B = Bs0 × Sb - Bd  |                |
-- > |                               | × Db               |                |
-- > +-------------------------------+--------------------+----------------+
-- > | @VK_BLEND_OP_REVERSE_SUBTRACT | R = Rd × Dr - Rs0  | A = Ad × Da -  |
-- > | @                             | × Sr               | As0 × Sa       |
-- > |                               | G = Gd × Dg - Gs0  |                |
-- > |                               | × Sg               |                |
-- > |                               | B = Bd × Db - Bs0  |                |
-- > |                               | × Sb               |                |
-- > +-------------------------------+--------------------+----------------+
-- > | @VK_BLEND_OP_MIN@             | R = min(Rs0,Rd)    | A =            |
-- > |                               | G = min(Gs0,Gd)    | min(As0,Ad)    |
-- > |                               | B = min(Bs0,Bd)    |                |
-- > +-------------------------------+--------------------+----------------+
-- > | @VK_BLEND_OP_MAX@             | R = max(Rs0,Rd)    | A =            |
-- > |                               | G = max(Gs0,Gd)    | max(As0,Ad)    |
-- > |                               | B = max(Bs0,Bd)    |                |
-- > +-------------------------------+--------------------+----------------+
-- >
-- > Basic Blend Operations
--
-- In this table, the following conventions are used:
--
-- -   Rs0, Gs0, Bs0 and As0 represent the first source color R, G, B, and
--     A components, respectively.
--
-- -   Rd, Gd, Bd and Ad represent the R, G, B, and A components of the
--     destination color. That is, the color currently in the corresponding
--     color attachment for this fragment\/sample.
--
-- -   Sr, Sg, Sb and Sa represent the source blend factor R, G, B, and A
--     components, respectively.
--
-- -   Dr, Dg, Db and Da represent the destination blend factor R, G, B,
--     and A components, respectively.
--
-- The blending operation produces a new set of values R, G, B and A, which
-- are written to the framebuffer attachment. If blending is not enabled
-- for this attachment, then R, G, B and A are assigned Rs0, Gs0, Bs0 and
-- As0, respectively.
--
-- If the color attachment is fixed-point, the components of the source and
-- destination values and blend factors are each clamped to [0,1] or [-1,1]
-- respectively for an unsigned normalized or signed normalized color
-- attachment prior to evaluating the blend operations. If the color
-- attachment is floating-point, no clamping occurs.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineColorBlendAttachmentState'
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
-- ** VkCompareOp

-- | VkCompareOp - Stencil comparison function
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineDepthStencilStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Sampler.VkSamplerCreateInfo', 'VkStencilOpState'
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

-- | @VK_COMPARE_OP_NEVER@ specifies that the test never passes.
pattern VK_COMPARE_OP_NEVER :: VkCompareOp
pattern VK_COMPARE_OP_NEVER = VkCompareOp 0

-- | @VK_COMPARE_OP_LESS@ specifies that the test passes when R \< S.
pattern VK_COMPARE_OP_LESS :: VkCompareOp
pattern VK_COMPARE_OP_LESS = VkCompareOp 1

-- | @VK_COMPARE_OP_EQUAL@ specifies that the test passes when R = S.
pattern VK_COMPARE_OP_EQUAL :: VkCompareOp
pattern VK_COMPARE_OP_EQUAL = VkCompareOp 2

-- | @VK_COMPARE_OP_LESS_OR_EQUAL@ specifies that the test passes when R ≤ S.
pattern VK_COMPARE_OP_LESS_OR_EQUAL :: VkCompareOp
pattern VK_COMPARE_OP_LESS_OR_EQUAL = VkCompareOp 3

-- | @VK_COMPARE_OP_GREATER@ specifies that the test passes when R > S.
pattern VK_COMPARE_OP_GREATER :: VkCompareOp
pattern VK_COMPARE_OP_GREATER = VkCompareOp 4

-- | @VK_COMPARE_OP_NOT_EQUAL@ specifies that the test passes when R ≠ S.
pattern VK_COMPARE_OP_NOT_EQUAL :: VkCompareOp
pattern VK_COMPARE_OP_NOT_EQUAL = VkCompareOp 5

-- | @VK_COMPARE_OP_GREATER_OR_EQUAL@ specifies that the test passes when R ≥
-- S.
pattern VK_COMPARE_OP_GREATER_OR_EQUAL :: VkCompareOp
pattern VK_COMPARE_OP_GREATER_OR_EQUAL = VkCompareOp 6

-- | @VK_COMPARE_OP_ALWAYS@ specifies that the test always passes.
pattern VK_COMPARE_OP_ALWAYS :: VkCompareOp
pattern VK_COMPARE_OP_ALWAYS = VkCompareOp 7
-- ** VkDynamicState

-- | VkDynamicState - Indicate which dynamic state is taken from dynamic
-- state commands
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineDynamicStateCreateInfo'
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

-- | @VK_DYNAMIC_STATE_VIEWPORT@ specifies that the @pViewports@ state in
-- @VkPipelineViewportStateCreateInfo@ will be ignored and /must/ be set
-- dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetViewport' before
-- any draw commands. The number of viewports used by a pipeline is still
-- specified by the @viewportCount@ member of
-- @VkPipelineViewportStateCreateInfo@.
pattern VK_DYNAMIC_STATE_VIEWPORT :: VkDynamicState
pattern VK_DYNAMIC_STATE_VIEWPORT = VkDynamicState 0

-- | @VK_DYNAMIC_STATE_SCISSOR@ specifies that the @pScissors@ state in
-- @VkPipelineViewportStateCreateInfo@ will be ignored and /must/ be set
-- dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetScissor' before
-- any draw commands. The number of scissor rectangles used by a pipeline
-- is still specified by the @scissorCount@ member of
-- @VkPipelineViewportStateCreateInfo@.
pattern VK_DYNAMIC_STATE_SCISSOR :: VkDynamicState
pattern VK_DYNAMIC_STATE_SCISSOR = VkDynamicState 1

-- | @VK_DYNAMIC_STATE_LINE_WIDTH@ specifies that the @lineWidth@ state in
-- @VkPipelineRasterizationStateCreateInfo@ will be ignored and /must/ be
-- set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetLineWidth' before
-- any draw commands that generate line primitives for the rasterizer.
pattern VK_DYNAMIC_STATE_LINE_WIDTH :: VkDynamicState
pattern VK_DYNAMIC_STATE_LINE_WIDTH = VkDynamicState 2

-- | @VK_DYNAMIC_STATE_DEPTH_BIAS@ specifies that the
-- @depthBiasConstantFactor@, @depthBiasClamp@ and @depthBiasSlopeFactor@
-- states in @VkPipelineRasterizationStateCreateInfo@ will be ignored and
-- /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetDepthBias' before
-- any draws are performed with @depthBiasEnable@ in
-- @VkPipelineRasterizationStateCreateInfo@ set to @VK_TRUE@.
pattern VK_DYNAMIC_STATE_DEPTH_BIAS :: VkDynamicState
pattern VK_DYNAMIC_STATE_DEPTH_BIAS = VkDynamicState 3

-- | @VK_DYNAMIC_STATE_BLEND_CONSTANTS@ specifies that the @blendConstants@
-- state in @VkPipelineColorBlendStateCreateInfo@ will be ignored and
-- /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetBlendConstants'
-- before any draws are performed with a pipeline state with
-- @VkPipelineColorBlendAttachmentState@ member @blendEnable@ set to
-- @VK_TRUE@ and any of the blend functions using a constant blend color.
pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS :: VkDynamicState
pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS = VkDynamicState 4

-- | @VK_DYNAMIC_STATE_DEPTH_BOUNDS@ specifies that the @minDepthBounds@ and
-- @maxDepthBounds@ states of 'VkPipelineDepthStencilStateCreateInfo' will
-- be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetDepthBounds'
-- before any draws are performed with a pipeline state with
-- @VkPipelineDepthStencilStateCreateInfo@ member @depthBoundsTestEnable@
-- set to @VK_TRUE@.
pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS :: VkDynamicState
pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS = VkDynamicState 5

-- | @VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK@ specifies that the @compareMask@
-- state in @VkPipelineDepthStencilStateCreateInfo@ for both @front@ and
-- @back@ will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask'
-- before any draws are performed with a pipeline state with
-- @VkPipelineDepthStencilStateCreateInfo@ member @stencilTestEnable@ set
-- to @VK_TRUE@
pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK :: VkDynamicState
pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK = VkDynamicState 6

-- | @VK_DYNAMIC_STATE_STENCIL_WRITE_MASK@ specifies that the @writeMask@
-- state in @VkPipelineDepthStencilStateCreateInfo@ for both @front@ and
-- @back@ will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetStencilWriteMask'
-- before any draws are performed with a pipeline state with
-- @VkPipelineDepthStencilStateCreateInfo@ member @stencilTestEnable@ set
-- to @VK_TRUE@
pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK :: VkDynamicState
pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK = VkDynamicState 7

-- | @VK_DYNAMIC_STATE_STENCIL_REFERENCE@ specifies that the @reference@
-- state in @VkPipelineDepthStencilStateCreateInfo@ for both @front@ and
-- @back@ will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetStencilReference'
-- before any draws are performed with a pipeline state with
-- @VkPipelineDepthStencilStateCreateInfo@ member @stencilTestEnable@ set
-- to @VK_TRUE@
pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE :: VkDynamicState
pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE = VkDynamicState 8
-- ** VkPolygonMode

-- | VkPolygonMode - Control polygon rasterization mode
--
-- = Description
-- #_description#
--
-- -   @VK_POLYGON_MODE_POINT@ specifies that polygon vertices are drawn as
--     points.
--
-- -   @VK_POLYGON_MODE_LINE@ specifies that polygon edges are drawn as
--     line segments.
--
-- -   @VK_POLYGON_MODE_FILL@ specifies that polygons are rendered using
--     the polygon rasterization rules in this section.
--
-- These modes affect only the final rasterization of polygons: in
-- particular, a polygon’s vertices are shaded and the polygon is clipped
-- and possibly culled before these modes are applied.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineRasterizationStateCreateInfo'
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
-- ** VkFrontFace

-- | VkFrontFace - Interpret polygon front-facing orientation
--
-- = Description
-- #_description#
--
-- -   @VK_FRONT_FACE_COUNTER_CLOCKWISE@ specifies that a triangle with
--     positive area is considered front-facing.
--
-- -   @VK_FRONT_FACE_CLOCKWISE@ specifies that a triangle with negative
--     area is considered front-facing.
--
-- Any triangle which is not front-facing is back-facing, including
-- zero-area triangles.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineRasterizationStateCreateInfo'
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
-- ** VkLogicOp

-- | VkLogicOp - Framebuffer logical operations
--
-- = Description
-- #_description#
--
-- The logical operations supported by Vulkan are summarized in the
-- following table in which
--
-- -   ¬ is bitwise invert,
--
-- -   ∧ is bitwise and,
--
-- -   ∨ is bitwise or,
--
-- -   ⊕ is bitwise exclusive or,
--
-- -   s is the fragment’s Rs0, Gs0, Bs0 or As0 component value for the
--     fragment output corresponding to the color attachment being updated,
--     and
--
-- -   d is the color attachment’s R, G, B or A component value:
--
-- > +-----------------------------------+-----------------------------------+
-- > | Mode                              | Operation                         |
-- > +===================================+===================================+
-- > | @VK_LOGIC_OP_CLEAR@               | 0                                 |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_AND@                 | s ∧ d                             |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_AND_REVERSE@         | s ∧ ¬ d                           |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_COPY@                | s                                 |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_AND_INVERTED@        | ¬ s ∧ d                           |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_NO_OP@               | d                                 |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_XOR@                 | s ⊕ d                             |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_OR@                  | s ∨ d                             |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_NOR@                 | ¬ (s ∨ d)                         |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_EQUIVALENT@          | ¬ (s ⊕ d)                         |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_INVERT@              | ¬ d                               |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_OR_REVERSE@          | s ∨ ¬ d                           |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_COPY_INVERTED@       | ¬ s                               |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_OR_INVERTED@         | ¬ s ∨ d                           |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_NAND@                | ¬ (s ∧ d)                         |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_LOGIC_OP_SET@                 | all 1s                            |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Logical Operations
--
-- The result of the logical operation is then written to the color
-- attachment as controlled by the component write mask, described in
-- <{html_spec_relative}#framebuffer-blendoperations Blend Operations>.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineColorBlendStateCreateInfo'
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
-- ** VkPrimitiveTopology

-- | VkPrimitiveTopology - Supported primitive topologies
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineInputAssemblyStateCreateInfo'
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
-- ** VkStencilOp

-- | VkStencilOp - Stencil comparison function
--
-- = Description
-- #_description#
--
-- -   @VK_STENCIL_OP_KEEP@ keeps the current value.
--
-- -   @VK_STENCIL_OP_ZERO@ sets the value to 0.
--
-- -   @VK_STENCIL_OP_REPLACE@ sets the value to @reference@.
--
-- -   @VK_STENCIL_OP_INCREMENT_AND_CLAMP@ increments the current value and
--     clamps to the maximum representable unsigned value.
--
-- -   @VK_STENCIL_OP_DECREMENT_AND_CLAMP@ decrements the current value and
--     clamps to 0.
--
-- -   @VK_STENCIL_OP_INVERT@ bitwise-inverts the current value.
--
-- -   @VK_STENCIL_OP_INCREMENT_AND_WRAP@ increments the current value and
--     wraps to 0 when the maximum value would have been exceeded.
--
-- -   @VK_STENCIL_OP_DECREMENT_AND_WRAP@ decrements the current value and
--     wraps to the maximum possible value when the value would go below 0.
--
-- For purposes of increment and decrement, the stencil bits are considered
-- as an unsigned integer.
--
-- If the stencil test fails, the sample’s coverage bit is cleared in the
-- fragment. If there is no stencil framebuffer attachment, stencil
-- modification /cannot/ occur, and it is as if the stencil tests always
-- pass.
--
-- If the stencil test passes, the @writeMask@ member of the
-- 'VkStencilOpState' structures controls how the updated stencil value is
-- written to the stencil framebuffer attachment.
--
-- The least significant s bits of @writeMask@, where s is the number of
-- bits in the stencil framebuffer attachment, specify an integer mask.
-- Where a 1 appears in this mask, the corresponding bit in the stencil
-- value in the depth\/stencil attachment is written; where a 0 appears,
-- the bit is not written. The @writeMask@ value uses either the
-- front-facing or back-facing state based on the facingness of the
-- fragment. Fragments generated by front-facing primitives use the front
-- mask and fragments generated by back-facing primitives use the back
-- mask.
--
-- = See Also
-- #_see_also#
--
-- 'VkStencilOpState'
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
-- ** VkVertexInputRate

-- | VkVertexInputRate - Specify rate at which vertex attributes are pulled
-- from buffers
--
-- = See Also
-- #_see_also#
--
-- 'VkVertexInputBindingDescription'
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

-- | @VK_VERTEX_INPUT_RATE_VERTEX@ specifies that vertex attribute addressing
-- is a function of the vertex index.
pattern VK_VERTEX_INPUT_RATE_VERTEX :: VkVertexInputRate
pattern VK_VERTEX_INPUT_RATE_VERTEX = VkVertexInputRate 0

-- | @VK_VERTEX_INPUT_RATE_INSTANCE@ specifies that vertex attribute
-- addressing is a function of the instance index.
pattern VK_VERTEX_INPUT_RATE_INSTANCE :: VkVertexInputRate
pattern VK_VERTEX_INPUT_RATE_INSTANCE = VkVertexInputRate 1
-- ** VkPipelineDepthStencilStateCreateFlags

-- | VkPipelineDepthStencilStateCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineDepthStencilStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineDepthStencilStateCreateInfo'
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


-- ** VkPipelineDynamicStateCreateFlags

-- | VkPipelineDynamicStateCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineDynamicStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineDynamicStateCreateInfo'
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


-- ** VkPipelineColorBlendStateCreateFlags

-- | VkPipelineColorBlendStateCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineColorBlendStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineColorBlendStateCreateInfo'
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


-- ** VkPipelineMultisampleStateCreateFlags

-- | VkPipelineMultisampleStateCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineMultisampleStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineMultisampleStateCreateInfo'
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


-- ** VkPipelineRasterizationStateCreateFlags

-- | VkPipelineRasterizationStateCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineRasterizationStateCreateFlags@ is a bitmask type for setting
-- a mask, but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineRasterizationStateCreateInfo'
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


-- ** VkPipelineViewportStateCreateFlags

-- | VkPipelineViewportStateCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineViewportStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineViewportStateCreateInfo'
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


-- ** VkPipelineTessellationStateCreateFlags

-- | VkPipelineTessellationStateCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineTessellationStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineTessellationStateCreateInfo'
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


-- ** VkPipelineInputAssemblyStateCreateFlags

-- | VkPipelineInputAssemblyStateCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineInputAssemblyStateCreateFlags@ is a bitmask type for setting
-- a mask, but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineInputAssemblyStateCreateInfo'
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


-- ** VkPipelineVertexInputStateCreateFlags

-- | VkPipelineVertexInputStateCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineVertexInputStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineVertexInputStateCreateInfo'
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


-- ** VkPipelineShaderStageCreateFlags

-- | VkPipelineShaderStageCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkPipelineShaderStageCreateFlags@ is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineShaderStageCreateInfo'
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


-- ** VkShaderStageFlagBits

-- | VkShaderStageFlagBits - Bitmask specifying a pipeline stage
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkShaderStageFlags',
-- 'Graphics.Vulkan.Extensions.VK_AMD_shader_info.vkGetShaderInfoAMD'
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

-- | @VK_SHADER_STAGE_VERTEX_BIT@ specifies the vertex stage.
pattern VK_SHADER_STAGE_VERTEX_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_VERTEX_BIT = VkShaderStageFlagBits 0x00000001

-- | @VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT@ specifies the tessellation
-- control stage.
pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT = VkShaderStageFlagBits 0x00000002

-- | @VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT@ specifies the tessellation
-- evaluation stage.
pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT = VkShaderStageFlagBits 0x00000004

-- | @VK_SHADER_STAGE_GEOMETRY_BIT@ specifies the geometry stage.
pattern VK_SHADER_STAGE_GEOMETRY_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_GEOMETRY_BIT = VkShaderStageFlagBits 0x00000008

-- | @VK_SHADER_STAGE_FRAGMENT_BIT@ specifies the fragment stage.
pattern VK_SHADER_STAGE_FRAGMENT_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_FRAGMENT_BIT = VkShaderStageFlagBits 0x00000010

-- | @VK_SHADER_STAGE_COMPUTE_BIT@ specifies the compute stage.
pattern VK_SHADER_STAGE_COMPUTE_BIT :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_COMPUTE_BIT = VkShaderStageFlagBits 0x00000020

-- | @VK_SHADER_STAGE_ALL_GRAPHICS@ is a combination of bits used as
-- shorthand to specify all graphics stages defined above (excluding the
-- compute stage).
pattern VK_SHADER_STAGE_ALL_GRAPHICS :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_ALL_GRAPHICS = VkShaderStageFlagBits 0x0000001f

-- | @VK_SHADER_STAGE_ALL@ is a combination of bits used as shorthand to
-- specify all shader stages supported by the device, including all
-- additional stages which are introduced by extensions.
pattern VK_SHADER_STAGE_ALL :: VkShaderStageFlagBits
pattern VK_SHADER_STAGE_ALL = VkShaderStageFlagBits 0x7fffffff
-- ** VkPipelineCreateFlagBits

-- | VkPipelineCreateFlagBits - Bitmask controlling how a pipeline is created
--
-- = Description
-- #_description#
--
-- -   @VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT@ specifies that the
--     created pipeline will not be optimized. Using this flag /may/ reduce
--     the time taken to create the pipeline.
--
-- -   @VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT@ specifies that the
--     pipeline to be created is allowed to be the parent of a pipeline
--     that will be created in a subsequent call to
--     'vkCreateGraphicsPipelines' or 'vkCreateComputePipelines'.
--
-- -   @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ specifies that the pipeline to
--     be created will be a child of a previously created parent pipeline.
--
-- It is valid to set both @VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT@ and
-- @VK_PIPELINE_CREATE_DERIVATIVE_BIT@. This allows a pipeline to be both a
-- parent and possibly a child in a pipeline hierarchy. See
-- <{html_spec_relative}#pipelines-pipeline-derivatives Pipeline Derivatives>
-- for more information.
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineCreateFlags'
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
-- ** VkColorComponentFlagBits

-- | VkColorComponentFlagBits - Bitmask controlling which components are
-- written to the framebuffer
--
-- = Description
-- #_description#
--
-- -   @VK_COLOR_COMPONENT_R_BIT@ specifies that the R value is written to
--     the color attachment for the appropriate sample. Otherwise, the
--     value in memory is unmodified.
--
-- -   @VK_COLOR_COMPONENT_G_BIT@ specifies that the G value is written to
--     the color attachment for the appropriate sample. Otherwise, the
--     value in memory is unmodified.
--
-- -   @VK_COLOR_COMPONENT_B_BIT@ specifies that the B value is written to
--     the color attachment for the appropriate sample. Otherwise, the
--     value in memory is unmodified.
--
-- -   @VK_COLOR_COMPONENT_A_BIT@ specifies that the A value is written to
--     the color attachment for the appropriate sample. Otherwise, the
--     value in memory is unmodified.
--
-- The color write mask operation is applied regardless of whether blending
-- is enabled.
--
-- = See Also
-- #_see_also#
--
-- 'VkColorComponentFlags'
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
-- ** VkCullModeFlagBits

-- | VkCullModeFlagBits - Bitmask controlling triangle culling
--
-- = Description
-- #_description#
--
-- -   @VK_CULL_MODE_NONE@ specifies that no triangles are discarded
--
-- -   @VK_CULL_MODE_FRONT_BIT@ specifies that front-facing triangles are
--     discarded
--
-- -   @VK_CULL_MODE_BACK_BIT@ specifies that back-facing triangles are
--     discarded
--
-- -   @VK_CULL_MODE_FRONT_AND_BACK@ specifies that all triangles are
--     discarded.
--
-- Following culling, fragments are produced for any triangles which have
-- not been discarded.
--
-- = See Also
-- #_see_also#
--
-- 'VkCullModeFlags'
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
-- | Dummy data to tag the 'Ptr' with
data VkPipeline_T
-- | VkPipeline - Opaque handle to a pipeline object
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkComputePipelineCreateInfo', 'VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkObjectTablePipelineEntryNVX',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindPipeline',
-- 'vkCreateComputePipelines', 'vkCreateGraphicsPipelines',
-- 'vkDestroyPipeline',
-- 'Graphics.Vulkan.Extensions.VK_AMD_shader_info.vkGetShaderInfoAMD'
type VkPipeline = Ptr VkPipeline_T
-- | Dummy data to tag the 'Ptr' with
data VkPipelineLayout_T
-- | VkPipelineLayout - Opaque handle to a pipeline layout object
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkObjectTableDescriptorSetEntryNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkObjectTablePushConstantEntryNVX',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdPushConstants',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.Core10.PipelineLayout.vkCreatePipelineLayout',
-- 'Graphics.Vulkan.Core10.PipelineLayout.vkDestroyPipelineLayout'
type VkPipelineLayout = Ptr VkPipelineLayout_T
-- | Dummy data to tag the 'Ptr' with
data VkRenderPass_T
-- | VkRenderPass - Opaque handle to a render pass object
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.Core10.Pass.VkFramebufferCreateInfo',
-- 'VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'Graphics.Vulkan.Core10.Pass.vkCreateRenderPass',
-- 'Graphics.Vulkan.Core10.Pass.vkDestroyRenderPass',
-- 'Graphics.Vulkan.Core10.Pass.vkGetRenderAreaGranularity'
type VkRenderPass = Ptr VkRenderPass_T
-- | vkCreateGraphicsPipelines - Create graphics pipelines
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that creates the graphics pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', indicating that
--     pipeline caching is disabled; or the handle of a valid
--     <{html_spec_relative}#pipelines-cache pipeline cache> object, in
--     which case use of that cache is enabled for the duration of the
--     command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is an array of @VkGraphicsPipelineCreateInfo@
--     structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting
--     graphics pipeline objects are returned.
--
-- = Description
-- #_description#
--
-- The 'VkGraphicsPipelineCreateInfo' structure includes an array of shader
-- create info structures containing all the desired active shader stages,
-- as well as creation info to define all relevant fixed-function stages,
-- and a pipeline layout.
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag, and the
--     @basePipelineIndex@ member of that same element is not @-1@,
--     @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag, the base pipeline /must/
--     have been created with the
--     @VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT@ flag set
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @pipelineCache@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @pipelineCache@
--     /must/ be a valid @VkPipelineCache@ handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid @VkGraphicsPipelineCreateInfo@ structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ @VkPipeline@ handles
--
-- -   @createInfoCount@ /must/ be greater than @0@
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INVALID_SHADER_NV@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkGraphicsPipelineCreateInfo', 'VkPipeline',
-- 'Graphics.Vulkan.Core10.PipelineCache.VkPipelineCache'
foreign import ccall "vkCreateGraphicsPipelines" vkCreateGraphicsPipelines :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkGraphicsPipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
-- | vkCreateComputePipelines - Creates a new compute pipeline object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that creates the compute pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', indicating that
--     pipeline caching is disabled; or the handle of a valid
--     <{html_spec_relative}#pipelines-cache pipeline cache> object, in
--     which case use of that cache is enabled for the duration of the
--     command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is an array of @VkComputePipelineCreateInfo@
--     structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting compute
--     pipeline objects are returned.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag, and the
--     @basePipelineIndex@ member of that same element is not @-1@,
--     @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag, the base pipeline /must/
--     have been created with the
--     @VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT@ flag set
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @pipelineCache@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @pipelineCache@
--     /must/ be a valid @VkPipelineCache@ handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid @VkComputePipelineCreateInfo@ structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ @VkPipeline@ handles
--
-- -   @createInfoCount@ /must/ be greater than @0@
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INVALID_SHADER_NV@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkPipeline',
-- 'Graphics.Vulkan.Core10.PipelineCache.VkPipelineCache'
foreign import ccall "vkCreateComputePipelines" vkCreateComputePipelines :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkComputePipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
-- | vkDestroyPipeline - Destroy a pipeline object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that destroys the pipeline.
--
-- -   @pipeline@ is the handle of the pipeline to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @pipeline@ /must/ have
--     completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @pipeline@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @pipeline@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @pipeline@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @pipeline@ /must/
--     be a valid @VkPipeline@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @pipeline@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @pipeline@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkPipeline'
foreign import ccall "vkDestroyPipeline" vkDestroyPipeline :: ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkOffset2D - Structure specifying a two-dimensional offset
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR',
-- 'VkRect2D',
-- 'Graphics.Vulkan.Extensions.VK_KHR_incremental_present.VkRectLayerKHR'
data VkOffset2D = VkOffset2D
  { -- No documentation found for Nested "VkOffset2D" "vkX"
  vkX :: Int32
  , -- No documentation found for Nested "VkOffset2D" "vkY"
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
-- | VkExtent2D - Structure specifying a two-dimensional extent
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayModeParametersKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplaySurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkMultisamplePropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT',
-- 'VkRect2D',
-- 'Graphics.Vulkan.Extensions.VK_KHR_incremental_present.VkRectLayerKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkSampleLocationsInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.Pass.vkGetRenderAreaGranularity'
data VkExtent2D = VkExtent2D
  { -- No documentation found for Nested "VkExtent2D" "vkWidth"
  vkWidth :: Word32
  , -- No documentation found for Nested "VkExtent2D" "vkHeight"
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
-- | VkViewport - Structure specifying a viewport
--
-- = Description
-- #_description#
--
-- The framebuffer depth coordinate @z@f /may/ be represented using either
-- a fixed-point or floating-point representation. However, a
-- floating-point representation /must/ be used if the depth\/stencil
-- attachment has a floating-point depth component. If an m-bit fixed-point
-- representation is used, we assume that it represents each value
-- <<data:image/png;base64,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 stem 64a1b37b405b2e2dc919e83a12e9feba>>,
-- where k ∈ { 0, 1, …​, 2m-1 }, as k (e.g. 1.0 is represented in binary as
-- a string of all ones).
--
-- The viewport parameters shown in the above equations are found from
-- these values as
--
-- []
--     ox = @x@ + @width@ \/ 2
--
-- []
--     oy = @y@ + @height@ \/ 2
--
-- []
--     oz = @minDepth@
--
-- []
--     px = @width@
--
-- []
--     py = @height@
--
-- []
--     pz = @maxDepth@ - @minDepth@.
--
-- The width and height of the
-- <{html_spec_relative}#features-limits-maxViewportDimensions implementation-dependent maximum viewport dimensions>
-- /must/ be greater than or equal to the width and height of the largest
-- image which /can/ be created and attached to a framebuffer.
--
-- The floating-point viewport bounds are represented with an
-- <{html_spec_relative}#features-limits-viewportSubPixelBits implementation-dependent precision>.
--
-- == Valid Usage
--
-- -   @width@ /must/ be greater than @0.0@
--
-- -   @width@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxViewportDimensions@[0]
--
-- -   @height@ /must/ be greater than @0.0@
--
-- -   The absolute value of @height@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxViewportDimensions@[1]
--
-- -   @x@ /must/ be greater than or equal to @viewportBoundsRange@[0]
--
-- -   (@x@ + @width@) /must/ be less than or equal to
--     @viewportBoundsRange@[1]
--
-- -   @y@ /must/ be greater than or equal to @viewportBoundsRange@[0]
--
-- -   (@y@ + @height@) /must/ be less than or equal to
--     @viewportBoundsRange@[1]
--
-- -   @minDepth@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- -   @maxDepth@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineViewportStateCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetViewport'
data VkViewport = VkViewport
  { -- No documentation found for Nested "VkViewport" "vkX"
  vkX :: CFloat
  , -- No documentation found for Nested "VkViewport" "vkY"
  vkY :: CFloat
  , -- No documentation found for Nested "VkViewport" "vkWidth"
  vkWidth :: CFloat
  , -- No documentation found for Nested "VkViewport" "vkHeight"
  vkHeight :: CFloat
  , -- No documentation found for Nested "VkViewport" "vkMinDepth"
  vkMinDepth :: CFloat
  , -- No documentation found for Nested "VkViewport" "vkMaxDepth"
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
-- | VkRect2D - Structure specifying a two-dimensional subregion
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkClearRect',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupRenderPassBeginInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.VkDisplayPresentInfoKHR',
-- 'VkExtent2D', 'VkOffset2D',
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.VkPipelineDiscardRectangleStateCreateInfoEXT',
-- 'VkPipelineViewportStateCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.vkCmdSetDiscardRectangleEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetScissor',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkGetPhysicalDevicePresentRectanglesKHR'
data VkRect2D = VkRect2D
  { -- No documentation found for Nested "VkRect2D" "vkOffset"
  vkOffset :: VkOffset2D
  , -- No documentation found for Nested "VkRect2D" "vkExtent"
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
-- | VkSpecializationMapEntry - Structure specifying a specialization map
-- entry
--
-- = Description
-- #_description#
--
-- If a @constantID@ value is not a specialization constant ID used in the
-- shader, that map entry does not affect the behavior of the pipeline.
--
-- == Valid Usage
--
-- -   For a @constantID@ specialization constant declared in a shader,
--     @size@ /must/ match the byte size of the @constantID@. If the
--     specialization constant is of type @boolean@, @size@ /must/ be the
--     byte size of @VkBool32@
--
-- = See Also
-- #_see_also#
--
-- 'VkSpecializationInfo'
data VkSpecializationMapEntry = VkSpecializationMapEntry
  { -- No documentation found for Nested "VkSpecializationMapEntry" "vkConstantID"
  vkConstantID :: Word32
  , -- No documentation found for Nested "VkSpecializationMapEntry" "vkOffset"
  vkOffset :: Word32
  , -- No documentation found for Nested "VkSpecializationMapEntry" "vkSize"
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
-- | VkSpecializationInfo - Structure specifying specialization info
--
-- = Description
-- #_description#
--
-- @pMapEntries@ points to a structure of type 'VkSpecializationMapEntry'.
--
-- == Valid Usage
--
-- -   The @offset@ member of each element of @pMapEntries@ /must/ be less
--     than @dataSize@
--
-- -   The @size@ member of each element of @pMapEntries@ /must/ be less
--     than or equal to @dataSize@ minus @offset@
--
-- -   If @mapEntryCount@ is not @0@, @pMapEntries@ /must/ be a valid
--     pointer to an array of @mapEntryCount@ valid
--     @VkSpecializationMapEntry@ structures
--
-- == Valid Usage (Implicit)
--
-- -   If @dataSize@ is not @0@, @pData@ /must/ be a valid pointer to an
--     array of @dataSize@ bytes
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineShaderStageCreateInfo', 'VkSpecializationMapEntry'
data VkSpecializationInfo = VkSpecializationInfo
  { -- No documentation found for Nested "VkSpecializationInfo" "vkMapEntryCount"
  vkMapEntryCount :: Word32
  , -- No documentation found for Nested "VkSpecializationInfo" "vkPMapEntries"
  vkPMapEntries :: Ptr VkSpecializationMapEntry
  , -- No documentation found for Nested "VkSpecializationInfo" "vkDataSize"
  vkDataSize :: CSize
  , -- No documentation found for Nested "VkSpecializationInfo" "vkPData"
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
-- | VkPipelineShaderStageCreateInfo - Structure specifying parameters of a
-- newly created pipeline shader stage
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @stage@ /must/ not be
--     @VK_SHADER_STAGE_GEOMETRY_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
--     feature is not enabled, @stage@ /must/ not be
--     @VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT@ or
--     @VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT@
--
-- -   @stage@ /must/ not be @VK_SHADER_STAGE_ALL_GRAPHICS@, or
--     @VK_SHADER_STAGE_ALL@
--
-- -   @pName@ /must/ be the name of an @OpEntryPoint@ in @module@ with an
--     execution model that matches @stage@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the @ClipDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     @VkPhysicalDeviceLimits@::@maxClipDistances@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the @CullDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     @VkPhysicalDeviceLimits@::@maxCullDistances@
--
-- -   If the identified entry point includes any variables in its
--     interface that are declared with the @ClipDistance@ or
--     @CullDistance@ @BuiltIn@ decoration, those variables /must/ not have
--     array sizes which sum to more than
--     @VkPhysicalDeviceLimits@::@maxCombinedClipAndCullDistances@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the @SampleMask@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     @VkPhysicalDeviceLimits@::@maxSampleMaskWords@
--
-- -   If @stage@ is @VK_SHADER_STAGE_VERTEX_BIT@, the identified entry
--     point /must/ not include any input variable in its interface that is
--     decorated with @CullDistance@
--
-- -   If @stage@ is @VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT@ or
--     @VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT@, and the identified
--     entry point has an @OpExecutionMode@ instruction that specifies a
--     patch size with @OutputVertices@, the patch size /must/ be greater
--     than @0@ and less than or equal to
--     @VkPhysicalDeviceLimits@::@maxTessellationPatchSize@
--
-- -   If @stage@ is @VK_SHADER_STAGE_GEOMETRY_BIT@, the identified entry
--     point /must/ have an @OpExecutionMode@ instruction that specifies a
--     maximum output vertex count that is greater than @0@ and less than
--     or equal to @VkPhysicalDeviceLimits@::@maxGeometryOutputVertices@
--
-- -   If @stage@ is @VK_SHADER_STAGE_GEOMETRY_BIT@, the identified entry
--     point /must/ have an @OpExecutionMode@ instruction that specifies an
--     invocation count that is greater than @0@ and less than or equal to
--     @VkPhysicalDeviceLimits@::@maxGeometryShaderInvocations@
--
-- -   If @stage@ is @VK_SHADER_STAGE_GEOMETRY_BIT@, and the identified
--     entry point writes to @Layer@ for any primitive, it /must/ write the
--     same value to @Layer@ for all vertices of a given primitive
--
-- -   If @stage@ is @VK_SHADER_STAGE_GEOMETRY_BIT@, and the identified
--     entry point writes to @ViewportIndex@ for any primitive, it /must/
--     write the same value to @ViewportIndex@ for all vertices of a given
--     primitive
--
-- -   If @stage@ is @VK_SHADER_STAGE_FRAGMENT_BIT@, the identified entry
--     point /must/ not include any output variables in its interface
--     decorated with @CullDistance@
--
-- -   If @stage@ is @VK_SHADER_STAGE_FRAGMENT_BIT@, and the identified
--     entry point writes to @FragDepth@ in any execution path, it /must/
--     write to @FragDepth@ in all execution paths
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @stage@ /must/ be a valid 'VkShaderStageFlagBits' value
--
-- -   @module@ /must/ be a valid @VkShaderModule@ handle
--
-- -   @pName@ /must/ be a null-terminated UTF-8 string
--
-- -   If @pSpecializationInfo@ is not @NULL@, @pSpecializationInfo@ /must/
--     be a valid pointer to a valid @VkSpecializationInfo@ structure
--
-- = See Also
-- #_see_also#
--
-- 'VkComputePipelineCreateInfo', 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineShaderStageCreateFlags',
-- 'Graphics.Vulkan.Core10.Shader.VkShaderModule', 'VkShaderStageFlagBits',
-- 'VkSpecializationInfo', 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo
  { -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "vkFlags"
  vkFlags :: VkPipelineShaderStageCreateFlags
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "vkStage"
  vkStage :: VkShaderStageFlagBits
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "vkModule"
  vkModule :: VkShaderModule
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "vkPName"
  vkPName :: Ptr CChar
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "vkPSpecializationInfo"
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
-- | VkComputePipelineCreateInfo - Structure specifying parameters of a newly
-- created compute pipeline
--
-- = Description
-- #_description#
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <{html_spec_relative}#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- @stage@ points to a structure of type @VkPipelineShaderStageCreateInfo@.
--
-- == Valid Usage
--
-- -   If @flags@ contains the @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag,
--     and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be a
--     valid handle to a compute @VkPipeline@
--
-- -   If @flags@ contains the @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag,
--     and @basePipelineHandle@ is
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be a valid index into the calling
--     command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag,
--     and @basePipelineIndex@ is not -1, @basePipelineHandle@ /must/ be
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @flags@ contains the @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag,
--     and @basePipelineHandle@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be -1
--
-- -   The @stage@ member of @stage@ /must/ be
--     @VK_SHADER_STAGE_COMPUTE_BIT@
--
-- -   The shader code for the entry point identified by @stage@ and the
--     rest of the state identified by this structure /must/ adhere to the
--     pipeline linking rules described in the
--     <{html_spec_relative}#interfaces Shader Interfaces> chapter
--
-- -   @layout@ /must/ be
--     <{html_spec_relative}#descriptorsets-pipelinelayout-consistency consistent>
--     with the layout of the compute shader specified in @stage@
--
-- -   The number of resources in @layout@ accessible to the compute shader
--     stage /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxPerStageResources@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be a valid combination of 'VkPipelineCreateFlagBits'
--     values
--
-- -   @stage@ /must/ be a valid @VkPipelineShaderStageCreateInfo@
--     structure
--
-- -   @layout@ /must/ be a valid @VkPipelineLayout@ handle
--
-- -   Both of @basePipelineHandle@, and @layout@ that are valid handles
--     /must/ have been created, allocated, or retrieved from the same
--     @VkDevice@
--
-- = See Also
-- #_see_also#
--
-- 'VkPipeline', 'VkPipelineCreateFlags', 'VkPipelineLayout',
-- 'VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateComputePipelines'
data VkComputePipelineCreateInfo = VkComputePipelineCreateInfo
  { -- No documentation found for Nested "VkComputePipelineCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "vkFlags"
  vkFlags :: VkPipelineCreateFlags
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "vkStage"
  vkStage :: VkPipelineShaderStageCreateInfo
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "vkLayout"
  vkLayout :: VkPipelineLayout
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "vkBasePipelineHandle"
  vkBasePipelineHandle :: VkPipeline
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "vkBasePipelineIndex"
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
-- | VkVertexInputBindingDescription - Structure specifying vertex input
-- binding description
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @binding@ /must/ be less than
--     @VkPhysicalDeviceLimits@::@maxVertexInputBindings@
--
-- -   @stride@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxVertexInputBindingStride@
--
-- == Valid Usage (Implicit)
--
-- -   @inputRate@ /must/ be a valid 'VkVertexInputRate' value
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineVertexInputStateCreateInfo', 'VkVertexInputRate'
data VkVertexInputBindingDescription = VkVertexInputBindingDescription
  { -- No documentation found for Nested "VkVertexInputBindingDescription" "vkBinding"
  vkBinding :: Word32
  , -- No documentation found for Nested "VkVertexInputBindingDescription" "vkStride"
  vkStride :: Word32
  , -- No documentation found for Nested "VkVertexInputBindingDescription" "vkInputRate"
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
-- | VkVertexInputAttributeDescription - Structure specifying vertex input
-- attribute description
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @location@ /must/ be less than
--     @VkPhysicalDeviceLimits@::@maxVertexInputAttributes@
--
-- -   @binding@ /must/ be less than
--     @VkPhysicalDeviceLimits@::@maxVertexInputBindings@
--
-- -   @offset@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxVertexInputAttributeOffset@
--
-- -   @format@ /must/ be allowed as a vertex buffer format, as specified
--     by the @VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT@ flag in
--     @VkFormatProperties@::@bufferFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- == Valid Usage (Implicit)
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.Core10.Core.VkFormat'
--     value
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Core.VkFormat',
-- 'VkPipelineVertexInputStateCreateInfo'
data VkVertexInputAttributeDescription = VkVertexInputAttributeDescription
  { -- No documentation found for Nested "VkVertexInputAttributeDescription" "vkLocation"
  vkLocation :: Word32
  , -- No documentation found for Nested "VkVertexInputAttributeDescription" "vkBinding"
  vkBinding :: Word32
  , -- No documentation found for Nested "VkVertexInputAttributeDescription" "vkFormat"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkVertexInputAttributeDescription" "vkOffset"
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
-- | VkPipelineVertexInputStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline vertex input state
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @vertexBindingDescriptionCount@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxVertexInputBindings@
--
-- -   @vertexAttributeDescriptionCount@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxVertexInputAttributes@
--
-- -   For every @binding@ specified by each element of
--     @pVertexAttributeDescriptions@, a @VkVertexInputBindingDescription@
--     /must/ exist in @pVertexBindingDescriptions@ with the same value of
--     @binding@
--
-- -   All elements of @pVertexBindingDescriptions@ /must/ describe
--     distinct binding numbers
--
-- -   All elements of @pVertexAttributeDescriptions@ /must/ describe
--     distinct attribute locations
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.VkPipelineVertexInputDivisorStateCreateInfoEXT'
--
-- -   @flags@ /must/ be @0@
--
-- -   If @vertexBindingDescriptionCount@ is not @0@,
--     @pVertexBindingDescriptions@ /must/ be a valid pointer to an array
--     of @vertexBindingDescriptionCount@ valid
--     @VkVertexInputBindingDescription@ structures
--
-- -   If @vertexAttributeDescriptionCount@ is not @0@,
--     @pVertexAttributeDescriptions@ /must/ be a valid pointer to an array
--     of @vertexAttributeDescriptionCount@ valid
--     @VkVertexInputAttributeDescription@ structures
--
-- = See Also
-- #_see_also#
--
-- 'VkGraphicsPipelineCreateInfo', 'VkPipelineVertexInputStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkVertexInputAttributeDescription', 'VkVertexInputBindingDescription'
data VkPipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo
  { -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "vkFlags"
  vkFlags :: VkPipelineVertexInputStateCreateFlags
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "vkVertexBindingDescriptionCount"
  vkVertexBindingDescriptionCount :: Word32
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "vkPVertexBindingDescriptions"
  vkPVertexBindingDescriptions :: Ptr VkVertexInputBindingDescription
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "vkVertexAttributeDescriptionCount"
  vkVertexAttributeDescriptionCount :: Word32
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "vkPVertexAttributeDescriptions"
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
-- | VkPipelineInputAssemblyStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline input assembly state
--
-- = Description
-- #_description#
--
-- Restarting the assembly of primitives discards the most recent index
-- values if those elements formed an incomplete primitive, and restarts
-- the primitive assembly using the subsequent indices, but only assembling
-- the immediately following element through the end of the originally
-- specified elements. The primitive restart index value comparison is
-- performed before adding the @vertexOffset@ value to the index value.
--
-- == Valid Usage
--
-- -   If @topology@ is @VK_PRIMITIVE_TOPOLOGY_POINT_LIST@,
--     @VK_PRIMITIVE_TOPOLOGY_LINE_LIST@,
--     @VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST@,
--     @VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY@,
--     @VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY@ or
--     @VK_PRIMITIVE_TOPOLOGY_PATCH_LIST@, @primitiveRestartEnable@ /must/
--     be @VK_FALSE@
--
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @topology@ /must/ not be any of
--     @VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY@,
--     @VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY@,
--     @VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY@ or
--     @VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
--     feature is not enabled, @topology@ /must/ not be
--     @VK_PRIMITIVE_TOPOLOGY_PATCH_LIST@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @topology@ /must/ be a valid 'VkPrimitiveTopology' value
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineInputAssemblyStateCreateFlags', 'VkPrimitiveTopology',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo
  { -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "vkFlags"
  vkFlags :: VkPipelineInputAssemblyStateCreateFlags
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "vkTopology"
  vkTopology :: VkPrimitiveTopology
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "vkPrimitiveRestartEnable"
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
-- | VkPipelineTessellationStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline tessellation state
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @patchControlPoints@ /must/ be greater than zero and less than or
--     equal to @VkPhysicalDeviceLimits@::@maxTessellationPatchSize@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkPipelineTessellationDomainOriginStateCreateInfo'
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineTessellationStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineTessellationStateCreateInfo = VkPipelineTessellationStateCreateInfo
  { -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "vkFlags"
  vkFlags :: VkPipelineTessellationStateCreateFlags
  , -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "vkPatchControlPoints"
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
-- | VkPipelineViewportStateCreateInfo - Structure specifying parameters of a
-- newly created pipeline viewport state
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   If the
--     <{html_spec_relative}#features-features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- -   If the
--     <{html_spec_relative}#features-features-multiViewport multiple viewports>
--     feature is not enabled, @scissorCount@ /must/ be @1@
--
-- -   @viewportCount@ /must/ be between @1@ and
--     @VkPhysicalDeviceLimits@::@maxViewports@, inclusive
--
-- -   @scissorCount@ /must/ be between @1@ and
--     @VkPhysicalDeviceLimits@::@maxViewports@, inclusive
--
-- -   @scissorCount@ and @viewportCount@ /must/ be identical
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle.VkPipelineViewportSwizzleStateCreateInfoNV'
--     or
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- -   @scissorCount@ /must/ be greater than @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkGraphicsPipelineCreateInfo', 'VkPipelineViewportStateCreateFlags',
-- 'VkRect2D', 'Graphics.Vulkan.Core10.Core.VkStructureType', 'VkViewport'
data VkPipelineViewportStateCreateInfo = VkPipelineViewportStateCreateInfo
  { -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "vkFlags"
  vkFlags :: VkPipelineViewportStateCreateFlags
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "vkViewportCount"
  vkViewportCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "vkPViewports"
  vkPViewports :: Ptr VkViewport
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "vkScissorCount"
  vkScissorCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "vkPScissors"
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
-- | VkPipelineRasterizationStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline rasterization state
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   If the
--     <{html_spec_relative}#features-features-depthClamp depth clamping>
--     feature is not enabled, @depthClampEnable@ /must/ be @VK_FALSE@
--
-- -   If the
--     <{html_spec_relative}#features-features-fillModeNonSolid non-solid fill modes>
--     feature is not enabled, @polygonMode@ /must/ be
--     @VK_POLYGON_MODE_FILL@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization.VkPipelineRasterizationConservativeStateCreateInfoEXT'
--     or
--     'Graphics.Vulkan.Extensions.VK_AMD_rasterization_order.VkPipelineRasterizationStateRasterizationOrderAMD'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @polygonMode@ /must/ be a valid 'VkPolygonMode' value
--
-- -   @cullMode@ /must/ be a valid combination of 'VkCullModeFlagBits'
--     values
--
-- -   @frontFace@ /must/ be a valid 'VkFrontFace' value
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'VkCullModeFlags', 'VkFrontFace',
-- 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineRasterizationStateCreateFlags', 'VkPolygonMode',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineRasterizationStateCreateInfo = VkPipelineRasterizationStateCreateInfo
  { -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkFlags"
  vkFlags :: VkPipelineRasterizationStateCreateFlags
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkDepthClampEnable"
  vkDepthClampEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkRasterizerDiscardEnable"
  vkRasterizerDiscardEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkPolygonMode"
  vkPolygonMode :: VkPolygonMode
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkCullMode"
  vkCullMode :: VkCullModeFlags
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkFrontFace"
  vkFrontFace :: VkFrontFace
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkDepthBiasEnable"
  vkDepthBiasEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkDepthBiasConstantFactor"
  vkDepthBiasConstantFactor :: CFloat
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkDepthBiasClamp"
  vkDepthBiasClamp :: CFloat
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkDepthBiasSlopeFactor"
  vkDepthBiasSlopeFactor :: CFloat
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "vkLineWidth"
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
-- | VkPipelineMultisampleStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline multisample state
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   If the
--     <{html_spec_relative}#features-features-sampleRateShading sample rate shading>
--     feature is not enabled, @sampleShadingEnable@ /must/ be @VK_FALSE@
--
-- -   If the
--     <{html_spec_relative}#features-features-alphaToOne alpha to one>
--     feature is not enabled, @alphaToOneEnable@ /must/ be @VK_FALSE@
--
-- -   @minSampleShading@ /must/ be in the range [0,1]
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.VkPipelineCoverageModulationStateCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color.VkPipelineCoverageToColorStateCreateInfoNV',
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @rasterizationSamples@ /must/ be a valid
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
--     value
--
-- -   If @pSampleMask@ is not @NULL@, @pSampleMask@ /must/ be a valid
--     pointer to an array of
--     <<data:image/png;base64,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 stem afc23f1498b2f7d63bea697fcc1fdc8e>>
--     @VkSampleMask@ values
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineMultisampleStateCreateFlags',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- @VkSampleMask@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineMultisampleStateCreateInfo = VkPipelineMultisampleStateCreateInfo
  { -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "vkFlags"
  vkFlags :: VkPipelineMultisampleStateCreateFlags
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "vkRasterizationSamples"
  vkRasterizationSamples :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "vkSampleShadingEnable"
  vkSampleShadingEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "vkMinSampleShading"
  vkMinSampleShading :: CFloat
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "vkPSampleMask"
  vkPSampleMask :: Ptr VkSampleMask
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "vkAlphaToCoverageEnable"
  vkAlphaToCoverageEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "vkAlphaToOneEnable"
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
-- | VkPipelineColorBlendAttachmentState - Structure specifying a pipeline
-- color blend attachment state
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   If the
--     <{html_spec_relative}#features-features-dualSrcBlend dual source blending>
--     feature is not enabled, @srcColorBlendFactor@ /must/ not be
--     @VK_BLEND_FACTOR_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_SRC1_ALPHA@, or
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@
--
-- -   If the
--     <{html_spec_relative}#features-features-dualSrcBlend dual source blending>
--     feature is not enabled, @dstColorBlendFactor@ /must/ not be
--     @VK_BLEND_FACTOR_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_SRC1_ALPHA@, or
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@
--
-- -   If the
--     <{html_spec_relative}#features-features-dualSrcBlend dual source blending>
--     feature is not enabled, @srcAlphaBlendFactor@ /must/ not be
--     @VK_BLEND_FACTOR_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_SRC1_ALPHA@, or
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@
--
-- -   If the
--     <{html_spec_relative}#features-features-dualSrcBlend dual source blending>
--     feature is not enabled, @dstAlphaBlendFactor@ /must/ not be
--     @VK_BLEND_FACTOR_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_SRC1_ALPHA@, or
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@
--
-- == Valid Usage (Implicit)
--
-- -   @srcColorBlendFactor@ /must/ be a valid 'VkBlendFactor' value
--
-- -   @dstColorBlendFactor@ /must/ be a valid 'VkBlendFactor' value
--
-- -   @colorBlendOp@ /must/ be a valid 'VkBlendOp' value
--
-- -   @srcAlphaBlendFactor@ /must/ be a valid 'VkBlendFactor' value
--
-- -   @dstAlphaBlendFactor@ /must/ be a valid 'VkBlendFactor' value
--
-- -   @alphaBlendOp@ /must/ be a valid 'VkBlendOp' value
--
-- -   @colorWriteMask@ /must/ be a valid combination of
--     'VkColorComponentFlagBits' values
--
-- = See Also
-- #_see_also#
--
-- 'VkBlendFactor', 'VkBlendOp', @VkBool32@, 'VkColorComponentFlags',
-- 'VkPipelineColorBlendStateCreateInfo'
data VkPipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState
  { -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "vkBlendEnable"
  vkBlendEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "vkSrcColorBlendFactor"
  vkSrcColorBlendFactor :: VkBlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "vkDstColorBlendFactor"
  vkDstColorBlendFactor :: VkBlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "vkColorBlendOp"
  vkColorBlendOp :: VkBlendOp
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "vkSrcAlphaBlendFactor"
  vkSrcAlphaBlendFactor :: VkBlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "vkDstAlphaBlendFactor"
  vkDstAlphaBlendFactor :: VkBlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "vkAlphaBlendOp"
  vkAlphaBlendOp :: VkBlendOp
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "vkColorWriteMask"
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
-- | VkPipelineColorBlendStateCreateInfo - Structure specifying parameters of
-- a newly created pipeline color blend state
--
-- = Description
-- #_description#
--
-- Each element of the @pAttachments@ array is a
-- 'VkPipelineColorBlendAttachmentState' structure specifying per-target
-- blending state for each individual color attachment. If the
-- <{html_spec_relative}#features-features-independentBlend independent blending>
-- feature is not enabled on the device, all
-- 'VkPipelineColorBlendAttachmentState' elements in the @pAttachments@
-- array /must/ be identical.
--
-- == Valid Usage
--
-- -   If the
--     <{html_spec_relative}#features-features-independentBlend independent blending>
--     feature is not enabled, all elements of @pAttachments@ /must/ be
--     identical
--
-- -   If the
--     <{html_spec_relative}#features-features-logicOp logic operations>
--     feature is not enabled, @logicOpEnable@ /must/ be @VK_FALSE@
--
-- -   If @logicOpEnable@ is @VK_TRUE@, @logicOp@ /must/ be a valid
--     'VkLogicOp' value
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPipelineColorBlendAdvancedStateCreateInfoEXT'
--
-- -   @flags@ /must/ be @0@
--
-- -   If @attachmentCount@ is not @0@, @pAttachments@ /must/ be a valid
--     pointer to an array of @attachmentCount@ valid
--     @VkPipelineColorBlendAttachmentState@ structures
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'VkGraphicsPipelineCreateInfo', 'VkLogicOp',
-- 'VkPipelineColorBlendAttachmentState',
-- 'VkPipelineColorBlendStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineColorBlendStateCreateInfo = VkPipelineColorBlendStateCreateInfo
  { -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "vkFlags"
  vkFlags :: VkPipelineColorBlendStateCreateFlags
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "vkLogicOpEnable"
  vkLogicOpEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "vkLogicOp"
  vkLogicOp :: VkLogicOp
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "vkAttachmentCount"
  vkAttachmentCount :: Word32
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "vkPAttachments"
  vkPAttachments :: Ptr VkPipelineColorBlendAttachmentState
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "vkBlendConstants"
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
-- | VkPipelineDynamicStateCreateInfo - Structure specifying parameters of a
-- newly created pipeline dynamic state
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   Each element of @pDynamicStates@ /must/ be unique
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @pDynamicStates@ /must/ be a valid pointer to an array of
--     @dynamicStateCount@ valid 'VkDynamicState' values
--
-- -   @dynamicStateCount@ /must/ be greater than @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkDynamicState', 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineDynamicStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo
  { -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "vkFlags"
  vkFlags :: VkPipelineDynamicStateCreateFlags
  , -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "vkDynamicStateCount"
  vkDynamicStateCount :: Word32
  , -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "vkPDynamicStates"
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
-- | VkStencilOpState - Structure specifying stencil operation state
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @failOp@ /must/ be a valid 'VkStencilOp' value
--
-- -   @passOp@ /must/ be a valid 'VkStencilOp' value
--
-- -   @depthFailOp@ /must/ be a valid 'VkStencilOp' value
--
-- -   @compareOp@ /must/ be a valid 'VkCompareOp' value
--
-- = See Also
-- #_see_also#
--
-- 'VkCompareOp', 'VkPipelineDepthStencilStateCreateInfo', 'VkStencilOp'
data VkStencilOpState = VkStencilOpState
  { -- No documentation found for Nested "VkStencilOpState" "vkFailOp"
  vkFailOp :: VkStencilOp
  , -- No documentation found for Nested "VkStencilOpState" "vkPassOp"
  vkPassOp :: VkStencilOp
  , -- No documentation found for Nested "VkStencilOpState" "vkDepthFailOp"
  vkDepthFailOp :: VkStencilOp
  , -- No documentation found for Nested "VkStencilOpState" "vkCompareOp"
  vkCompareOp :: VkCompareOp
  , -- No documentation found for Nested "VkStencilOpState" "vkCompareMask"
  vkCompareMask :: Word32
  , -- No documentation found for Nested "VkStencilOpState" "vkWriteMask"
  vkWriteMask :: Word32
  , -- No documentation found for Nested "VkStencilOpState" "vkReference"
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
-- | VkPipelineDepthStencilStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline depth stencil state
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   If the
--     <{html_spec_relative}#features-features-depthBounds depth bounds testing>
--     feature is not enabled, @depthBoundsTestEnable@ /must/ be @VK_FALSE@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @depthCompareOp@ /must/ be a valid 'VkCompareOp' value
--
-- -   @front@ /must/ be a valid @VkStencilOpState@ structure
--
-- -   @back@ /must/ be a valid @VkStencilOpState@ structure
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'VkCompareOp', 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineDepthStencilStateCreateFlags', 'VkStencilOpState',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo
  { -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkFlags"
  vkFlags :: VkPipelineDepthStencilStateCreateFlags
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkDepthTestEnable"
  vkDepthTestEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkDepthWriteEnable"
  vkDepthWriteEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkDepthCompareOp"
  vkDepthCompareOp :: VkCompareOp
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkDepthBoundsTestEnable"
  vkDepthBoundsTestEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkStencilTestEnable"
  vkStencilTestEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkFront"
  vkFront :: VkStencilOpState
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkBack"
  vkBack :: VkStencilOpState
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkMinDepthBounds"
  vkMinDepthBounds :: CFloat
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "vkMaxDepthBounds"
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
-- | VkGraphicsPipelineCreateInfo - Structure specifying parameters of a
-- newly created graphics pipeline
--
-- = Description
-- #_description#
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <{html_spec_relative}#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- @pStages@ points to an array of 'VkPipelineShaderStageCreateInfo'
-- structures, which were previously described in
-- <{html_spec_relative}#pipelines-compute Compute Pipelines>.
--
-- @pDynamicState@ points to a structure of type
-- 'VkPipelineDynamicStateCreateInfo'.
--
-- == Valid Usage
--
-- -   If @flags@ contains the @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag,
--     and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be a
--     valid handle to a graphics @VkPipeline@
--
-- -   If @flags@ contains the @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag,
--     and @basePipelineHandle@ is
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be a valid index into the calling
--     command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag,
--     and @basePipelineIndex@ is not -1, @basePipelineHandle@ /must/ be
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @flags@ contains the @VK_PIPELINE_CREATE_DERIVATIVE_BIT@ flag,
--     and @basePipelineHandle@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be -1
--
-- -   The @stage@ member of each element of @pStages@ /must/ be unique
--
-- -   The @stage@ member of one element of @pStages@ /must/ be
--     @VK_SHADER_STAGE_VERTEX_BIT@
--
-- -   The @stage@ member of each element of @pStages@ /must/ not be
--     @VK_SHADER_STAGE_COMPUTE_BIT@
--
-- -   If @pStages@ includes a tessellation control shader stage, it /must/
--     include a tessellation evaluation shader stage
--
-- -   If @pStages@ includes a tessellation evaluation shader stage, it
--     /must/ include a tessellation control shader stage
--
-- -   If @pStages@ includes a tessellation control shader stage and a
--     tessellation evaluation shader stage, @pTessellationState@ /must/ be
--     a valid pointer to a valid @VkPipelineTessellationStateCreateInfo@
--     structure
--
-- -   If @pStages@ includes tessellation shader stages, the shader code of
--     at least one stage /must/ contain an @OpExecutionMode@ instruction
--     that specifies the type of subdivision in the pipeline
--
-- -   If @pStages@ includes tessellation shader stages, and the shader
--     code of both stages contain an @OpExecutionMode@ instruction that
--     specifies the type of subdivision in the pipeline, they /must/ both
--     specify the same subdivision mode
--
-- -   If @pStages@ includes tessellation shader stages, the shader code of
--     at least one stage /must/ contain an @OpExecutionMode@ instruction
--     that specifies the output patch size in the pipeline
--
-- -   If @pStages@ includes tessellation shader stages, and the shader
--     code of both contain an @OpExecutionMode@ instruction that specifies
--     the out patch size in the pipeline, they /must/ both specify the
--     same patch size
--
-- -   If @pStages@ includes tessellation shader stages, the @topology@
--     member of @pInputAssembly@ /must/ be
--     @VK_PRIMITIVE_TOPOLOGY_PATCH_LIST@
--
-- -   If the @topology@ member of @pInputAssembly@ is
--     @VK_PRIMITIVE_TOPOLOGY_PATCH_LIST@, @pStages@ /must/ include
--     tessellation shader stages
--
-- -   If @pStages@ includes a geometry shader stage, and does not include
--     any tessellation shader stages, its shader code /must/ contain an
--     @OpExecutionMode@ instruction that specifies an input primitive type
--     that is <{html_spec_relative}#shaders-geometry-execution compatible>
--     with the primitive topology specified in @pInputAssembly@
--
-- -   If @pStages@ includes a geometry shader stage, and also includes
--     tessellation shader stages, its shader code /must/ contain an
--     @OpExecutionMode@ instruction that specifies an input primitive type
--     that is <{html_spec_relative}#shaders-geometry-execution compatible>
--     with the primitive topology that is output by the tessellation
--     stages
--
-- -   If @pStages@ includes a fragment shader stage and a geometry shader
--     stage, and the fragment shader code reads from an input variable
--     that is decorated with @PrimitiveID@, then the geometry shader code
--     /must/ write to a matching output variable, decorated with
--     @PrimitiveID@, in all execution paths
--
-- -   If @pStages@ includes a fragment shader stage, its shader code
--     /must/ not read from any input attachment that is defined as
--     @VK_ATTACHMENT_UNUSED@ in @subpass@
--
-- -   The shader code for the entry points identified by @pStages@, and
--     the rest of the state identified by this structure /must/ adhere to
--     the pipeline linking rules described in the
--     <{html_spec_relative}#interfaces Shader Interfaces> chapter
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@ in the
--     @VkAttachmentReference@ defined by @subpass@, the @depthWriteEnable@
--     member of @pDepthStencilState@ /must/ be @VK_FALSE@
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@ in the
--     @VkAttachmentReference@ defined by @subpass@, the @failOp@, @passOp@
--     and @depthFailOp@ members of each of the @front@ and @back@ members
--     of @pDepthStencilState@ /must/ be @VK_STENCIL_OP_KEEP@
--
-- -   If rasterization is not disabled and the subpass uses color
--     attachments, then for each color attachment in the subpass the
--     @blendEnable@ member of the corresponding element of the
--     @pAttachment@ member of @pColorBlendState@ /must/ be @VK_FALSE@ if
--     the @format@ of the attachment does not support color blend
--     operations, as specified by the
--     @VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ or
--     @VkFormatProperties@::@optimalTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- -   If rasterization is not disabled and the subpass uses color
--     attachments, the @attachmentCount@ member of @pColorBlendState@
--     /must/ be equal to the @colorAttachmentCount@ used to create
--     @subpass@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     @VK_DYNAMIC_STATE_VIEWPORT@, the @pViewports@ member of
--     @pViewportState@ /must/ be a valid pointer to an array of
--     @pViewportState@::@viewportCount@ @VkViewport@ structures
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     @VK_DYNAMIC_STATE_SCISSOR@, the @pScissors@ member of
--     @pViewportState@ /must/ be a valid pointer to an array of
--     @pViewportState@::@scissorCount@ @VkRect2D@ structures
--
-- -   If the wide lines feature is not enabled, and no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     @VK_DYNAMIC_STATE_LINE_WIDTH@, the @lineWidth@ member of
--     @pRasterizationState@ /must/ be @1.0@
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     @VK_FALSE@, @pViewportState@ /must/ be a valid pointer to a valid
--     @VkPipelineViewportStateCreateInfo@ structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     @VK_FALSE@, @pMultisampleState@ /must/ be a valid pointer to a valid
--     @VkPipelineMultisampleStateCreateInfo@ structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     @VK_FALSE@, and @subpass@ uses a depth\/stencil attachment,
--     @pDepthStencilState@ /must/ be a valid pointer to a valid
--     @VkPipelineDepthStencilStateCreateInfo@ structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     @VK_FALSE@, and @subpass@ uses color attachments, @pColorBlendState@
--     /must/ be a valid pointer to a valid
--     @VkPipelineColorBlendStateCreateInfo@ structure
--
-- -   If the depth bias clamping feature is not enabled, no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     @VK_DYNAMIC_STATE_DEPTH_BIAS@, and the @depthBiasEnable@ member of
--     @pRasterizationState@ is @VK_TRUE@, the @depthBiasClamp@ member of
--     @pRasterizationState@ /must/ be @0.0@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     @VK_DYNAMIC_STATE_DEPTH_BOUNDS@, and the @depthBoundsTestEnable@
--     member of @pDepthStencilState@ is @VK_TRUE@, the @minDepthBounds@
--     and @maxDepthBounds@ members of @pDepthStencilState@ /must/ be
--     between @0.0@ and @1.0@, inclusive
--
-- -   @layout@ /must/ be
--     <{html_spec_relative}#descriptorsets-pipelinelayout-consistency consistent>
--     with all shaders specified in @pStages@
--
-- -   If @subpass@ uses color and\/or depth\/stencil attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be the
--     same as the sample count for those subpass attachments
--
-- -   If @subpass@ does not use any color and\/or depth\/stencil
--     attachments, then the @rasterizationSamples@ member of
--     @pMultisampleState@ /must/ follow the rules for a
--     <{html_spec_relative}#renderpass-noattachments zero-attachment subpass>
--
-- -   @subpass@ /must/ be a valid subpass within @renderPass@
--
-- -   The number of resources in @layout@ accessible to each shader stage
--     that is used by the pipeline /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxPerStageResources@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.VkPipelineDiscardRectangleStateCreateInfoEXT'
--
-- -   @flags@ /must/ be a valid combination of 'VkPipelineCreateFlagBits'
--     values
--
-- -   @pStages@ /must/ be a valid pointer to an array of @stageCount@
--     valid @VkPipelineShaderStageCreateInfo@ structures
--
-- -   @pVertexInputState@ /must/ be a valid pointer to a valid
--     @VkPipelineVertexInputStateCreateInfo@ structure
--
-- -   @pInputAssemblyState@ /must/ be a valid pointer to a valid
--     @VkPipelineInputAssemblyStateCreateInfo@ structure
--
-- -   @pRasterizationState@ /must/ be a valid pointer to a valid
--     @VkPipelineRasterizationStateCreateInfo@ structure
--
-- -   If @pDynamicState@ is not @NULL@, @pDynamicState@ /must/ be a valid
--     pointer to a valid @VkPipelineDynamicStateCreateInfo@ structure
--
-- -   @layout@ /must/ be a valid @VkPipelineLayout@ handle
--
-- -   @renderPass@ /must/ be a valid @VkRenderPass@ handle
--
-- -   @stageCount@ /must/ be greater than @0@
--
-- -   Each of @basePipelineHandle@, @layout@, and @renderPass@ that are
--     valid handles /must/ have been created, allocated, or retrieved from
--     the same @VkDevice@
--
-- = See Also
-- #_see_also#
--
-- 'VkPipeline', 'VkPipelineColorBlendStateCreateInfo',
-- 'VkPipelineCreateFlags', 'VkPipelineDepthStencilStateCreateInfo',
-- 'VkPipelineDynamicStateCreateInfo',
-- 'VkPipelineInputAssemblyStateCreateInfo', 'VkPipelineLayout',
-- 'VkPipelineMultisampleStateCreateInfo',
-- 'VkPipelineRasterizationStateCreateInfo',
-- 'VkPipelineShaderStageCreateInfo',
-- 'VkPipelineTessellationStateCreateInfo',
-- 'VkPipelineVertexInputStateCreateInfo',
-- 'VkPipelineViewportStateCreateInfo', 'VkRenderPass',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateGraphicsPipelines'
data VkGraphicsPipelineCreateInfo = VkGraphicsPipelineCreateInfo
  { -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkFlags"
  vkFlags :: VkPipelineCreateFlags
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkStageCount"
  vkStageCount :: Word32
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPStages"
  vkPStages :: Ptr VkPipelineShaderStageCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPVertexInputState"
  vkPVertexInputState :: Ptr VkPipelineVertexInputStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPInputAssemblyState"
  vkPInputAssemblyState :: Ptr VkPipelineInputAssemblyStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPTessellationState"
  vkPTessellationState :: Ptr VkPipelineTessellationStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPViewportState"
  vkPViewportState :: Ptr VkPipelineViewportStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPRasterizationState"
  vkPRasterizationState :: Ptr VkPipelineRasterizationStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPMultisampleState"
  vkPMultisampleState :: Ptr VkPipelineMultisampleStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPDepthStencilState"
  vkPDepthStencilState :: Ptr VkPipelineDepthStencilStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPColorBlendState"
  vkPColorBlendState :: Ptr VkPipelineColorBlendStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkPDynamicState"
  vkPDynamicState :: Ptr VkPipelineDynamicStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkLayout"
  vkLayout :: VkPipelineLayout
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkRenderPass"
  vkRenderPass :: VkRenderPass
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkSubpass"
  vkSubpass :: Word32
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkBasePipelineHandle"
  vkBasePipelineHandle :: VkPipeline
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "vkBasePipelineIndex"
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
-- | VkPipelineCreateFlags - Bitmask of VkPipelineCreateFlagBits
--
-- = Description
-- #_description#
--
-- @VkPipelineCreateFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkPipelineCreateFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkComputePipelineCreateInfo', 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineCreateFlagBits'
type VkPipelineCreateFlags = VkPipelineCreateFlagBits
-- | VkColorComponentFlags - Bitmask of VkColorComponentFlagBits
--
-- = Description
-- #_description#
--
-- @VkColorComponentFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkColorComponentFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkColorComponentFlagBits', 'VkPipelineColorBlendAttachmentState'
type VkColorComponentFlags = VkColorComponentFlagBits
-- | VkCullModeFlags - Bitmask of VkCullModeFlagBits
--
-- = Description
-- #_description#
--
-- @VkCullModeFlags@ is a bitmask type for setting a mask of zero or more
-- 'VkCullModeFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkCullModeFlagBits', 'VkPipelineRasterizationStateCreateInfo'
type VkCullModeFlags = VkCullModeFlagBits
-- | VkSampleMask - Mask of sample coverage information
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'VkPipelineMultisampleStateCreateInfo'
type VkSampleMask = Word32
