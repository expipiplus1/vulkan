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
  ( CChar(..)
  , CFloat(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( Ptr
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
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkSampleCountFlagBits(..)
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
--
-- The semantics of each enum value is described in the table below:
--
-- +--------------------------------------------+---------------------+--------+
-- | VkBlendFactor                              | RGB Blend Factors   | Alpha  |
-- |                                            | (Sr,Sg,Sb) or       | Blend  |
-- |                                            | (Dr,Dg,Db)          | Factor |
-- |                                            |                     | (Sa or |
-- |                                            |                     | Da)    |
-- +============================================+=====================+========+
-- | @VK_BLEND_FACTOR_ZERO@                     | (0,0,0)             | 0      |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_ONE@                      | (1,1,1)             | 1      |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_SRC_COLOR@                | (Rs0,Gs0,Bs0)       | As0    |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR@      | (1-Rs0,1-Gs0,1-Bs0) | 1-As0  |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_DST_COLOR@                | (Rd,Gd,Bd)          | Ad     |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR@      | (1-Rd,1-Gd,1-Bd)    | 1-Ad   |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_SRC_ALPHA@                | (As0,As0,As0)       | As0    |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA@      | (1-As0,1-As0,1-As0) | 1-As0  |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_DST_ALPHA@                | (Ad,Ad,Ad)          | Ad     |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA@      | (1-Ad,1-Ad,1-Ad)    | 1-Ad   |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_CONSTANT_COLOR@           | (Rc,Gc,Bc)          | Ac     |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR@ | (1-Rc,1-Gc,1-Bc)    | 1-Ac   |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_CONSTANT_ALPHA@           | (Ac,Ac,Ac)          | Ac     |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA@ | (1-Ac,1-Ac,1-Ac)    | 1-Ac   |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_SRC_ALPHA_SATURATE@       | (f,f,f); f =        | 1      |
-- |                                            | min(As0,1-Ad)       |        |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_SRC1_COLOR@               | (Rs1,Gs1,Bs1)       | As1    |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@     | (1-Rs1,1-Gs1,1-Bs1) | 1-As1  |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_SRC1_ALPHA@               | (As1,As1,As1)       | As1    |
-- +--------------------------------------------+---------------------+--------+
-- | @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@     | (1-As1,1-As1,1-As1) | 1-As1  |
-- +--------------------------------------------+---------------------+--------+
--
-- Blend Factors
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
--
-- The semantics of each basic blend operations is described in the table
-- below:
--
-- +--------------------------------+--------------------+----------------+
-- | VkBlendOp                      | RGB Components     | Alpha          |
-- |                                |                    | Component      |
-- +================================+====================+================+
-- | @VK_BLEND_OP_ADD@              | R = Rs0 × Sr + Rd  | A = As0 × Sa + |
-- |                                | × Dr               | Ad × Da        |
-- |                                | G = Gs0 × Sg + Gd  |                |
-- |                                | × Dg               |                |
-- |                                | B = Bs0 × Sb + Bd  |                |
-- |                                | × Db               |                |
-- +--------------------------------+--------------------+----------------+
-- | @VK_BLEND_OP_SUBTRACT@         | R = Rs0 × Sr - Rd  | A = As0 × Sa - |
-- |                                | × Dr               | Ad × Da        |
-- |                                | G = Gs0 × Sg - Gd  |                |
-- |                                | × Dg               |                |
-- |                                | B = Bs0 × Sb - Bd  |                |
-- |                                | × Db               |                |
-- +--------------------------------+--------------------+----------------+
-- | @VK_BLEND_OP_REVERSE_SUBTRACT@ | R = Rd × Dr - Rs0  | A = Ad × Da -  |
-- |                                | × Sr               | As0 × Sa       |
-- |                                | G = Gd × Dg - Gs0  |                |
-- |                                | × Sg               |                |
-- |                                | B = Bd × Db - Bs0  |                |
-- |                                | × Sb               |                |
-- +--------------------------------+--------------------+----------------+
-- | @VK_BLEND_OP_MIN@              | R = min(Rs0,Rd)    | A =            |
-- |                                | G = min(Gs0,Gd)    | min(As0,Ad)    |
-- |                                | B = min(Bs0,Bd)    |                |
-- +--------------------------------+--------------------+----------------+
-- | @VK_BLEND_OP_MAX@              | R = max(Rs0,Rd)    | A =            |
-- |                                | G = max(Gs0,Gd)    | max(As0,Ad)    |
-- |                                | B = max(Bs0,Bd)    |                |
-- +--------------------------------+--------------------+----------------+
--
-- Basic Blend Operations
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
-- -   @VK_POLYGON_MODE_FILL_RECTANGLE_NV@ specifies that polygons are
--     rendered using polygon rasterization rules, modified to consider a
--     sample within the primitive if the sample location is inside the
--     axis-aligned bounding box of the triangle after projection. Note
--     that the barycentric weights used in attribute interpolation /can/
--     extend outside the range [0,1] when these primitives are shaded.
--     Special treatment is given to a sample position on the boundary edge
--     of the bounding box. In such a case, if two rectangles lie on either
--     side of a common edge (with identical endpoints) on which a sample
--     position lies, then exactly one of the triangles /must/ produce a
--     fragment that covers that sample during rasterization.
--
--     Polygons rendered in @VK_POLYGON_MODE_FILL_RECTANGLE_NV@ mode /may/
--     be clipped by the frustum or by user clip planes. If clipping is
--     applied, the triangle is culled rather than clipped.
--
--     Area calculation and facingness are determined for
--     @VK_POLYGON_MODE_FILL_RECTANGLE_NV@ mode using the triangle’s
--     vertices.
--
-- These modes affect only the final rasterization of polygons: in
-- particular, a polygon’s vertices are shaded and the polygon is clipped
-- and possibly culled before these modes are applied.
--
-- = See Also
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
-- +-----------------------------------+-----------------------------------+
-- | Mode                              | Operation                         |
-- +===================================+===================================+
-- | @VK_LOGIC_OP_CLEAR@               | 0                                 |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_AND@                 | s ∧ d                             |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_AND_REVERSE@         | s ∧ ¬ d                           |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_COPY@                | s                                 |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_AND_INVERTED@        | ¬ s ∧ d                           |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_NO_OP@               | d                                 |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_XOR@                 | s ⊕ d                             |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_OR@                  | s ∨ d                             |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_NOR@                 | ¬ (s ∨ d)                         |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_EQUIVALENT@          | ¬ (s ⊕ d)                         |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_INVERT@              | ¬ d                               |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_OR_REVERSE@          | s ∨ ¬ d                           |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_COPY_INVERTED@       | ¬ s                               |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_OR_INVERTED@         | ¬ s ∨ d                           |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_NAND@                | ¬ (s ∧ d)                         |
-- +-----------------------------------+-----------------------------------+
-- | @VK_LOGIC_OP_SET@                 | all 1s                            |
-- +-----------------------------------+-----------------------------------+
--
-- Logical Operations
--
-- The result of the logical operation is then written to the color
-- attachment as controlled by the component write mask, described in
-- [Blend
-- Operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-blendoperations).
--
-- = See Also
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
-- = See Also
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
--
-- @VkPipelineDepthStencilStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
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
--
-- @VkPipelineDynamicStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
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
--
-- @VkPipelineColorBlendStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
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
--
-- @VkPipelineMultisampleStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
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
--
-- @VkPipelineRasterizationStateCreateFlags@ is a bitmask type for setting
-- a mask, but is currently reserved for future use.
--
-- = See Also
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
--
-- @VkPipelineViewportStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
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
--
-- @VkPipelineTessellationStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
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
--
-- @VkPipelineInputAssemblyStateCreateFlags@ is a bitmask type for setting
-- a mask, but is currently reserved for future use.
--
-- = See Also
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
--
-- @VkPipelineVertexInputStateCreateFlags@ is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
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
--
-- @VkPipelineShaderStageCreateFlags@ is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
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
-- -   @VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT@ specifies that
--     any shader input variables decorated as @DeviceIndex@ will be
--     assigned values as if they were decorated as @ViewIndex@.
--
-- -   @VK_PIPELINE_CREATE_DISPATCH_BASE@ specifies that a compute pipeline
--     /can/ be used with
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.vkCmdDispatchBase'
--     with a non-zero base workgroup.
--
-- It is valid to set both @VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT@ and
-- @VK_PIPELINE_CREATE_DERIVATIVE_BIT@. This allows a pipeline to be both a
-- parent and possibly a child in a pipeline hierarchy. See [Pipeline
-- Derivatives](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#pipelines-pipeline-derivatives)
-- for more information.
--
-- = See Also
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
-- = See Also
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
-- = See Also
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
-- = See Also
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
--
-- -   @device@ is the logical device that creates the graphics pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', indicating that
--     pipeline caching is disabled; or the handle of a valid [pipeline
--     cache](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#pipelines-cache)
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is an array of @VkGraphicsPipelineCreateInfo@
--     structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting
--     graphics pipeline objects are returned.
--
-- = Description
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
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INVALID_SHADER_NV@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkGraphicsPipelineCreateInfo', 'VkPipeline',
-- 'Graphics.Vulkan.Core10.PipelineCache.VkPipelineCache'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateGraphicsPipelines" vkCreateGraphicsPipelines :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkGraphicsPipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
-- | vkCreateComputePipelines - Creates a new compute pipeline object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the compute pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', indicating that
--     pipeline caching is disabled; or the handle of a valid [pipeline
--     cache](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#pipelines-cache)
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is an array of @VkComputePipelineCreateInfo@
--     structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting compute
--     pipeline objects are returned.
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
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INVALID_SHADER_NV@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkPipeline',
-- 'Graphics.Vulkan.Core10.PipelineCache.VkPipelineCache'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateComputePipelines" vkCreateComputePipelines :: ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkComputePipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
-- | vkDestroyPipeline - Destroy a pipeline object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the pipeline.
--
-- -   @pipeline@ is the handle of the pipeline to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
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
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkPipeline'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyPipeline" vkDestroyPipeline :: ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | VkOffset2D - Structure specifying a two-dimensional offset
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR',
-- 'VkRect2D',
-- 'Graphics.Vulkan.Extensions.VK_KHR_incremental_present.VkRectLayerKHR'
data VkOffset2D = VkOffset2D
  { -- | @x@ is the x offset.
  vkX :: Int32
  , -- | @y@ is the y offset.
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
-- = See Also
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
  { -- | @width@ is the width of the extent.
  vkWidth :: Word32
  , -- | @height@ is the height of the extent.
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
--
-- The framebuffer depth coordinate @z@f /may/ be represented using either
-- a fixed-point or floating-point representation. However, a
-- floating-point representation /must/ be used if the depth\/stencil
-- attachment has a floating-point depth component. If an m-bit fixed-point
-- representation is used, we assume that it represents each value
-- \(\frac{k}{2^m - 1}\), where k ∈ { 0, 1, …​, 2m-1 }, as k (e.g. 1.0 is
-- represented in binary as a string of all ones).
--
-- The viewport parameters shown in the above equations are found from
-- these values as
--
-- -   ox = @x@ + @width@ \/ 2
--
-- -   oy = @y@ + @height@ \/ 2
--
-- -   oz = @minDepth@
--
-- -   px = @width@
--
-- -   py = @height@
--
-- -   pz = @maxDepth@ - @minDepth@.
--
-- The application /can/ specify a negative term for @height@, which has
-- the effect of negating the y coordinate in clip space before performing
-- the transform. When using a negative @height@, the application /should/
-- also adjust the @y@ value to point to the lower left corner of the
-- viewport instead of the upper left corner. Using the negative @height@
-- allows the application to avoid having to negate the y component of the
-- @Position@ output from the last vertex processing stage in shaders that
-- also target other graphics APIs.
--
-- The width and height of the [implementation-dependent maximum viewport
-- dimensions](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-maxViewportDimensions)
-- /must/ be greater than or equal to the width and height of the largest
-- image which /can/ be created and attached to a framebuffer.
--
-- The floating-point viewport bounds are represented with an
-- [implementation-dependent
-- precision](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-viewportSubPixelBits).
--
-- == Valid Usage
--
-- -   @width@ /must/ be greater than @0.0@
--
-- -   @width@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxViewportDimensions@[0]
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
-- -   @y@ /must/ be less than or equal to @viewportBoundsRange@[1]
--
-- -   (@y@ + @height@) /must/ be greater than or equal to
--     @viewportBoundsRange@[0]
--
-- -   (@y@ + @height@) /must/ be less than or equal to
--     @viewportBoundsRange@[1]
--
-- -   Unless @{html_spec_relative}#VK_EXT_depth_range_unrestricted@
--     extension is enabled @minDepth@ /must/ be between @0.0@ and @1.0@,
--     inclusive
--
-- -   Unless @{html_spec_relative}#VK_EXT_depth_range_unrestricted@
--     extension is enabled @maxDepth@ /must/ be between @0.0@ and @1.0@,
--     inclusive
--
-- = See Also
--
-- 'VkPipelineViewportStateCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetViewport'
data VkViewport = VkViewport
  { -- | @x@ and @y@ are the viewport’s upper left corner (x,y).
  vkX :: CFloat
  , -- No documentation found for Nested "VkViewport" "y"
  vkY :: CFloat
  , -- | @width@ and @height@ are the viewport’s width and height, respectively.
  vkWidth :: CFloat
  , -- No documentation found for Nested "VkViewport" "height"
  vkHeight :: CFloat
  , -- | @minDepth@ and @maxDepth@ are the depth range for the viewport. It is
  -- valid for @minDepth@ to be greater than or equal to @maxDepth@.
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
-- | VkRect2D - Structure specifying a two-dimensional subregion
--
-- = See Also
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
  { -- | @offset@ is a 'VkOffset2D' specifying the rectangle offset.
  vkOffset :: VkOffset2D
  , -- | @extent@ is a 'VkExtent2D' specifying the rectangle extent.
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
--
-- 'VkSpecializationInfo'
data VkSpecializationMapEntry = VkSpecializationMapEntry
  { -- | @constantID@ is the ID of the specialization constant in SPIR-V.
  vkConstantID :: Word32
  , -- | @offset@ is the byte offset of the specialization constant value within
  -- the supplied data buffer.
  vkOffset :: Word32
  , -- | @size@ is the byte size of the specialization constant value within the
  -- supplied data buffer.
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
--
-- 'VkPipelineShaderStageCreateInfo', 'VkSpecializationMapEntry'
data VkSpecializationInfo = VkSpecializationInfo
  { -- | @mapEntryCount@ is the number of entries in the @pMapEntries@ array.
  vkMapEntryCount :: Word32
  , -- | @pMapEntries@ is a pointer to an array of @VkSpecializationMapEntry@
  -- which maps constant IDs to offsets in @pData@.
  vkPMapEntries :: Ptr VkSpecializationMapEntry
  , -- | @dataSize@ is the byte size of the @pData@ buffer.
  vkDataSize :: CSize
  , -- | @pData@ contains the actual constant values to specialize with.
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
-- == Valid Usage
--
-- -   If the [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     feature is not enabled, @stage@ /must/ not be
--     @VK_SHADER_STAGE_GEOMETRY_BIT@
--
-- -   If the [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
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
-- -   If @stage@ is @VK_SHADER_STAGE_FRAGMENT_BIT@, and the identified
--     entry point writes to @FragStencilRefEXT@ in any execution path, it
--     /must/ write to @FragStencilRefEXT@ in all execution paths
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
--
-- 'VkComputePipelineCreateInfo', 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineShaderStageCreateFlags',
-- 'Graphics.Vulkan.Core10.Shader.VkShaderModule', 'VkShaderStageFlagBits',
-- 'VkSpecializationInfo', 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineShaderStageCreateInfo = VkPipelineShaderStageCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineShaderStageCreateFlags
  , -- | @stage@ is a 'VkShaderStageFlagBits' value specifying a single pipeline
  -- stage.
  vkStage :: VkShaderStageFlagBits
  , -- | @module@ is a @VkShaderModule@ object that contains the shader for this
  -- stage.
  vkModule :: VkShaderModule
  , -- | @pName@ is a pointer to a null-terminated UTF-8 string specifying the
  -- entry point name of the shader for this stage.
  vkPName :: Ptr CChar
  , -- | @pSpecializationInfo@ is a pointer to 'VkSpecializationInfo', as
  -- described in [Specialization
  -- Constants](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#pipelines-specialization-constants),
  -- and /can/ be @NULL@.
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
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in [Pipeline
-- Derivatives](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#pipelines-pipeline-derivatives).
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
--     pipeline linking rules described in the [Shader
--     Interfaces](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#interfaces)
--     chapter
--
-- -   @layout@ /must/ be
--     [consistent](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency)
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
--
-- 'VkPipeline', 'VkPipelineCreateFlags', 'VkPipelineLayout',
-- 'VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateComputePipelines'
data VkComputePipelineCreateInfo = VkComputePipelineCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkPipelineCreateFlagBits' specifying how the
  -- pipeline will be generated.
  vkFlags :: VkPipelineCreateFlags
  , -- | @stage@ is a 'VkPipelineShaderStageCreateInfo' describing the compute
  -- shader.
  vkStage :: VkPipelineShaderStageCreateInfo
  , -- | @layout@ is the description of binding locations used by both the
  -- pipeline and descriptor sets used with the pipeline.
  vkLayout :: VkPipelineLayout
  , -- | @basePipelineHandle@ is a pipeline to derive from
  vkBasePipelineHandle :: VkPipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
  -- as a pipeline to derive from
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
--
-- 'VkPipelineVertexInputStateCreateInfo', 'VkVertexInputRate'
data VkVertexInputBindingDescription = VkVertexInputBindingDescription
  { -- | @binding@ is the binding number that this structure describes.
  vkBinding :: Word32
  , -- | @stride@ is the distance in bytes between two consecutive elements
  -- within the buffer.
  vkStride :: Word32
  , -- | @inputRate@ is a 'VkVertexInputRate' value specifying whether vertex
  -- attribute addressing is a function of the vertex index or of the
  -- instance index.
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
--
-- 'Graphics.Vulkan.Core10.Core.VkFormat',
-- 'VkPipelineVertexInputStateCreateInfo'
data VkVertexInputAttributeDescription = VkVertexInputAttributeDescription
  { -- | @location@ is the shader binding location number for this attribute.
  vkLocation :: Word32
  , -- | @binding@ is the binding number which this attribute takes its data
  -- from.
  vkBinding :: Word32
  , -- | @format@ is the size and type of the vertex attribute data.
  vkFormat :: VkFormat
  , -- | @offset@ is a byte offset of this attribute relative to the start of an
  -- element in the vertex input binding.
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
--
-- 'VkGraphicsPipelineCreateInfo', 'VkPipelineVertexInputStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkVertexInputAttributeDescription', 'VkVertexInputBindingDescription'
data VkPipelineVertexInputStateCreateInfo = VkPipelineVertexInputStateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineVertexInputStateCreateFlags
  , -- | @vertexBindingDescriptionCount@ is the number of vertex binding
  -- descriptions provided in @pVertexBindingDescriptions@.
  vkVertexBindingDescriptionCount :: Word32
  , -- | @pVertexBindingDescriptions@ is a pointer to an array of
  -- @VkVertexInputBindingDescription@ structures.
  vkPVertexBindingDescriptions :: Ptr VkVertexInputBindingDescription
  , -- | @vertexAttributeDescriptionCount@ is the number of vertex attribute
  -- descriptions provided in @pVertexAttributeDescriptions@.
  vkVertexAttributeDescriptionCount :: Word32
  , -- | @pVertexAttributeDescriptions@ is a pointer to an array of
  -- @VkVertexInputAttributeDescription@ structures.
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
-- -   If the [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     feature is not enabled, @topology@ /must/ not be any of
--     @VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY@,
--     @VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY@,
--     @VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY@ or
--     @VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY@
--
-- -   If the [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
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
--
-- @VkBool32@, 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineInputAssemblyStateCreateFlags', 'VkPrimitiveTopology',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineInputAssemblyStateCreateInfo = VkPipelineInputAssemblyStateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineInputAssemblyStateCreateFlags
  , -- | @topology@ is a 'VkPrimitiveTopology' defining the primitive topology,
  -- as described below.
  vkTopology :: VkPrimitiveTopology
  , -- | @primitiveRestartEnable@ controls whether a special vertex index value
  -- is treated as restarting the assembly of primitives. This enable only
  -- applies to indexed draws
  -- ('Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDrawIndexed' and
  -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect'),
  -- and the special index value is either 0xFFFFFFFF when the @indexType@
  -- parameter of @vkCmdBindIndexBuffer@ is equal to @VK_INDEX_TYPE_UINT32@,
  -- or 0xFFFF when @indexType@ is equal to @VK_INDEX_TYPE_UINT16@. Primitive
  -- restart is not allowed for “list” topologies.
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
--
-- 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineTessellationStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineTessellationStateCreateInfo = VkPipelineTessellationStateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineTessellationStateCreateFlags
  , -- | @patchControlPoints@ number of control points per patch.
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
-- == Valid Usage
--
-- -   If the [multiple
--     viewports](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiViewport)
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- -   If the [multiple
--     viewports](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiViewport)
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
-- -   If the @viewportWScalingEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'
--     structure chained to the @pNext@ chain is @VK_TRUE@, the
--     @viewportCount@ member of the
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'
--     structure /must/ be equal to @viewportCount@
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
--
-- 'VkGraphicsPipelineCreateInfo', 'VkPipelineViewportStateCreateFlags',
-- 'VkRect2D', 'Graphics.Vulkan.Core10.Core.VkStructureType', 'VkViewport'
data VkPipelineViewportStateCreateInfo = VkPipelineViewportStateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineViewportStateCreateFlags
  , -- | @viewportCount@ is the number of viewports used by the pipeline.
  vkViewportCount :: Word32
  , -- | @pViewports@ is a pointer to an array of 'VkViewport' structures,
  -- defining the viewport transforms. If the viewport state is dynamic, this
  -- member is ignored.
  vkPViewports :: Ptr VkViewport
  , -- | @scissorCount@ is the number of
  -- [scissors](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-scissor)
  -- and /must/ match the number of viewports.
  vkScissorCount :: Word32
  , -- | @pScissors@ is a pointer to an array of @VkRect2D@ structures which
  -- define the rectangular bounds of the scissor for the corresponding
  -- viewport. If the scissor state is dynamic, this member is ignored.
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
--
-- The application /can/ also add a
-- @VkPipelineRasterizationStateRasterizationOrderAMD@ structure to the
-- @pNext@ chain of a @VkPipelineRasterizationStateCreateInfo@ structure.
-- This structure enables selecting the rasterization order to use when
-- rendering with the corresponding graphics pipeline as described in
-- [Rasterization
-- Order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primrast-order).
--
-- == Valid Usage
--
-- -   If the [depth
--     clamping](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-depthClamp)
--     feature is not enabled, @depthClampEnable@ /must/ be @VK_FALSE@
--
-- -   If the [non-solid fill
--     modes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-fillModeNonSolid)
--     feature is not enabled, @polygonMode@ /must/ be
--     @VK_POLYGON_MODE_FILL@ or @VK_POLYGON_MODE_FILL_RECTANGLE_NV@
--
-- -   If the @{html_spec_relative}#VK_NV_fill_rectangle@ extension is not
--     enabled, @polygonMode@ /must/ not be
--     @VK_POLYGON_MODE_FILL_RECTANGLE_NV@
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
--
-- @VkBool32@, 'VkCullModeFlags', 'VkFrontFace',
-- 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineRasterizationStateCreateFlags', 'VkPolygonMode',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineRasterizationStateCreateInfo = VkPipelineRasterizationStateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineRasterizationStateCreateFlags
  , -- | @depthClampEnable@ controls whether to clamp the fragment’s depth values
  -- instead of clipping primitives to the z planes of the frustum, as
  -- described in [Primitive
  -- Clipping](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vertexpostproc-clipping).
  vkDepthClampEnable :: VkBool32
  , -- | @rasterizerDiscardEnable@ controls whether primitives are discarded
  -- immediately before the rasterization stage.
  vkRasterizerDiscardEnable :: VkBool32
  , -- | @polygonMode@ is the triangle rendering mode. See 'VkPolygonMode'.
  vkPolygonMode :: VkPolygonMode
  , -- | @cullMode@ is the triangle facing direction used for primitive culling.
  -- See 'VkCullModeFlagBits'.
  vkCullMode :: VkCullModeFlags
  , -- | @frontFace@ is a 'VkFrontFace' value specifying the front-facing
  -- triangle orientation to be used for culling.
  vkFrontFace :: VkFrontFace
  , -- | @depthBiasEnable@ controls whether to bias fragment depth values.
  vkDepthBiasEnable :: VkBool32
  , -- | @depthBiasConstantFactor@ is a scalar factor controlling the constant
  -- depth value added to each fragment.
  vkDepthBiasConstantFactor :: CFloat
  , -- | @depthBiasClamp@ is the maximum (or minimum) depth bias of a fragment.
  vkDepthBiasClamp :: CFloat
  , -- | @depthBiasSlopeFactor@ is a scalar factor applied to a fragment’s slope
  -- in depth bias calculations.
  vkDepthBiasSlopeFactor :: CFloat
  , -- | @lineWidth@ is the width of rasterized line segments.
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
-- == Valid Usage
--
-- -   If the [sample rate
--     shading](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-sampleRateShading)
--     feature is not enabled, @sampleShadingEnable@ /must/ be @VK_FALSE@
--
-- -   If the [alpha to
--     one](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-alphaToOne)
--     feature is not enabled, @alphaToOneEnable@ /must/ be @VK_FALSE@
--
-- -   @minSampleShading@ /must/ be in the range [0,1]
--
-- -   If the subpass has any color attachments and @rasterizationSamples@
--     is greater than the number of color samples, then
--     @sampleShadingEnable@ /must/ be @VK_FALSE@
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
--     \(\lceil{\mathit{rasterizationSamples} \over 32}\rceil\)
--     @VkSampleMask@ values
--
-- = See Also
--
-- @VkBool32@, 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineMultisampleStateCreateFlags',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- @VkSampleMask@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineMultisampleStateCreateInfo = VkPipelineMultisampleStateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineMultisampleStateCreateFlags
  , -- | @rasterizationSamples@ is a
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
  -- specifying the number of samples per pixel used in rasterization.
  vkRasterizationSamples :: VkSampleCountFlagBits
  , -- | @sampleShadingEnable@ /can/ be used to enable [Sample
  -- Shading](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primsrast-sampleshading).
  vkSampleShadingEnable :: VkBool32
  , -- | @minSampleShading@ specifies a minimum fraction of sample shading if
  -- @sampleShadingEnable@ is set to @VK_TRUE@.
  vkMinSampleShading :: CFloat
  , -- | @pSampleMask@ is a bitmask of static coverage information that is ANDed
  -- with the coverage information generated during rasterization, as
  -- described in [Sample
  -- Mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-samplemask).
  vkPSampleMask :: Ptr VkSampleMask
  , -- | @alphaToCoverageEnable@ controls whether a temporary coverage value is
  -- generated based on the alpha component of the fragment’s first color
  -- output as specified in the [Multisample
  -- Coverage](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-covg)
  -- section.
  vkAlphaToCoverageEnable :: VkBool32
  , -- | @alphaToOneEnable@ controls whether the alpha component of the
  -- fragment’s first color output is replaced with one as described in
  -- [Multisample
  -- Coverage](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-covg).
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
-- == Valid Usage
--
-- -   If the [dual source
--     blending](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-dualSrcBlend)
--     feature is not enabled, @srcColorBlendFactor@ /must/ not be
--     @VK_BLEND_FACTOR_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_SRC1_ALPHA@, or
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@
--
-- -   If the [dual source
--     blending](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-dualSrcBlend)
--     feature is not enabled, @dstColorBlendFactor@ /must/ not be
--     @VK_BLEND_FACTOR_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_SRC1_ALPHA@, or
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@
--
-- -   If the [dual source
--     blending](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-dualSrcBlend)
--     feature is not enabled, @srcAlphaBlendFactor@ /must/ not be
--     @VK_BLEND_FACTOR_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_SRC1_ALPHA@, or
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@
--
-- -   If the [dual source
--     blending](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-dualSrcBlend)
--     feature is not enabled, @dstAlphaBlendFactor@ /must/ not be
--     @VK_BLEND_FACTOR_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR@,
--     @VK_BLEND_FACTOR_SRC1_ALPHA@, or
--     @VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA@
--
-- -   If either of @colorBlendOp@ or @alphaBlendOp@ is an [advanced blend
--     operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-blend-advanced),
--     then @colorBlendOp@ /must/ equal @alphaBlendOp@
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is @VK_FALSE@ and @colorBlendOp@ is an [advanced blend
--     operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-blend-advanced),
--     then @colorBlendOp@ /must/ be the same for all attachments.
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is @VK_FALSE@ and @alphaBlendOp@ is an [advanced blend
--     operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-blend-advanced),
--     then @alphaBlendOp@ /must/ be the same for all attachments.
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendAllOperations@
--     is @VK_FALSE@, then @colorBlendOp@ /must/ not be
--     @VK_BLEND_OP_ZERO_EXT@, @VK_BLEND_OP_SRC_EXT@,
--     @VK_BLEND_OP_DST_EXT@, @VK_BLEND_OP_SRC_OVER_EXT@,
--     @VK_BLEND_OP_DST_OVER_EXT@, @VK_BLEND_OP_SRC_IN_EXT@,
--     @VK_BLEND_OP_DST_IN_EXT@, @VK_BLEND_OP_SRC_OUT_EXT@,
--     @VK_BLEND_OP_DST_OUT_EXT@, @VK_BLEND_OP_SRC_ATOP_EXT@,
--     @VK_BLEND_OP_DST_ATOP_EXT@, @VK_BLEND_OP_XOR_EXT@,
--     @VK_BLEND_OP_INVERT_EXT@, @VK_BLEND_OP_INVERT_RGB_EXT@,
--     @VK_BLEND_OP_LINEARDODGE_EXT@, @VK_BLEND_OP_LINEARBURN_EXT@,
--     @VK_BLEND_OP_VIVIDLIGHT_EXT@, @VK_BLEND_OP_LINEARLIGHT_EXT@,
--     @VK_BLEND_OP_PINLIGHT_EXT@, @VK_BLEND_OP_HARDMIX_EXT@,
--     @VK_BLEND_OP_PLUS_EXT@, @VK_BLEND_OP_PLUS_CLAMPED_EXT@,
--     @VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT@, @VK_BLEND_OP_PLUS_DARKER_EXT@,
--     @VK_BLEND_OP_MINUS_EXT@, @VK_BLEND_OP_MINUS_CLAMPED_EXT@,
--     @VK_BLEND_OP_CONTRAST_EXT@, @VK_BLEND_OP_INVERT_OVG_EXT@,
--     @VK_BLEND_OP_RED_EXT@, @VK_BLEND_OP_GREEN_EXT@, or
--     @VK_BLEND_OP_BLUE_EXT@
--
-- -   If @colorBlendOp@ or @alphaBlendOp@ is an [advanced blend
--     operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-blend-advanced),
--     then
--     'Graphics.Vulkan.Core10.Pass.VkSubpassDescription'::@colorAttachmentCount@
--     of the subpass this pipeline is compiled against /must/ be less than
--     or equal to
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'::advancedBlendMaxColorAttachments
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
--
-- 'VkBlendFactor', 'VkBlendOp', @VkBool32@, 'VkColorComponentFlags',
-- 'VkPipelineColorBlendStateCreateInfo'
data VkPipelineColorBlendAttachmentState = VkPipelineColorBlendAttachmentState
  { -- | @blendEnable@ controls whether blending is enabled for the corresponding
  -- color attachment. If blending is not enabled, the source fragment’s
  -- color for that attachment is passed through unmodified.
  vkBlendEnable :: VkBool32
  , -- | @srcColorBlendFactor@ selects which blend factor is used to determine
  -- the source factors (Sr,Sg,Sb).
  vkSrcColorBlendFactor :: VkBlendFactor
  , -- | @dstColorBlendFactor@ selects which blend factor is used to determine
  -- the destination factors (Dr,Dg,Db).
  vkDstColorBlendFactor :: VkBlendFactor
  , -- | @colorBlendOp@ selects which blend operation is used to calculate the
  -- RGB values to write to the color attachment.
  vkColorBlendOp :: VkBlendOp
  , -- | @srcAlphaBlendFactor@ selects which blend factor is used to determine
  -- the source factor Sa.
  vkSrcAlphaBlendFactor :: VkBlendFactor
  , -- | @dstAlphaBlendFactor@ selects which blend factor is used to determine
  -- the destination factor Da.
  vkDstAlphaBlendFactor :: VkBlendFactor
  , -- | @alphaBlendOp@ selects which blend operation is use to calculate the
  -- alpha values to write to the color attachment.
  vkAlphaBlendOp :: VkBlendOp
  , -- | @colorWriteMask@ is a bitmask of 'VkColorComponentFlagBits' specifying
  -- which of the R, G, B, and\/or A components are enabled for writing, as
  -- described for the [Color Write
  -- Mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-color-write-mask).
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
--
-- Each element of the @pAttachments@ array is a
-- 'VkPipelineColorBlendAttachmentState' structure specifying per-target
-- blending state for each individual color attachment. If the [independent
-- blending](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-independentBlend)
-- feature is not enabled on the device, all
-- 'VkPipelineColorBlendAttachmentState' elements in the @pAttachments@
-- array /must/ be identical.
--
-- == Valid Usage
--
-- -   If the [independent
--     blending](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-independentBlend)
--     feature is not enabled, all elements of @pAttachments@ /must/ be
--     identical
--
-- -   If the [logic
--     operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-logicOp)
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
--
-- @VkBool32@, 'VkGraphicsPipelineCreateInfo', 'VkLogicOp',
-- 'VkPipelineColorBlendAttachmentState',
-- 'VkPipelineColorBlendStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineColorBlendStateCreateInfo = VkPipelineColorBlendStateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineColorBlendStateCreateFlags
  , -- | @logicOpEnable@ controls whether to apply [Logical
  -- Operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-logicop).
  vkLogicOpEnable :: VkBool32
  , -- | @logicOp@ selects which logical operation to apply.
  vkLogicOp :: VkLogicOp
  , -- | @attachmentCount@ is the number of @VkPipelineColorBlendAttachmentState@
  -- elements in @pAttachments@. This value /must/ equal the
  -- @colorAttachmentCount@ for the subpass in which this pipeline is used.
  vkAttachmentCount :: Word32
  , -- | @pAttachments@: is a pointer to array of per target attachment states.
  vkPAttachments :: Ptr VkPipelineColorBlendAttachmentState
  , -- | @blendConstants@ is an array of four values used as the R, G, B, and A
  -- components of the blend constant that are used in blending, depending on
  -- the [blend
  -- factor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-blendfactors).
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
--
-- 'VkDynamicState', 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineDynamicStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineDynamicStateCreateInfo = VkPipelineDynamicStateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineDynamicStateCreateFlags
  , -- | @dynamicStateCount@ is the number of elements in the @pDynamicStates@
  -- array.
  vkDynamicStateCount :: Word32
  , -- | @pDynamicStates@ is an array of 'VkDynamicState' values specifying which
  -- pieces of pipeline state will use the values from dynamic state commands
  -- rather than from pipeline state creation info.
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
--
-- 'VkCompareOp', 'VkPipelineDepthStencilStateCreateInfo', 'VkStencilOp'
data VkStencilOpState = VkStencilOpState
  { -- | @failOp@ is a 'VkStencilOp' value specifying the action performed on
  -- samples that fail the stencil test.
  vkFailOp :: VkStencilOp
  , -- | @passOp@ is a 'VkStencilOp' value specifying the action performed on
  -- samples that pass both the depth and stencil tests.
  vkPassOp :: VkStencilOp
  , -- | @depthFailOp@ is a 'VkStencilOp' value specifying the action performed
  -- on samples that pass the stencil test and fail the depth test.
  vkDepthFailOp :: VkStencilOp
  , -- | @compareOp@ is a 'VkCompareOp' value specifying the comparison operator
  -- used in the stencil test.
  vkCompareOp :: VkCompareOp
  , -- | @compareMask@ selects the bits of the unsigned integer stencil values
  -- participating in the stencil test.
  vkCompareMask :: Word32
  , -- | @writeMask@ selects the bits of the unsigned integer stencil values
  -- updated by the stencil test in the stencil framebuffer attachment.
  vkWriteMask :: Word32
  , -- | @reference@ is an integer reference value that is used in the unsigned
  -- stencil comparison.
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
-- == Valid Usage
--
-- -   If the [depth bounds
--     testing](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-depthBounds)
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
--
-- @VkBool32@, 'VkCompareOp', 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineDepthStencilStateCreateFlags', 'VkStencilOpState',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineDepthStencilStateCreateInfo = VkPipelineDepthStencilStateCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineDepthStencilStateCreateFlags
  , -- | @depthTestEnable@ controls whether [depth
  -- testing](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-depth)
  -- is enabled.
  vkDepthTestEnable :: VkBool32
  , -- | @depthWriteEnable@ controls whether [depth
  -- writes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-depth-write)
  -- are enabled when @depthTestEnable@ is @VK_TRUE@. Depth writes are always
  -- disabled when @depthTestEnable@ is @VK_FALSE@.
  vkDepthWriteEnable :: VkBool32
  , -- | @depthCompareOp@ is the comparison operator used in the [depth
  -- test](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-depth).
  vkDepthCompareOp :: VkCompareOp
  , -- | @depthBoundsTestEnable@ controls whether [depth bounds
  -- testing](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-dbt)
  -- is enabled.
  vkDepthBoundsTestEnable :: VkBool32
  , -- | @stencilTestEnable@ controls whether [stencil
  -- testing](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-stencil)
  -- is enabled.
  vkStencilTestEnable :: VkBool32
  , -- | @front@ and @back@ control the parameters of the [stencil
  -- test](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-stencil).
  vkFront :: VkStencilOpState
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "back"
  vkBack :: VkStencilOpState
  , -- | @minDepthBounds@ and @maxDepthBounds@ define the range of values used in
  -- the [depth bounds
  -- test](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragops-dbt).
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
-- | VkGraphicsPipelineCreateInfo - Structure specifying parameters of a
-- newly created graphics pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in [Pipeline
-- Derivatives](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#pipelines-pipeline-derivatives).
--
-- @pStages@ points to an array of 'VkPipelineShaderStageCreateInfo'
-- structures, which were previously described in [Compute
-- Pipelines](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#pipelines-compute).
--
-- @pDynamicState@ points to a structure of type
-- 'VkPipelineDynamicStateCreateInfo'.
--
-- If any shader stage fails to compile, the compile log will be reported
-- back to the application, and @VK_ERROR_INVALID_SHADER_NV@ will be
-- generated.
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
--     that is
--     [compatible](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-geometry-execution)
--     with the primitive topology specified in @pInputAssembly@
--
-- -   If @pStages@ includes a geometry shader stage, and also includes
--     tessellation shader stages, its shader code /must/ contain an
--     @OpExecutionMode@ instruction that specifies an input primitive type
--     that is
--     [compatible](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-geometry-execution)
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
--     the pipeline linking rules described in the [Shader
--     Interfaces](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#interfaces)
--     chapter
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL@ in the
--     @VkAttachmentReference@ defined by @subpass@, the @depthWriteEnable@
--     member of @pDepthStencilState@ /must/ be @VK_FALSE@
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL@ in the
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
-- -   If the @{html_spec_relative}#VK_EXT_depth_range_unrestricted@
--     extension is not enabled and no element of the @pDynamicStates@
--     member of @pDynamicState@ is @VK_DYNAMIC_STATE_DEPTH_BOUNDS@, and
--     the @depthBoundsTestEnable@ member of @pDepthStencilState@ is
--     @VK_TRUE@, the @minDepthBounds@ and @maxDepthBounds@ members of
--     @pDepthStencilState@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     @VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT@, and the
--     @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     structure chained to the @pNext@ chain of @pMultisampleState@ is
--     @VK_TRUE@, @sampleLocationsInfo.sampleLocationGridSize.width@ /must/
--     evenly divide
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkMultisamplePropertiesEXT'::@sampleLocationGridSize.width@
--     as returned by
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.vkGetPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     @VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT@, and the
--     @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     structure chained to the @pNext@ chain of @pMultisampleState@ is
--     @VK_TRUE@, @sampleLocationsInfo.sampleLocationGridSize.height@
--     /must/ evenly divide
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkMultisamplePropertiesEXT'::@sampleLocationGridSize.height@
--     as returned by
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.vkGetPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     @VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT@, and the
--     @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     structure chained to the @pNext@ chain of @pMultisampleState@ is
--     @VK_TRUE@, @sampleLocationsInfo.sampleLocationsPerPixel@ /must/
--     equal @rasterizationSamples@
--
-- -   If the @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     structure chained to the @pNext@ chain of @pMultisampleState@ is
--     @VK_TRUE@, the fragment shader code /must/ not statically use the
--     extended instruction @InterpolateAtSample@
--
-- -   @layout@ /must/ be
--     [consistent](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency)
--     with all shaders specified in @pStages@
--
-- -   If @subpass@ uses color and\/or depth\/stencil attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ equal
--     the maximum of the sample counts of those subpass attachments
--
-- -   If @subpass@ has a depth\/stencil attachment and depth test, stencil
--     test, or depth bounds test are enabled, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be the
--     same as the sample count of the depth\/stencil attachment
--
-- -   If @subpass@ has any color attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be
--     greater than or equal to the sample count for those subpass
--     attachments
--
-- -   If @subpass@ does not use any color and\/or depth\/stencil
--     attachments, then the @rasterizationSamples@ member of
--     @pMultisampleState@ /must/ follow the rules for a [zero-attachment
--     subpass](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-noattachments)
--
-- -   @subpass@ /must/ be a valid subpass within @renderPass@
--
-- -   If the @renderPass@ has multiview enabled and @subpass@ has more
--     than one bit set in the view mask and @multiviewTessellationShader@
--     is not enabled, then @pStages@ /must/ not include tessellation
--     shaders.
--
-- -   If the @renderPass@ has multiview enabled and @subpass@ has more
--     than one bit set in the view mask and @multiviewGeometryShader@ is
--     not enabled, then @pStages@ /must/ not include a geometry shader.
--
-- -   If the @renderPass@ has multiview enabled and @subpass@ has more
--     than one bit set in the view mask, shaders in the pipeline /must/
--     not write to the @Layer@ built-in output
--
-- -   If the @renderPass@ has multiview enabled, then all shaders /must/
--     not include variables decorated with the @Layer@ built-in decoration
--     in their interfaces.
--
-- -   @flags@ /must/ not contain the @VK_PIPELINE_CREATE_DISPATCH_BASE@
--     flag.
--
-- -   If @pStages@ includes a fragment shader stage and an input
--     attachment was referenced by the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo'
--     at @renderPass@ create time, its shader code /must/ not read from
--     any aspect that was not specified in the @aspectMask@ of the
--     corresponding
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkInputAttachmentAspectReference'
--     structure.
--
-- -   The number of resources in @layout@ accessible to each shader stage
--     that is used by the pipeline /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxPerStageResources@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     @VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV@, and the
--     @viewportWScalingEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'
--     structure, chained to the @pNext@ chain of @pViewportState@, is
--     @VK_TRUE@, the @pViewportWScalings@ member of the
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'
--     /must/ be a pointer to an array of
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     valid
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.VkViewportWScalingNV'
--     structures
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
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkPipelineCreateFlagBits' specifying how the
  -- pipeline will be generated.
  vkFlags :: VkPipelineCreateFlags
  , -- | @stageCount@ is the number of entries in the @pStages@ array.
  vkStageCount :: Word32
  , -- | @pStages@ is an array of size @stageCount@ structures of type
  -- 'VkPipelineShaderStageCreateInfo' describing the set of the shader
  -- stages to be included in the graphics pipeline.
  vkPStages :: Ptr VkPipelineShaderStageCreateInfo
  , -- | @pVertexInputState@ is a pointer to an instance of the
  -- 'VkPipelineVertexInputStateCreateInfo' structure.
  vkPVertexInputState :: Ptr VkPipelineVertexInputStateCreateInfo
  , -- | @pInputAssemblyState@ is a pointer to an instance of the
  -- 'VkPipelineInputAssemblyStateCreateInfo' structure which determines
  -- input assembly behavior, as described in [Drawing
  -- Commands](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#drawing).
  vkPInputAssemblyState :: Ptr VkPipelineInputAssemblyStateCreateInfo
  , -- | @pTessellationState@ is a pointer to an instance of the
  -- 'VkPipelineTessellationStateCreateInfo' structure, and is ignored if the
  -- pipeline does not include a tessellation control shader stage and
  -- tessellation evaluation shader stage.
  vkPTessellationState :: Ptr VkPipelineTessellationStateCreateInfo
  , -- | @pViewportState@ is a pointer to an instance of the
  -- 'VkPipelineViewportStateCreateInfo' structure, and is ignored if the
  -- pipeline has rasterization disabled.
  vkPViewportState :: Ptr VkPipelineViewportStateCreateInfo
  , -- | @pRasterizationState@ is a pointer to an instance of the
  -- 'VkPipelineRasterizationStateCreateInfo' structure.
  vkPRasterizationState :: Ptr VkPipelineRasterizationStateCreateInfo
  , -- | @pMultisampleState@ is a pointer to an instance of the
  -- 'VkPipelineMultisampleStateCreateInfo', and is ignored if the pipeline
  -- has rasterization disabled.
  vkPMultisampleState :: Ptr VkPipelineMultisampleStateCreateInfo
  , -- | @pDepthStencilState@ is a pointer to an instance of the
  -- 'VkPipelineDepthStencilStateCreateInfo' structure, and is ignored if the
  -- pipeline has rasterization disabled or if the subpass of the render pass
  -- the pipeline is created against does not use a depth\/stencil
  -- attachment.
  vkPDepthStencilState :: Ptr VkPipelineDepthStencilStateCreateInfo
  , -- | @pColorBlendState@ is a pointer to an instance of the
  -- 'VkPipelineColorBlendStateCreateInfo' structure, and is ignored if the
  -- pipeline has rasterization disabled or if the subpass of the render pass
  -- the pipeline is created against does not use any color attachments.
  vkPColorBlendState :: Ptr VkPipelineColorBlendStateCreateInfo
  , -- | @pDynamicState@ is a pointer to 'VkPipelineDynamicStateCreateInfo' and
  -- is used to indicate which properties of the pipeline state object are
  -- dynamic and /can/ be changed independently of the pipeline state. This
  -- /can/ be @NULL@, which means no state in the pipeline is considered
  -- dynamic.
  vkPDynamicState :: Ptr VkPipelineDynamicStateCreateInfo
  , -- | @layout@ is the description of binding locations used by both the
  -- pipeline and descriptor sets used with the pipeline.
  vkLayout :: VkPipelineLayout
  , -- | @renderPass@ is a handle to a render pass object describing the
  -- environment in which the pipeline will be used; the pipeline /must/ only
  -- be used with an instance of any render pass compatible with the one
  -- provided. See [Render Pass
  -- Compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility)
  -- for more information.
  vkRenderPass :: VkRenderPass
  , -- | @subpass@ is the index of the subpass in the render pass where this
  -- pipeline will be used.
  vkSubpass :: Word32
  , -- | @basePipelineHandle@ is a pipeline to derive from.
  vkBasePipelineHandle :: VkPipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
  -- as a pipeline to derive from.
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
--
-- @VkPipelineCreateFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkPipelineCreateFlagBits'.
--
-- = See Also
--
-- 'VkComputePipelineCreateInfo', 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineCreateFlagBits'
type VkPipelineCreateFlags = VkPipelineCreateFlagBits
-- | VkColorComponentFlags - Bitmask of VkColorComponentFlagBits
--
-- = Description
--
-- @VkColorComponentFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkColorComponentFlagBits'.
--
-- = See Also
--
-- 'VkColorComponentFlagBits', 'VkPipelineColorBlendAttachmentState'
type VkColorComponentFlags = VkColorComponentFlagBits
-- | VkCullModeFlags - Bitmask of VkCullModeFlagBits
--
-- = Description
--
-- @VkCullModeFlags@ is a bitmask type for setting a mask of zero or more
-- 'VkCullModeFlagBits'.
--
-- = See Also
--
-- 'VkCullModeFlagBits', 'VkPipelineRasterizationStateCreateInfo'
type VkCullModeFlags = VkCullModeFlagBits
-- | VkSampleMask - Mask of sample coverage information
--
-- = See Also
--
-- 'VkPipelineMultisampleStateCreateInfo'
type VkSampleMask = Word32
