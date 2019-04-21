{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Pipeline
  ( BlendFactor
  , pattern BLEND_FACTOR_ZERO
  , pattern BLEND_FACTOR_ONE
  , pattern BLEND_FACTOR_SRC_COLOR
  , pattern BLEND_FACTOR_ONE_MINUS_SRC_COLOR
  , pattern BLEND_FACTOR_DST_COLOR
  , pattern BLEND_FACTOR_ONE_MINUS_DST_COLOR
  , pattern BLEND_FACTOR_SRC_ALPHA
  , pattern BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
  , pattern BLEND_FACTOR_DST_ALPHA
  , pattern BLEND_FACTOR_ONE_MINUS_DST_ALPHA
  , pattern BLEND_FACTOR_CONSTANT_COLOR
  , pattern BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR
  , pattern BLEND_FACTOR_CONSTANT_ALPHA
  , pattern BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA
  , pattern BLEND_FACTOR_SRC_ALPHA_SATURATE
  , pattern BLEND_FACTOR_SRC1_COLOR
  , pattern BLEND_FACTOR_ONE_MINUS_SRC1_COLOR
  , pattern BLEND_FACTOR_SRC1_ALPHA
  , pattern BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
  , BlendOp
  , pattern BLEND_OP_ADD
  , pattern BLEND_OP_SUBTRACT
  , pattern BLEND_OP_REVERSE_SUBTRACT
  , pattern BLEND_OP_MIN
  , pattern BLEND_OP_MAX
  , ColorComponentFlagBits
  , pattern COLOR_COMPONENT_R_BIT
  , pattern COLOR_COMPONENT_G_BIT
  , pattern COLOR_COMPONENT_B_BIT
  , pattern COLOR_COMPONENT_A_BIT
  , ColorComponentFlags
  , CompareOp
  , pattern COMPARE_OP_NEVER
  , pattern COMPARE_OP_LESS
  , pattern COMPARE_OP_EQUAL
  , pattern COMPARE_OP_LESS_OR_EQUAL
  , pattern COMPARE_OP_GREATER
  , pattern COMPARE_OP_NOT_EQUAL
  , pattern COMPARE_OP_GREATER_OR_EQUAL
  , pattern COMPARE_OP_ALWAYS
  , withCStructComputePipelineCreateInfo
  , fromCStructComputePipelineCreateInfo
  , ComputePipelineCreateInfo(..)
  , CullModeFlagBits
  , pattern CULL_MODE_FRONT_BIT
  , pattern CULL_MODE_BACK_BIT
  , pattern CULL_MODE_NONE
  , pattern CULL_MODE_FRONT_AND_BACK
  , CullModeFlags
  , DynamicState
  , pattern DYNAMIC_STATE_VIEWPORT
  , pattern DYNAMIC_STATE_SCISSOR
  , pattern DYNAMIC_STATE_LINE_WIDTH
  , pattern DYNAMIC_STATE_DEPTH_BIAS
  , pattern DYNAMIC_STATE_BLEND_CONSTANTS
  , pattern DYNAMIC_STATE_DEPTH_BOUNDS
  , pattern DYNAMIC_STATE_STENCIL_COMPARE_MASK
  , pattern DYNAMIC_STATE_STENCIL_WRITE_MASK
  , pattern DYNAMIC_STATE_STENCIL_REFERENCE
  , withCStructExtent2D
  , fromCStructExtent2D
  , Extent2D(..)
  , FrontFace
  , pattern FRONT_FACE_COUNTER_CLOCKWISE
  , pattern FRONT_FACE_CLOCKWISE
  , withCStructGraphicsPipelineCreateInfo
  , fromCStructGraphicsPipelineCreateInfo
  , GraphicsPipelineCreateInfo(..)
  , LogicOp
  , pattern LOGIC_OP_CLEAR
  , pattern LOGIC_OP_AND
  , pattern LOGIC_OP_AND_REVERSE
  , pattern LOGIC_OP_COPY
  , pattern LOGIC_OP_AND_INVERTED
  , pattern LOGIC_OP_NO_OP
  , pattern LOGIC_OP_XOR
  , pattern LOGIC_OP_OR
  , pattern LOGIC_OP_NOR
  , pattern LOGIC_OP_EQUIVALENT
  , pattern LOGIC_OP_INVERT
  , pattern LOGIC_OP_OR_REVERSE
  , pattern LOGIC_OP_COPY_INVERTED
  , pattern LOGIC_OP_OR_INVERTED
  , pattern LOGIC_OP_NAND
  , pattern LOGIC_OP_SET
  , withCStructOffset2D
  , fromCStructOffset2D
  , Offset2D(..)
  , Pipeline
  , withCStructPipelineColorBlendAttachmentState
  , fromCStructPipelineColorBlendAttachmentState
  , PipelineColorBlendAttachmentState(..)
  , PipelineColorBlendStateCreateFlags
  , withCStructPipelineColorBlendStateCreateInfo
  , fromCStructPipelineColorBlendStateCreateInfo
  , PipelineColorBlendStateCreateInfo(..)
  , PipelineCreateFlagBits
  , pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
  , pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
  , pattern PIPELINE_CREATE_DERIVATIVE_BIT
  , PipelineCreateFlags
  , PipelineDepthStencilStateCreateFlags
  , withCStructPipelineDepthStencilStateCreateInfo
  , fromCStructPipelineDepthStencilStateCreateInfo
  , PipelineDepthStencilStateCreateInfo(..)
  , PipelineDynamicStateCreateFlags
  , withCStructPipelineDynamicStateCreateInfo
  , fromCStructPipelineDynamicStateCreateInfo
  , PipelineDynamicStateCreateInfo(..)
  , PipelineInputAssemblyStateCreateFlags
  , withCStructPipelineInputAssemblyStateCreateInfo
  , fromCStructPipelineInputAssemblyStateCreateInfo
  , PipelineInputAssemblyStateCreateInfo(..)
  , PipelineLayout
  , PipelineMultisampleStateCreateFlags
  , withCStructPipelineMultisampleStateCreateInfo
  , fromCStructPipelineMultisampleStateCreateInfo
  , PipelineMultisampleStateCreateInfo(..)
  , PipelineRasterizationStateCreateFlags
  , withCStructPipelineRasterizationStateCreateInfo
  , fromCStructPipelineRasterizationStateCreateInfo
  , PipelineRasterizationStateCreateInfo(..)
  , PipelineShaderStageCreateFlags
  , withCStructPipelineShaderStageCreateInfo
  , fromCStructPipelineShaderStageCreateInfo
  , PipelineShaderStageCreateInfo(..)
  , PipelineTessellationStateCreateFlags
  , withCStructPipelineTessellationStateCreateInfo
  , fromCStructPipelineTessellationStateCreateInfo
  , PipelineTessellationStateCreateInfo(..)
  , PipelineVertexInputStateCreateFlags
  , withCStructPipelineVertexInputStateCreateInfo
  , fromCStructPipelineVertexInputStateCreateInfo
  , PipelineVertexInputStateCreateInfo(..)
  , PipelineViewportStateCreateFlags
  , withCStructPipelineViewportStateCreateInfo
  , fromCStructPipelineViewportStateCreateInfo
  , PipelineViewportStateCreateInfo(..)
  , PolygonMode
  , pattern POLYGON_MODE_FILL
  , pattern POLYGON_MODE_LINE
  , pattern POLYGON_MODE_POINT
  , PrimitiveTopology
  , pattern PRIMITIVE_TOPOLOGY_POINT_LIST
  , pattern PRIMITIVE_TOPOLOGY_LINE_LIST
  , pattern PRIMITIVE_TOPOLOGY_LINE_STRIP
  , pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
  , pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
  , pattern PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
  , pattern PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
  , pattern PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
  , pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
  , pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
  , pattern PRIMITIVE_TOPOLOGY_PATCH_LIST
  , withCStructRect2D
  , fromCStructRect2D
  , Rect2D(..)
  , RenderPass
  , SampleMask
  , ShaderStageFlagBits
  , pattern SHADER_STAGE_VERTEX_BIT
  , pattern SHADER_STAGE_TESSELLATION_CONTROL_BIT
  , pattern SHADER_STAGE_TESSELLATION_EVALUATION_BIT
  , pattern SHADER_STAGE_GEOMETRY_BIT
  , pattern SHADER_STAGE_FRAGMENT_BIT
  , pattern SHADER_STAGE_COMPUTE_BIT
  , pattern SHADER_STAGE_ALL_GRAPHICS
  , pattern SHADER_STAGE_ALL
  , withCStructSpecializationInfo
  , fromCStructSpecializationInfo
  , SpecializationInfo(..)
  , withCStructSpecializationMapEntry
  , fromCStructSpecializationMapEntry
  , SpecializationMapEntry(..)
  , StencilOp
  , pattern STENCIL_OP_KEEP
  , pattern STENCIL_OP_ZERO
  , pattern STENCIL_OP_REPLACE
  , pattern STENCIL_OP_INCREMENT_AND_CLAMP
  , pattern STENCIL_OP_DECREMENT_AND_CLAMP
  , pattern STENCIL_OP_INVERT
  , pattern STENCIL_OP_INCREMENT_AND_WRAP
  , pattern STENCIL_OP_DECREMENT_AND_WRAP
  , withCStructStencilOpState
  , fromCStructStencilOpState
  , StencilOpState(..)
  , withCStructVertexInputAttributeDescription
  , fromCStructVertexInputAttributeDescription
  , VertexInputAttributeDescription(..)
  , withCStructVertexInputBindingDescription
  , fromCStructVertexInputBindingDescription
  , VertexInputBindingDescription(..)
  , VertexInputRate
  , pattern VERTEX_INPUT_RATE_VERTEX
  , pattern VERTEX_INPUT_RATE_INSTANCE
  , withCStructViewport
  , fromCStructViewport
  , Viewport(..)
  , createComputePipelines
  , createGraphicsPipelines
  , destroyPipeline
  , withComputePipelines
  , withGraphicsPipelines
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Bits
  ( zeroBits
  )
import Data.ByteString
  ( ByteString
  , packCString
  , packCStringLen
  , useAsCString
  )
import qualified Data.ByteString
  ( empty
  , length
  )
import Data.ByteString.Unsafe
  ( unsafeUseAsCString
  )
import Data.Coerce
  ( coerce
  )
import Data.Function
  ( (&)
  )
import Data.Int
  ( Int32
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Vector.Generic.Sized
  ( fromTuple
  )
import qualified Data.Vector.Storable.Sized
  ( unsafeIndex
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  , CSize(..)
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkSampleCountFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkBlendFactor(..)
  , VkBlendOp(..)
  , VkColorComponentFlagBits(..)
  , VkCompareOp(..)
  , VkComputePipelineCreateInfo(..)
  , VkCullModeFlagBits(..)
  , VkDynamicState(..)
  , VkExtent2D(..)
  , VkFrontFace(..)
  , VkGraphicsPipelineCreateInfo(..)
  , VkLogicOp(..)
  , VkOffset2D(..)
  , VkPipelineColorBlendAttachmentState(..)
  , VkPipelineColorBlendStateCreateFlags(..)
  , VkPipelineColorBlendStateCreateInfo(..)
  , VkPipelineCreateFlagBits(..)
  , VkPipelineDepthStencilStateCreateFlags(..)
  , VkPipelineDepthStencilStateCreateInfo(..)
  , VkPipelineDynamicStateCreateFlags(..)
  , VkPipelineDynamicStateCreateInfo(..)
  , VkPipelineInputAssemblyStateCreateFlags(..)
  , VkPipelineInputAssemblyStateCreateInfo(..)
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
  , VkPrimitiveTopology(..)
  , VkRect2D(..)
  , VkShaderStageFlagBits(..)
  , VkSpecializationInfo(..)
  , VkSpecializationMapEntry(..)
  , VkStencilOp(..)
  , VkStencilOpState(..)
  , VkVertexInputAttributeDescription(..)
  , VkVertexInputBindingDescription(..)
  , VkVertexInputRate(..)
  , VkViewport(..)
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
  , VkSampleMask
  , vkCreateComputePipelines
  , vkCreateGraphicsPipelines
  , vkDestroyPipeline
  , pattern VK_BLEND_FACTOR_CONSTANT_ALPHA
  , pattern VK_BLEND_FACTOR_CONSTANT_COLOR
  , pattern VK_BLEND_FACTOR_DST_ALPHA
  , pattern VK_BLEND_FACTOR_DST_COLOR
  , pattern VK_BLEND_FACTOR_ONE
  , pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
  , pattern VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR
  , pattern VK_BLEND_FACTOR_SRC1_ALPHA
  , pattern VK_BLEND_FACTOR_SRC1_COLOR
  , pattern VK_BLEND_FACTOR_SRC_ALPHA
  , pattern VK_BLEND_FACTOR_SRC_ALPHA_SATURATE
  , pattern VK_BLEND_FACTOR_SRC_COLOR
  , pattern VK_BLEND_FACTOR_ZERO
  , pattern VK_BLEND_OP_ADD
  , pattern VK_BLEND_OP_MAX
  , pattern VK_BLEND_OP_MIN
  , pattern VK_BLEND_OP_REVERSE_SUBTRACT
  , pattern VK_BLEND_OP_SUBTRACT
  , pattern VK_COLOR_COMPONENT_A_BIT
  , pattern VK_COLOR_COMPONENT_B_BIT
  , pattern VK_COLOR_COMPONENT_G_BIT
  , pattern VK_COLOR_COMPONENT_R_BIT
  , pattern VK_COMPARE_OP_ALWAYS
  , pattern VK_COMPARE_OP_EQUAL
  , pattern VK_COMPARE_OP_GREATER
  , pattern VK_COMPARE_OP_GREATER_OR_EQUAL
  , pattern VK_COMPARE_OP_LESS
  , pattern VK_COMPARE_OP_LESS_OR_EQUAL
  , pattern VK_COMPARE_OP_NEVER
  , pattern VK_COMPARE_OP_NOT_EQUAL
  , pattern VK_CULL_MODE_BACK_BIT
  , pattern VK_CULL_MODE_FRONT_AND_BACK
  , pattern VK_CULL_MODE_FRONT_BIT
  , pattern VK_CULL_MODE_NONE
  , pattern VK_DYNAMIC_STATE_BLEND_CONSTANTS
  , pattern VK_DYNAMIC_STATE_DEPTH_BIAS
  , pattern VK_DYNAMIC_STATE_DEPTH_BOUNDS
  , pattern VK_DYNAMIC_STATE_LINE_WIDTH
  , pattern VK_DYNAMIC_STATE_SCISSOR
  , pattern VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK
  , pattern VK_DYNAMIC_STATE_STENCIL_REFERENCE
  , pattern VK_DYNAMIC_STATE_STENCIL_WRITE_MASK
  , pattern VK_DYNAMIC_STATE_VIEWPORT
  , pattern VK_FRONT_FACE_CLOCKWISE
  , pattern VK_FRONT_FACE_COUNTER_CLOCKWISE
  , pattern VK_LOGIC_OP_AND
  , pattern VK_LOGIC_OP_AND_INVERTED
  , pattern VK_LOGIC_OP_AND_REVERSE
  , pattern VK_LOGIC_OP_CLEAR
  , pattern VK_LOGIC_OP_COPY
  , pattern VK_LOGIC_OP_COPY_INVERTED
  , pattern VK_LOGIC_OP_EQUIVALENT
  , pattern VK_LOGIC_OP_INVERT
  , pattern VK_LOGIC_OP_NAND
  , pattern VK_LOGIC_OP_NOR
  , pattern VK_LOGIC_OP_NO_OP
  , pattern VK_LOGIC_OP_OR
  , pattern VK_LOGIC_OP_OR_INVERTED
  , pattern VK_LOGIC_OP_OR_REVERSE
  , pattern VK_LOGIC_OP_SET
  , pattern VK_LOGIC_OP_XOR
  , pattern VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
  , pattern VK_PIPELINE_CREATE_DERIVATIVE_BIT
  , pattern VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
  , pattern VK_POLYGON_MODE_FILL
  , pattern VK_POLYGON_MODE_LINE
  , pattern VK_POLYGON_MODE_POINT
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP
  , pattern VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_PATCH_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_POINT_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
  , pattern VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
  , pattern VK_SHADER_STAGE_ALL
  , pattern VK_SHADER_STAGE_ALL_GRAPHICS
  , pattern VK_SHADER_STAGE_COMPUTE_BIT
  , pattern VK_SHADER_STAGE_FRAGMENT_BIT
  , pattern VK_SHADER_STAGE_GEOMETRY_BIT
  , pattern VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
  , pattern VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
  , pattern VK_SHADER_STAGE_VERTEX_BIT
  , pattern VK_STENCIL_OP_DECREMENT_AND_CLAMP
  , pattern VK_STENCIL_OP_DECREMENT_AND_WRAP
  , pattern VK_STENCIL_OP_INCREMENT_AND_CLAMP
  , pattern VK_STENCIL_OP_INCREMENT_AND_WRAP
  , pattern VK_STENCIL_OP_INVERT
  , pattern VK_STENCIL_OP_KEEP
  , pattern VK_STENCIL_OP_REPLACE
  , pattern VK_STENCIL_OP_ZERO
  , pattern VK_VERTEX_INPUT_RATE_INSTANCE
  , pattern VK_VERTEX_INPUT_RATE_VERTEX
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , SampleCountFlagBits
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCache
  )
import Graphics.Vulkan.Core10.Shader
  ( ShaderModule
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( padVector
  , withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- | VkBlendFactor - Framebuffer blending factors
--
-- = Description
--
-- The semantics of each enum value is described in the table below:
--
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VkBl | RGB Blend Factors | Alpha  |
-- > | endFactor'                              | (Sr,Sg,Sb) or     | Blend  |
-- > |                                         | (Dr,Dg,Db)        | Factor |
-- > |                                         |                   | (Sa or |
-- > |                                         |                   | Da)    |
-- > +=========================================+===================+========+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (0,0,0)           | 0      |
-- > | LEND_FACTOR_ZERO'                       |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (1,1,1)           | 1      |
-- > | LEND_FACTOR_ONE'                        |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (Rs0,Gs0,Bs0)     | As0    |
-- > | LEND_FACTOR_SRC_COLOR'                  |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (1-Rs0,1-Gs0,1-Bs | 1-As0  |
-- > | LEND_FACTOR_ONE_MINUS_SRC_COLOR'        | 0)                |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (Rd,Gd,Bd)        | Ad     |
-- > | LEND_FACTOR_DST_COLOR'                  |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (1-Rd,1-Gd,1-Bd)  | 1-Ad   |
-- > | LEND_FACTOR_ONE_MINUS_DST_COLOR'        |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (As0,As0,As0)     | As0    |
-- > | LEND_FACTOR_SRC_ALPHA'                  |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (1-As0,1-As0,1-As | 1-As0  |
-- > | LEND_FACTOR_ONE_MINUS_SRC_ALPHA'        | 0)                |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (Ad,Ad,Ad)        | Ad     |
-- > | LEND_FACTOR_DST_ALPHA'                  |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (1-Ad,1-Ad,1-Ad)  | 1-Ad   |
-- > | LEND_FACTOR_ONE_MINUS_DST_ALPHA'        |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (Rc,Gc,Bc)        | Ac     |
-- > | LEND_FACTOR_CONSTANT_COLOR'             |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (1-Rc,1-Gc,1-Bc)  | 1-Ac   |
-- > | LEND_FACTOR_ONE_MINUS_CONSTANT_COLOR'   |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (Ac,Ac,Ac)        | Ac     |
-- > | LEND_FACTOR_CONSTANT_ALPHA'             |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (1-Ac,1-Ac,1-Ac)  | 1-Ac   |
-- > | LEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA'   |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (f,f,f); f =      | 1      |
-- > | LEND_FACTOR_SRC_ALPHA_SATURATE'         | min(As0,1-Ad)     |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (Rs1,Gs1,Bs1)     | As1    |
-- > | LEND_FACTOR_SRC1_COLOR'                 |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (1-Rs1,1-Gs1,1-Bs | 1-As1  |
-- > | LEND_FACTOR_ONE_MINUS_SRC1_COLOR'       | 1)                |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (As1,As1,As1)     | As1    |
-- > | LEND_FACTOR_SRC1_ALPHA'                 |                   |        |
-- > +-----------------------------------------+-------------------+--------+
-- > | 'Graphics.Vulkan.C.Core10.Pipeline.VK_B | (1-As1,1-As1,1-As | 1-As1  |
-- > | LEND_FACTOR_ONE_MINUS_SRC1_ALPHA'       | 1)                |        |
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
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState'
type BlendFactor = VkBlendFactor


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ZERO"
pattern BLEND_FACTOR_ZERO :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ZERO = VK_BLEND_FACTOR_ZERO


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ONE"
pattern BLEND_FACTOR_ONE :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ONE = VK_BLEND_FACTOR_ONE


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_SRC_COLOR"
pattern BLEND_FACTOR_SRC_COLOR :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_SRC_COLOR = VK_BLEND_FACTOR_SRC_COLOR


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ONE_MINUS_SRC_COLOR"
pattern BLEND_FACTOR_ONE_MINUS_SRC_COLOR :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ONE_MINUS_SRC_COLOR = VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_DST_COLOR"
pattern BLEND_FACTOR_DST_COLOR :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_DST_COLOR = VK_BLEND_FACTOR_DST_COLOR


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ONE_MINUS_DST_COLOR"
pattern BLEND_FACTOR_ONE_MINUS_DST_COLOR :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ONE_MINUS_DST_COLOR = VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_SRC_ALPHA"
pattern BLEND_FACTOR_SRC_ALPHA :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_SRC_ALPHA = VK_BLEND_FACTOR_SRC_ALPHA


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ONE_MINUS_SRC_ALPHA"
pattern BLEND_FACTOR_ONE_MINUS_SRC_ALPHA :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ONE_MINUS_SRC_ALPHA = VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_DST_ALPHA"
pattern BLEND_FACTOR_DST_ALPHA :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_DST_ALPHA = VK_BLEND_FACTOR_DST_ALPHA


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ONE_MINUS_DST_ALPHA"
pattern BLEND_FACTOR_ONE_MINUS_DST_ALPHA :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ONE_MINUS_DST_ALPHA = VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_CONSTANT_COLOR"
pattern BLEND_FACTOR_CONSTANT_COLOR :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_CONSTANT_COLOR = VK_BLEND_FACTOR_CONSTANT_COLOR


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR"
pattern BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR = VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_CONSTANT_ALPHA"
pattern BLEND_FACTOR_CONSTANT_ALPHA :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_CONSTANT_ALPHA = VK_BLEND_FACTOR_CONSTANT_ALPHA


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA"
pattern BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA = VK_BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_SRC_ALPHA_SATURATE"
pattern BLEND_FACTOR_SRC_ALPHA_SATURATE :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_SRC_ALPHA_SATURATE = VK_BLEND_FACTOR_SRC_ALPHA_SATURATE


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_SRC1_COLOR"
pattern BLEND_FACTOR_SRC1_COLOR :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_SRC1_COLOR = VK_BLEND_FACTOR_SRC1_COLOR


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ONE_MINUS_SRC1_COLOR"
pattern BLEND_FACTOR_ONE_MINUS_SRC1_COLOR :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ONE_MINUS_SRC1_COLOR = VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_SRC1_ALPHA"
pattern BLEND_FACTOR_SRC1_ALPHA :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_SRC1_ALPHA = VK_BLEND_FACTOR_SRC1_ALPHA


-- No documentation found for Nested "BlendFactor" "BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA"
pattern BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA :: (a ~ BlendFactor) => a
pattern BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA = VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA

-- | VkBlendOp - Framebuffer blending operations
--
-- = Description
--
-- The semantics of each basic blend operations is described in the table
-- below:
--
-- > +-------------------------------+--------------------+----------------+
-- > | 'Graphics.Vulkan.C.Core10.Pip | RGB Components     | Alpha          |
-- > | eline.VkBlendOp'              |                    | Component      |
-- > +===============================+====================+================+
-- > | 'Graphics.Vulkan.C.Core10.Pip | R = Rs0 × Sr + Rd  | A = As0 × Sa + |
-- > | eline.VK_BLEND_OP_ADD'        | × Dr               | Ad × Da        |
-- > |                               | G = Gs0 × Sg + Gd  |                |
-- > |                               | × Dg               |                |
-- > |                               | B = Bs0 × Sb + Bd  |                |
-- > |                               | × Db               |                |
-- > +-------------------------------+--------------------+----------------+
-- > | 'Graphics.Vulkan.C.Core10.Pip | R = Rs0 × Sr - Rd  | A = As0 × Sa - |
-- > | eline.VK_BLEND_OP_SUBTRACT'   | × Dr               | Ad × Da        |
-- > |                               | G = Gs0 × Sg - Gd  |                |
-- > |                               | × Dg               |                |
-- > |                               | B = Bs0 × Sb - Bd  |                |
-- > |                               | × Db               |                |
-- > +-------------------------------+--------------------+----------------+
-- > | 'Graphics.Vulkan.C.Core10.Pip | R = Rd × Dr - Rs0  | A = Ad × Da -  |
-- > | eline.VK_BLEND_OP_REVERSE_SUB | × Sr               | As0 × Sa       |
-- > | TRACT'                        | G = Gd × Dg - Gs0  |                |
-- > |                               | × Sg               |                |
-- > |                               | B = Bd × Db - Bs0  |                |
-- > |                               | × Sb               |                |
-- > +-------------------------------+--------------------+----------------+
-- > | 'Graphics.Vulkan.C.Core10.Pip | R = min(Rs0,Rd)    | A =            |
-- > | eline.VK_BLEND_OP_MIN'        | G = min(Gs0,Gd)    | min(As0,Ad)    |
-- > |                               | B = min(Bs0,Bd)    |                |
-- > +-------------------------------+--------------------+----------------+
-- > | 'Graphics.Vulkan.C.Core10.Pip | R = max(Rs0,Rd)    | A =            |
-- > | eline.VK_BLEND_OP_MAX'        | G = max(Gs0,Gd)    | max(As0,Ad)    |
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
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState'
type BlendOp = VkBlendOp


-- No documentation found for Nested "BlendOp" "BLEND_OP_ADD"
pattern BLEND_OP_ADD :: (a ~ BlendOp) => a
pattern BLEND_OP_ADD = VK_BLEND_OP_ADD


-- No documentation found for Nested "BlendOp" "BLEND_OP_SUBTRACT"
pattern BLEND_OP_SUBTRACT :: (a ~ BlendOp) => a
pattern BLEND_OP_SUBTRACT = VK_BLEND_OP_SUBTRACT


-- No documentation found for Nested "BlendOp" "BLEND_OP_REVERSE_SUBTRACT"
pattern BLEND_OP_REVERSE_SUBTRACT :: (a ~ BlendOp) => a
pattern BLEND_OP_REVERSE_SUBTRACT = VK_BLEND_OP_REVERSE_SUBTRACT


-- No documentation found for Nested "BlendOp" "BLEND_OP_MIN"
pattern BLEND_OP_MIN :: (a ~ BlendOp) => a
pattern BLEND_OP_MIN = VK_BLEND_OP_MIN


-- No documentation found for Nested "BlendOp" "BLEND_OP_MAX"
pattern BLEND_OP_MAX :: (a ~ BlendOp) => a
pattern BLEND_OP_MAX = VK_BLEND_OP_MAX

-- | VkColorComponentFlagBits - Bitmask controlling which components are
-- written to the framebuffer
--
-- = Description
--
-- The color write mask operation is applied regardless of whether blending
-- is enabled.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkColorComponentFlags'
type ColorComponentFlagBits = VkColorComponentFlagBits


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COLOR_COMPONENT_R_BIT' specifies
-- that the R value is written to the color attachment for the appropriate
-- sample. Otherwise, the value in memory is unmodified.
pattern COLOR_COMPONENT_R_BIT :: (a ~ ColorComponentFlagBits) => a
pattern COLOR_COMPONENT_R_BIT = VK_COLOR_COMPONENT_R_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COLOR_COMPONENT_G_BIT' specifies
-- that the G value is written to the color attachment for the appropriate
-- sample. Otherwise, the value in memory is unmodified.
pattern COLOR_COMPONENT_G_BIT :: (a ~ ColorComponentFlagBits) => a
pattern COLOR_COMPONENT_G_BIT = VK_COLOR_COMPONENT_G_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COLOR_COMPONENT_B_BIT' specifies
-- that the B value is written to the color attachment for the appropriate
-- sample. Otherwise, the value in memory is unmodified.
pattern COLOR_COMPONENT_B_BIT :: (a ~ ColorComponentFlagBits) => a
pattern COLOR_COMPONENT_B_BIT = VK_COLOR_COMPONENT_B_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COLOR_COMPONENT_A_BIT' specifies
-- that the A value is written to the color attachment for the appropriate
-- sample. Otherwise, the value in memory is unmodified.
pattern COLOR_COMPONENT_A_BIT :: (a ~ ColorComponentFlagBits) => a
pattern COLOR_COMPONENT_A_BIT = VK_COLOR_COMPONENT_A_BIT

-- | VkColorComponentFlags - Bitmask of VkColorComponentFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkColorComponentFlags' is a bitmask
-- type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkColorComponentFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkColorComponentFlagBits',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState'
type ColorComponentFlags = ColorComponentFlagBits

-- | VkCompareOp - Stencil comparison function
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkStencilOpState'
type CompareOp = VkCompareOp


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COMPARE_OP_NEVER' specifies that
-- the test never passes.
pattern COMPARE_OP_NEVER :: (a ~ CompareOp) => a
pattern COMPARE_OP_NEVER = VK_COMPARE_OP_NEVER


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COMPARE_OP_LESS' specifies that
-- the test passes when R \< S.
pattern COMPARE_OP_LESS :: (a ~ CompareOp) => a
pattern COMPARE_OP_LESS = VK_COMPARE_OP_LESS


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COMPARE_OP_EQUAL' specifies that
-- the test passes when R = S.
pattern COMPARE_OP_EQUAL :: (a ~ CompareOp) => a
pattern COMPARE_OP_EQUAL = VK_COMPARE_OP_EQUAL


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COMPARE_OP_LESS_OR_EQUAL'
-- specifies that the test passes when R ≤ S.
pattern COMPARE_OP_LESS_OR_EQUAL :: (a ~ CompareOp) => a
pattern COMPARE_OP_LESS_OR_EQUAL = VK_COMPARE_OP_LESS_OR_EQUAL


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COMPARE_OP_GREATER' specifies that
-- the test passes when R > S.
pattern COMPARE_OP_GREATER :: (a ~ CompareOp) => a
pattern COMPARE_OP_GREATER = VK_COMPARE_OP_GREATER


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COMPARE_OP_NOT_EQUAL' specifies
-- that the test passes when R ≠ S.
pattern COMPARE_OP_NOT_EQUAL :: (a ~ CompareOp) => a
pattern COMPARE_OP_NOT_EQUAL = VK_COMPARE_OP_NOT_EQUAL


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COMPARE_OP_GREATER_OR_EQUAL'
-- specifies that the test passes when R ≥ S.
pattern COMPARE_OP_GREATER_OR_EQUAL :: (a ~ CompareOp) => a
pattern COMPARE_OP_GREATER_OR_EQUAL = VK_COMPARE_OP_GREATER_OR_EQUAL


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_COMPARE_OP_ALWAYS' specifies that
-- the test always passes.
pattern COMPARE_OP_ALWAYS :: (a ~ CompareOp) => a
pattern COMPARE_OP_ALWAYS = VK_COMPARE_OP_ALWAYS


-- | VkComputePipelineCreateInfo - Structure specifying parameters of a newly
-- created compute pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- @stage@ points to a structure of type
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo'.
--
-- == Valid Usage
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be
--     a valid handle to a compute
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be a valid index into the calling
--     command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not -1, @basePipelineHandle@ /must/
--     be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be -1
--
-- -   The @stage@ member of @stage@ /must/ be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_COMPUTE_BIT'
--
-- -   The shader code for the entry point identified by @stage@ and the
--     rest of the state identified by this structure /must/ adhere to the
--     pipeline linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   @layout@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with the layout of the compute shader specified in @stage@
--
-- -   The number of resources in @layout@ accessible to the compute shader
--     stage /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageResources@
--
-- Unresolved directive in VkComputePipelineCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkComputePipelineCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines'
data ComputePipelineCreateInfo = ComputePipelineCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ComputePipelineCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "flags"
  flags :: PipelineCreateFlags
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "stage"
  stage :: PipelineShaderStageCreateInfo
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "layout"
  layout :: PipelineLayout
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "basePipelineHandle"
  basePipelineHandle :: Pipeline
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "basePipelineIndex"
  basePipelineIndex :: Int32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkComputePipelineCreateInfo' and
-- marshal a 'ComputePipelineCreateInfo' into it. The 'VkComputePipelineCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructComputePipelineCreateInfo :: ComputePipelineCreateInfo -> (VkComputePipelineCreateInfo -> IO a) -> IO a
withCStructComputePipelineCreateInfo marshalled cont = withCStructPipelineShaderStageCreateInfo (stage (marshalled :: ComputePipelineCreateInfo)) (\stage'' -> maybeWith withSomeVkStruct (next (marshalled :: ComputePipelineCreateInfo)) (\pPNext -> cont (VkComputePipelineCreateInfo VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO pPNext (flags (marshalled :: ComputePipelineCreateInfo)) stage'' (layout (marshalled :: ComputePipelineCreateInfo)) (basePipelineHandle (marshalled :: ComputePipelineCreateInfo)) (basePipelineIndex (marshalled :: ComputePipelineCreateInfo)))))

-- | A function to read a 'VkComputePipelineCreateInfo' and all additional
-- structures in the pointer chain into a 'ComputePipelineCreateInfo'.
fromCStructComputePipelineCreateInfo :: VkComputePipelineCreateInfo -> IO ComputePipelineCreateInfo
fromCStructComputePipelineCreateInfo c = ComputePipelineCreateInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkComputePipelineCreateInfo)))
                                                                   <*> pure (vkFlags (c :: VkComputePipelineCreateInfo))
                                                                   <*> (fromCStructPipelineShaderStageCreateInfo (vkStage (c :: VkComputePipelineCreateInfo)))
                                                                   <*> pure (vkLayout (c :: VkComputePipelineCreateInfo))
                                                                   <*> pure (vkBasePipelineHandle (c :: VkComputePipelineCreateInfo))
                                                                   <*> pure (vkBasePipelineIndex (c :: VkComputePipelineCreateInfo))

instance Zero ComputePipelineCreateInfo where
  zero = ComputePipelineCreateInfo Nothing
                                   zero
                                   zero
                                   zero
                                   zero
                                   zero


-- | VkCullModeFlagBits - Bitmask controlling triangle culling
--
-- = Description
--
-- Following culling, fragments are produced for any triangles which have
-- not been discarded.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkCullModeFlags'
type CullModeFlagBits = VkCullModeFlagBits


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_CULL_MODE_FRONT_BIT' specifies
-- that front-facing triangles are discarded
pattern CULL_MODE_FRONT_BIT :: (a ~ CullModeFlagBits) => a
pattern CULL_MODE_FRONT_BIT = VK_CULL_MODE_FRONT_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_CULL_MODE_BACK_BIT' specifies that
-- back-facing triangles are discarded
pattern CULL_MODE_BACK_BIT :: (a ~ CullModeFlagBits) => a
pattern CULL_MODE_BACK_BIT = VK_CULL_MODE_BACK_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_CULL_MODE_NONE' specifies that no
-- triangles are discarded
pattern CULL_MODE_NONE :: (a ~ CullModeFlagBits) => a
pattern CULL_MODE_NONE = VK_CULL_MODE_NONE


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_CULL_MODE_FRONT_AND_BACK'
-- specifies that all triangles are discarded.
pattern CULL_MODE_FRONT_AND_BACK :: (a ~ CullModeFlagBits) => a
pattern CULL_MODE_FRONT_AND_BACK = VK_CULL_MODE_FRONT_AND_BACK

-- | VkCullModeFlags - Bitmask of VkCullModeFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkCullModeFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkCullModeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkCullModeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
type CullModeFlags = CullModeFlagBits

-- | VkDynamicState - Indicate which dynamic state is taken from dynamic
-- state commands
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDynamicStateCreateInfo'
type DynamicState = VkDynamicState


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_VIEWPORT' specifies
-- that the @pViewports@ state in
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetViewport' before
-- any draw commands. The number of viewports used by a pipeline is still
-- specified by the @viewportCount@ member of
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'.
pattern DYNAMIC_STATE_VIEWPORT :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_VIEWPORT = VK_DYNAMIC_STATE_VIEWPORT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_SCISSOR' specifies
-- that the @pScissors@ state in
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetScissor' before
-- any draw commands. The number of scissor rectangles used by a pipeline
-- is still specified by the @scissorCount@ member of
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'.
pattern DYNAMIC_STATE_SCISSOR :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_SCISSOR = VK_DYNAMIC_STATE_SCISSOR


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_LINE_WIDTH'
-- specifies that the @lineWidth@ state in
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetLineWidth'
-- before any draw commands that generate line primitives for the
-- rasterizer.
pattern DYNAMIC_STATE_LINE_WIDTH :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_LINE_WIDTH = VK_DYNAMIC_STATE_LINE_WIDTH


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_DEPTH_BIAS'
-- specifies that the @depthBiasConstantFactor@, @depthBiasClamp@ and
-- @depthBiasSlopeFactor@ states in
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetDepthBias'
-- before any draws are performed with @depthBiasEnable@ in
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
-- set to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'.
pattern DYNAMIC_STATE_DEPTH_BIAS :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_DEPTH_BIAS = VK_DYNAMIC_STATE_DEPTH_BIAS


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_BLEND_CONSTANTS'
-- specifies that the @blendConstants@ state in
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetBlendConstants'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState'
-- member @blendEnable@ set to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' and
-- any of the blend functions using a constant blend color.
pattern DYNAMIC_STATE_BLEND_CONSTANTS :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_BLEND_CONSTANTS = VK_DYNAMIC_STATE_BLEND_CONSTANTS


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_DEPTH_BOUNDS'
-- specifies that the @minDepthBounds@ and @maxDepthBounds@ states of
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
-- will be ignored and /must/ be set dynamically with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetDepthBounds'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
-- member @depthBoundsTestEnable@ set to
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'.
pattern DYNAMIC_STATE_DEPTH_BOUNDS :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_DEPTH_BOUNDS = VK_DYNAMIC_STATE_DEPTH_BOUNDS


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK'
-- specifies that the @compareMask@ state in
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
-- for both @front@ and @back@ will be ignored and /must/ be set
-- dynamically with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
-- member @stencilTestEnable@ set to
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
pattern DYNAMIC_STATE_STENCIL_COMPARE_MASK :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_STENCIL_COMPARE_MASK = VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_STENCIL_WRITE_MASK'
-- specifies that the @writeMask@ state in
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
-- for both @front@ and @back@ will be ignored and /must/ be set
-- dynamically with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilWriteMask'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
-- member @stencilTestEnable@ set to
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
pattern DYNAMIC_STATE_STENCIL_WRITE_MASK :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_STENCIL_WRITE_MASK = VK_DYNAMIC_STATE_STENCIL_WRITE_MASK


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_STENCIL_REFERENCE'
-- specifies that the @reference@ state in
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
-- for both @front@ and @back@ will be ignored and /must/ be set
-- dynamically with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilReference'
-- before any draws are performed with a pipeline state with
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
-- member @stencilTestEnable@ set to
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
pattern DYNAMIC_STATE_STENCIL_REFERENCE :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_STENCIL_REFERENCE = VK_DYNAMIC_STATE_STENCIL_REFERENCE


-- | VkExtent2D - Structure specifying a two-dimensional extent
--
-- = Description
--
-- Unresolved directive in VkExtent2D.txt -
-- include::{generated}\/validity\/structs\/VkExtent2D.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.C.Core10.Pass.vkGetRenderAreaGranularity'
data Extent2D = Extent2D
  { -- No documentation found for Nested "Extent2D" "width"
  width :: Word32
  , -- No documentation found for Nested "Extent2D" "height"
  height :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExtent2D' and
-- marshal a 'Extent2D' into it. The 'VkExtent2D' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExtent2D :: Extent2D -> (VkExtent2D -> IO a) -> IO a
withCStructExtent2D marshalled cont = cont (VkExtent2D (width (marshalled :: Extent2D)) (height (marshalled :: Extent2D)))

-- | A function to read a 'VkExtent2D' and all additional
-- structures in the pointer chain into a 'Extent2D'.
fromCStructExtent2D :: VkExtent2D -> IO Extent2D
fromCStructExtent2D c = Extent2D <$> pure (vkWidth (c :: VkExtent2D))
                                 <*> pure (vkHeight (c :: VkExtent2D))

instance Zero Extent2D where
  zero = Extent2D zero
                  zero


-- | VkFrontFace - Interpret polygon front-facing orientation
--
-- = Description
--
-- Any triangle which is not front-facing is back-facing, including
-- zero-area triangles.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
type FrontFace = VkFrontFace


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_FRONT_FACE_COUNTER_CLOCKWISE'
-- specifies that a triangle with positive area is considered front-facing.
pattern FRONT_FACE_COUNTER_CLOCKWISE :: (a ~ FrontFace) => a
pattern FRONT_FACE_COUNTER_CLOCKWISE = VK_FRONT_FACE_COUNTER_CLOCKWISE


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_FRONT_FACE_CLOCKWISE' specifies
-- that a triangle with negative area is considered front-facing.
pattern FRONT_FACE_CLOCKWISE :: (a ~ FrontFace) => a
pattern FRONT_FACE_CLOCKWISE = VK_FRONT_FACE_CLOCKWISE


-- | VkGraphicsPipelineCreateInfo - Structure specifying parameters of a
-- newly created graphics pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- @pStages@ points to an array of
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo'
-- structures, which were previously described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-compute Compute Pipelines>.
--
-- @pDynamicState@ points to a structure of type
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDynamicStateCreateInfo'.
--
-- If any shader stage fails to compile, the compile log will be reported
-- back to the application, and
-- 'Graphics.Vulkan.C.Extensions.VK_NV_glsl_shader.VK_ERROR_INVALID_SHADER_NV'
-- will be generated.
--
-- == Valid Usage
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be
--     a valid handle to a graphics
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be a valid index into the calling
--     command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not -1, @basePipelineHandle@ /must/
--     be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be -1
--
-- -   The @stage@ member of each element of @pStages@ /must/ be unique
--
-- -   The geometric shader stages provided in @pStages@ /must/ be either
--     from the mesh shading pipeline (@stage@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_SHADER_STAGE_TASK_BIT_NV'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_SHADER_STAGE_MESH_BIT_NV')
--     or from the primitive shading pipeline (@stage@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_VERTEX_BIT',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_GEOMETRY_BIT').
--
-- -   The @stage@ member of one element of @pStages@ /must/ be either
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_VERTEX_BIT' or
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_SHADER_STAGE_MESH_BIT_NV'.
--
-- -   The @stage@ member of each element of @pStages@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_COMPUTE_BIT'
--
-- -   If @pStages@ includes a tessellation control shader stage, it /must/
--     include a tessellation evaluation shader stage
--
-- -   If @pStages@ includes a tessellation evaluation shader stage, it
--     /must/ include a tessellation control shader stage
--
-- -   If @pStages@ includes a tessellation control shader stage and a
--     tessellation evaluation shader stage, @pTessellationState@ /must/ be
--     a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineTessellationStateCreateInfo'
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
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_PATCH_LIST'
--
-- -   If the @topology@ member of @pInputAssembly@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     @pStages@ /must/ include tessellation shader stages
--
-- -   If @pStages@ includes a geometry shader stage, and does not include
--     any tessellation shader stages, its shader code /must/ contain an
--     @OpExecutionMode@ instruction that specifies an input primitive type
--     that is
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-geometry-execution compatible>
--     with the primitive topology specified in @pInputAssembly@
--
-- -   If @pStages@ includes a geometry shader stage, and also includes
--     tessellation shader stages, its shader code /must/ contain an
--     @OpExecutionMode@ instruction that specifies an input primitive type
--     that is
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-geometry-execution compatible>
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
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED' in
--     @subpass@
--
-- -   The shader code for the entry points identified by @pStages@, and
--     the rest of the state identified by this structure /must/ adhere to
--     the pipeline linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     in the 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' defined
--     by @subpass@, the @depthWriteEnable@ member of @pDepthStencilState@
--     /must/ be 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     in the 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' defined
--     by @subpass@, the @failOp@, @passOp@ and @depthFailOp@ members of
--     each of the @front@ and @back@ members of @pDepthStencilState@
--     /must/ be 'Graphics.Vulkan.C.Core10.Pipeline.VK_STENCIL_OP_KEEP'
--
-- -   If rasterization is not disabled and the subpass uses color
--     attachments, then for each color attachment in the subpass the
--     @blendEnable@ member of the corresponding element of the
--     @pAttachment@ member of @pColorBlendState@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE' if the attached image’s
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-format-features format features>
--     does not contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT'.
--
-- -   If rasterization is not disabled and the subpass uses color
--     attachments, the @attachmentCount@ member of @pColorBlendState@
--     /must/ be equal to the @colorAttachmentCount@ used to create
--     @subpass@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_VIEWPORT', the
--     @pViewports@ member of @pViewportState@ /must/ be a valid pointer to
--     an array of @pViewportState@::@viewportCount@ valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkViewport' structures
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_SCISSOR', the
--     @pScissors@ member of @pViewportState@ /must/ be a valid pointer to
--     an array of @pViewportState@::@scissorCount@
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures
--
-- -   If the wide lines feature is not enabled, and no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_LINE_WIDTH', the
--     @lineWidth@ member of @pRasterizationState@ /must/ be @1.0@
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE', @pViewportState@ /must/ be
--     a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'
--     structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE', @pMultisampleState@ /must/
--     be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'
--     structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE', and @subpass@ uses a
--     depth\/stencil attachment, @pDepthStencilState@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
--     structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE', and @subpass@ uses color
--     attachments, @pColorBlendState@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo'
--     structure
--
-- -   If the depth bias clamping feature is not enabled, no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_DEPTH_BIAS', and
--     the @depthBiasEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the @depthBiasClamp@ member
--     of @pRasterizationState@ /must/ be @0.0@
--
-- -   If the
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
--     extension is not enabled and no element of the @pDynamicStates@
--     member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_DEPTH_BOUNDS',
--     and the @depthBoundsTestEnable@ member of @pDepthStencilState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the @minDepthBounds@ and
--     @maxDepthBounds@ members of @pDepthStencilState@ /must/ be between
--     @0.0@ and @1.0@, inclusive
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT',
--     and the @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     structure chained to the @pNext@ chain of @pMultisampleState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     @sampleLocationsInfo.sampleLocationGridSize.width@ /must/ evenly
--     divide
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkMultisamplePropertiesEXT'::@sampleLocationGridSize.width@
--     as returned by
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.vkGetPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT',
--     and the @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     structure chained to the @pNext@ chain of @pMultisampleState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     @sampleLocationsInfo.sampleLocationGridSize.height@ /must/ evenly
--     divide
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkMultisamplePropertiesEXT'::@sampleLocationGridSize.height@
--     as returned by
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.vkGetPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT',
--     and the @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     structure chained to the @pNext@ chain of @pMultisampleState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     @sampleLocationsInfo.sampleLocationsPerPixel@ /must/ equal
--     @rasterizationSamples@
--
-- -   If the @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     structure chained to the @pNext@ chain of @pMultisampleState@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the fragment shader code
--     /must/ not statically use the extended instruction
--     @InterpolateAtSample@
--
-- -   @layout@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with all shaders specified in @pStages@
--
-- -   If neither the @VK_AMD_mixed_attachment_samples@ nor the
--     @VK_NV_framebuffer_mixed_samples@ extensions are enabled, and if
--     @subpass@ uses color and\/or depth\/stencil attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be the
--     same as the sample count for those subpass attachments
--
-- -   If the @VK_AMD_mixed_attachment_samples@ extension is enabled, and
--     if @subpass@ uses color and\/or depth\/stencil attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ equal
--     the maximum of the sample counts of those subpass attachments
--
-- -   If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     if @subpass@ has a depth\/stencil attachment and depth test, stencil
--     test, or depth bounds test are enabled, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be the
--     same as the sample count of the depth\/stencil attachment
--
-- -   If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     if @subpass@ has any color attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be
--     greater than or equal to the sample count for those subpass
--     attachments
--
-- -   If @subpass@ does not use any color and\/or depth\/stencil
--     attachments, then the @rasterizationSamples@ member of
--     @pMultisampleState@ /must/ follow the rules for a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-noattachments zero-attachment subpass>
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
-- -   @flags@ /must/ not contain the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_PIPELINE_CREATE_DISPATCH_BASE'
--     flag.
--
-- -   If @pStages@ includes a fragment shader stage and an input
--     attachment was referenced by the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo'
--     at @renderPass@ create time, its shader code /must/ not read from
--     any aspect that was not specified in the @aspectMask@ of the
--     corresponding
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkInputAttachmentAspectReference'
--     structure.
--
-- -   The number of resources in @layout@ accessible to each shader stage
--     that is used by the pipeline /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV',
--     and the @viewportWScalingEnable@ member of a
--     'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'
--     structure, chained to the @pNext@ chain of @pViewportState@, is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the @pViewportWScalings@
--     member of the
--     'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'
--     /must/ be a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.VkViewportWScalingNV'
--     structures
--
-- -   If @pStages@ includes a vertex shader stage, @pVertexInputState@
--     /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateInfo'
--     structure
--
-- -   If @pStages@ includes a vertex shader stage, @pInputAssemblyState@
--     /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineInputAssemblyStateCreateInfo'
--     structure
--
-- -   The @Xfb@ execution mode /can/ be specified by only one shader stage
--     in @pStages@
--
-- -   If any shader stage in @pStages@ specifies @Xfb@ execution mode it
--     /must/ be the last vertex processing stage
--
-- -   If a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--     value other than zero is specified, all variables in the output
--     interface of the entry point being compiled decorated with
--     @Position@, @PointSize@, @ClipDistance@, or @CullDistance@ /must/
--     all be decorated with identical @Stream@ values that match the
--     @rasterizationStream@
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--     is zero, or not specified, all variables in the output interface of
--     the entry point being compiled decorated with @Position@,
--     @PointSize@, @ClipDistance@, or @CullDistance@ /must/ all be
--     decorated with a @Stream@ value of zero, or /must/ not specify the
--     @Stream@ decoration
--
-- -   If the last vertex processing stage is a geometry shader, and that
--     geometry shader uses the @GeometryStreams@ capability, then
--     'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackFeaturesEXT'::@geometryStreams@
--     feature /must/ be enabled
--
-- -   If there are any mesh shader stages in the pipeline there /must/ not
--     be any shader stage in the pipeline with a @Xfb@ execution mode.
--
-- Unresolved directive in VkGraphicsPipelineCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkGraphicsPipelineCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDynamicStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineInputAssemblyStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineTessellationStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines'
data GraphicsPipelineCreateInfo = GraphicsPipelineCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "flags"
  flags :: PipelineCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pStages"
  stages :: Vector PipelineShaderStageCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pVertexInputState"
  vertexInputState :: Maybe PipelineVertexInputStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pInputAssemblyState"
  inputAssemblyState :: Maybe PipelineInputAssemblyStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pTessellationState"
  tessellationState :: Maybe PipelineTessellationStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pViewportState"
  viewportState :: Maybe PipelineViewportStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pRasterizationState"
  rasterizationState :: PipelineRasterizationStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pMultisampleState"
  multisampleState :: Maybe PipelineMultisampleStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pDepthStencilState"
  depthStencilState :: Maybe PipelineDepthStencilStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pColorBlendState"
  colorBlendState :: Maybe PipelineColorBlendStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pDynamicState"
  dynamicState :: Maybe PipelineDynamicStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "layout"
  layout :: PipelineLayout
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "renderPass"
  renderPass :: RenderPass
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "subpass"
  subpass :: Word32
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "basePipelineHandle"
  basePipelineHandle :: Pipeline
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "basePipelineIndex"
  basePipelineIndex :: Int32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkGraphicsPipelineCreateInfo' and
-- marshal a 'GraphicsPipelineCreateInfo' into it. The 'VkGraphicsPipelineCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructGraphicsPipelineCreateInfo :: GraphicsPipelineCreateInfo -> (VkGraphicsPipelineCreateInfo -> IO a) -> IO a
withCStructGraphicsPipelineCreateInfo marshalled cont = maybeWith (\a -> withCStructPipelineDynamicStateCreateInfo a . flip with) (dynamicState (marshalled :: GraphicsPipelineCreateInfo)) (\pPDynamicState -> maybeWith (\a -> withCStructPipelineColorBlendStateCreateInfo a . flip with) (colorBlendState (marshalled :: GraphicsPipelineCreateInfo)) (\pPColorBlendState -> maybeWith (\a -> withCStructPipelineDepthStencilStateCreateInfo a . flip with) (depthStencilState (marshalled :: GraphicsPipelineCreateInfo)) (\pPDepthStencilState -> maybeWith (\a -> withCStructPipelineMultisampleStateCreateInfo a . flip with) (multisampleState (marshalled :: GraphicsPipelineCreateInfo)) (\pPMultisampleState -> (\a -> withCStructPipelineRasterizationStateCreateInfo a . flip with) (rasterizationState (marshalled :: GraphicsPipelineCreateInfo)) (\pPRasterizationState -> maybeWith (\a -> withCStructPipelineViewportStateCreateInfo a . flip with) (viewportState (marshalled :: GraphicsPipelineCreateInfo)) (\pPViewportState -> maybeWith (\a -> withCStructPipelineTessellationStateCreateInfo a . flip with) (tessellationState (marshalled :: GraphicsPipelineCreateInfo)) (\pPTessellationState -> maybeWith (\a -> withCStructPipelineInputAssemblyStateCreateInfo a . flip with) (inputAssemblyState (marshalled :: GraphicsPipelineCreateInfo)) (\pPInputAssemblyState -> maybeWith (\a -> withCStructPipelineVertexInputStateCreateInfo a . flip with) (vertexInputState (marshalled :: GraphicsPipelineCreateInfo)) (\pPVertexInputState -> withVec withCStructPipelineShaderStageCreateInfo (stages (marshalled :: GraphicsPipelineCreateInfo)) (\pPStages -> maybeWith withSomeVkStruct (next (marshalled :: GraphicsPipelineCreateInfo)) (\pPNext -> cont (VkGraphicsPipelineCreateInfo VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO pPNext (flags (marshalled :: GraphicsPipelineCreateInfo)) (fromIntegral (Data.Vector.length (stages (marshalled :: GraphicsPipelineCreateInfo)))) pPStages pPVertexInputState pPInputAssemblyState pPTessellationState pPViewportState pPRasterizationState pPMultisampleState pPDepthStencilState pPColorBlendState pPDynamicState (layout (marshalled :: GraphicsPipelineCreateInfo)) (renderPass (marshalled :: GraphicsPipelineCreateInfo)) (subpass (marshalled :: GraphicsPipelineCreateInfo)) (basePipelineHandle (marshalled :: GraphicsPipelineCreateInfo)) (basePipelineIndex (marshalled :: GraphicsPipelineCreateInfo))))))))))))))

-- | A function to read a 'VkGraphicsPipelineCreateInfo' and all additional
-- structures in the pointer chain into a 'GraphicsPipelineCreateInfo'.
fromCStructGraphicsPipelineCreateInfo :: VkGraphicsPipelineCreateInfo -> IO GraphicsPipelineCreateInfo
fromCStructGraphicsPipelineCreateInfo c = GraphicsPipelineCreateInfo <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkGraphicsPipelineCreateInfo)))
                                                                     <*> pure (vkFlags (c :: VkGraphicsPipelineCreateInfo))
                                                                     -- Length valued member elided
                                                                     <*> (Data.Vector.generateM (fromIntegral (vkStageCount (c :: VkGraphicsPipelineCreateInfo))) (((fromCStructPipelineShaderStageCreateInfo <=<) . peekElemOff) (vkPStages (c :: VkGraphicsPipelineCreateInfo))))
                                                                     <*> maybePeek (fromCStructPipelineVertexInputStateCreateInfo <=< peek) (vkPVertexInputState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineInputAssemblyStateCreateInfo <=< peek) (vkPInputAssemblyState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineTessellationStateCreateInfo <=< peek) (vkPTessellationState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineViewportStateCreateInfo <=< peek) (vkPViewportState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> (fromCStructPipelineRasterizationStateCreateInfo <=< peek) (vkPRasterizationState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineMultisampleStateCreateInfo <=< peek) (vkPMultisampleState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineDepthStencilStateCreateInfo <=< peek) (vkPDepthStencilState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineColorBlendStateCreateInfo <=< peek) (vkPColorBlendState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineDynamicStateCreateInfo <=< peek) (vkPDynamicState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkLayout (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkRenderPass (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkSubpass (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkBasePipelineHandle (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkBasePipelineIndex (c :: VkGraphicsPipelineCreateInfo))

instance Zero GraphicsPipelineCreateInfo where
  zero = GraphicsPipelineCreateInfo Nothing
                                    zero
                                    Data.Vector.empty
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    zero
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero


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
-- > +-----------------------------------+-----------------------------------+
-- > | Mode                              | Operation                         |
-- > +===================================+===================================+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | 0                                 |
-- > | e.VK_LOGIC_OP_CLEAR'              |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | s ∧ d                             |
-- > | e.VK_LOGIC_OP_AND'                |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | s ∧ ¬ d                           |
-- > | e.VK_LOGIC_OP_AND_REVERSE'        |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | s                                 |
-- > | e.VK_LOGIC_OP_COPY'               |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | ¬ s ∧ d                           |
-- > | e.VK_LOGIC_OP_AND_INVERTED'       |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | d                                 |
-- > | e.VK_LOGIC_OP_NO_OP'              |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | s ⊕ d                             |
-- > | e.VK_LOGIC_OP_XOR'                |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | s ∨ d                             |
-- > | e.VK_LOGIC_OP_OR'                 |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | ¬ (s ∨ d)                         |
-- > | e.VK_LOGIC_OP_NOR'                |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | ¬ (s ⊕ d)                         |
-- > | e.VK_LOGIC_OP_EQUIVALENT'         |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | ¬ d                               |
-- > | e.VK_LOGIC_OP_INVERT'             |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | s ∨ ¬ d                           |
-- > | e.VK_LOGIC_OP_OR_REVERSE'         |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | ¬ s                               |
-- > | e.VK_LOGIC_OP_COPY_INVERTED'      |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | ¬ s ∨ d                           |
-- > | e.VK_LOGIC_OP_OR_INVERTED'        |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | ¬ (s ∧ d)                         |
-- > | e.VK_LOGIC_OP_NAND'               |                                   |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Pipelin | all 1s                            |
-- > | e.VK_LOGIC_OP_SET'                |                                   |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Logical Operations
--
-- The result of the logical operation is then written to the color
-- attachment as controlled by the component write mask, described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blendoperations Blend Operations>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo'
type LogicOp = VkLogicOp


-- No documentation found for Nested "LogicOp" "LOGIC_OP_CLEAR"
pattern LOGIC_OP_CLEAR :: (a ~ LogicOp) => a
pattern LOGIC_OP_CLEAR = VK_LOGIC_OP_CLEAR


-- No documentation found for Nested "LogicOp" "LOGIC_OP_AND"
pattern LOGIC_OP_AND :: (a ~ LogicOp) => a
pattern LOGIC_OP_AND = VK_LOGIC_OP_AND


-- No documentation found for Nested "LogicOp" "LOGIC_OP_AND_REVERSE"
pattern LOGIC_OP_AND_REVERSE :: (a ~ LogicOp) => a
pattern LOGIC_OP_AND_REVERSE = VK_LOGIC_OP_AND_REVERSE


-- No documentation found for Nested "LogicOp" "LOGIC_OP_COPY"
pattern LOGIC_OP_COPY :: (a ~ LogicOp) => a
pattern LOGIC_OP_COPY = VK_LOGIC_OP_COPY


-- No documentation found for Nested "LogicOp" "LOGIC_OP_AND_INVERTED"
pattern LOGIC_OP_AND_INVERTED :: (a ~ LogicOp) => a
pattern LOGIC_OP_AND_INVERTED = VK_LOGIC_OP_AND_INVERTED


-- No documentation found for Nested "LogicOp" "LOGIC_OP_NO_OP"
pattern LOGIC_OP_NO_OP :: (a ~ LogicOp) => a
pattern LOGIC_OP_NO_OP = VK_LOGIC_OP_NO_OP


-- No documentation found for Nested "LogicOp" "LOGIC_OP_XOR"
pattern LOGIC_OP_XOR :: (a ~ LogicOp) => a
pattern LOGIC_OP_XOR = VK_LOGIC_OP_XOR


-- No documentation found for Nested "LogicOp" "LOGIC_OP_OR"
pattern LOGIC_OP_OR :: (a ~ LogicOp) => a
pattern LOGIC_OP_OR = VK_LOGIC_OP_OR


-- No documentation found for Nested "LogicOp" "LOGIC_OP_NOR"
pattern LOGIC_OP_NOR :: (a ~ LogicOp) => a
pattern LOGIC_OP_NOR = VK_LOGIC_OP_NOR


-- No documentation found for Nested "LogicOp" "LOGIC_OP_EQUIVALENT"
pattern LOGIC_OP_EQUIVALENT :: (a ~ LogicOp) => a
pattern LOGIC_OP_EQUIVALENT = VK_LOGIC_OP_EQUIVALENT


-- No documentation found for Nested "LogicOp" "LOGIC_OP_INVERT"
pattern LOGIC_OP_INVERT :: (a ~ LogicOp) => a
pattern LOGIC_OP_INVERT = VK_LOGIC_OP_INVERT


-- No documentation found for Nested "LogicOp" "LOGIC_OP_OR_REVERSE"
pattern LOGIC_OP_OR_REVERSE :: (a ~ LogicOp) => a
pattern LOGIC_OP_OR_REVERSE = VK_LOGIC_OP_OR_REVERSE


-- No documentation found for Nested "LogicOp" "LOGIC_OP_COPY_INVERTED"
pattern LOGIC_OP_COPY_INVERTED :: (a ~ LogicOp) => a
pattern LOGIC_OP_COPY_INVERTED = VK_LOGIC_OP_COPY_INVERTED


-- No documentation found for Nested "LogicOp" "LOGIC_OP_OR_INVERTED"
pattern LOGIC_OP_OR_INVERTED :: (a ~ LogicOp) => a
pattern LOGIC_OP_OR_INVERTED = VK_LOGIC_OP_OR_INVERTED


-- No documentation found for Nested "LogicOp" "LOGIC_OP_NAND"
pattern LOGIC_OP_NAND :: (a ~ LogicOp) => a
pattern LOGIC_OP_NAND = VK_LOGIC_OP_NAND


-- No documentation found for Nested "LogicOp" "LOGIC_OP_SET"
pattern LOGIC_OP_SET :: (a ~ LogicOp) => a
pattern LOGIC_OP_SET = VK_LOGIC_OP_SET


-- | VkOffset2D - Structure specifying a two-dimensional offset
--
-- = Description
--
-- Unresolved directive in VkOffset2D.txt -
-- include::{generated}\/validity\/structs\/VkOffset2D.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D'
data Offset2D = Offset2D
  { -- No documentation found for Nested "Offset2D" "x"
  x :: Int32
  , -- No documentation found for Nested "Offset2D" "y"
  y :: Int32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkOffset2D' and
-- marshal a 'Offset2D' into it. The 'VkOffset2D' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructOffset2D :: Offset2D -> (VkOffset2D -> IO a) -> IO a
withCStructOffset2D marshalled cont = cont (VkOffset2D (x (marshalled :: Offset2D)) (y (marshalled :: Offset2D)))

-- | A function to read a 'VkOffset2D' and all additional
-- structures in the pointer chain into a 'Offset2D'.
fromCStructOffset2D :: VkOffset2D -> IO Offset2D
fromCStructOffset2D c = Offset2D <$> pure (vkX (c :: VkOffset2D))
                                 <*> pure (vkY (c :: VkOffset2D))

instance Zero Offset2D where
  zero = Offset2D zero
                  zero


-- | VkPipeline - Opaque handle to a pipeline object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindPipeline',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkDestroyPipeline'
type Pipeline = VkPipeline


-- | VkPipelineColorBlendAttachmentState - Structure specifying a pipeline
-- color blend attachment state
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @srcColorBlendFactor@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_COLOR',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_ALPHA', or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @dstColorBlendFactor@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_COLOR',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_ALPHA', or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @srcAlphaBlendFactor@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_COLOR',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_ALPHA', or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @dstAlphaBlendFactor@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_COLOR',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_ALPHA', or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If either of @colorBlendOp@ or @alphaBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @colorBlendOp@ /must/ equal @alphaBlendOp@
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' and @colorBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @colorBlendOp@ /must/ be the same for all attachments.
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' and @alphaBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @alphaBlendOp@ /must/ be the same for all attachments.
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendAllOperations@
--     is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then @colorBlendOp@
--     /must/ not be
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_ZERO_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_SRC_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_DST_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_SRC_OVER_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_DST_OVER_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_SRC_IN_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_DST_IN_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_SRC_OUT_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_DST_OUT_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_SRC_ATOP_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_DST_ATOP_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_XOR_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_INVERT_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_INVERT_RGB_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_LINEARDODGE_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_LINEARBURN_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_VIVIDLIGHT_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_LINEARLIGHT_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_PINLIGHT_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_HARDMIX_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_PLUS_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_PLUS_CLAMPED_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_PLUS_DARKER_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_MINUS_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_MINUS_CLAMPED_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_CONTRAST_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_INVERT_OVG_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_RED_EXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_GREEN_EXT',
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VK_BLEND_OP_BLUE_EXT'
--
-- -   If @colorBlendOp@ or @alphaBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription'::@colorAttachmentCount@
--     of the subpass this pipeline is compiled against /must/ be less than
--     or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT'::advancedBlendMaxColorAttachments
--
-- Unresolved directive in VkPipelineColorBlendAttachmentState.txt -
-- include::{generated}\/validity\/structs\/VkPipelineColorBlendAttachmentState.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkBlendFactor',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkBlendOp',
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkColorComponentFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo'
data PipelineColorBlendAttachmentState = PipelineColorBlendAttachmentState
  { -- No documentation found for Nested "PipelineColorBlendAttachmentState" "blendEnable"
  blendEnable :: Bool
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "srcColorBlendFactor"
  srcColorBlendFactor :: BlendFactor
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "dstColorBlendFactor"
  dstColorBlendFactor :: BlendFactor
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "colorBlendOp"
  colorBlendOp :: BlendOp
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "srcAlphaBlendFactor"
  srcAlphaBlendFactor :: BlendFactor
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "dstAlphaBlendFactor"
  dstAlphaBlendFactor :: BlendFactor
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "alphaBlendOp"
  alphaBlendOp :: BlendOp
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "colorWriteMask"
  colorWriteMask :: ColorComponentFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineColorBlendAttachmentState' and
-- marshal a 'PipelineColorBlendAttachmentState' into it. The 'VkPipelineColorBlendAttachmentState' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineColorBlendAttachmentState :: PipelineColorBlendAttachmentState -> (VkPipelineColorBlendAttachmentState -> IO a) -> IO a
withCStructPipelineColorBlendAttachmentState marshalled cont = cont (VkPipelineColorBlendAttachmentState (boolToBool32 (blendEnable (marshalled :: PipelineColorBlendAttachmentState))) (srcColorBlendFactor (marshalled :: PipelineColorBlendAttachmentState)) (dstColorBlendFactor (marshalled :: PipelineColorBlendAttachmentState)) (colorBlendOp (marshalled :: PipelineColorBlendAttachmentState)) (srcAlphaBlendFactor (marshalled :: PipelineColorBlendAttachmentState)) (dstAlphaBlendFactor (marshalled :: PipelineColorBlendAttachmentState)) (alphaBlendOp (marshalled :: PipelineColorBlendAttachmentState)) (colorWriteMask (marshalled :: PipelineColorBlendAttachmentState)))

-- | A function to read a 'VkPipelineColorBlendAttachmentState' and all additional
-- structures in the pointer chain into a 'PipelineColorBlendAttachmentState'.
fromCStructPipelineColorBlendAttachmentState :: VkPipelineColorBlendAttachmentState -> IO PipelineColorBlendAttachmentState
fromCStructPipelineColorBlendAttachmentState c = PipelineColorBlendAttachmentState <$> pure (bool32ToBool (vkBlendEnable (c :: VkPipelineColorBlendAttachmentState)))
                                                                                   <*> pure (vkSrcColorBlendFactor (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkDstColorBlendFactor (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkColorBlendOp (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkSrcAlphaBlendFactor (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkDstAlphaBlendFactor (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkAlphaBlendOp (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkColorWriteMask (c :: VkPipelineColorBlendAttachmentState))

instance Zero PipelineColorBlendAttachmentState where
  zero = PipelineColorBlendAttachmentState False
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero


-- | VkPipelineColorBlendStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo'
type PipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags


-- | VkPipelineColorBlendStateCreateInfo - Structure specifying parameters of
-- a newly created pipeline color blend state
--
-- = Description
--
-- Each element of the @pAttachments@ array is a
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState'
-- structure specifying per-target blending state for each individual color
-- attachment. If the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-independentBlend independent blending>
-- feature is not enabled on the device, all
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState'
-- elements in the @pAttachments@ array /must/ be identical.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-independentBlend independent blending>
--     feature is not enabled, all elements of @pAttachments@ /must/ be
--     identical
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-logicOp logic operations>
--     feature is not enabled, @logicOpEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If @logicOpEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     @logicOp@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkLogicOp' value
--
-- Unresolved directive in VkPipelineColorBlendStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineColorBlendStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkLogicOp',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineColorBlendStateCreateInfo = PipelineColorBlendStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "flags"
  flags :: PipelineColorBlendStateCreateFlags
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "logicOpEnable"
  logicOpEnable :: Bool
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "logicOp"
  logicOp :: LogicOp
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "pAttachments"
  attachments :: Vector PipelineColorBlendAttachmentState
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "blendConstants"
  blendConstants :: (CFloat, CFloat, CFloat, CFloat)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineColorBlendStateCreateInfo' and
-- marshal a 'PipelineColorBlendStateCreateInfo' into it. The 'VkPipelineColorBlendStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineColorBlendStateCreateInfo :: PipelineColorBlendStateCreateInfo -> (VkPipelineColorBlendStateCreateInfo -> IO a) -> IO a
withCStructPipelineColorBlendStateCreateInfo marshalled cont = withVec withCStructPipelineColorBlendAttachmentState (attachments (marshalled :: PipelineColorBlendStateCreateInfo)) (\pPAttachments -> maybeWith withSomeVkStruct (next (marshalled :: PipelineColorBlendStateCreateInfo)) (\pPNext -> cont (VkPipelineColorBlendStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO pPNext (flags (marshalled :: PipelineColorBlendStateCreateInfo)) (boolToBool32 (logicOpEnable (marshalled :: PipelineColorBlendStateCreateInfo))) (logicOp (marshalled :: PipelineColorBlendStateCreateInfo)) (fromIntegral (Data.Vector.length (attachments (marshalled :: PipelineColorBlendStateCreateInfo)))) pPAttachments (fromTuple (blendConstants (marshalled :: PipelineColorBlendStateCreateInfo))))))

-- | A function to read a 'VkPipelineColorBlendStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineColorBlendStateCreateInfo'.
fromCStructPipelineColorBlendStateCreateInfo :: VkPipelineColorBlendStateCreateInfo -> IO PipelineColorBlendStateCreateInfo
fromCStructPipelineColorBlendStateCreateInfo c = PipelineColorBlendStateCreateInfo <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineColorBlendStateCreateInfo)))
                                                                                   <*> pure (vkFlags (c :: VkPipelineColorBlendStateCreateInfo))
                                                                                   <*> pure (bool32ToBool (vkLogicOpEnable (c :: VkPipelineColorBlendStateCreateInfo)))
                                                                                   <*> pure (vkLogicOp (c :: VkPipelineColorBlendStateCreateInfo))
                                                                                   -- Length valued member elided
                                                                                   <*> (Data.Vector.generateM (fromIntegral (vkAttachmentCount (c :: VkPipelineColorBlendStateCreateInfo))) (((fromCStructPipelineColorBlendAttachmentState <=<) . peekElemOff) (vkPAttachments (c :: VkPipelineColorBlendStateCreateInfo))))
                                                                                   <*> pure (let v = (vkBlendConstants (c :: VkPipelineColorBlendStateCreateInfo)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                                                   , Data.Vector.Storable.Sized.unsafeIndex v 1
                                                                                   , Data.Vector.Storable.Sized.unsafeIndex v 2
                                                                                   , Data.Vector.Storable.Sized.unsafeIndex v 3 ))

instance Zero PipelineColorBlendStateCreateInfo where
  zero = PipelineColorBlendStateCreateInfo Nothing
                                           zero
                                           False
                                           zero
                                           Data.Vector.empty
                                           (zero, zero, zero, zero)


-- | VkPipelineCreateFlagBits - Bitmask controlling how a pipeline is created
--
-- = Description
--
-- It is valid to set both
-- 'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
-- and
-- 'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'.
-- This allows a pipeline to be both a parent and possibly a child in a
-- pipeline hierarchy. See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>
-- for more information.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlags'
type PipelineCreateFlagBits = VkPipelineCreateFlagBits


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT'
-- specifies that the created pipeline will not be optimized. Using this
-- flag /may/ reduce the time taken to create the pipeline.
pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT :: (a ~ PipelineCreateFlagBits) => a
pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
-- specifies that the pipeline to be created is allowed to be the parent of
-- a pipeline that will be created in a subsequent call to
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines' or
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines'.
pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT :: (a ~ PipelineCreateFlagBits) => a
pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
-- specifies that the pipeline to be created will be a child of a
-- previously created parent pipeline.
pattern PIPELINE_CREATE_DERIVATIVE_BIT :: (a ~ PipelineCreateFlagBits) => a
pattern PIPELINE_CREATE_DERIVATIVE_BIT = VK_PIPELINE_CREATE_DERIVATIVE_BIT

-- | VkPipelineCreateFlags - Bitmask of VkPipelineCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlags' is a bitmask
-- type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlagBits'
type PipelineCreateFlags = PipelineCreateFlagBits

-- | VkPipelineDepthStencilStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
type PipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags


-- | VkPipelineDepthStencilStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline depth stencil state
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-depthBounds depth bounds testing>
--     feature is not enabled, @depthBoundsTestEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- Unresolved directive in VkPipelineDepthStencilStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineDepthStencilStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkCompareOp',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkStencilOpState',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineDepthStencilStateCreateInfo = PipelineDepthStencilStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "flags"
  flags :: PipelineDepthStencilStateCreateFlags
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "depthTestEnable"
  depthTestEnable :: Bool
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "depthWriteEnable"
  depthWriteEnable :: Bool
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "depthCompareOp"
  depthCompareOp :: CompareOp
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "depthBoundsTestEnable"
  depthBoundsTestEnable :: Bool
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "stencilTestEnable"
  stencilTestEnable :: Bool
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "front"
  front :: StencilOpState
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "back"
  back :: StencilOpState
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "minDepthBounds"
  minDepthBounds :: CFloat
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "maxDepthBounds"
  maxDepthBounds :: CFloat
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineDepthStencilStateCreateInfo' and
-- marshal a 'PipelineDepthStencilStateCreateInfo' into it. The 'VkPipelineDepthStencilStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineDepthStencilStateCreateInfo :: PipelineDepthStencilStateCreateInfo -> (VkPipelineDepthStencilStateCreateInfo -> IO a) -> IO a
withCStructPipelineDepthStencilStateCreateInfo marshalled cont = withCStructStencilOpState (back (marshalled :: PipelineDepthStencilStateCreateInfo)) (\back'' -> withCStructStencilOpState (front (marshalled :: PipelineDepthStencilStateCreateInfo)) (\front'' -> maybeWith withSomeVkStruct (next (marshalled :: PipelineDepthStencilStateCreateInfo)) (\pPNext -> cont (VkPipelineDepthStencilStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO pPNext (flags (marshalled :: PipelineDepthStencilStateCreateInfo)) (boolToBool32 (depthTestEnable (marshalled :: PipelineDepthStencilStateCreateInfo))) (boolToBool32 (depthWriteEnable (marshalled :: PipelineDepthStencilStateCreateInfo))) (depthCompareOp (marshalled :: PipelineDepthStencilStateCreateInfo)) (boolToBool32 (depthBoundsTestEnable (marshalled :: PipelineDepthStencilStateCreateInfo))) (boolToBool32 (stencilTestEnable (marshalled :: PipelineDepthStencilStateCreateInfo))) front'' back'' (minDepthBounds (marshalled :: PipelineDepthStencilStateCreateInfo)) (maxDepthBounds (marshalled :: PipelineDepthStencilStateCreateInfo))))))

-- | A function to read a 'VkPipelineDepthStencilStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineDepthStencilStateCreateInfo'.
fromCStructPipelineDepthStencilStateCreateInfo :: VkPipelineDepthStencilStateCreateInfo -> IO PipelineDepthStencilStateCreateInfo
fromCStructPipelineDepthStencilStateCreateInfo c = PipelineDepthStencilStateCreateInfo <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (vkFlags (c :: VkPipelineDepthStencilStateCreateInfo))
                                                                                       <*> pure (bool32ToBool (vkDepthTestEnable (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (bool32ToBool (vkDepthWriteEnable (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (vkDepthCompareOp (c :: VkPipelineDepthStencilStateCreateInfo))
                                                                                       <*> pure (bool32ToBool (vkDepthBoundsTestEnable (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (bool32ToBool (vkStencilTestEnable (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> (fromCStructStencilOpState (vkFront (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> (fromCStructStencilOpState (vkBack (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (vkMinDepthBounds (c :: VkPipelineDepthStencilStateCreateInfo))
                                                                                       <*> pure (vkMaxDepthBounds (c :: VkPipelineDepthStencilStateCreateInfo))

instance Zero PipelineDepthStencilStateCreateInfo where
  zero = PipelineDepthStencilStateCreateInfo Nothing
                                             zero
                                             False
                                             False
                                             zero
                                             False
                                             False
                                             zero
                                             zero
                                             zero
                                             zero


-- | VkPipelineDynamicStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDynamicStateCreateFlags' is
-- a bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDynamicStateCreateInfo'
type PipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags


-- | VkPipelineDynamicStateCreateInfo - Structure specifying parameters of a
-- newly created pipeline dynamic state
--
-- == Valid Usage
--
-- -   Each element of @pDynamicStates@ /must/ be unique
--
-- Unresolved directive in VkPipelineDynamicStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineDynamicStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkDynamicState',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDynamicStateCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineDynamicStateCreateInfo = PipelineDynamicStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineDynamicStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineDynamicStateCreateInfo" "flags"
  flags :: PipelineDynamicStateCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineDynamicStateCreateInfo" "pDynamicStates"
  dynamicStates :: Vector DynamicState
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineDynamicStateCreateInfo' and
-- marshal a 'PipelineDynamicStateCreateInfo' into it. The 'VkPipelineDynamicStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineDynamicStateCreateInfo :: PipelineDynamicStateCreateInfo -> (VkPipelineDynamicStateCreateInfo -> IO a) -> IO a
withCStructPipelineDynamicStateCreateInfo marshalled cont = withVec (&) (dynamicStates (marshalled :: PipelineDynamicStateCreateInfo)) (\pPDynamicStates -> maybeWith withSomeVkStruct (next (marshalled :: PipelineDynamicStateCreateInfo)) (\pPNext -> cont (VkPipelineDynamicStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO pPNext (flags (marshalled :: PipelineDynamicStateCreateInfo)) (fromIntegral (Data.Vector.length (dynamicStates (marshalled :: PipelineDynamicStateCreateInfo)))) pPDynamicStates)))

-- | A function to read a 'VkPipelineDynamicStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineDynamicStateCreateInfo'.
fromCStructPipelineDynamicStateCreateInfo :: VkPipelineDynamicStateCreateInfo -> IO PipelineDynamicStateCreateInfo
fromCStructPipelineDynamicStateCreateInfo c = PipelineDynamicStateCreateInfo <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineDynamicStateCreateInfo)))
                                                                             <*> pure (vkFlags (c :: VkPipelineDynamicStateCreateInfo))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkDynamicStateCount (c :: VkPipelineDynamicStateCreateInfo))) (peekElemOff (vkPDynamicStates (c :: VkPipelineDynamicStateCreateInfo))))

instance Zero PipelineDynamicStateCreateInfo where
  zero = PipelineDynamicStateCreateInfo Nothing
                                        zero
                                        Data.Vector.empty


-- | VkPipelineInputAssemblyStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineInputAssemblyStateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineInputAssemblyStateCreateInfo'
type PipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags


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
-- -   If @topology@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_POINT_LIST',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_LINE_LIST',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY'
--     or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     @primitiveRestartEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @topology@ /must/ not be any of
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY',
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY'
--     or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @topology@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_PATCH_LIST'
--
-- Unresolved directive in VkPipelineInputAssemblyStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineInputAssemblyStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineInputAssemblyStateCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPrimitiveTopology',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineInputAssemblyStateCreateInfo = PipelineInputAssemblyStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "flags"
  flags :: PipelineInputAssemblyStateCreateFlags
  , -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "topology"
  topology :: PrimitiveTopology
  , -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "primitiveRestartEnable"
  primitiveRestartEnable :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineInputAssemblyStateCreateInfo' and
-- marshal a 'PipelineInputAssemblyStateCreateInfo' into it. The 'VkPipelineInputAssemblyStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineInputAssemblyStateCreateInfo :: PipelineInputAssemblyStateCreateInfo -> (VkPipelineInputAssemblyStateCreateInfo -> IO a) -> IO a
withCStructPipelineInputAssemblyStateCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineInputAssemblyStateCreateInfo)) (\pPNext -> cont (VkPipelineInputAssemblyStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO pPNext (flags (marshalled :: PipelineInputAssemblyStateCreateInfo)) (topology (marshalled :: PipelineInputAssemblyStateCreateInfo)) (boolToBool32 (primitiveRestartEnable (marshalled :: PipelineInputAssemblyStateCreateInfo)))))

-- | A function to read a 'VkPipelineInputAssemblyStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineInputAssemblyStateCreateInfo'.
fromCStructPipelineInputAssemblyStateCreateInfo :: VkPipelineInputAssemblyStateCreateInfo -> IO PipelineInputAssemblyStateCreateInfo
fromCStructPipelineInputAssemblyStateCreateInfo c = PipelineInputAssemblyStateCreateInfo <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineInputAssemblyStateCreateInfo)))
                                                                                         <*> pure (vkFlags (c :: VkPipelineInputAssemblyStateCreateInfo))
                                                                                         <*> pure (vkTopology (c :: VkPipelineInputAssemblyStateCreateInfo))
                                                                                         <*> pure (bool32ToBool (vkPrimitiveRestartEnable (c :: VkPipelineInputAssemblyStateCreateInfo)))

instance Zero PipelineInputAssemblyStateCreateInfo where
  zero = PipelineInputAssemblyStateCreateInfo Nothing
                                              zero
                                              zero
                                              False


-- | VkPipelineLayout - Opaque handle to a pipeline layout object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPushConstants',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.vkCreatePipelineLayout',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.vkDestroyPipelineLayout'
type PipelineLayout = VkPipelineLayout

-- | VkPipelineMultisampleStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'
type PipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags


-- | VkPipelineMultisampleStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline multisample state
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sampleRateShading sample rate shading>
--     feature is not enabled, @sampleShadingEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-alphaToOne alpha to one>
--     feature is not enabled, @alphaToOneEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   @minSampleShading@ /must/ be in the range [0,1]
--
-- -   If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     if the subpass has any color attachments and @rasterizationSamples@
--     is greater than the number of color samples, then
--     @sampleShadingEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- Unresolved directive in VkPipelineMultisampleStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineMultisampleStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkSampleMask',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineMultisampleStateCreateInfo = PipelineMultisampleStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "flags"
  flags :: PipelineMultisampleStateCreateFlags
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "rasterizationSamples"
  rasterizationSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "sampleShadingEnable"
  sampleShadingEnable :: Bool
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "minSampleShading"
  minSampleShading :: CFloat
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "pSampleMask"
  sampleMask :: Vector SampleMask
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "alphaToCoverageEnable"
  alphaToCoverageEnable :: Bool
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "alphaToOneEnable"
  alphaToOneEnable :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineMultisampleStateCreateInfo' and
-- marshal a 'PipelineMultisampleStateCreateInfo' into it. The 'VkPipelineMultisampleStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineMultisampleStateCreateInfo :: PipelineMultisampleStateCreateInfo -> (VkPipelineMultisampleStateCreateInfo -> IO a) -> IO a
withCStructPipelineMultisampleStateCreateInfo marshalled cont = withVec (flip ($)) (padVector zeroBits (fromIntegral ((coerce (rasterizationSamples (marshalled :: PipelineMultisampleStateCreateInfo)) :: VkFlags) + 31) `quot` 32) (sampleMask (marshalled :: PipelineMultisampleStateCreateInfo))) (\pPSampleMask -> maybeWith withSomeVkStruct (next (marshalled :: PipelineMultisampleStateCreateInfo)) (\pPNext -> cont (VkPipelineMultisampleStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO pPNext (flags (marshalled :: PipelineMultisampleStateCreateInfo)) (rasterizationSamples (marshalled :: PipelineMultisampleStateCreateInfo)) (boolToBool32 (sampleShadingEnable (marshalled :: PipelineMultisampleStateCreateInfo))) (minSampleShading (marshalled :: PipelineMultisampleStateCreateInfo)) pPSampleMask (boolToBool32 (alphaToCoverageEnable (marshalled :: PipelineMultisampleStateCreateInfo))) (boolToBool32 (alphaToOneEnable (marshalled :: PipelineMultisampleStateCreateInfo))))))

-- | A function to read a 'VkPipelineMultisampleStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineMultisampleStateCreateInfo'.
fromCStructPipelineMultisampleStateCreateInfo :: VkPipelineMultisampleStateCreateInfo -> IO PipelineMultisampleStateCreateInfo
fromCStructPipelineMultisampleStateCreateInfo c = PipelineMultisampleStateCreateInfo <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineMultisampleStateCreateInfo)))
                                                                                     <*> pure (vkFlags (c :: VkPipelineMultisampleStateCreateInfo))
                                                                                     <*> pure (vkRasterizationSamples (c :: VkPipelineMultisampleStateCreateInfo))
                                                                                     <*> pure (bool32ToBool (vkSampleShadingEnable (c :: VkPipelineMultisampleStateCreateInfo)))
                                                                                     <*> pure (vkMinSampleShading (c :: VkPipelineMultisampleStateCreateInfo))
                                                                                     <*> (Data.Vector.generateM (fromIntegral (((coerce (vkRasterizationSamples (c :: VkPipelineMultisampleStateCreateInfo)) :: VkFlags) + 31) `quot` 32)) (peekElemOff (vkPSampleMask (c :: VkPipelineMultisampleStateCreateInfo))))
                                                                                     <*> pure (bool32ToBool (vkAlphaToCoverageEnable (c :: VkPipelineMultisampleStateCreateInfo)))
                                                                                     <*> pure (bool32ToBool (vkAlphaToOneEnable (c :: VkPipelineMultisampleStateCreateInfo)))

instance Zero PipelineMultisampleStateCreateInfo where
  zero = PipelineMultisampleStateCreateInfo Nothing
                                            zero
                                            zero
                                            False
                                            zero
                                            Data.Vector.empty
                                            False
                                            False


-- | VkPipelineRasterizationStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
type PipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags


-- | VkPipelineRasterizationStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline rasterization state
--
-- = Description
--
-- The application /can/ also add a
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order.VkPipelineRasterizationStateRasterizationOrderAMD'
-- structure to the @pNext@ chain of a
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
-- structure. This structure enables selecting the rasterization order to
-- use when rendering with the corresponding graphics pipeline as described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primrast-order Rasterization Order>.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-depthClamp depth clamping>
--     feature is not enabled, @depthClampEnable@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-fillModeNonSolid non-solid fill modes>
--     feature is not enabled, @polygonMode@ /must/ be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_POLYGON_MODE_FILL' or
--     'Graphics.Vulkan.C.Extensions.VK_NV_fill_rectangle.VK_POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- -   If the
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_NV_fill_rectangle@
--     extension is not enabled, @polygonMode@ /must/ not be
--     'Graphics.Vulkan.C.Extensions.VK_NV_fill_rectangle.VK_POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- Unresolved directive in VkPipelineRasterizationStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineRasterizationStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkCullModeFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkFrontFace',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPolygonMode',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineRasterizationStateCreateInfo = PipelineRasterizationStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "flags"
  flags :: PipelineRasterizationStateCreateFlags
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthClampEnable"
  depthClampEnable :: Bool
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "rasterizerDiscardEnable"
  rasterizerDiscardEnable :: Bool
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "polygonMode"
  polygonMode :: PolygonMode
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "cullMode"
  cullMode :: CullModeFlags
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "frontFace"
  frontFace :: FrontFace
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasEnable"
  depthBiasEnable :: Bool
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasConstantFactor"
  depthBiasConstantFactor :: CFloat
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasClamp"
  depthBiasClamp :: CFloat
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasSlopeFactor"
  depthBiasSlopeFactor :: CFloat
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "lineWidth"
  lineWidth :: CFloat
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineRasterizationStateCreateInfo' and
-- marshal a 'PipelineRasterizationStateCreateInfo' into it. The 'VkPipelineRasterizationStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineRasterizationStateCreateInfo :: PipelineRasterizationStateCreateInfo -> (VkPipelineRasterizationStateCreateInfo -> IO a) -> IO a
withCStructPipelineRasterizationStateCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineRasterizationStateCreateInfo)) (\pPNext -> cont (VkPipelineRasterizationStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO pPNext (flags (marshalled :: PipelineRasterizationStateCreateInfo)) (boolToBool32 (depthClampEnable (marshalled :: PipelineRasterizationStateCreateInfo))) (boolToBool32 (rasterizerDiscardEnable (marshalled :: PipelineRasterizationStateCreateInfo))) (polygonMode (marshalled :: PipelineRasterizationStateCreateInfo)) (cullMode (marshalled :: PipelineRasterizationStateCreateInfo)) (frontFace (marshalled :: PipelineRasterizationStateCreateInfo)) (boolToBool32 (depthBiasEnable (marshalled :: PipelineRasterizationStateCreateInfo))) (depthBiasConstantFactor (marshalled :: PipelineRasterizationStateCreateInfo)) (depthBiasClamp (marshalled :: PipelineRasterizationStateCreateInfo)) (depthBiasSlopeFactor (marshalled :: PipelineRasterizationStateCreateInfo)) (lineWidth (marshalled :: PipelineRasterizationStateCreateInfo))))

-- | A function to read a 'VkPipelineRasterizationStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineRasterizationStateCreateInfo'.
fromCStructPipelineRasterizationStateCreateInfo :: VkPipelineRasterizationStateCreateInfo -> IO PipelineRasterizationStateCreateInfo
fromCStructPipelineRasterizationStateCreateInfo c = PipelineRasterizationStateCreateInfo <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRasterizationStateCreateInfo)))
                                                                                         <*> pure (vkFlags (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (bool32ToBool (vkDepthClampEnable (c :: VkPipelineRasterizationStateCreateInfo)))
                                                                                         <*> pure (bool32ToBool (vkRasterizerDiscardEnable (c :: VkPipelineRasterizationStateCreateInfo)))
                                                                                         <*> pure (vkPolygonMode (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkCullMode (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkFrontFace (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (bool32ToBool (vkDepthBiasEnable (c :: VkPipelineRasterizationStateCreateInfo)))
                                                                                         <*> pure (vkDepthBiasConstantFactor (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkDepthBiasClamp (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkDepthBiasSlopeFactor (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkLineWidth (c :: VkPipelineRasterizationStateCreateInfo))

instance Zero PipelineRasterizationStateCreateInfo where
  zero = PipelineRasterizationStateCreateInfo Nothing
                                              zero
                                              False
                                              False
                                              zero
                                              zero
                                              zero
                                              False
                                              zero
                                              zero
                                              zero
                                              zero


-- | VkPipelineShaderStageCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateFlags' is
-- a bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo'
type PipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags


-- | VkPipelineShaderStageCreateInfo - Structure specifying parameters of a
-- newly created pipeline shader stage
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stage@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_GEOMETRY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stage@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-meshShader mesh shader>
--     feature is not enabled, @stage@ /must/ not be
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_SHADER_STAGE_MESH_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-taskShader task shader>
--     feature is not enabled, @stage@ /must/ not be
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_SHADER_STAGE_TASK_BIT_NV'
--
-- -   @stage@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_ALL_GRAPHICS', or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_ALL'
--
-- -   @pName@ /must/ be the name of an @OpEntryPoint@ in @module@ with an
--     execution model that matches @stage@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the @ClipDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxClipDistances@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the @CullDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxCullDistances@
--
-- -   If the identified entry point includes any variables in its
--     interface that are declared with the @ClipDistance@ or
--     @CullDistance@ @BuiltIn@ decoration, those variables /must/ not have
--     array sizes which sum to more than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxCombinedClipAndCullDistances@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the 'SampleMask' @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxSampleMaskWords@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_VERTEX_BIT', the
--     identified entry point /must/ not include any input variable in its
--     interface that is decorated with @CullDistance@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     and the identified entry point has an @OpExecutionMode@ instruction
--     that specifies a patch size with @OutputVertices@, the patch size
--     /must/ be greater than @0@ and less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxTessellationPatchSize@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies a maximum output vertex count that is
--     greater than @0@ and less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxGeometryOutputVertices@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies an invocation count that is greater than
--     @0@ and less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxGeometryShaderInvocations@
--
-- -   If @stage@ is a vertex processing stage, and the identified entry
--     point writes to @Layer@ for any primitive, it /must/ write the same
--     value to @Layer@ for all vertices of a given primitive
--
-- -   If @stage@ is a vertex processing stage, and the identified entry
--     point writes to @ViewportIndex@ for any primitive, it /must/ write
--     the same value to @ViewportIndex@ for all vertices of a given
--     primitive
--
-- -   If @stage@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_FRAGMENT_BIT',
--     the identified entry point /must/ not include any output variables
--     in its interface decorated with @CullDistance@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_FRAGMENT_BIT',
--     and the identified entry point writes to @FragDepth@ in any
--     execution path, it /must/ write to @FragDepth@ in all execution
--     paths
--
-- -   If @stage@ is
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_FRAGMENT_BIT',
--     and the identified entry point writes to @FragStencilRefEXT@ in any
--     execution path, it /must/ write to @FragStencilRefEXT@ in all
--     execution paths
--
-- -   If @stage@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_SHADER_STAGE_MESH_BIT_NV',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies a maximum output vertex count,
--     @OutputVertices@, that is greater than @0@ and less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkPhysicalDeviceMeshShaderPropertiesNV'::@maxMeshOutputVertices@.
--
-- -   If @stage@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_SHADER_STAGE_MESH_BIT_NV',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies a maximum output primitive count,
--     @OutputPrimitivesNV@, that is greater than @0@ and less than or
--     equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VkPhysicalDeviceMeshShaderPropertiesNV'::@maxMeshOutputPrimitives@.
--
-- Unresolved directive in VkPipelineShaderStageCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineShaderStageCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModule',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkSpecializationInfo',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineShaderStageCreateInfo = PipelineShaderStageCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineShaderStageCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "flags"
  flags :: PipelineShaderStageCreateFlags
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "stage"
  stage :: ShaderStageFlagBits
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "module"
  module' :: ShaderModule
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "pName"
  name :: ByteString
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "pSpecializationInfo"
  specializationInfo :: Maybe SpecializationInfo
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineShaderStageCreateInfo' and
-- marshal a 'PipelineShaderStageCreateInfo' into it. The 'VkPipelineShaderStageCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineShaderStageCreateInfo :: PipelineShaderStageCreateInfo -> (VkPipelineShaderStageCreateInfo -> IO a) -> IO a
withCStructPipelineShaderStageCreateInfo marshalled cont = maybeWith (\a -> withCStructSpecializationInfo a . flip with) (specializationInfo (marshalled :: PipelineShaderStageCreateInfo)) (\pPSpecializationInfo -> useAsCString (name (marshalled :: PipelineShaderStageCreateInfo)) (\pPName -> maybeWith withSomeVkStruct (next (marshalled :: PipelineShaderStageCreateInfo)) (\pPNext -> cont (VkPipelineShaderStageCreateInfo VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO pPNext (flags (marshalled :: PipelineShaderStageCreateInfo)) (stage (marshalled :: PipelineShaderStageCreateInfo)) (module' (marshalled :: PipelineShaderStageCreateInfo)) pPName pPSpecializationInfo))))

-- | A function to read a 'VkPipelineShaderStageCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineShaderStageCreateInfo'.
fromCStructPipelineShaderStageCreateInfo :: VkPipelineShaderStageCreateInfo -> IO PipelineShaderStageCreateInfo
fromCStructPipelineShaderStageCreateInfo c = PipelineShaderStageCreateInfo <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineShaderStageCreateInfo)))
                                                                           <*> pure (vkFlags (c :: VkPipelineShaderStageCreateInfo))
                                                                           <*> pure (vkStage (c :: VkPipelineShaderStageCreateInfo))
                                                                           <*> pure (vkModule (c :: VkPipelineShaderStageCreateInfo))
                                                                           <*> packCString (vkPName (c :: VkPipelineShaderStageCreateInfo))
                                                                           <*> maybePeek (fromCStructSpecializationInfo <=< peek) (vkPSpecializationInfo (c :: VkPipelineShaderStageCreateInfo))

instance Zero PipelineShaderStageCreateInfo where
  zero = PipelineShaderStageCreateInfo Nothing
                                       zero
                                       zero
                                       zero
                                       Data.ByteString.empty
                                       Nothing


-- | VkPipelineTessellationStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineTessellationStateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineTessellationStateCreateInfo'
type PipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags


-- | VkPipelineTessellationStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline tessellation state
--
-- == Valid Usage
--
-- Unresolved directive in VkPipelineTessellationStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineTessellationStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineTessellationStateCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PipelineTessellationStateCreateInfo = PipelineTessellationStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineTessellationStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineTessellationStateCreateInfo" "flags"
  flags :: PipelineTessellationStateCreateFlags
  , -- No documentation found for Nested "PipelineTessellationStateCreateInfo" "patchControlPoints"
  patchControlPoints :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineTessellationStateCreateInfo' and
-- marshal a 'PipelineTessellationStateCreateInfo' into it. The 'VkPipelineTessellationStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineTessellationStateCreateInfo :: PipelineTessellationStateCreateInfo -> (VkPipelineTessellationStateCreateInfo -> IO a) -> IO a
withCStructPipelineTessellationStateCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineTessellationStateCreateInfo)) (\pPNext -> cont (VkPipelineTessellationStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO pPNext (flags (marshalled :: PipelineTessellationStateCreateInfo)) (patchControlPoints (marshalled :: PipelineTessellationStateCreateInfo))))

-- | A function to read a 'VkPipelineTessellationStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineTessellationStateCreateInfo'.
fromCStructPipelineTessellationStateCreateInfo :: VkPipelineTessellationStateCreateInfo -> IO PipelineTessellationStateCreateInfo
fromCStructPipelineTessellationStateCreateInfo c = PipelineTessellationStateCreateInfo <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineTessellationStateCreateInfo)))
                                                                                       <*> pure (vkFlags (c :: VkPipelineTessellationStateCreateInfo))
                                                                                       <*> pure (vkPatchControlPoints (c :: VkPipelineTessellationStateCreateInfo))

instance Zero PipelineTessellationStateCreateInfo where
  zero = PipelineTessellationStateCreateInfo Nothing
                                             zero
                                             zero


-- | VkPipelineVertexInputStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateInfo'
type PipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags


-- | VkPipelineVertexInputStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline vertex input state
--
-- == Valid Usage
--
-- -   @vertexBindingDescriptionCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   @vertexAttributeDescriptionCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxVertexInputAttributes@
--
-- -   For every @binding@ specified by each element of
--     @pVertexAttributeDescriptions@, a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputBindingDescription'
--     /must/ exist in @pVertexBindingDescriptions@ with the same value of
--     @binding@
--
-- -   All elements of @pVertexBindingDescriptions@ /must/ describe
--     distinct binding numbers
--
-- -   All elements of @pVertexAttributeDescriptions@ /must/ describe
--     distinct attribute locations
--
-- Unresolved directive in VkPipelineVertexInputStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineVertexInputStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputAttributeDescription',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputBindingDescription'
data PipelineVertexInputStateCreateInfo = PipelineVertexInputStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "flags"
  flags :: PipelineVertexInputStateCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "pVertexBindingDescriptions"
  vertexBindingDescriptions :: Vector VertexInputBindingDescription
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "pVertexAttributeDescriptions"
  vertexAttributeDescriptions :: Vector VertexInputAttributeDescription
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineVertexInputStateCreateInfo' and
-- marshal a 'PipelineVertexInputStateCreateInfo' into it. The 'VkPipelineVertexInputStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineVertexInputStateCreateInfo :: PipelineVertexInputStateCreateInfo -> (VkPipelineVertexInputStateCreateInfo -> IO a) -> IO a
withCStructPipelineVertexInputStateCreateInfo marshalled cont = withVec withCStructVertexInputAttributeDescription (vertexAttributeDescriptions (marshalled :: PipelineVertexInputStateCreateInfo)) (\pPVertexAttributeDescriptions -> withVec withCStructVertexInputBindingDescription (vertexBindingDescriptions (marshalled :: PipelineVertexInputStateCreateInfo)) (\pPVertexBindingDescriptions -> maybeWith withSomeVkStruct (next (marshalled :: PipelineVertexInputStateCreateInfo)) (\pPNext -> cont (VkPipelineVertexInputStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO pPNext (flags (marshalled :: PipelineVertexInputStateCreateInfo)) (fromIntegral (Data.Vector.length (vertexBindingDescriptions (marshalled :: PipelineVertexInputStateCreateInfo)))) pPVertexBindingDescriptions (fromIntegral (Data.Vector.length (vertexAttributeDescriptions (marshalled :: PipelineVertexInputStateCreateInfo)))) pPVertexAttributeDescriptions))))

-- | A function to read a 'VkPipelineVertexInputStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineVertexInputStateCreateInfo'.
fromCStructPipelineVertexInputStateCreateInfo :: VkPipelineVertexInputStateCreateInfo -> IO PipelineVertexInputStateCreateInfo
fromCStructPipelineVertexInputStateCreateInfo c = PipelineVertexInputStateCreateInfo <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineVertexInputStateCreateInfo)))
                                                                                     <*> pure (vkFlags (c :: VkPipelineVertexInputStateCreateInfo))
                                                                                     -- Length valued member elided
                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkVertexBindingDescriptionCount (c :: VkPipelineVertexInputStateCreateInfo))) (((fromCStructVertexInputBindingDescription <=<) . peekElemOff) (vkPVertexBindingDescriptions (c :: VkPipelineVertexInputStateCreateInfo))))
                                                                                     -- Length valued member elided
                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkVertexAttributeDescriptionCount (c :: VkPipelineVertexInputStateCreateInfo))) (((fromCStructVertexInputAttributeDescription <=<) . peekElemOff) (vkPVertexAttributeDescriptions (c :: VkPipelineVertexInputStateCreateInfo))))

instance Zero PipelineVertexInputStateCreateInfo where
  zero = PipelineVertexInputStateCreateInfo Nothing
                                            zero
                                            Data.Vector.empty
                                            Data.Vector.empty


-- | VkPipelineViewportStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'
type PipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags


-- | VkPipelineViewportStateCreateInfo - Structure specifying parameters of a
-- newly created pipeline viewport state
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @scissorCount@ /must/ be @1@
--
-- -   @viewportCount@ /must/ be between @1@ and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   @scissorCount@ /must/ be between @1@ and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   @scissorCount@ and @viewportCount@ /must/ be identical
--
-- -   If the @viewportWScalingEnable@ member of a
--     'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'
--     structure chained to the @pNext@ chain is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the @viewportCount@ member
--     of the
--     'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV'
--     structure /must/ be equal to @viewportCount@
--
-- Unresolved directive in VkPipelineViewportStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineViewportStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkViewport'
data PipelineViewportStateCreateInfo = PipelineViewportStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineViewportStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportStateCreateInfo" "flags"
  flags :: PipelineViewportStateCreateFlags
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportStateCreateInfo" "pViewports"
  viewports :: Maybe (Vector Viewport)
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportStateCreateInfo" "pScissors"
  scissors :: Maybe (Vector Rect2D)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineViewportStateCreateInfo' and
-- marshal a 'PipelineViewportStateCreateInfo' into it. The 'VkPipelineViewportStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineViewportStateCreateInfo :: PipelineViewportStateCreateInfo -> (VkPipelineViewportStateCreateInfo -> IO a) -> IO a
withCStructPipelineViewportStateCreateInfo marshalled cont = maybeWith (withVec withCStructRect2D) (scissors (marshalled :: PipelineViewportStateCreateInfo)) (\pPScissors -> maybeWith (withVec withCStructViewport) (viewports (marshalled :: PipelineViewportStateCreateInfo)) (\pPViewports -> maybeWith withSomeVkStruct (next (marshalled :: PipelineViewportStateCreateInfo)) (\pPNext -> cont (VkPipelineViewportStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO pPNext (flags (marshalled :: PipelineViewportStateCreateInfo)) (maybe 0 (fromIntegral . Data.Vector.length) (viewports (marshalled :: PipelineViewportStateCreateInfo))) pPViewports (maybe 0 (fromIntegral . Data.Vector.length) (scissors (marshalled :: PipelineViewportStateCreateInfo))) pPScissors))))

-- | A function to read a 'VkPipelineViewportStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineViewportStateCreateInfo'.
fromCStructPipelineViewportStateCreateInfo :: VkPipelineViewportStateCreateInfo -> IO PipelineViewportStateCreateInfo
fromCStructPipelineViewportStateCreateInfo c = PipelineViewportStateCreateInfo <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportStateCreateInfo)))
                                                                               <*> pure (vkFlags (c :: VkPipelineViewportStateCreateInfo))
                                                                               -- Optional length valued member elided
                                                                               <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkViewportCount (c :: VkPipelineViewportStateCreateInfo))) (((fromCStructViewport <=<) . peekElemOff) p)) (vkPViewports (c :: VkPipelineViewportStateCreateInfo))
                                                                               -- Optional length valued member elided
                                                                               <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkScissorCount (c :: VkPipelineViewportStateCreateInfo))) (((fromCStructRect2D <=<) . peekElemOff) p)) (vkPScissors (c :: VkPipelineViewportStateCreateInfo))

instance Zero PipelineViewportStateCreateInfo where
  zero = PipelineViewportStateCreateInfo Nothing
                                         zero
                                         Nothing
                                         Nothing


-- | VkPolygonMode - Control polygon rasterization mode
--
-- = Description
--
-- These modes affect only the final rasterization of polygons: in
-- particular, a polygon’s vertices are shaded and the polygon is clipped
-- and possibly culled before these modes are applied.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
type PolygonMode = VkPolygonMode


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_POLYGON_MODE_FILL' specifies that
-- polygons are rendered using the polygon rasterization rules in this
-- section.
pattern POLYGON_MODE_FILL :: (a ~ PolygonMode) => a
pattern POLYGON_MODE_FILL = VK_POLYGON_MODE_FILL


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_POLYGON_MODE_LINE' specifies that
-- polygon edges are drawn as line segments.
pattern POLYGON_MODE_LINE :: (a ~ PolygonMode) => a
pattern POLYGON_MODE_LINE = VK_POLYGON_MODE_LINE


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_POLYGON_MODE_POINT' specifies that
-- polygon vertices are drawn as points.
pattern POLYGON_MODE_POINT :: (a ~ PolygonMode) => a
pattern POLYGON_MODE_POINT = VK_POLYGON_MODE_POINT

-- | VkPrimitiveTopology - Supported primitive topologies
--
-- = Description
--
-- Each primitive topology, and its construction from a list of vertices,
-- is described in detail below with a supporting diagram, according to the
-- following key:
--
-- > +----+----------+-----------------------------------------------------+
-- > | << | Vertex   | A point in 3-dimensional space. Positions chosen    |
-- > | da |          | within the diagrams are arbitrary and for           |
-- > | ta |          | illustration only.                                  |
-- > | :i |          |                                                     |
-- > | ma |          |                                                     |
-- > | ge |          |                                                     |
-- > | /s |          |                                                     |
-- > | vg |          |                                                     |
-- > | +x |          |                                                     |
-- > | ml |          |                                                     |
-- > | ;b |          |                                                     |
-- > | as |          |                                                     |
-- > | e6 |          |                                                     |
-- > | 4, |          |                                                     |
-- > |  p |          |                                                     |
-- > | ri |          |                                                     |
-- > | mi |          |                                                     |
-- > | ti |          |                                                     |
-- > | ve |          |                                                     |
-- > |  t |          |                                                     |
-- > | op |          |                                                     |
-- > | ol |          |                                                     |
-- > | og |          |                                                     |
-- > | y  |          |                                                     |
-- > | ke |          |                                                     |
-- > | y  |          |                                                     |
-- > | ve |          |                                                     |
-- > | rt |          |                                                     |
-- > | ex |          |                                                     |
-- > | >> |          |                                                     |
-- > +----+----------+-----------------------------------------------------+
-- > | << | Vertex   | Sequence position of a vertex within the provided   |
-- > | da | Number   | vertex data.                                        |
-- > | ta |          |                                                     |
-- > | :i |          |                                                     |
-- > | ma |          |                                                     |
-- > | ge |          |                                                     |
-- > | /s |          |                                                     |
-- > | vg |          |                                                     |
-- > | +x |          |                                                     |
-- > | ml |          |                                                     |
-- > | ;b |          |                                                     |
-- > | as |          |                                                     |
-- > | e6 |          |                                                     |
-- > | 4, |          |                                                     |
-- > |  p |          |                                                     |
-- > | ri |          |                                                     |
-- > | mi |          |                                                     |
-- > | ti |          |                                                     |
-- > | ve |          |                                                     |
-- > |  t |          |                                                     |
-- > | op |          |                                                     |
-- > | ol |          |                                                     |
-- > | og |          |                                                     |
-- > | y  |          |                                                     |
-- > | ke |          |                                                     |
-- > | y  |          |                                                     |
-- > | ve |          |                                                     |
-- > | rt |          |                                                     |
-- > | ex |          |                                                     |
-- > |  n |          |                                                     |
-- > | um |          |                                                     |
-- > | be |          |                                                     |
-- > | r> |          |                                                     |
-- > | >  |          |                                                     |
-- > +----+----------+-----------------------------------------------------+
-- > | << | Provokin | Provoking vertex within the main primitive. The     |
-- > | da | g        | arrow points along an edge of the relevant          |
-- > | ta | Vertex   | primitive, following winding order. Used in         |
-- > | :i |          | <https://www.khronos.org/registry/vulkan/specs/1.1- |
-- > | ma |          | extensions/html/vkspec.html#vertexpostproc-flatshad |
-- > | ge |          | ing flat shading>.                                  |
-- > | /s |          |                                                     |
-- > | vg |          |                                                     |
-- > | +x |          |                                                     |
-- > | ml |          |                                                     |
-- > | ;b |          |                                                     |
-- > | as |          |                                                     |
-- > | e6 |          |                                                     |
-- > | 4, |          |                                                     |
-- > |  p |          |                                                     |
-- > | ri |          |                                                     |
-- > | mi |          |                                                     |
-- > | ti |          |                                                     |
-- > | ve |          |                                                     |
-- > |  t |          |                                                     |
-- > | op |          |                                                     |
-- > | ol |          |                                                     |
-- > | og |          |                                                     |
-- > | y  |          |                                                     |
-- > | ke |          |                                                     |
-- > | y  |          |                                                     |
-- > | pr |          |                                                     |
-- > | ov |          |                                                     |
-- > | ok |          |                                                     |
-- > | in |          |                                                     |
-- > | g  |          |                                                     |
-- > | ve |          |                                                     |
-- > | rt |          |                                                     |
-- > | ex |          |                                                     |
-- > | >> |          |                                                     |
-- > +----+----------+-----------------------------------------------------+
-- > | << | Primitiv | An edge connecting the points of a main primitive.  |
-- > | da | e        |                                                     |
-- > | ta | Edge     |                                                     |
-- > | :i |          |                                                     |
-- > | ma |          |                                                     |
-- > | ge |          |                                                     |
-- > | /s |          |                                                     |
-- > | vg |          |                                                     |
-- > | +x |          |                                                     |
-- > | ml |          |                                                     |
-- > | ;b |          |                                                     |
-- > | as |          |                                                     |
-- > | e6 |          |                                                     |
-- > | 4, |          |                                                     |
-- > |  p |          |                                                     |
-- > | ri |          |                                                     |
-- > | mi |          |                                                     |
-- > | ti |          |                                                     |
-- > | ve |          |                                                     |
-- > |  t |          |                                                     |
-- > | op |          |                                                     |
-- > | ol |          |                                                     |
-- > | og |          |                                                     |
-- > | y  |          |                                                     |
-- > | ke |          |                                                     |
-- > | y  |          |                                                     |
-- > | ed |          |                                                     |
-- > | ge |          |                                                     |
-- > | >> |          |                                                     |
-- > +----+----------+-----------------------------------------------------+
-- > | << | Adjacenc | Points connected by these lines do not contribute   |
-- > | da | y        | to a main primitive, and are only accessible in a   |
-- > | ta | Edge     | <https://www.khronos.org/registry/vulkan/specs/1.1- |
-- > | :i |          | extensions/html/vkspec.html#geometry geometry shade |
-- > | ma |          | r>.                                                 |
-- > | ge |          |                                                     |
-- > | /s |          |                                                     |
-- > | vg |          |                                                     |
-- > | +x |          |                                                     |
-- > | ml |          |                                                     |
-- > | ;b |          |                                                     |
-- > | as |          |                                                     |
-- > | e6 |          |                                                     |
-- > | 4, |          |                                                     |
-- > |  p |          |                                                     |
-- > | ri |          |                                                     |
-- > | mi |          |                                                     |
-- > | ti |          |                                                     |
-- > | ve |          |                                                     |
-- > |  t |          |                                                     |
-- > | op |          |                                                     |
-- > | ol |          |                                                     |
-- > | og |          |                                                     |
-- > | y  |          |                                                     |
-- > | ke |          |                                                     |
-- > | y  |          |                                                     |
-- > | ad |          |                                                     |
-- > | ja |          |                                                     |
-- > | ce |          |                                                     |
-- > | nc |          |                                                     |
-- > | y  |          |                                                     |
-- > | ed |          |                                                     |
-- > | ge |          |                                                     |
-- > | >> |          |                                                     |
-- > +----+----------+-----------------------------------------------------+
-- > | << | Winding  | The relative order in which vertices are defined    |
-- > | da | Order    | within a primitive, used in the                     |
-- > | ta |          | <https://www.khronos.org/registry/vulkan/specs/1.1- |
-- > | :i |          | extensions/html/vkspec.html#primsrast-polygons-basi |
-- > | ma |          | c facing determination>.                            |
-- > | ge |          | This ordering has no specific start or end point.   |
-- > | /s |          |                                                     |
-- > | vg |          |                                                     |
-- > | +x |          |                                                     |
-- > | ml |          |                                                     |
-- > | ;b |          |                                                     |
-- > | as |          |                                                     |
-- > | e6 |          |                                                     |
-- > | 4, |          |                                                     |
-- > |  p |          |                                                     |
-- > | ri |          |                                                     |
-- > | mi |          |                                                     |
-- > | ti |          |                                                     |
-- > | ve |          |                                                     |
-- > |  t |          |                                                     |
-- > | op |          |                                                     |
-- > | ol |          |                                                     |
-- > | og |          |                                                     |
-- > | y  |          |                                                     |
-- > | ke |          |                                                     |
-- > | y  |          |                                                     |
-- > | wi |          |                                                     |
-- > | nd |          |                                                     |
-- > | in |          |                                                     |
-- > | g  |          |                                                     |
-- > | or |          |                                                     |
-- > | de |          |                                                     |
-- > | r> |          |                                                     |
-- > | >  |          |                                                     |
-- > +----+----------+-----------------------------------------------------+
--
-- The diagrams are supported with mathematical definitions where the
-- vertices (v) and primitives (p) are numbered starting from 0; v0 is the
-- first vertex in the provided data and p0 is the first primitive in the
-- set of primitives defined by the vertices and topology.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineInputAssemblyStateCreateInfo'
type PrimitiveTopology = VkPrimitiveTopology


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_POINT_LIST'
-- specifies a series of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-point-lists separate point primitives>.
pattern PRIMITIVE_TOPOLOGY_POINT_LIST :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_POINT_LIST = VK_PRIMITIVE_TOPOLOGY_POINT_LIST


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_LINE_LIST'
-- specifies a series of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-line-lists separate line primitives>.
pattern PRIMITIVE_TOPOLOGY_LINE_LIST :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_LINE_LIST = VK_PRIMITIVE_TOPOLOGY_LINE_LIST


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_LINE_STRIP'
-- specifies a series of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-line-strips connected line primitives>
-- with consecutive lines sharing a vertex.
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP = VK_PRIMITIVE_TOPOLOGY_LINE_STRIP


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST'
-- specifies a series of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-triangle-lists separate triangle primitives>.
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP'
-- specifies a series of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-triangle-strips connected triangle primitives>
-- with consecutive triangles sharing an edge.
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN'
-- specifies a series of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-triangle-fans connected triangle primitives>
-- with all triangles sharing a common vertex.
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_FAN :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY'
-- specifies a series
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-line-lists-with-adjacency separate line primitives with adjacency>.
pattern PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY = VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY'
-- specifies a series
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-line-strips-with-adjacency connected line primitives with adjacency>,
-- with consecutive primitives sharing three vertices.
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY = VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY'
-- specifies a series
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-line-lists-with-adjacency separate triangle primitives with adjacency>.
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY'
-- specifies
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-line-lists-with-adjacency connected triangle primitives with adjacency>,
-- with consecutive triangles sharing an edge.
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_PRIMITIVE_TOPOLOGY_PATCH_LIST'
-- specifies
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing-patch-lists separate patch primitives>.
pattern PRIMITIVE_TOPOLOGY_PATCH_LIST :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_PATCH_LIST = VK_PRIMITIVE_TOPOLOGY_PATCH_LIST


-- | VkRect2D - Structure specifying a two-dimensional subregion
--
-- = Description
--
-- Unresolved directive in VkRect2D.txt -
-- include::{generated}\/validity\/structs\/VkRect2D.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearRect',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupRenderPassBeginInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkOffset2D',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetScissor'
data Rect2D = Rect2D
  { -- No documentation found for Nested "Rect2D" "offset"
  offset :: Offset2D
  , -- No documentation found for Nested "Rect2D" "extent"
  extent :: Extent2D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRect2D' and
-- marshal a 'Rect2D' into it. The 'VkRect2D' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRect2D :: Rect2D -> (VkRect2D -> IO a) -> IO a
withCStructRect2D marshalled cont = withCStructExtent2D (extent (marshalled :: Rect2D)) (\extent'' -> withCStructOffset2D (offset (marshalled :: Rect2D)) (\offset'' -> cont (VkRect2D offset'' extent'')))

-- | A function to read a 'VkRect2D' and all additional
-- structures in the pointer chain into a 'Rect2D'.
fromCStructRect2D :: VkRect2D -> IO Rect2D
fromCStructRect2D c = Rect2D <$> (fromCStructOffset2D (vkOffset (c :: VkRect2D)))
                             <*> (fromCStructExtent2D (vkExtent (c :: VkRect2D)))

instance Zero Rect2D where
  zero = Rect2D zero
                zero


-- | VkRenderPass - Opaque handle to a render pass object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateRenderPass',
-- 'Graphics.Vulkan.C.Core10.Pass.vkDestroyRenderPass',
-- 'Graphics.Vulkan.C.Core10.Pass.vkGetRenderAreaGranularity'
type RenderPass = VkRenderPass

-- | VkSampleMask - Mask of sample coverage information
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'
type SampleMask = VkSampleMask
  

-- | VkShaderStageFlagBits - Bitmask specifying a pipeline stage
--
-- = Description
--
-- __Note__
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_ALL_GRAPHICS' only
-- includes the original five graphics stages included in Vulkan 1.0, and
-- not any stages added by extensions. Thus, it may not have the desired
-- effect in all cases.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags'
type ShaderStageFlagBits = VkShaderStageFlagBits


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_VERTEX_BIT' specifies
-- the vertex stage.
pattern SHADER_STAGE_VERTEX_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_VERTEX_BIT = VK_SHADER_STAGE_VERTEX_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT'
-- specifies the tessellation control stage.
pattern SHADER_STAGE_TESSELLATION_CONTROL_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_TESSELLATION_CONTROL_BIT = VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
-- specifies the tessellation evaluation stage.
pattern SHADER_STAGE_TESSELLATION_EVALUATION_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_TESSELLATION_EVALUATION_BIT = VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_GEOMETRY_BIT'
-- specifies the geometry stage.
pattern SHADER_STAGE_GEOMETRY_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_GEOMETRY_BIT = VK_SHADER_STAGE_GEOMETRY_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_FRAGMENT_BIT'
-- specifies the fragment stage.
pattern SHADER_STAGE_FRAGMENT_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_FRAGMENT_BIT = VK_SHADER_STAGE_FRAGMENT_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_COMPUTE_BIT'
-- specifies the compute stage.
pattern SHADER_STAGE_COMPUTE_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_COMPUTE_BIT = VK_SHADER_STAGE_COMPUTE_BIT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_ALL_GRAPHICS' is a
-- combination of bits used as shorthand to specify all graphics stages
-- defined above (excluding the compute stage).
pattern SHADER_STAGE_ALL_GRAPHICS :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_ALL_GRAPHICS = VK_SHADER_STAGE_ALL_GRAPHICS


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_ALL' is a combination
-- of bits used as shorthand to specify all shader stages supported by the
-- device, including all additional stages which are introduced by
-- extensions.
pattern SHADER_STAGE_ALL :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_ALL = VK_SHADER_STAGE_ALL


-- | VkSpecializationInfo - Structure specifying specialization info
--
-- = Description
--
-- @pMapEntries@ points to a structure of type
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkSpecializationMapEntry'.
--
-- == Valid Usage
--
-- -   The @offset@ member of each element of @pMapEntries@ /must/ be less
--     than @dataSize@
--
-- -   The @size@ member of each element of @pMapEntries@ /must/ be less
--     than or equal to @dataSize@ minus @offset@
--
-- Unresolved directive in VkSpecializationInfo.txt -
-- include::{generated}\/validity\/structs\/VkSpecializationInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkSpecializationMapEntry'
data SpecializationInfo = SpecializationInfo
  { -- Length valued member elided
  -- No documentation found for Nested "SpecializationInfo" "pMapEntries"
  mapEntries :: Vector SpecializationMapEntry
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "SpecializationInfo" "pData"
  data' :: ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSpecializationInfo' and
-- marshal a 'SpecializationInfo' into it. The 'VkSpecializationInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSpecializationInfo :: SpecializationInfo -> (VkSpecializationInfo -> IO a) -> IO a
withCStructSpecializationInfo marshalled cont = unsafeUseAsCString (data' (marshalled :: SpecializationInfo)) (\pPData -> withVec withCStructSpecializationMapEntry (mapEntries (marshalled :: SpecializationInfo)) (\pPMapEntries -> cont (VkSpecializationInfo (fromIntegral (Data.Vector.length (mapEntries (marshalled :: SpecializationInfo)))) pPMapEntries (fromIntegral (Data.ByteString.length (data' (marshalled :: SpecializationInfo)))) (castPtr pPData))))

-- | A function to read a 'VkSpecializationInfo' and all additional
-- structures in the pointer chain into a 'SpecializationInfo'.
fromCStructSpecializationInfo :: VkSpecializationInfo -> IO SpecializationInfo
fromCStructSpecializationInfo c = SpecializationInfo <$> -- Length valued member elided
                                                     (Data.Vector.generateM (fromIntegral (vkMapEntryCount (c :: VkSpecializationInfo))) (((fromCStructSpecializationMapEntry <=<) . peekElemOff) (vkPMapEntries (c :: VkSpecializationInfo))))
                                                     -- Bytestring length valued member elided
                                                     <*> packCStringLen (castPtr (vkPData (c :: VkSpecializationInfo)), fromIntegral (vkDataSize (c :: VkSpecializationInfo)))

instance Zero SpecializationInfo where
  zero = SpecializationInfo Data.Vector.empty
                            Data.ByteString.empty



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
--     byte size of 'Graphics.Vulkan.C.Core10.Core.VkBool32'
--
-- Unresolved directive in VkSpecializationMapEntry.txt -
-- include::{generated}\/validity\/structs\/VkSpecializationMapEntry.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkSpecializationInfo'
data SpecializationMapEntry = SpecializationMapEntry
  { -- No documentation found for Nested "SpecializationMapEntry" "constantID"
  constantID :: Word32
  , -- No documentation found for Nested "SpecializationMapEntry" "offset"
  offset :: Word32
  , -- No documentation found for Nested "SpecializationMapEntry" "size"
  size :: CSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSpecializationMapEntry' and
-- marshal a 'SpecializationMapEntry' into it. The 'VkSpecializationMapEntry' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSpecializationMapEntry :: SpecializationMapEntry -> (VkSpecializationMapEntry -> IO a) -> IO a
withCStructSpecializationMapEntry marshalled cont = cont (VkSpecializationMapEntry (constantID (marshalled :: SpecializationMapEntry)) (offset (marshalled :: SpecializationMapEntry)) (size (marshalled :: SpecializationMapEntry)))

-- | A function to read a 'VkSpecializationMapEntry' and all additional
-- structures in the pointer chain into a 'SpecializationMapEntry'.
fromCStructSpecializationMapEntry :: VkSpecializationMapEntry -> IO SpecializationMapEntry
fromCStructSpecializationMapEntry c = SpecializationMapEntry <$> pure (vkConstantID (c :: VkSpecializationMapEntry))
                                                             <*> pure (vkOffset (c :: VkSpecializationMapEntry))
                                                             <*> pure (vkSize (c :: VkSpecializationMapEntry))

instance Zero SpecializationMapEntry where
  zero = SpecializationMapEntry zero
                                zero
                                zero


-- | VkStencilOp - Stencil comparison function
--
-- = Description
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
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkStencilOpState' structures controls
-- how the updated stencil value is written to the stencil framebuffer
-- attachment.
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
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkStencilOpState'
type StencilOp = VkStencilOp


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_STENCIL_OP_KEEP' keeps the current
-- value.
pattern STENCIL_OP_KEEP :: (a ~ StencilOp) => a
pattern STENCIL_OP_KEEP = VK_STENCIL_OP_KEEP


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_STENCIL_OP_ZERO' sets the value to
-- 0.
pattern STENCIL_OP_ZERO :: (a ~ StencilOp) => a
pattern STENCIL_OP_ZERO = VK_STENCIL_OP_ZERO


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_STENCIL_OP_REPLACE' sets the value
-- to @reference@.
pattern STENCIL_OP_REPLACE :: (a ~ StencilOp) => a
pattern STENCIL_OP_REPLACE = VK_STENCIL_OP_REPLACE


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_STENCIL_OP_INCREMENT_AND_CLAMP'
-- increments the current value and clamps to the maximum representable
-- unsigned value.
pattern STENCIL_OP_INCREMENT_AND_CLAMP :: (a ~ StencilOp) => a
pattern STENCIL_OP_INCREMENT_AND_CLAMP = VK_STENCIL_OP_INCREMENT_AND_CLAMP


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_STENCIL_OP_DECREMENT_AND_CLAMP'
-- decrements the current value and clamps to 0.
pattern STENCIL_OP_DECREMENT_AND_CLAMP :: (a ~ StencilOp) => a
pattern STENCIL_OP_DECREMENT_AND_CLAMP = VK_STENCIL_OP_DECREMENT_AND_CLAMP


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_STENCIL_OP_INVERT' bitwise-inverts
-- the current value.
pattern STENCIL_OP_INVERT :: (a ~ StencilOp) => a
pattern STENCIL_OP_INVERT = VK_STENCIL_OP_INVERT


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_STENCIL_OP_INCREMENT_AND_WRAP'
-- increments the current value and wraps to 0 when the maximum value would
-- have been exceeded.
pattern STENCIL_OP_INCREMENT_AND_WRAP :: (a ~ StencilOp) => a
pattern STENCIL_OP_INCREMENT_AND_WRAP = VK_STENCIL_OP_INCREMENT_AND_WRAP


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_STENCIL_OP_DECREMENT_AND_WRAP'
-- decrements the current value and wraps to the maximum possible value
-- when the value would go below 0.
pattern STENCIL_OP_DECREMENT_AND_WRAP :: (a ~ StencilOp) => a
pattern STENCIL_OP_DECREMENT_AND_WRAP = VK_STENCIL_OP_DECREMENT_AND_WRAP


-- | VkStencilOpState - Structure specifying stencil operation state
--
-- = Description
--
-- Unresolved directive in VkStencilOpState.txt -
-- include::{generated}\/validity\/structs\/VkStencilOpState.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkCompareOp',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkStencilOp'
data StencilOpState = StencilOpState
  { -- No documentation found for Nested "StencilOpState" "failOp"
  failOp :: StencilOp
  , -- No documentation found for Nested "StencilOpState" "passOp"
  passOp :: StencilOp
  , -- No documentation found for Nested "StencilOpState" "depthFailOp"
  depthFailOp :: StencilOp
  , -- No documentation found for Nested "StencilOpState" "compareOp"
  compareOp :: CompareOp
  , -- No documentation found for Nested "StencilOpState" "compareMask"
  compareMask :: Word32
  , -- No documentation found for Nested "StencilOpState" "writeMask"
  writeMask :: Word32
  , -- No documentation found for Nested "StencilOpState" "reference"
  reference :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkStencilOpState' and
-- marshal a 'StencilOpState' into it. The 'VkStencilOpState' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructStencilOpState :: StencilOpState -> (VkStencilOpState -> IO a) -> IO a
withCStructStencilOpState marshalled cont = cont (VkStencilOpState (failOp (marshalled :: StencilOpState)) (passOp (marshalled :: StencilOpState)) (depthFailOp (marshalled :: StencilOpState)) (compareOp (marshalled :: StencilOpState)) (compareMask (marshalled :: StencilOpState)) (writeMask (marshalled :: StencilOpState)) (reference (marshalled :: StencilOpState)))

-- | A function to read a 'VkStencilOpState' and all additional
-- structures in the pointer chain into a 'StencilOpState'.
fromCStructStencilOpState :: VkStencilOpState -> IO StencilOpState
fromCStructStencilOpState c = StencilOpState <$> pure (vkFailOp (c :: VkStencilOpState))
                                             <*> pure (vkPassOp (c :: VkStencilOpState))
                                             <*> pure (vkDepthFailOp (c :: VkStencilOpState))
                                             <*> pure (vkCompareOp (c :: VkStencilOpState))
                                             <*> pure (vkCompareMask (c :: VkStencilOpState))
                                             <*> pure (vkWriteMask (c :: VkStencilOpState))
                                             <*> pure (vkReference (c :: VkStencilOpState))

instance Zero StencilOpState where
  zero = StencilOpState zero
                        zero
                        zero
                        zero
                        zero
                        zero
                        zero



-- | VkVertexInputAttributeDescription - Structure specifying vertex input
-- attribute description
--
-- == Valid Usage
--
-- Unresolved directive in VkVertexInputAttributeDescription.txt -
-- include::{generated}\/validity\/structs\/VkVertexInputAttributeDescription.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateInfo'
data VertexInputAttributeDescription = VertexInputAttributeDescription
  { -- No documentation found for Nested "VertexInputAttributeDescription" "location"
  location :: Word32
  , -- No documentation found for Nested "VertexInputAttributeDescription" "binding"
  binding :: Word32
  , -- No documentation found for Nested "VertexInputAttributeDescription" "format"
  format :: Format
  , -- No documentation found for Nested "VertexInputAttributeDescription" "offset"
  offset :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkVertexInputAttributeDescription' and
-- marshal a 'VertexInputAttributeDescription' into it. The 'VkVertexInputAttributeDescription' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructVertexInputAttributeDescription :: VertexInputAttributeDescription -> (VkVertexInputAttributeDescription -> IO a) -> IO a
withCStructVertexInputAttributeDescription marshalled cont = cont (VkVertexInputAttributeDescription (location (marshalled :: VertexInputAttributeDescription)) (binding (marshalled :: VertexInputAttributeDescription)) (format (marshalled :: VertexInputAttributeDescription)) (offset (marshalled :: VertexInputAttributeDescription)))

-- | A function to read a 'VkVertexInputAttributeDescription' and all additional
-- structures in the pointer chain into a 'VertexInputAttributeDescription'.
fromCStructVertexInputAttributeDescription :: VkVertexInputAttributeDescription -> IO VertexInputAttributeDescription
fromCStructVertexInputAttributeDescription c = VertexInputAttributeDescription <$> pure (vkLocation (c :: VkVertexInputAttributeDescription))
                                                                               <*> pure (vkBinding (c :: VkVertexInputAttributeDescription))
                                                                               <*> pure (vkFormat (c :: VkVertexInputAttributeDescription))
                                                                               <*> pure (vkOffset (c :: VkVertexInputAttributeDescription))

instance Zero VertexInputAttributeDescription where
  zero = VertexInputAttributeDescription zero
                                         zero
                                         zero
                                         zero



-- | VkVertexInputBindingDescription - Structure specifying vertex input
-- binding description
--
-- == Valid Usage
--
-- Unresolved directive in VkVertexInputBindingDescription.txt -
-- include::{generated}\/validity\/structs\/VkVertexInputBindingDescription.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputRate'
data VertexInputBindingDescription = VertexInputBindingDescription
  { -- No documentation found for Nested "VertexInputBindingDescription" "binding"
  binding :: Word32
  , -- No documentation found for Nested "VertexInputBindingDescription" "stride"
  stride :: Word32
  , -- No documentation found for Nested "VertexInputBindingDescription" "inputRate"
  inputRate :: VertexInputRate
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkVertexInputBindingDescription' and
-- marshal a 'VertexInputBindingDescription' into it. The 'VkVertexInputBindingDescription' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructVertexInputBindingDescription :: VertexInputBindingDescription -> (VkVertexInputBindingDescription -> IO a) -> IO a
withCStructVertexInputBindingDescription marshalled cont = cont (VkVertexInputBindingDescription (binding (marshalled :: VertexInputBindingDescription)) (stride (marshalled :: VertexInputBindingDescription)) (inputRate (marshalled :: VertexInputBindingDescription)))

-- | A function to read a 'VkVertexInputBindingDescription' and all additional
-- structures in the pointer chain into a 'VertexInputBindingDescription'.
fromCStructVertexInputBindingDescription :: VkVertexInputBindingDescription -> IO VertexInputBindingDescription
fromCStructVertexInputBindingDescription c = VertexInputBindingDescription <$> pure (vkBinding (c :: VkVertexInputBindingDescription))
                                                                           <*> pure (vkStride (c :: VkVertexInputBindingDescription))
                                                                           <*> pure (vkInputRate (c :: VkVertexInputBindingDescription))

instance Zero VertexInputBindingDescription where
  zero = VertexInputBindingDescription zero
                                       zero
                                       zero


-- | VkVertexInputRate - Specify rate at which vertex attributes are pulled
-- from buffers
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputBindingDescription'
type VertexInputRate = VkVertexInputRate


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_VERTEX_INPUT_RATE_VERTEX'
-- specifies that vertex attribute addressing is a function of the vertex
-- index.
pattern VERTEX_INPUT_RATE_VERTEX :: (a ~ VertexInputRate) => a
pattern VERTEX_INPUT_RATE_VERTEX = VK_VERTEX_INPUT_RATE_VERTEX


-- | 'Graphics.Vulkan.C.Core10.Pipeline.VK_VERTEX_INPUT_RATE_INSTANCE'
-- specifies that vertex attribute addressing is a function of the instance
-- index.
pattern VERTEX_INPUT_RATE_INSTANCE :: (a ~ VertexInputRate) => a
pattern VERTEX_INPUT_RATE_INSTANCE = VK_VERTEX_INPUT_RATE_INSTANCE


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
-- The width and height of the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-maxViewportDimensions implementation-dependent maximum viewport dimensions>
-- /must/ be greater than or equal to the width and height of the largest
-- image which /can/ be created and attached to a framebuffer.
--
-- The floating-point viewport bounds are represented with an
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-viewportSubPixelBits implementation-dependent precision>.
--
-- == Valid Usage
--
-- -   @width@ /must/ be greater than @0.0@
--
-- -   @width@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewportDimensions@[0]
--
-- -   The absolute value of @height@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewportDimensions@[1]
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
-- -   Unless
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
--     extension is enabled @minDepth@ /must/ be between @0.0@ and @1.0@,
--     inclusive
--
-- -   Unless
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
--     extension is enabled @maxDepth@ /must/ be between @0.0@ and @1.0@,
--     inclusive
--
-- Unresolved directive in VkViewport.txt -
-- include::{generated}\/validity\/structs\/VkViewport.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetViewport'
data Viewport = Viewport
  { -- No documentation found for Nested "Viewport" "x"
  x :: CFloat
  , -- No documentation found for Nested "Viewport" "y"
  y :: CFloat
  , -- No documentation found for Nested "Viewport" "width"
  width :: CFloat
  , -- No documentation found for Nested "Viewport" "height"
  height :: CFloat
  , -- No documentation found for Nested "Viewport" "minDepth"
  minDepth :: CFloat
  , -- No documentation found for Nested "Viewport" "maxDepth"
  maxDepth :: CFloat
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkViewport' and
-- marshal a 'Viewport' into it. The 'VkViewport' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructViewport :: Viewport -> (VkViewport -> IO a) -> IO a
withCStructViewport marshalled cont = cont (VkViewport (x (marshalled :: Viewport)) (y (marshalled :: Viewport)) (width (marshalled :: Viewport)) (height (marshalled :: Viewport)) (minDepth (marshalled :: Viewport)) (maxDepth (marshalled :: Viewport)))

-- | A function to read a 'VkViewport' and all additional
-- structures in the pointer chain into a 'Viewport'.
fromCStructViewport :: VkViewport -> IO Viewport
fromCStructViewport c = Viewport <$> pure (vkX (c :: VkViewport))
                                 <*> pure (vkY (c :: VkViewport))
                                 <*> pure (vkWidth (c :: VkViewport))
                                 <*> pure (vkHeight (c :: VkViewport))
                                 <*> pure (vkMinDepth (c :: VkViewport))
                                 <*> pure (vkMaxDepth (c :: VkViewport))

instance Zero Viewport where
  zero = Viewport zero
                  zero
                  zero
                  zero
                  zero
                  zero



-- | vkCreateComputePipelines - Creates a new compute pipeline object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the compute pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', indicating that
--     pipeline caching is disabled; or the handle of a valid
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-cache pipeline cache>
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is an array of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo'
--     structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting compute
--     pipeline objects are returned.
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- Unresolved directive in vkCreateComputePipelines.txt -
-- include::{generated}\/validity\/protos\/vkCreateComputePipelines.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache'
createComputePipelines :: Device ->  PipelineCache ->  Vector ComputePipelineCreateInfo ->  Maybe AllocationCallbacks ->  IO (Vector Pipeline)
createComputePipelines = \(Device device' commandTable) -> \pipelineCache' -> \createInfos' -> \allocator -> allocaArray ((Data.Vector.length createInfos')) (\pPipelines' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> withVec withCStructComputePipelineCreateInfo createInfos' (\pCreateInfos' -> vkCreateComputePipelines commandTable device' pipelineCache' (fromIntegral $ Data.Vector.length createInfos') pCreateInfos' pAllocator pPipelines' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((Data.Vector.generateM ((Data.Vector.length createInfos')) (peekElemOff pPipelines')))))))


-- | vkCreateGraphicsPipelines - Create graphics pipelines
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the graphics pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', indicating that
--     pipeline caching is disabled; or the handle of a valid
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-cache pipeline cache>
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is an array of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting
--     graphics pipeline objects are returned.
--
-- = Description
--
-- The 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
-- structure includes an array of shader create info structures containing
-- all the desired active shader stages, as well as creation info to define
-- all relevant fixed-function stages, and a pipeline layout.
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- Unresolved directive in vkCreateGraphicsPipelines.txt -
-- include::{generated}\/validity\/protos\/vkCreateGraphicsPipelines.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache'
createGraphicsPipelines :: Device ->  PipelineCache ->  Vector GraphicsPipelineCreateInfo ->  Maybe AllocationCallbacks ->  IO (Vector Pipeline)
createGraphicsPipelines = \(Device device' commandTable) -> \pipelineCache' -> \createInfos' -> \allocator -> allocaArray ((Data.Vector.length createInfos')) (\pPipelines' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> withVec withCStructGraphicsPipelineCreateInfo createInfos' (\pCreateInfos' -> vkCreateGraphicsPipelines commandTable device' pipelineCache' (fromIntegral $ Data.Vector.length createInfos') pCreateInfos' pAllocator pPipelines' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((Data.Vector.generateM ((Data.Vector.length createInfos')) (peekElemOff pPipelines')))))))


-- | vkDestroyPipeline - Destroy a pipeline object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the pipeline.
--
-- -   @pipeline@ is the handle of the pipeline to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @pipeline@ /must/ have
--     completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @pipeline@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @pipeline@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyPipeline.txt -
-- include::{generated}\/validity\/protos\/vkDestroyPipeline.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
destroyPipeline :: Device ->  Pipeline ->  Maybe AllocationCallbacks ->  IO ()
destroyPipeline = \(Device device' commandTable) -> \pipeline' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyPipeline commandTable device' pipeline' pAllocator *> (pure ()))

-- | A safe wrapper for 'createComputePipelines' and 'destroyPipeline' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withComputePipelines
  :: Device -> PipelineCache -> Vector (ComputePipelineCreateInfo) -> Maybe (AllocationCallbacks) -> (Vector (Pipeline) -> IO a) -> IO a
withComputePipelines device pipelineCache computePipelineCreateInfo allocationCallbacks = bracket
  (createComputePipelines device pipelineCache computePipelineCreateInfo allocationCallbacks)
  (traverse (\o -> destroyPipeline device o allocationCallbacks))

-- | A safe wrapper for 'createGraphicsPipelines' and 'destroyPipeline' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withGraphicsPipelines
  :: Device -> PipelineCache -> Vector (GraphicsPipelineCreateInfo) -> Maybe (AllocationCallbacks) -> (Vector (Pipeline) -> IO a) -> IO a
withGraphicsPipelines device pipelineCache graphicsPipelineCreateInfo allocationCallbacks = bracket
  (createGraphicsPipelines device pipelineCache graphicsPipelineCreateInfo allocationCallbacks)
  (traverse (\o -> destroyPipeline device o allocationCallbacks))
