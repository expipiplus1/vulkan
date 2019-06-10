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
  , pattern BLEND_OP_ZERO_EXT
  , pattern BLEND_OP_SRC_EXT
  , pattern BLEND_OP_DST_EXT
  , pattern BLEND_OP_SRC_OVER_EXT
  , pattern BLEND_OP_DST_OVER_EXT
  , pattern BLEND_OP_SRC_IN_EXT
  , pattern BLEND_OP_DST_IN_EXT
  , pattern BLEND_OP_SRC_OUT_EXT
  , pattern BLEND_OP_DST_OUT_EXT
  , pattern BLEND_OP_SRC_ATOP_EXT
  , pattern BLEND_OP_DST_ATOP_EXT
  , pattern BLEND_OP_XOR_EXT
  , pattern BLEND_OP_MULTIPLY_EXT
  , pattern BLEND_OP_SCREEN_EXT
  , pattern BLEND_OP_OVERLAY_EXT
  , pattern BLEND_OP_DARKEN_EXT
  , pattern BLEND_OP_LIGHTEN_EXT
  , pattern BLEND_OP_COLORDODGE_EXT
  , pattern BLEND_OP_COLORBURN_EXT
  , pattern BLEND_OP_HARDLIGHT_EXT
  , pattern BLEND_OP_SOFTLIGHT_EXT
  , pattern BLEND_OP_DIFFERENCE_EXT
  , pattern BLEND_OP_EXCLUSION_EXT
  , pattern BLEND_OP_INVERT_EXT
  , pattern BLEND_OP_INVERT_RGB_EXT
  , pattern BLEND_OP_LINEARDODGE_EXT
  , pattern BLEND_OP_LINEARBURN_EXT
  , pattern BLEND_OP_VIVIDLIGHT_EXT
  , pattern BLEND_OP_LINEARLIGHT_EXT
  , pattern BLEND_OP_PINLIGHT_EXT
  , pattern BLEND_OP_HARDMIX_EXT
  , pattern BLEND_OP_HSL_HUE_EXT
  , pattern BLEND_OP_HSL_SATURATION_EXT
  , pattern BLEND_OP_HSL_COLOR_EXT
  , pattern BLEND_OP_HSL_LUMINOSITY_EXT
  , pattern BLEND_OP_PLUS_EXT
  , pattern BLEND_OP_PLUS_CLAMPED_EXT
  , pattern BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
  , pattern BLEND_OP_PLUS_DARKER_EXT
  , pattern BLEND_OP_MINUS_EXT
  , pattern BLEND_OP_MINUS_CLAMPED_EXT
  , pattern BLEND_OP_CONTRAST_EXT
  , pattern BLEND_OP_INVERT_OVG_EXT
  , pattern BLEND_OP_RED_EXT
  , pattern BLEND_OP_GREEN_EXT
  , pattern BLEND_OP_BLUE_EXT
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
#if defined(VK_USE_PLATFORM_GGP)
  , ComputePipelineCreateInfo(..)
#endif
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
  , pattern DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  , pattern DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
  , pattern DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
  , pattern DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
  , pattern DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
  , pattern DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  , Extent2D(..)
  , FrontFace
  , pattern FRONT_FACE_COUNTER_CLOCKWISE
  , pattern FRONT_FACE_CLOCKWISE
#if defined(VK_USE_PLATFORM_GGP)
  , GraphicsPipelineCreateInfo(..)
#endif
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
  , Offset2D(..)
  , Pipeline
  , PipelineColorBlendAttachmentState(..)
  , PipelineColorBlendStateCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineColorBlendStateCreateInfo(..)
#endif
  , PipelineCreateFlagBits
  , pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT
  , pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT
  , pattern PIPELINE_CREATE_DERIVATIVE_BIT
  , pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern PIPELINE_CREATE_DISPATCH_BASE
  , pattern PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
  , PipelineCreateFlags
  , PipelineDepthStencilStateCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineDepthStencilStateCreateInfo(..)
#endif
  , PipelineDynamicStateCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineDynamicStateCreateInfo(..)
#endif
  , PipelineInputAssemblyStateCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineInputAssemblyStateCreateInfo(..)
#endif
  , PipelineLayout
  , PipelineMultisampleStateCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineMultisampleStateCreateInfo(..)
#endif
  , PipelineRasterizationStateCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineRasterizationStateCreateInfo(..)
#endif
  , PipelineShaderStageCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineShaderStageCreateInfo(..)
#endif
  , PipelineTessellationStateCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineTessellationStateCreateInfo(..)
#endif
  , PipelineVertexInputStateCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineVertexInputStateCreateInfo(..)
#endif
  , PipelineViewportStateCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , PipelineViewportStateCreateInfo(..)
#endif
  , PolygonMode
  , pattern POLYGON_MODE_FILL
  , pattern POLYGON_MODE_LINE
  , pattern POLYGON_MODE_POINT
  , pattern POLYGON_MODE_FILL_RECTANGLE_NV
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
  , pattern SHADER_STAGE_RAYGEN_BIT_NV
  , pattern SHADER_STAGE_ANY_HIT_BIT_NV
  , pattern SHADER_STAGE_CLOSEST_HIT_BIT_NV
  , pattern SHADER_STAGE_MISS_BIT_NV
  , pattern SHADER_STAGE_INTERSECTION_BIT_NV
  , pattern SHADER_STAGE_CALLABLE_BIT_NV
  , pattern SHADER_STAGE_TASK_BIT_NV
  , pattern SHADER_STAGE_MESH_BIT_NV
  , SpecializationInfo(..)
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
  , StencilOpState(..)
  , VertexInputAttributeDescription(..)
  , VertexInputBindingDescription(..)
  , VertexInputRate
  , pattern VERTEX_INPUT_RATE_VERTEX
  , pattern VERTEX_INPUT_RATE_INSTANCE
  , Viewport(..)
  , createComputePipelines
  , createGraphicsPipelines
  , destroyPipeline
  , withComputePipelines
  , withGraphicsPipelines
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.ByteString
  ( ByteString
  )
#endif
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkBlendFactor(..)
  , VkBlendOp(..)
  , VkColorComponentFlagBits(..)
  , VkCompareOp(..)
  , VkCullModeFlagBits(..)
  , VkDynamicState(..)
  , VkFrontFace(..)
  , VkLogicOp(..)
  , VkPipelineColorBlendStateCreateFlags(..)
  , VkPipelineCreateFlagBits(..)
  , VkPipelineDepthStencilStateCreateFlags(..)
  , VkPipelineDynamicStateCreateFlags(..)
  , VkPipelineInputAssemblyStateCreateFlags(..)
  , VkPipelineMultisampleStateCreateFlags(..)
  , VkPipelineRasterizationStateCreateFlags(..)
  , VkPipelineShaderStageCreateFlags(..)
  , VkPipelineTessellationStateCreateFlags(..)
  , VkPipelineVertexInputStateCreateFlags(..)
  , VkPipelineViewportStateCreateFlags(..)
  , VkPolygonMode(..)
  , VkPrimitiveTopology(..)
  , VkShaderStageFlagBits(..)
  , VkStencilOp(..)
  , VkVertexInputRate(..)
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( pattern VK_BLEND_OP_BLUE_EXT
  , pattern VK_BLEND_OP_COLORBURN_EXT
  , pattern VK_BLEND_OP_COLORDODGE_EXT
  , pattern VK_BLEND_OP_CONTRAST_EXT
  , pattern VK_BLEND_OP_DARKEN_EXT
  , pattern VK_BLEND_OP_DIFFERENCE_EXT
  , pattern VK_BLEND_OP_DST_ATOP_EXT
  , pattern VK_BLEND_OP_DST_EXT
  , pattern VK_BLEND_OP_DST_IN_EXT
  , pattern VK_BLEND_OP_DST_OUT_EXT
  , pattern VK_BLEND_OP_DST_OVER_EXT
  , pattern VK_BLEND_OP_EXCLUSION_EXT
  , pattern VK_BLEND_OP_GREEN_EXT
  , pattern VK_BLEND_OP_HARDLIGHT_EXT
  , pattern VK_BLEND_OP_HARDMIX_EXT
  , pattern VK_BLEND_OP_HSL_COLOR_EXT
  , pattern VK_BLEND_OP_HSL_HUE_EXT
  , pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT
  , pattern VK_BLEND_OP_HSL_SATURATION_EXT
  , pattern VK_BLEND_OP_INVERT_EXT
  , pattern VK_BLEND_OP_INVERT_OVG_EXT
  , pattern VK_BLEND_OP_INVERT_RGB_EXT
  , pattern VK_BLEND_OP_LIGHTEN_EXT
  , pattern VK_BLEND_OP_LINEARBURN_EXT
  , pattern VK_BLEND_OP_LINEARDODGE_EXT
  , pattern VK_BLEND_OP_LINEARLIGHT_EXT
  , pattern VK_BLEND_OP_MINUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_MINUS_EXT
  , pattern VK_BLEND_OP_MULTIPLY_EXT
  , pattern VK_BLEND_OP_OVERLAY_EXT
  , pattern VK_BLEND_OP_PINLIGHT_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_PLUS_DARKER_EXT
  , pattern VK_BLEND_OP_PLUS_EXT
  , pattern VK_BLEND_OP_RED_EXT
  , pattern VK_BLEND_OP_SCREEN_EXT
  , pattern VK_BLEND_OP_SOFTLIGHT_EXT
  , pattern VK_BLEND_OP_SRC_ATOP_EXT
  , pattern VK_BLEND_OP_SRC_EXT
  , pattern VK_BLEND_OP_SRC_IN_EXT
  , pattern VK_BLEND_OP_SRC_OUT_EXT
  , pattern VK_BLEND_OP_SRC_OVER_EXT
  , pattern VK_BLEND_OP_VIVIDLIGHT_EXT
  , pattern VK_BLEND_OP_XOR_EXT
  , pattern VK_BLEND_OP_ZERO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( pattern VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( pattern VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_fill_rectangle
  ( pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( pattern VK_SHADER_STAGE_MESH_BIT_NV
  , pattern VK_SHADER_STAGE_TASK_BIT_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
  , pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV
  , pattern VK_SHADER_STAGE_CALLABLE_BIT_NV
  , pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV
  , pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV
  , pattern VK_SHADER_STAGE_MISS_BIT_NV
  , pattern VK_SHADER_STAGE_RAYGEN_BIT_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( pattern VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( pattern VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV
  , pattern VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( SampleCountFlagBits
  )
#endif
import Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCache
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Shader
  ( ShaderModule
  )
#endif
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "BlendFactor"
type BlendFactor = VkBlendFactor


{-# complete BLEND_FACTOR_ZERO, BLEND_FACTOR_ONE, BLEND_FACTOR_SRC_COLOR, BLEND_FACTOR_ONE_MINUS_SRC_COLOR, BLEND_FACTOR_DST_COLOR, BLEND_FACTOR_ONE_MINUS_DST_COLOR, BLEND_FACTOR_SRC_ALPHA, BLEND_FACTOR_ONE_MINUS_SRC_ALPHA, BLEND_FACTOR_DST_ALPHA, BLEND_FACTOR_ONE_MINUS_DST_ALPHA, BLEND_FACTOR_CONSTANT_COLOR, BLEND_FACTOR_ONE_MINUS_CONSTANT_COLOR, BLEND_FACTOR_CONSTANT_ALPHA, BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA, BLEND_FACTOR_SRC_ALPHA_SATURATE, BLEND_FACTOR_SRC1_COLOR, BLEND_FACTOR_ONE_MINUS_SRC1_COLOR, BLEND_FACTOR_SRC1_ALPHA, BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA :: BlendFactor #-}


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

-- No documentation found for TopLevel "BlendOp"
type BlendOp = VkBlendOp


{-# complete BLEND_OP_ADD, BLEND_OP_SUBTRACT, BLEND_OP_REVERSE_SUBTRACT, BLEND_OP_MIN, BLEND_OP_MAX, BLEND_OP_ZERO_EXT, BLEND_OP_SRC_EXT, BLEND_OP_DST_EXT, BLEND_OP_SRC_OVER_EXT, BLEND_OP_DST_OVER_EXT, BLEND_OP_SRC_IN_EXT, BLEND_OP_DST_IN_EXT, BLEND_OP_SRC_OUT_EXT, BLEND_OP_DST_OUT_EXT, BLEND_OP_SRC_ATOP_EXT, BLEND_OP_DST_ATOP_EXT, BLEND_OP_XOR_EXT, BLEND_OP_MULTIPLY_EXT, BLEND_OP_SCREEN_EXT, BLEND_OP_OVERLAY_EXT, BLEND_OP_DARKEN_EXT, BLEND_OP_LIGHTEN_EXT, BLEND_OP_COLORDODGE_EXT, BLEND_OP_COLORBURN_EXT, BLEND_OP_HARDLIGHT_EXT, BLEND_OP_SOFTLIGHT_EXT, BLEND_OP_DIFFERENCE_EXT, BLEND_OP_EXCLUSION_EXT, BLEND_OP_INVERT_EXT, BLEND_OP_INVERT_RGB_EXT, BLEND_OP_LINEARDODGE_EXT, BLEND_OP_LINEARBURN_EXT, BLEND_OP_VIVIDLIGHT_EXT, BLEND_OP_LINEARLIGHT_EXT, BLEND_OP_PINLIGHT_EXT, BLEND_OP_HARDMIX_EXT, BLEND_OP_HSL_HUE_EXT, BLEND_OP_HSL_SATURATION_EXT, BLEND_OP_HSL_COLOR_EXT, BLEND_OP_HSL_LUMINOSITY_EXT, BLEND_OP_PLUS_EXT, BLEND_OP_PLUS_CLAMPED_EXT, BLEND_OP_PLUS_CLAMPED_ALPHA_EXT, BLEND_OP_PLUS_DARKER_EXT, BLEND_OP_MINUS_EXT, BLEND_OP_MINUS_CLAMPED_EXT, BLEND_OP_CONTRAST_EXT, BLEND_OP_INVERT_OVG_EXT, BLEND_OP_RED_EXT, BLEND_OP_GREEN_EXT, BLEND_OP_BLUE_EXT :: BlendOp #-}


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


-- No documentation found for Nested "BlendOp" "BLEND_OP_ZERO_EXT"
pattern BLEND_OP_ZERO_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_ZERO_EXT = VK_BLEND_OP_ZERO_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_SRC_EXT"
pattern BLEND_OP_SRC_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_SRC_EXT = VK_BLEND_OP_SRC_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_DST_EXT"
pattern BLEND_OP_DST_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_DST_EXT = VK_BLEND_OP_DST_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_SRC_OVER_EXT"
pattern BLEND_OP_SRC_OVER_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_SRC_OVER_EXT = VK_BLEND_OP_SRC_OVER_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_DST_OVER_EXT"
pattern BLEND_OP_DST_OVER_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_DST_OVER_EXT = VK_BLEND_OP_DST_OVER_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_SRC_IN_EXT"
pattern BLEND_OP_SRC_IN_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_SRC_IN_EXT = VK_BLEND_OP_SRC_IN_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_DST_IN_EXT"
pattern BLEND_OP_DST_IN_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_DST_IN_EXT = VK_BLEND_OP_DST_IN_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_SRC_OUT_EXT"
pattern BLEND_OP_SRC_OUT_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_SRC_OUT_EXT = VK_BLEND_OP_SRC_OUT_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_DST_OUT_EXT"
pattern BLEND_OP_DST_OUT_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_DST_OUT_EXT = VK_BLEND_OP_DST_OUT_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_SRC_ATOP_EXT"
pattern BLEND_OP_SRC_ATOP_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_SRC_ATOP_EXT = VK_BLEND_OP_SRC_ATOP_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_DST_ATOP_EXT"
pattern BLEND_OP_DST_ATOP_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_DST_ATOP_EXT = VK_BLEND_OP_DST_ATOP_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_XOR_EXT"
pattern BLEND_OP_XOR_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_XOR_EXT = VK_BLEND_OP_XOR_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_MULTIPLY_EXT"
pattern BLEND_OP_MULTIPLY_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_MULTIPLY_EXT = VK_BLEND_OP_MULTIPLY_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_SCREEN_EXT"
pattern BLEND_OP_SCREEN_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_SCREEN_EXT = VK_BLEND_OP_SCREEN_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_OVERLAY_EXT"
pattern BLEND_OP_OVERLAY_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_OVERLAY_EXT = VK_BLEND_OP_OVERLAY_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_DARKEN_EXT"
pattern BLEND_OP_DARKEN_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_DARKEN_EXT = VK_BLEND_OP_DARKEN_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_LIGHTEN_EXT"
pattern BLEND_OP_LIGHTEN_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_LIGHTEN_EXT = VK_BLEND_OP_LIGHTEN_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_COLORDODGE_EXT"
pattern BLEND_OP_COLORDODGE_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_COLORDODGE_EXT = VK_BLEND_OP_COLORDODGE_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_COLORBURN_EXT"
pattern BLEND_OP_COLORBURN_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_COLORBURN_EXT = VK_BLEND_OP_COLORBURN_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_HARDLIGHT_EXT"
pattern BLEND_OP_HARDLIGHT_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_HARDLIGHT_EXT = VK_BLEND_OP_HARDLIGHT_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_SOFTLIGHT_EXT"
pattern BLEND_OP_SOFTLIGHT_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_SOFTLIGHT_EXT = VK_BLEND_OP_SOFTLIGHT_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_DIFFERENCE_EXT"
pattern BLEND_OP_DIFFERENCE_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_DIFFERENCE_EXT = VK_BLEND_OP_DIFFERENCE_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_EXCLUSION_EXT"
pattern BLEND_OP_EXCLUSION_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_EXCLUSION_EXT = VK_BLEND_OP_EXCLUSION_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_INVERT_EXT"
pattern BLEND_OP_INVERT_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_INVERT_EXT = VK_BLEND_OP_INVERT_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_INVERT_RGB_EXT"
pattern BLEND_OP_INVERT_RGB_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_INVERT_RGB_EXT = VK_BLEND_OP_INVERT_RGB_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_LINEARDODGE_EXT"
pattern BLEND_OP_LINEARDODGE_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_LINEARDODGE_EXT = VK_BLEND_OP_LINEARDODGE_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_LINEARBURN_EXT"
pattern BLEND_OP_LINEARBURN_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_LINEARBURN_EXT = VK_BLEND_OP_LINEARBURN_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_VIVIDLIGHT_EXT"
pattern BLEND_OP_VIVIDLIGHT_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_VIVIDLIGHT_EXT = VK_BLEND_OP_VIVIDLIGHT_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_LINEARLIGHT_EXT"
pattern BLEND_OP_LINEARLIGHT_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_LINEARLIGHT_EXT = VK_BLEND_OP_LINEARLIGHT_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_PINLIGHT_EXT"
pattern BLEND_OP_PINLIGHT_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_PINLIGHT_EXT = VK_BLEND_OP_PINLIGHT_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_HARDMIX_EXT"
pattern BLEND_OP_HARDMIX_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_HARDMIX_EXT = VK_BLEND_OP_HARDMIX_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_HSL_HUE_EXT"
pattern BLEND_OP_HSL_HUE_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_HSL_HUE_EXT = VK_BLEND_OP_HSL_HUE_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_HSL_SATURATION_EXT"
pattern BLEND_OP_HSL_SATURATION_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_HSL_SATURATION_EXT = VK_BLEND_OP_HSL_SATURATION_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_HSL_COLOR_EXT"
pattern BLEND_OP_HSL_COLOR_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_HSL_COLOR_EXT = VK_BLEND_OP_HSL_COLOR_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_HSL_LUMINOSITY_EXT"
pattern BLEND_OP_HSL_LUMINOSITY_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_HSL_LUMINOSITY_EXT = VK_BLEND_OP_HSL_LUMINOSITY_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_PLUS_EXT"
pattern BLEND_OP_PLUS_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_PLUS_EXT = VK_BLEND_OP_PLUS_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_PLUS_CLAMPED_EXT"
pattern BLEND_OP_PLUS_CLAMPED_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_PLUS_CLAMPED_EXT = VK_BLEND_OP_PLUS_CLAMPED_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_PLUS_CLAMPED_ALPHA_EXT"
pattern BLEND_OP_PLUS_CLAMPED_ALPHA_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_PLUS_CLAMPED_ALPHA_EXT = VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_PLUS_DARKER_EXT"
pattern BLEND_OP_PLUS_DARKER_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_PLUS_DARKER_EXT = VK_BLEND_OP_PLUS_DARKER_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_MINUS_EXT"
pattern BLEND_OP_MINUS_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_MINUS_EXT = VK_BLEND_OP_MINUS_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_MINUS_CLAMPED_EXT"
pattern BLEND_OP_MINUS_CLAMPED_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_MINUS_CLAMPED_EXT = VK_BLEND_OP_MINUS_CLAMPED_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_CONTRAST_EXT"
pattern BLEND_OP_CONTRAST_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_CONTRAST_EXT = VK_BLEND_OP_CONTRAST_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_INVERT_OVG_EXT"
pattern BLEND_OP_INVERT_OVG_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_INVERT_OVG_EXT = VK_BLEND_OP_INVERT_OVG_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_RED_EXT"
pattern BLEND_OP_RED_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_RED_EXT = VK_BLEND_OP_RED_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_GREEN_EXT"
pattern BLEND_OP_GREEN_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_GREEN_EXT = VK_BLEND_OP_GREEN_EXT


-- No documentation found for Nested "BlendOp" "BLEND_OP_BLUE_EXT"
pattern BLEND_OP_BLUE_EXT :: (a ~ BlendOp) => a
pattern BLEND_OP_BLUE_EXT = VK_BLEND_OP_BLUE_EXT

-- No documentation found for TopLevel "ColorComponentFlagBits"
type ColorComponentFlagBits = VkColorComponentFlagBits


{-# complete COLOR_COMPONENT_R_BIT, COLOR_COMPONENT_G_BIT, COLOR_COMPONENT_B_BIT, COLOR_COMPONENT_A_BIT :: ColorComponentFlagBits #-}


-- No documentation found for Nested "ColorComponentFlagBits" "COLOR_COMPONENT_R_BIT"
pattern COLOR_COMPONENT_R_BIT :: (a ~ ColorComponentFlagBits) => a
pattern COLOR_COMPONENT_R_BIT = VK_COLOR_COMPONENT_R_BIT


-- No documentation found for Nested "ColorComponentFlagBits" "COLOR_COMPONENT_G_BIT"
pattern COLOR_COMPONENT_G_BIT :: (a ~ ColorComponentFlagBits) => a
pattern COLOR_COMPONENT_G_BIT = VK_COLOR_COMPONENT_G_BIT


-- No documentation found for Nested "ColorComponentFlagBits" "COLOR_COMPONENT_B_BIT"
pattern COLOR_COMPONENT_B_BIT :: (a ~ ColorComponentFlagBits) => a
pattern COLOR_COMPONENT_B_BIT = VK_COLOR_COMPONENT_B_BIT


-- No documentation found for Nested "ColorComponentFlagBits" "COLOR_COMPONENT_A_BIT"
pattern COLOR_COMPONENT_A_BIT :: (a ~ ColorComponentFlagBits) => a
pattern COLOR_COMPONENT_A_BIT = VK_COLOR_COMPONENT_A_BIT

-- No documentation found for TopLevel "ColorComponentFlags"
type ColorComponentFlags = ColorComponentFlagBits

-- No documentation found for TopLevel "CompareOp"
type CompareOp = VkCompareOp


{-# complete COMPARE_OP_NEVER, COMPARE_OP_LESS, COMPARE_OP_EQUAL, COMPARE_OP_LESS_OR_EQUAL, COMPARE_OP_GREATER, COMPARE_OP_NOT_EQUAL, COMPARE_OP_GREATER_OR_EQUAL, COMPARE_OP_ALWAYS :: CompareOp #-}


-- No documentation found for Nested "CompareOp" "COMPARE_OP_NEVER"
pattern COMPARE_OP_NEVER :: (a ~ CompareOp) => a
pattern COMPARE_OP_NEVER = VK_COMPARE_OP_NEVER


-- No documentation found for Nested "CompareOp" "COMPARE_OP_LESS"
pattern COMPARE_OP_LESS :: (a ~ CompareOp) => a
pattern COMPARE_OP_LESS = VK_COMPARE_OP_LESS


-- No documentation found for Nested "CompareOp" "COMPARE_OP_EQUAL"
pattern COMPARE_OP_EQUAL :: (a ~ CompareOp) => a
pattern COMPARE_OP_EQUAL = VK_COMPARE_OP_EQUAL


-- No documentation found for Nested "CompareOp" "COMPARE_OP_LESS_OR_EQUAL"
pattern COMPARE_OP_LESS_OR_EQUAL :: (a ~ CompareOp) => a
pattern COMPARE_OP_LESS_OR_EQUAL = VK_COMPARE_OP_LESS_OR_EQUAL


-- No documentation found for Nested "CompareOp" "COMPARE_OP_GREATER"
pattern COMPARE_OP_GREATER :: (a ~ CompareOp) => a
pattern COMPARE_OP_GREATER = VK_COMPARE_OP_GREATER


-- No documentation found for Nested "CompareOp" "COMPARE_OP_NOT_EQUAL"
pattern COMPARE_OP_NOT_EQUAL :: (a ~ CompareOp) => a
pattern COMPARE_OP_NOT_EQUAL = VK_COMPARE_OP_NOT_EQUAL


-- No documentation found for Nested "CompareOp" "COMPARE_OP_GREATER_OR_EQUAL"
pattern COMPARE_OP_GREATER_OR_EQUAL :: (a ~ CompareOp) => a
pattern COMPARE_OP_GREATER_OR_EQUAL = VK_COMPARE_OP_GREATER_OR_EQUAL


-- No documentation found for Nested "CompareOp" "COMPARE_OP_ALWAYS"
pattern COMPARE_OP_ALWAYS :: (a ~ CompareOp) => a
pattern COMPARE_OP_ALWAYS = VK_COMPARE_OP_ALWAYS


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkComputePipelineCreateInfo"
data ComputePipelineCreateInfo = ComputePipelineCreateInfo
  { -- No documentation found for Nested "ComputePipelineCreateInfo" "pNext"
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

instance Zero ComputePipelineCreateInfo where
  zero = ComputePipelineCreateInfo Nothing
                                   zero
                                   zero
                                   zero
                                   zero
                                   zero

#endif

-- No documentation found for TopLevel "CullModeFlagBits"
type CullModeFlagBits = VkCullModeFlagBits


{-# complete CULL_MODE_FRONT_BIT, CULL_MODE_BACK_BIT, CULL_MODE_NONE, CULL_MODE_FRONT_AND_BACK :: CullModeFlagBits #-}


-- No documentation found for Nested "CullModeFlagBits" "CULL_MODE_FRONT_BIT"
pattern CULL_MODE_FRONT_BIT :: (a ~ CullModeFlagBits) => a
pattern CULL_MODE_FRONT_BIT = VK_CULL_MODE_FRONT_BIT


-- No documentation found for Nested "CullModeFlagBits" "CULL_MODE_BACK_BIT"
pattern CULL_MODE_BACK_BIT :: (a ~ CullModeFlagBits) => a
pattern CULL_MODE_BACK_BIT = VK_CULL_MODE_BACK_BIT


-- No documentation found for Nested "CullModeFlagBits" "CULL_MODE_NONE"
pattern CULL_MODE_NONE :: (a ~ CullModeFlagBits) => a
pattern CULL_MODE_NONE = VK_CULL_MODE_NONE


-- No documentation found for Nested "CullModeFlagBits" "CULL_MODE_FRONT_AND_BACK"
pattern CULL_MODE_FRONT_AND_BACK :: (a ~ CullModeFlagBits) => a
pattern CULL_MODE_FRONT_AND_BACK = VK_CULL_MODE_FRONT_AND_BACK

-- No documentation found for TopLevel "CullModeFlags"
type CullModeFlags = CullModeFlagBits

-- No documentation found for TopLevel "DynamicState"
type DynamicState = VkDynamicState


{-# complete DYNAMIC_STATE_VIEWPORT, DYNAMIC_STATE_SCISSOR, DYNAMIC_STATE_LINE_WIDTH, DYNAMIC_STATE_DEPTH_BIAS, DYNAMIC_STATE_BLEND_CONSTANTS, DYNAMIC_STATE_DEPTH_BOUNDS, DYNAMIC_STATE_STENCIL_COMPARE_MASK, DYNAMIC_STATE_STENCIL_WRITE_MASK, DYNAMIC_STATE_STENCIL_REFERENCE, DYNAMIC_STATE_VIEWPORT_W_SCALING_NV, DYNAMIC_STATE_DISCARD_RECTANGLE_EXT, DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT, DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV, DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV, DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV :: DynamicState #-}


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_VIEWPORT"
pattern DYNAMIC_STATE_VIEWPORT :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_VIEWPORT = VK_DYNAMIC_STATE_VIEWPORT


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_SCISSOR"
pattern DYNAMIC_STATE_SCISSOR :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_SCISSOR = VK_DYNAMIC_STATE_SCISSOR


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_LINE_WIDTH"
pattern DYNAMIC_STATE_LINE_WIDTH :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_LINE_WIDTH = VK_DYNAMIC_STATE_LINE_WIDTH


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_DEPTH_BIAS"
pattern DYNAMIC_STATE_DEPTH_BIAS :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_DEPTH_BIAS = VK_DYNAMIC_STATE_DEPTH_BIAS


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_BLEND_CONSTANTS"
pattern DYNAMIC_STATE_BLEND_CONSTANTS :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_BLEND_CONSTANTS = VK_DYNAMIC_STATE_BLEND_CONSTANTS


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_DEPTH_BOUNDS"
pattern DYNAMIC_STATE_DEPTH_BOUNDS :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_DEPTH_BOUNDS = VK_DYNAMIC_STATE_DEPTH_BOUNDS


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_STENCIL_COMPARE_MASK"
pattern DYNAMIC_STATE_STENCIL_COMPARE_MASK :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_STENCIL_COMPARE_MASK = VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_STENCIL_WRITE_MASK"
pattern DYNAMIC_STATE_STENCIL_WRITE_MASK :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_STENCIL_WRITE_MASK = VK_DYNAMIC_STATE_STENCIL_WRITE_MASK


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_STENCIL_REFERENCE"
pattern DYNAMIC_STATE_STENCIL_REFERENCE :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_STENCIL_REFERENCE = VK_DYNAMIC_STATE_STENCIL_REFERENCE


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_VIEWPORT_W_SCALING_NV"
pattern DYNAMIC_STATE_VIEWPORT_W_SCALING_NV :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_VIEWPORT_W_SCALING_NV = VK_DYNAMIC_STATE_VIEWPORT_W_SCALING_NV


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_DISCARD_RECTANGLE_EXT"
pattern DYNAMIC_STATE_DISCARD_RECTANGLE_EXT :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_DISCARD_RECTANGLE_EXT = VK_DYNAMIC_STATE_DISCARD_RECTANGLE_EXT


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT"
pattern DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT = VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV"
pattern DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV = VK_DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV"
pattern DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV = VK_DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV


-- No documentation found for Nested "DynamicState" "DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV"
pattern DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV :: (a ~ DynamicState) => a
pattern DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV = VK_DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV


-- No documentation found for TopLevel "VkExtent2D"
data Extent2D = Extent2D
  { -- No documentation found for Nested "Extent2D" "width"
  width :: Word32
  , -- No documentation found for Nested "Extent2D" "height"
  height :: Word32
  }
  deriving (Show, Eq)

instance Zero Extent2D where
  zero = Extent2D zero
                  zero


-- No documentation found for TopLevel "FrontFace"
type FrontFace = VkFrontFace


{-# complete FRONT_FACE_COUNTER_CLOCKWISE, FRONT_FACE_CLOCKWISE :: FrontFace #-}


-- No documentation found for Nested "FrontFace" "FRONT_FACE_COUNTER_CLOCKWISE"
pattern FRONT_FACE_COUNTER_CLOCKWISE :: (a ~ FrontFace) => a
pattern FRONT_FACE_COUNTER_CLOCKWISE = VK_FRONT_FACE_COUNTER_CLOCKWISE


-- No documentation found for Nested "FrontFace" "FRONT_FACE_CLOCKWISE"
pattern FRONT_FACE_CLOCKWISE :: (a ~ FrontFace) => a
pattern FRONT_FACE_CLOCKWISE = VK_FRONT_FACE_CLOCKWISE


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkGraphicsPipelineCreateInfo"
data GraphicsPipelineCreateInfo = GraphicsPipelineCreateInfo
  { -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "flags"
  flags :: PipelineCreateFlags
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

instance Zero GraphicsPipelineCreateInfo where
  zero = GraphicsPipelineCreateInfo Nothing
                                    zero
                                    mempty
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

#endif

-- No documentation found for TopLevel "LogicOp"
type LogicOp = VkLogicOp


{-# complete LOGIC_OP_CLEAR, LOGIC_OP_AND, LOGIC_OP_AND_REVERSE, LOGIC_OP_COPY, LOGIC_OP_AND_INVERTED, LOGIC_OP_NO_OP, LOGIC_OP_XOR, LOGIC_OP_OR, LOGIC_OP_NOR, LOGIC_OP_EQUIVALENT, LOGIC_OP_INVERT, LOGIC_OP_OR_REVERSE, LOGIC_OP_COPY_INVERTED, LOGIC_OP_OR_INVERTED, LOGIC_OP_NAND, LOGIC_OP_SET :: LogicOp #-}


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


-- No documentation found for TopLevel "VkOffset2D"
data Offset2D = Offset2D
  { -- No documentation found for Nested "Offset2D" "x"
  x :: Int32
  , -- No documentation found for Nested "Offset2D" "y"
  y :: Int32
  }
  deriving (Show, Eq)

instance Zero Offset2D where
  zero = Offset2D zero
                  zero


-- No documentation found for TopLevel "Pipeline"
type Pipeline = VkPipeline


-- No documentation found for TopLevel "VkPipelineColorBlendAttachmentState"
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

instance Zero PipelineColorBlendAttachmentState where
  zero = PipelineColorBlendAttachmentState False
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero


-- No documentation found for TopLevel "PipelineColorBlendStateCreateFlags"
type PipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags


-- No complete pragma for PipelineColorBlendStateCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineColorBlendStateCreateInfo"
data PipelineColorBlendStateCreateInfo = PipelineColorBlendStateCreateInfo
  { -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "flags"
  flags :: PipelineColorBlendStateCreateFlags
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "logicOpEnable"
  logicOpEnable :: Bool
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "logicOp"
  logicOp :: LogicOp
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "pAttachments"
  attachments :: Vector PipelineColorBlendAttachmentState
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "blendConstants"
  blendConstants :: (Float, Float, Float, Float)
  }
  deriving (Show, Eq)

instance Zero PipelineColorBlendStateCreateInfo where
  zero = PipelineColorBlendStateCreateInfo Nothing
                                           zero
                                           False
                                           zero
                                           mempty
                                           (zero, zero, zero, zero)

#endif

-- No documentation found for TopLevel "PipelineCreateFlagBits"
type PipelineCreateFlagBits = VkPipelineCreateFlagBits


{-# complete PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT, PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT, PIPELINE_CREATE_DERIVATIVE_BIT, PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT, PIPELINE_CREATE_DISPATCH_BASE, PIPELINE_CREATE_DEFER_COMPILE_BIT_NV :: PipelineCreateFlagBits #-}


-- No documentation found for Nested "PipelineCreateFlagBits" "PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT"
pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT :: (a ~ PipelineCreateFlagBits) => a
pattern PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT = VK_PIPELINE_CREATE_DISABLE_OPTIMIZATION_BIT


-- No documentation found for Nested "PipelineCreateFlagBits" "PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT"
pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT :: (a ~ PipelineCreateFlagBits) => a
pattern PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT = VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT


-- No documentation found for Nested "PipelineCreateFlagBits" "PIPELINE_CREATE_DERIVATIVE_BIT"
pattern PIPELINE_CREATE_DERIVATIVE_BIT :: (a ~ PipelineCreateFlagBits) => a
pattern PIPELINE_CREATE_DERIVATIVE_BIT = VK_PIPELINE_CREATE_DERIVATIVE_BIT


-- No documentation found for Nested "PipelineCreateFlagBits" "PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT"
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT :: (a ~ PipelineCreateFlagBits) => a
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT = VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT


-- No documentation found for Nested "PipelineCreateFlagBits" "PIPELINE_CREATE_DISPATCH_BASE"
pattern PIPELINE_CREATE_DISPATCH_BASE :: (a ~ PipelineCreateFlagBits) => a
pattern PIPELINE_CREATE_DISPATCH_BASE = VK_PIPELINE_CREATE_DISPATCH_BASE


-- No documentation found for Nested "PipelineCreateFlagBits" "PIPELINE_CREATE_DEFER_COMPILE_BIT_NV"
pattern PIPELINE_CREATE_DEFER_COMPILE_BIT_NV :: (a ~ PipelineCreateFlagBits) => a
pattern PIPELINE_CREATE_DEFER_COMPILE_BIT_NV = VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV

-- No documentation found for TopLevel "PipelineCreateFlags"
type PipelineCreateFlags = PipelineCreateFlagBits

-- No documentation found for TopLevel "PipelineDepthStencilStateCreateFlags"
type PipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags


-- No complete pragma for PipelineDepthStencilStateCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineDepthStencilStateCreateInfo"
data PipelineDepthStencilStateCreateInfo = PipelineDepthStencilStateCreateInfo
  { -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "pNext"
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
  minDepthBounds :: Float
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "maxDepthBounds"
  maxDepthBounds :: Float
  }
  deriving (Show, Eq)

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

#endif

-- No documentation found for TopLevel "PipelineDynamicStateCreateFlags"
type PipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags


-- No complete pragma for PipelineDynamicStateCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineDynamicStateCreateInfo"
data PipelineDynamicStateCreateInfo = PipelineDynamicStateCreateInfo
  { -- No documentation found for Nested "PipelineDynamicStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineDynamicStateCreateInfo" "flags"
  flags :: PipelineDynamicStateCreateFlags
  , -- No documentation found for Nested "PipelineDynamicStateCreateInfo" "pDynamicStates"
  dynamicStates :: Vector DynamicState
  }
  deriving (Show, Eq)

instance Zero PipelineDynamicStateCreateInfo where
  zero = PipelineDynamicStateCreateInfo Nothing
                                        zero
                                        mempty

#endif

-- No documentation found for TopLevel "PipelineInputAssemblyStateCreateFlags"
type PipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags


-- No complete pragma for PipelineInputAssemblyStateCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineInputAssemblyStateCreateInfo"
data PipelineInputAssemblyStateCreateInfo = PipelineInputAssemblyStateCreateInfo
  { -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "flags"
  flags :: PipelineInputAssemblyStateCreateFlags
  , -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "topology"
  topology :: PrimitiveTopology
  , -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "primitiveRestartEnable"
  primitiveRestartEnable :: Bool
  }
  deriving (Show, Eq)

instance Zero PipelineInputAssemblyStateCreateInfo where
  zero = PipelineInputAssemblyStateCreateInfo Nothing
                                              zero
                                              zero
                                              False

#endif

-- No documentation found for TopLevel "PipelineLayout"
type PipelineLayout = VkPipelineLayout

-- No documentation found for TopLevel "PipelineMultisampleStateCreateFlags"
type PipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags


-- No complete pragma for PipelineMultisampleStateCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineMultisampleStateCreateInfo"
data PipelineMultisampleStateCreateInfo = PipelineMultisampleStateCreateInfo
  { -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "flags"
  flags :: PipelineMultisampleStateCreateFlags
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "rasterizationSamples"
  rasterizationSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "sampleShadingEnable"
  sampleShadingEnable :: Bool
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "minSampleShading"
  minSampleShading :: Float
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "pSampleMask"
  sampleMask :: Either Word32 (Vector SampleMask)
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "alphaToCoverageEnable"
  alphaToCoverageEnable :: Bool
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "alphaToOneEnable"
  alphaToOneEnable :: Bool
  }
  deriving (Show, Eq)

instance Zero PipelineMultisampleStateCreateInfo where
  zero = PipelineMultisampleStateCreateInfo Nothing
                                            zero
                                            zero
                                            False
                                            zero
                                            (Left 0)
                                            False
                                            False

#endif

-- No documentation found for TopLevel "PipelineRasterizationStateCreateFlags"
type PipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags


-- No complete pragma for PipelineRasterizationStateCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineRasterizationStateCreateInfo"
data PipelineRasterizationStateCreateInfo = PipelineRasterizationStateCreateInfo
  { -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "pNext"
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
  depthBiasConstantFactor :: Float
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasClamp"
  depthBiasClamp :: Float
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasSlopeFactor"
  depthBiasSlopeFactor :: Float
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "lineWidth"
  lineWidth :: Float
  }
  deriving (Show, Eq)

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

#endif

-- No documentation found for TopLevel "PipelineShaderStageCreateFlags"
type PipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags


-- No complete pragma for PipelineShaderStageCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineShaderStageCreateInfo"
data PipelineShaderStageCreateInfo = PipelineShaderStageCreateInfo
  { -- No documentation found for Nested "PipelineShaderStageCreateInfo" "pNext"
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

instance Zero PipelineShaderStageCreateInfo where
  zero = PipelineShaderStageCreateInfo Nothing
                                       zero
                                       zero
                                       zero
                                       mempty
                                       Nothing

#endif

-- No documentation found for TopLevel "PipelineTessellationStateCreateFlags"
type PipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags


-- No complete pragma for PipelineTessellationStateCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineTessellationStateCreateInfo"
data PipelineTessellationStateCreateInfo = PipelineTessellationStateCreateInfo
  { -- No documentation found for Nested "PipelineTessellationStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineTessellationStateCreateInfo" "flags"
  flags :: PipelineTessellationStateCreateFlags
  , -- No documentation found for Nested "PipelineTessellationStateCreateInfo" "patchControlPoints"
  patchControlPoints :: Word32
  }
  deriving (Show, Eq)

instance Zero PipelineTessellationStateCreateInfo where
  zero = PipelineTessellationStateCreateInfo Nothing
                                             zero
                                             zero

#endif

-- No documentation found for TopLevel "PipelineVertexInputStateCreateFlags"
type PipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags


-- No complete pragma for PipelineVertexInputStateCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineVertexInputStateCreateInfo"
data PipelineVertexInputStateCreateInfo = PipelineVertexInputStateCreateInfo
  { -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "flags"
  flags :: PipelineVertexInputStateCreateFlags
  , -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "pVertexBindingDescriptions"
  vertexBindingDescriptions :: Vector VertexInputBindingDescription
  , -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "pVertexAttributeDescriptions"
  vertexAttributeDescriptions :: Vector VertexInputAttributeDescription
  }
  deriving (Show, Eq)

instance Zero PipelineVertexInputStateCreateInfo where
  zero = PipelineVertexInputStateCreateInfo Nothing
                                            zero
                                            mempty
                                            mempty

#endif

-- No documentation found for TopLevel "PipelineViewportStateCreateFlags"
type PipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags


-- No complete pragma for PipelineViewportStateCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineViewportStateCreateInfo"
data PipelineViewportStateCreateInfo = PipelineViewportStateCreateInfo
  { -- No documentation found for Nested "PipelineViewportStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportStateCreateInfo" "flags"
  flags :: PipelineViewportStateCreateFlags
  , -- No documentation found for Nested "PipelineViewportStateCreateInfo" "pViewports"
  viewports :: Either Word32 (Vector Viewport)
  , -- No documentation found for Nested "PipelineViewportStateCreateInfo" "pScissors"
  scissors :: Either Word32 (Vector Rect2D)
  }
  deriving (Show, Eq)

instance Zero PipelineViewportStateCreateInfo where
  zero = PipelineViewportStateCreateInfo Nothing
                                         zero
                                         (Left 0)
                                         (Left 0)

#endif

-- No documentation found for TopLevel "PolygonMode"
type PolygonMode = VkPolygonMode


{-# complete POLYGON_MODE_FILL, POLYGON_MODE_LINE, POLYGON_MODE_POINT, POLYGON_MODE_FILL_RECTANGLE_NV :: PolygonMode #-}


-- No documentation found for Nested "PolygonMode" "POLYGON_MODE_FILL"
pattern POLYGON_MODE_FILL :: (a ~ PolygonMode) => a
pattern POLYGON_MODE_FILL = VK_POLYGON_MODE_FILL


-- No documentation found for Nested "PolygonMode" "POLYGON_MODE_LINE"
pattern POLYGON_MODE_LINE :: (a ~ PolygonMode) => a
pattern POLYGON_MODE_LINE = VK_POLYGON_MODE_LINE


-- No documentation found for Nested "PolygonMode" "POLYGON_MODE_POINT"
pattern POLYGON_MODE_POINT :: (a ~ PolygonMode) => a
pattern POLYGON_MODE_POINT = VK_POLYGON_MODE_POINT


-- No documentation found for Nested "PolygonMode" "POLYGON_MODE_FILL_RECTANGLE_NV"
pattern POLYGON_MODE_FILL_RECTANGLE_NV :: (a ~ PolygonMode) => a
pattern POLYGON_MODE_FILL_RECTANGLE_NV = VK_POLYGON_MODE_FILL_RECTANGLE_NV

-- No documentation found for TopLevel "PrimitiveTopology"
type PrimitiveTopology = VkPrimitiveTopology


{-# complete PRIMITIVE_TOPOLOGY_POINT_LIST, PRIMITIVE_TOPOLOGY_LINE_LIST, PRIMITIVE_TOPOLOGY_LINE_STRIP, PRIMITIVE_TOPOLOGY_TRIANGLE_LIST, PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP, PRIMITIVE_TOPOLOGY_TRIANGLE_FAN, PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY, PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY, PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY, PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY, PRIMITIVE_TOPOLOGY_PATCH_LIST :: PrimitiveTopology #-}


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_POINT_LIST"
pattern PRIMITIVE_TOPOLOGY_POINT_LIST :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_POINT_LIST = VK_PRIMITIVE_TOPOLOGY_POINT_LIST


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_LINE_LIST"
pattern PRIMITIVE_TOPOLOGY_LINE_LIST :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_LINE_LIST = VK_PRIMITIVE_TOPOLOGY_LINE_LIST


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_LINE_STRIP"
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP = VK_PRIMITIVE_TOPOLOGY_LINE_STRIP


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_TRIANGLE_LIST"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_TRIANGLE_FAN"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_FAN :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_FAN = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY"
pattern PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY = VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY"
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY = VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY


-- No documentation found for Nested "PrimitiveTopology" "PRIMITIVE_TOPOLOGY_PATCH_LIST"
pattern PRIMITIVE_TOPOLOGY_PATCH_LIST :: (a ~ PrimitiveTopology) => a
pattern PRIMITIVE_TOPOLOGY_PATCH_LIST = VK_PRIMITIVE_TOPOLOGY_PATCH_LIST


-- No documentation found for TopLevel "VkRect2D"
data Rect2D = Rect2D
  { -- No documentation found for Nested "Rect2D" "offset"
  offset :: Offset2D
  , -- No documentation found for Nested "Rect2D" "extent"
  extent :: Extent2D
  }
  deriving (Show, Eq)

instance Zero Rect2D where
  zero = Rect2D zero
                zero


-- No documentation found for TopLevel "RenderPass"
type RenderPass = VkRenderPass

-- No documentation found for TopLevel "SampleMask"
type SampleMask = VkSampleMask
  

-- No documentation found for TopLevel "ShaderStageFlagBits"
type ShaderStageFlagBits = VkShaderStageFlagBits


{-# complete SHADER_STAGE_VERTEX_BIT, SHADER_STAGE_TESSELLATION_CONTROL_BIT, SHADER_STAGE_TESSELLATION_EVALUATION_BIT, SHADER_STAGE_GEOMETRY_BIT, SHADER_STAGE_FRAGMENT_BIT, SHADER_STAGE_COMPUTE_BIT, SHADER_STAGE_ALL_GRAPHICS, SHADER_STAGE_ALL, SHADER_STAGE_RAYGEN_BIT_NV, SHADER_STAGE_ANY_HIT_BIT_NV, SHADER_STAGE_CLOSEST_HIT_BIT_NV, SHADER_STAGE_MISS_BIT_NV, SHADER_STAGE_INTERSECTION_BIT_NV, SHADER_STAGE_CALLABLE_BIT_NV, SHADER_STAGE_TASK_BIT_NV, SHADER_STAGE_MESH_BIT_NV :: ShaderStageFlagBits #-}


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_VERTEX_BIT"
pattern SHADER_STAGE_VERTEX_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_VERTEX_BIT = VK_SHADER_STAGE_VERTEX_BIT


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_TESSELLATION_CONTROL_BIT"
pattern SHADER_STAGE_TESSELLATION_CONTROL_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_TESSELLATION_CONTROL_BIT = VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_TESSELLATION_EVALUATION_BIT"
pattern SHADER_STAGE_TESSELLATION_EVALUATION_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_TESSELLATION_EVALUATION_BIT = VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_GEOMETRY_BIT"
pattern SHADER_STAGE_GEOMETRY_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_GEOMETRY_BIT = VK_SHADER_STAGE_GEOMETRY_BIT


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_FRAGMENT_BIT"
pattern SHADER_STAGE_FRAGMENT_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_FRAGMENT_BIT = VK_SHADER_STAGE_FRAGMENT_BIT


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_COMPUTE_BIT"
pattern SHADER_STAGE_COMPUTE_BIT :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_COMPUTE_BIT = VK_SHADER_STAGE_COMPUTE_BIT


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_ALL_GRAPHICS"
pattern SHADER_STAGE_ALL_GRAPHICS :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_ALL_GRAPHICS = VK_SHADER_STAGE_ALL_GRAPHICS


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_ALL"
pattern SHADER_STAGE_ALL :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_ALL = VK_SHADER_STAGE_ALL


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_RAYGEN_BIT_NV"
pattern SHADER_STAGE_RAYGEN_BIT_NV :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_RAYGEN_BIT_NV = VK_SHADER_STAGE_RAYGEN_BIT_NV


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_ANY_HIT_BIT_NV"
pattern SHADER_STAGE_ANY_HIT_BIT_NV :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_ANY_HIT_BIT_NV = VK_SHADER_STAGE_ANY_HIT_BIT_NV


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_CLOSEST_HIT_BIT_NV"
pattern SHADER_STAGE_CLOSEST_HIT_BIT_NV :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_CLOSEST_HIT_BIT_NV = VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_MISS_BIT_NV"
pattern SHADER_STAGE_MISS_BIT_NV :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_MISS_BIT_NV = VK_SHADER_STAGE_MISS_BIT_NV


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_INTERSECTION_BIT_NV"
pattern SHADER_STAGE_INTERSECTION_BIT_NV :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_INTERSECTION_BIT_NV = VK_SHADER_STAGE_INTERSECTION_BIT_NV


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_CALLABLE_BIT_NV"
pattern SHADER_STAGE_CALLABLE_BIT_NV :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_CALLABLE_BIT_NV = VK_SHADER_STAGE_CALLABLE_BIT_NV


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_TASK_BIT_NV"
pattern SHADER_STAGE_TASK_BIT_NV :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_TASK_BIT_NV = VK_SHADER_STAGE_TASK_BIT_NV


-- No documentation found for Nested "ShaderStageFlagBits" "SHADER_STAGE_MESH_BIT_NV"
pattern SHADER_STAGE_MESH_BIT_NV :: (a ~ ShaderStageFlagBits) => a
pattern SHADER_STAGE_MESH_BIT_NV = VK_SHADER_STAGE_MESH_BIT_NV


-- No documentation found for TopLevel "VkSpecializationInfo"
data SpecializationInfo = SpecializationInfo
  { -- No documentation found for Nested "SpecializationInfo" "pMapEntries"
  mapEntries :: Vector SpecializationMapEntry
  , -- No documentation found for Nested "SpecializationInfo" "dataSize"
  dataSize :: CSize
  , -- No documentation found for Nested "SpecializationInfo" "pData"
  data' :: Ptr ()
  }
  deriving (Show, Eq)

instance Zero SpecializationInfo where
  zero = SpecializationInfo mempty
                            zero
                            nullPtr



-- No documentation found for TopLevel "VkSpecializationMapEntry"
data SpecializationMapEntry = SpecializationMapEntry
  { -- No documentation found for Nested "SpecializationMapEntry" "constantID"
  constantID :: Word32
  , -- No documentation found for Nested "SpecializationMapEntry" "offset"
  offset :: Word32
  , -- No documentation found for Nested "SpecializationMapEntry" "size"
  size :: CSize
  }
  deriving (Show, Eq)

instance Zero SpecializationMapEntry where
  zero = SpecializationMapEntry zero
                                zero
                                zero


-- No documentation found for TopLevel "StencilOp"
type StencilOp = VkStencilOp


{-# complete STENCIL_OP_KEEP, STENCIL_OP_ZERO, STENCIL_OP_REPLACE, STENCIL_OP_INCREMENT_AND_CLAMP, STENCIL_OP_DECREMENT_AND_CLAMP, STENCIL_OP_INVERT, STENCIL_OP_INCREMENT_AND_WRAP, STENCIL_OP_DECREMENT_AND_WRAP :: StencilOp #-}


-- No documentation found for Nested "StencilOp" "STENCIL_OP_KEEP"
pattern STENCIL_OP_KEEP :: (a ~ StencilOp) => a
pattern STENCIL_OP_KEEP = VK_STENCIL_OP_KEEP


-- No documentation found for Nested "StencilOp" "STENCIL_OP_ZERO"
pattern STENCIL_OP_ZERO :: (a ~ StencilOp) => a
pattern STENCIL_OP_ZERO = VK_STENCIL_OP_ZERO


-- No documentation found for Nested "StencilOp" "STENCIL_OP_REPLACE"
pattern STENCIL_OP_REPLACE :: (a ~ StencilOp) => a
pattern STENCIL_OP_REPLACE = VK_STENCIL_OP_REPLACE


-- No documentation found for Nested "StencilOp" "STENCIL_OP_INCREMENT_AND_CLAMP"
pattern STENCIL_OP_INCREMENT_AND_CLAMP :: (a ~ StencilOp) => a
pattern STENCIL_OP_INCREMENT_AND_CLAMP = VK_STENCIL_OP_INCREMENT_AND_CLAMP


-- No documentation found for Nested "StencilOp" "STENCIL_OP_DECREMENT_AND_CLAMP"
pattern STENCIL_OP_DECREMENT_AND_CLAMP :: (a ~ StencilOp) => a
pattern STENCIL_OP_DECREMENT_AND_CLAMP = VK_STENCIL_OP_DECREMENT_AND_CLAMP


-- No documentation found for Nested "StencilOp" "STENCIL_OP_INVERT"
pattern STENCIL_OP_INVERT :: (a ~ StencilOp) => a
pattern STENCIL_OP_INVERT = VK_STENCIL_OP_INVERT


-- No documentation found for Nested "StencilOp" "STENCIL_OP_INCREMENT_AND_WRAP"
pattern STENCIL_OP_INCREMENT_AND_WRAP :: (a ~ StencilOp) => a
pattern STENCIL_OP_INCREMENT_AND_WRAP = VK_STENCIL_OP_INCREMENT_AND_WRAP


-- No documentation found for Nested "StencilOp" "STENCIL_OP_DECREMENT_AND_WRAP"
pattern STENCIL_OP_DECREMENT_AND_WRAP :: (a ~ StencilOp) => a
pattern STENCIL_OP_DECREMENT_AND_WRAP = VK_STENCIL_OP_DECREMENT_AND_WRAP


-- No documentation found for TopLevel "VkStencilOpState"
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

instance Zero StencilOpState where
  zero = StencilOpState zero
                        zero
                        zero
                        zero
                        zero
                        zero
                        zero



-- No documentation found for TopLevel "VkVertexInputAttributeDescription"
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

instance Zero VertexInputAttributeDescription where
  zero = VertexInputAttributeDescription zero
                                         zero
                                         zero
                                         zero



-- No documentation found for TopLevel "VkVertexInputBindingDescription"
data VertexInputBindingDescription = VertexInputBindingDescription
  { -- No documentation found for Nested "VertexInputBindingDescription" "binding"
  binding :: Word32
  , -- No documentation found for Nested "VertexInputBindingDescription" "stride"
  stride :: Word32
  , -- No documentation found for Nested "VertexInputBindingDescription" "inputRate"
  inputRate :: VertexInputRate
  }
  deriving (Show, Eq)

instance Zero VertexInputBindingDescription where
  zero = VertexInputBindingDescription zero
                                       zero
                                       zero


-- No documentation found for TopLevel "VertexInputRate"
type VertexInputRate = VkVertexInputRate


{-# complete VERTEX_INPUT_RATE_VERTEX, VERTEX_INPUT_RATE_INSTANCE :: VertexInputRate #-}


-- No documentation found for Nested "VertexInputRate" "VERTEX_INPUT_RATE_VERTEX"
pattern VERTEX_INPUT_RATE_VERTEX :: (a ~ VertexInputRate) => a
pattern VERTEX_INPUT_RATE_VERTEX = VK_VERTEX_INPUT_RATE_VERTEX


-- No documentation found for Nested "VertexInputRate" "VERTEX_INPUT_RATE_INSTANCE"
pattern VERTEX_INPUT_RATE_INSTANCE :: (a ~ VertexInputRate) => a
pattern VERTEX_INPUT_RATE_INSTANCE = VK_VERTEX_INPUT_RATE_INSTANCE


-- No documentation found for TopLevel "VkViewport"
data Viewport = Viewport
  { -- No documentation found for Nested "Viewport" "x"
  x :: Float
  , -- No documentation found for Nested "Viewport" "y"
  y :: Float
  , -- No documentation found for Nested "Viewport" "width"
  width :: Float
  , -- No documentation found for Nested "Viewport" "height"
  height :: Float
  , -- No documentation found for Nested "Viewport" "minDepth"
  minDepth :: Float
  , -- No documentation found for Nested "Viewport" "maxDepth"
  maxDepth :: Float
  }
  deriving (Show, Eq)

instance Zero Viewport where
  zero = Viewport zero
                  zero
                  zero
                  zero
                  zero
                  zero



-- No documentation found for TopLevel "vkCreateComputePipelines"
createComputePipelines :: Device ->  PipelineCache ->  Vector ComputePipelineCreateInfo ->  Maybe AllocationCallbacks ->  IO (Vector Pipeline)
createComputePipelines = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateGraphicsPipelines"
createGraphicsPipelines :: Device ->  PipelineCache ->  Vector GraphicsPipelineCreateInfo ->  Maybe AllocationCallbacks ->  IO (Vector Pipeline)
createGraphicsPipelines = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyPipeline"
destroyPipeline :: Device ->  Pipeline ->  Maybe AllocationCallbacks ->  IO ()
destroyPipeline = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createComputePipelines' and 'destroyPipeline' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withComputePipelines
  :: Device -> PipelineCache -> Vector ComputePipelineCreateInfo -> Maybe AllocationCallbacks -> (Vector Pipeline -> IO a) -> IO a
withComputePipelines device pipelineCache computePipelineCreateInfo allocationCallbacks = bracket
  (createComputePipelines device pipelineCache computePipelineCreateInfo allocationCallbacks)
  (traverse (\o -> destroyPipeline device o allocationCallbacks))

-- | A safe wrapper for 'createGraphicsPipelines' and 'destroyPipeline' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withGraphicsPipelines
  :: Device -> PipelineCache -> Vector GraphicsPipelineCreateInfo -> Maybe AllocationCallbacks -> (Vector Pipeline -> IO a) -> IO a
withGraphicsPipelines device pipelineCache graphicsPipelineCreateInfo allocationCallbacks = bracket
  (createGraphicsPipelines device pipelineCache graphicsPipelineCreateInfo allocationCallbacks)
  (traverse (\o -> destroyPipeline device o allocationCallbacks))
