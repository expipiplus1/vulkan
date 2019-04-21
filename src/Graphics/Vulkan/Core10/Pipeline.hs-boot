{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Pipeline
  ( BlendFactor
  , BlendOp
  , ColorComponentFlagBits
  , ColorComponentFlags
  , CompareOp
  , CullModeFlagBits
  , CullModeFlags
  , DynamicState
  , FrontFace
  , LogicOp
  , Pipeline
  , PipelineColorBlendStateCreateFlags
  , PipelineCreateFlagBits
  , PipelineCreateFlags
  , PipelineDepthStencilStateCreateFlags
  , PipelineDynamicStateCreateFlags
  , PipelineInputAssemblyStateCreateFlags
  , PipelineLayout
  , PipelineMultisampleStateCreateFlags
  , PipelineRasterizationStateCreateFlags
  , PipelineShaderStageCreateFlags
  , PipelineTessellationStateCreateFlags
  , PipelineVertexInputStateCreateFlags
  , PipelineViewportStateCreateFlags
  , PolygonMode
  , PrimitiveTopology
  , RenderPass
  , SampleMask
  , ShaderStageFlagBits
  , StencilOp
  , VertexInputRate
  ) where




import Graphics.Vulkan.C.Core10.Pipeline
  ( VkSampleMask
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkBlendFactor
  , VkBlendOp
  , VkColorComponentFlagBits
  , VkCompareOp
  , VkCullModeFlagBits
  , VkDynamicState
  , VkFrontFace
  , VkLogicOp
  , VkPipelineColorBlendStateCreateFlags
  , VkPipelineCreateFlagBits
  , VkPipelineDepthStencilStateCreateFlags
  , VkPipelineDynamicStateCreateFlags
  , VkPipelineInputAssemblyStateCreateFlags
  , VkPipelineMultisampleStateCreateFlags
  , VkPipelineRasterizationStateCreateFlags
  , VkPipelineShaderStageCreateFlags
  , VkPipelineTessellationStateCreateFlags
  , VkPipelineVertexInputStateCreateFlags
  , VkPipelineViewportStateCreateFlags
  , VkPolygonMode
  , VkPrimitiveTopology
  , VkShaderStageFlagBits
  , VkStencilOp
  , VkVertexInputRate
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
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

-- | VkVertexInputRate - Specify rate at which vertex attributes are pulled
-- from buffers
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputBindingDescription'
type VertexInputRate = VkVertexInputRate
