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
  , VkPipeline
  , VkPipelineColorBlendStateCreateFlags
  , VkPipelineCreateFlagBits
  , VkPipelineDepthStencilStateCreateFlags
  , VkPipelineDynamicStateCreateFlags
  , VkPipelineInputAssemblyStateCreateFlags
  , VkPipelineLayout
  , VkPipelineMultisampleStateCreateFlags
  , VkPipelineRasterizationStateCreateFlags
  , VkPipelineShaderStageCreateFlags
  , VkPipelineTessellationStateCreateFlags
  , VkPipelineVertexInputStateCreateFlags
  , VkPipelineViewportStateCreateFlags
  , VkPolygonMode
  , VkPrimitiveTopology
  , VkRenderPass
  , VkShaderStageFlagBits
  , VkStencilOp
  , VkVertexInputRate
  )


-- No documentation found for TopLevel "BlendFactor"
type BlendFactor = VkBlendFactor

-- No documentation found for TopLevel "BlendOp"
type BlendOp = VkBlendOp

-- No documentation found for TopLevel "ColorComponentFlagBits"
type ColorComponentFlagBits = VkColorComponentFlagBits

-- No documentation found for TopLevel "ColorComponentFlags"
type ColorComponentFlags = ColorComponentFlagBits

-- No documentation found for TopLevel "CompareOp"
type CompareOp = VkCompareOp

-- No documentation found for TopLevel "CullModeFlagBits"
type CullModeFlagBits = VkCullModeFlagBits

-- No documentation found for TopLevel "CullModeFlags"
type CullModeFlags = CullModeFlagBits

-- No documentation found for TopLevel "DynamicState"
type DynamicState = VkDynamicState

-- No documentation found for TopLevel "FrontFace"
type FrontFace = VkFrontFace

-- No documentation found for TopLevel "LogicOp"
type LogicOp = VkLogicOp

-- No documentation found for TopLevel "Pipeline"
type Pipeline = VkPipeline

-- No documentation found for TopLevel "PipelineColorBlendStateCreateFlags"
type PipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags

-- No documentation found for TopLevel "PipelineCreateFlagBits"
type PipelineCreateFlagBits = VkPipelineCreateFlagBits

-- No documentation found for TopLevel "PipelineCreateFlags"
type PipelineCreateFlags = PipelineCreateFlagBits

-- No documentation found for TopLevel "PipelineDepthStencilStateCreateFlags"
type PipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags

-- No documentation found for TopLevel "PipelineDynamicStateCreateFlags"
type PipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags

-- No documentation found for TopLevel "PipelineInputAssemblyStateCreateFlags"
type PipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags

-- No documentation found for TopLevel "PipelineLayout"
type PipelineLayout = VkPipelineLayout

-- No documentation found for TopLevel "PipelineMultisampleStateCreateFlags"
type PipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags

-- No documentation found for TopLevel "PipelineRasterizationStateCreateFlags"
type PipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags

-- No documentation found for TopLevel "PipelineShaderStageCreateFlags"
type PipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags

-- No documentation found for TopLevel "PipelineTessellationStateCreateFlags"
type PipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags

-- No documentation found for TopLevel "PipelineVertexInputStateCreateFlags"
type PipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags

-- No documentation found for TopLevel "PipelineViewportStateCreateFlags"
type PipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags

-- No documentation found for TopLevel "PolygonMode"
type PolygonMode = VkPolygonMode

-- No documentation found for TopLevel "PrimitiveTopology"
type PrimitiveTopology = VkPrimitiveTopology

-- No documentation found for TopLevel "RenderPass"
type RenderPass = VkRenderPass

-- No documentation found for TopLevel "SampleMask"
type SampleMask = VkSampleMask
  

-- No documentation found for TopLevel "ShaderStageFlagBits"
type ShaderStageFlagBits = VkShaderStageFlagBits

-- No documentation found for TopLevel "StencilOp"
type StencilOp = VkStencilOp

-- No documentation found for TopLevel "VertexInputRate"
type VertexInputRate = VkVertexInputRate
