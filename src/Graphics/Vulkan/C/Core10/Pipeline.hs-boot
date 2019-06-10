{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Pipeline
  ( VkBlendFactor
  , VkBlendOp
  , VkColorComponentFlagBits
  , VkColorComponentFlags
  , VkCompareOp
  , VkComputePipelineCreateInfo
  , VkCullModeFlagBits
  , VkCullModeFlags
  , VkDynamicState
  , VkExtent2D
  , VkFrontFace
  , VkGraphicsPipelineCreateInfo
  , VkLogicOp
  , VkOffset2D
  , VkPipeline
  , VkPipelineColorBlendAttachmentState
  , VkPipelineColorBlendStateCreateFlags
  , VkPipelineColorBlendStateCreateInfo
  , VkPipelineCreateFlagBits
  , VkPipelineCreateFlags
  , VkPipelineDepthStencilStateCreateFlags
  , VkPipelineDepthStencilStateCreateInfo
  , VkPipelineDynamicStateCreateFlags
  , VkPipelineDynamicStateCreateInfo
  , VkPipelineInputAssemblyStateCreateFlags
  , VkPipelineInputAssemblyStateCreateInfo
  , VkPipelineLayout
  , VkPipelineMultisampleStateCreateFlags
  , VkPipelineMultisampleStateCreateInfo
  , VkPipelineRasterizationStateCreateFlags
  , VkPipelineRasterizationStateCreateInfo
  , VkPipelineShaderStageCreateFlags
  , VkPipelineShaderStageCreateInfo
  , VkPipelineTessellationStateCreateFlags
  , VkPipelineTessellationStateCreateInfo
  , VkPipelineVertexInputStateCreateFlags
  , VkPipelineVertexInputStateCreateInfo
  , VkPipelineViewportStateCreateFlags
  , VkPipelineViewportStateCreateInfo
  , VkPolygonMode
  , VkPrimitiveTopology
  , VkRect2D
  , VkRenderPass
  , VkSampleMask
  , VkShaderStageFlagBits
  , VkSpecializationInfo
  , VkSpecializationMapEntry
  , VkStencilOp
  , VkStencilOpState
  , VkVertexInputAttributeDescription
  , VkVertexInputBindingDescription
  , VkVertexInputRate
  , VkViewport
  , FN_vkCreateComputePipelines
  , PFN_vkCreateComputePipelines
  , FN_vkCreateGraphicsPipelines
  , PFN_vkCreateGraphicsPipelines
  , FN_vkDestroyPipeline
  , PFN_vkDestroyPipeline
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCache
  )


data VkBlendFactor

data VkBlendOp

data VkColorComponentFlagBits

-- No documentation found for TopLevel "VkColorComponentFlags"
type VkColorComponentFlags = VkColorComponentFlagBits

data VkCompareOp

data VkComputePipelineCreateInfo

data VkCullModeFlagBits

-- No documentation found for TopLevel "VkCullModeFlags"
type VkCullModeFlags = VkCullModeFlagBits

data VkDynamicState

data VkExtent2D

data VkFrontFace

data VkGraphicsPipelineCreateInfo

data VkLogicOp

data VkOffset2D

-- | Dummy data to tag the 'Ptr' with
data VkPipeline_T
-- No documentation found for TopLevel "VkPipeline"
type VkPipeline = Ptr VkPipeline_T

data VkPipelineColorBlendAttachmentState

data VkPipelineColorBlendStateCreateFlags

data VkPipelineColorBlendStateCreateInfo

data VkPipelineCreateFlagBits

-- No documentation found for TopLevel "VkPipelineCreateFlags"
type VkPipelineCreateFlags = VkPipelineCreateFlagBits

data VkPipelineDepthStencilStateCreateFlags

data VkPipelineDepthStencilStateCreateInfo

data VkPipelineDynamicStateCreateFlags

data VkPipelineDynamicStateCreateInfo

data VkPipelineInputAssemblyStateCreateFlags

data VkPipelineInputAssemblyStateCreateInfo

-- | Dummy data to tag the 'Ptr' with
data VkPipelineLayout_T
-- No documentation found for TopLevel "VkPipelineLayout"
type VkPipelineLayout = Ptr VkPipelineLayout_T

data VkPipelineMultisampleStateCreateFlags

data VkPipelineMultisampleStateCreateInfo

data VkPipelineRasterizationStateCreateFlags

data VkPipelineRasterizationStateCreateInfo

data VkPipelineShaderStageCreateFlags

data VkPipelineShaderStageCreateInfo

data VkPipelineTessellationStateCreateFlags

data VkPipelineTessellationStateCreateInfo

data VkPipelineVertexInputStateCreateFlags

data VkPipelineVertexInputStateCreateInfo

data VkPipelineViewportStateCreateFlags

data VkPipelineViewportStateCreateInfo

data VkPolygonMode

data VkPrimitiveTopology

data VkRect2D

-- | Dummy data to tag the 'Ptr' with
data VkRenderPass_T
-- No documentation found for TopLevel "VkRenderPass"
type VkRenderPass = Ptr VkRenderPass_T

-- No documentation found for TopLevel "VkSampleMask"
type VkSampleMask = Word32

data VkShaderStageFlagBits

data VkSpecializationInfo

data VkSpecializationMapEntry

data VkStencilOp

data VkStencilOpState

data VkVertexInputAttributeDescription

data VkVertexInputBindingDescription

data VkVertexInputRate

data VkViewport

type FN_vkCreateComputePipelines = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkComputePipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
type PFN_vkCreateComputePipelines = FunPtr FN_vkCreateComputePipelines

type FN_vkCreateGraphicsPipelines = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkGraphicsPipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
type PFN_vkCreateGraphicsPipelines = FunPtr FN_vkCreateGraphicsPipelines

type FN_vkDestroyPipeline = ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyPipeline = FunPtr FN_vkDestroyPipeline
