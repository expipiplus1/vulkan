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

-- | VkColorComponentFlags - Bitmask of VkColorComponentFlagBits
--
-- = Description
--
-- 'VkColorComponentFlags' is a bitmask type for setting a mask of zero or
-- more 'VkColorComponentFlagBits'.
--
-- = See Also
--
-- 'VkColorComponentFlagBits', 'VkPipelineColorBlendAttachmentState'
type VkColorComponentFlags = VkColorComponentFlagBits

data VkCompareOp

data VkComputePipelineCreateInfo

data VkCullModeFlagBits

-- | VkCullModeFlags - Bitmask of VkCullModeFlagBits
--
-- = Description
--
-- 'VkCullModeFlags' is a bitmask type for setting a mask of zero or more
-- 'VkCullModeFlagBits'.
--
-- = See Also
--
-- 'VkCullModeFlagBits', 'VkPipelineRasterizationStateCreateInfo'
type VkCullModeFlags = VkCullModeFlagBits

data VkDynamicState

data VkExtent2D

data VkFrontFace

data VkGraphicsPipelineCreateInfo

data VkLogicOp

data VkOffset2D

-- | Dummy data to tag the 'Ptr' with
data VkPipeline_T
-- | VkPipeline - Opaque handle to a pipeline object
--
-- = See Also
--
-- 'VkComputePipelineCreateInfo', 'VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePipelineEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindPipeline',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCompileDeferredNV',
-- 'vkCreateComputePipelines', 'vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCreateRayTracingPipelinesNV',
-- 'vkDestroyPipeline',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetRayTracingShaderGroupHandlesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.vkGetShaderInfoAMD'
type VkPipeline = Ptr VkPipeline_T

data VkPipelineColorBlendAttachmentState

data VkPipelineColorBlendStateCreateFlags

data VkPipelineColorBlendStateCreateInfo

data VkPipelineCreateFlagBits

-- | VkPipelineCreateFlags - Bitmask of VkPipelineCreateFlagBits
--
-- = Description
--
-- 'VkPipelineCreateFlags' is a bitmask type for setting a mask of zero or
-- more 'VkPipelineCreateFlagBits'.
--
-- = See Also
--
-- 'VkComputePipelineCreateInfo', 'VkGraphicsPipelineCreateInfo',
-- 'VkPipelineCreateFlagBits',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV'
type VkPipelineCreateFlags = VkPipelineCreateFlagBits

data VkPipelineDepthStencilStateCreateFlags

data VkPipelineDepthStencilStateCreateInfo

data VkPipelineDynamicStateCreateFlags

data VkPipelineDynamicStateCreateInfo

data VkPipelineInputAssemblyStateCreateFlags

data VkPipelineInputAssemblyStateCreateInfo

-- | Dummy data to tag the 'Ptr' with
data VkPipelineLayout_T
-- | VkPipelineLayout - Opaque handle to a pipeline layout object
--
-- = See Also
--
-- 'VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableDescriptorSetEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePushConstantEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPushConstants',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.vkCreatePipelineLayout',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.vkDestroyPipelineLayout'
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
-- | VkRenderPass - Opaque handle to a render pass object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo',
-- 'VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateRenderPass',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.vkCreateRenderPass2KHR',
-- 'Graphics.Vulkan.C.Core10.Pass.vkDestroyRenderPass',
-- 'Graphics.Vulkan.C.Core10.Pass.vkGetRenderAreaGranularity'
type VkRenderPass = Ptr VkRenderPass_T

-- | VkSampleMask - Mask of sample coverage information
--
-- = See Also
--
-- 'VkPipelineMultisampleStateCreateInfo'
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
