{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.PipelineLayout
  ( DescriptorSetLayout
  , PipelineLayoutCreateFlags
  , ShaderStageFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkPipelineLayoutCreateFlags
  , VkDescriptorSetLayout
  )
import {-# source #-} Graphics.Vulkan.Core10.Pipeline
  ( ShaderStageFlagBits
  )


-- | VkDescriptorSetLayout - Opaque handle to a descriptor set layout object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkDestroyDescriptorSetLayout'
type DescriptorSetLayout = VkDescriptorSetLayout

-- | VkPipelineLayoutCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateFlags' is
-- a bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo'
type PipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags

-- | VkShaderStageFlags - Bitmask of VkShaderStageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePushConstantEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkPhysicalDeviceCooperativeMatrixPropertiesNV',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPushConstantRange',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VkShaderStatisticsInfoAMD',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPushConstants'
type ShaderStageFlags = ShaderStageFlagBits
