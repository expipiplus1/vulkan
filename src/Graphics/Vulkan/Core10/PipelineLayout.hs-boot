{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.PipelineLayout
  ( DescriptorSetLayout
  , PipelineLayoutCreateFlags
  , ShaderStageFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  , VkPipelineLayoutCreateFlags
  )
import {-# source #-} Graphics.Vulkan.Core10.Pipeline
  ( ShaderStageFlagBits
  )


-- No documentation found for TopLevel "DescriptorSetLayout"
type DescriptorSetLayout = VkDescriptorSetLayout

-- No documentation found for TopLevel "PipelineLayoutCreateFlags"
type PipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags

-- No documentation found for TopLevel "ShaderStageFlags"
type ShaderStageFlags = ShaderStageFlagBits
