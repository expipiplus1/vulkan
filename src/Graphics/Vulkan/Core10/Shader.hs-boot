{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Shader
  ( ShaderModule
  , ShaderModuleCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModuleCreateFlags
  , VkShaderModule
  )


-- | VkShaderModule - Opaque handle to a shader module object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Shader.vkCreateShaderModule',
-- 'Graphics.Vulkan.C.Core10.Shader.vkDestroyShaderModule'
type ShaderModule = VkShaderModule

-- | VkShaderModuleCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateFlags' is a bitmask
-- type for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateInfo'
type ShaderModuleCreateFlags = VkShaderModuleCreateFlags
