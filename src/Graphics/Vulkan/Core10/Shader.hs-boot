{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Shader
  ( ShaderModule
  , ShaderModuleCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModule
  , VkShaderModuleCreateFlags
  )


-- No documentation found for TopLevel "ShaderModule"
type ShaderModule = VkShaderModule

-- No documentation found for TopLevel "ShaderModuleCreateFlags"
type ShaderModuleCreateFlags = VkShaderModuleCreateFlags
