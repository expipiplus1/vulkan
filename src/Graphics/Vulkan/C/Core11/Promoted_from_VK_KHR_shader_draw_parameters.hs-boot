{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( VkPhysicalDeviceShaderDrawParameterFeatures
  , VkPhysicalDeviceShaderDrawParametersFeatures
  ) where







-- | VkPhysicalDeviceShaderDrawParameterFeatures - Structure describing
-- shader draw parameter features that can be supported by an
-- implementation
--
-- = Description
--
-- If the 'VkPhysicalDeviceShaderDrawParameterFeatures' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with a value indicating whether the feature is supported.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
type VkPhysicalDeviceShaderDrawParameterFeatures = VkPhysicalDeviceShaderDrawParametersFeatures

data VkPhysicalDeviceShaderDrawParametersFeatures
