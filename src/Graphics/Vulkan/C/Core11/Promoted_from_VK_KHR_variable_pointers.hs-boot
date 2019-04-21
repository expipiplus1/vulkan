{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointerFeatures
  , VkPhysicalDeviceVariablePointersFeatures
  ) where







-- | VkPhysicalDeviceVariablePointerFeatures - Structure describing variable
-- pointers features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceVariablePointerFeatures' structure
-- describe the following features:
--
-- = Description
--
-- -   @variablePointersStorageBuffer@ specifies whether the implementation
--     supports the SPIR-V @VariablePointersStorageBuffer@ capability. When
--     this feature is not enabled, shader modules /must/ not declare the
--     @SPV_KHR_variable_pointers@ extension or the
--     @VariablePointersStorageBuffer@ capability.
--
-- -   @variablePointers@ specifies whether the implementation supports the
--     SPIR-V @VariablePointers@ capability. When this feature is not
--     enabled, shader modules /must/ not declare the @VariablePointers@
--     capability.
--
-- If the 'VkPhysicalDeviceVariablePointerFeatures' structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'VkPhysicalDeviceVariablePointerFeatures' /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to
-- enable the features.
--
-- == Valid Usage
--
-- -   If @variablePointers@ is enabled then
--     @variablePointersStorageBuffer@ /must/ also be enabled.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
type VkPhysicalDeviceVariablePointerFeatures = VkPhysicalDeviceVariablePointersFeatures

data VkPhysicalDeviceVariablePointersFeatures
