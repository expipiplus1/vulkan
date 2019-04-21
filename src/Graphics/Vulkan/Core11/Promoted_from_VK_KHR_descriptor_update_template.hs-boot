{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplate
  , DescriptorUpdateTemplateCreateFlags
  , DescriptorUpdateTemplateCreateFlagsKHR
  , DescriptorUpdateTemplateType
  , DescriptorUpdateTemplateTypeKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateFlags
  , VkDescriptorUpdateTemplateType
  , VkDescriptorUpdateTemplate
  )


-- | VkDescriptorUpdateTemplate - Opaque handle to a descriptor update
-- template
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate'
type DescriptorUpdateTemplate = VkDescriptorUpdateTemplate

-- | VkDescriptorUpdateTemplateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo'
type DescriptorUpdateTemplateCreateFlags = VkDescriptorUpdateTemplateCreateFlags

-- No documentation found for TopLevel "DescriptorUpdateTemplateCreateFlagsKHR"
type DescriptorUpdateTemplateCreateFlagsKHR = DescriptorUpdateTemplateCreateFlags

-- | VkDescriptorUpdateTemplateType - Indicates the valid usage of the
-- descriptor update template
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo'
type DescriptorUpdateTemplateType = VkDescriptorUpdateTemplateType

-- No documentation found for TopLevel "DescriptorUpdateTemplateTypeKHR"
type DescriptorUpdateTemplateTypeKHR = DescriptorUpdateTemplateType
