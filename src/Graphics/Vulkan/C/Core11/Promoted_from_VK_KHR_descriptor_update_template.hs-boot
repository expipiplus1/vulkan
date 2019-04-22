{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplate
  , VkDescriptorUpdateTemplateCreateFlags
  , VkDescriptorUpdateTemplateCreateInfo
  , VkDescriptorUpdateTemplateEntry
  , VkDescriptorUpdateTemplateType
  , FN_vkCreateDescriptorUpdateTemplate
  , PFN_vkCreateDescriptorUpdateTemplate
  , FN_vkDestroyDescriptorUpdateTemplate
  , PFN_vkDestroyDescriptorUpdateTemplate
  , FN_vkUpdateDescriptorSetWithTemplate
  , PFN_vkUpdateDescriptorSetWithTemplate
  ) where

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
import {-# source #-} Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorSet
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  )


-- | Dummy data to tag the 'Ptr' with
data VkDescriptorUpdateTemplate_T
-- | VkDescriptorUpdateTemplate - Opaque handle to a descriptor update
-- template
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetWithTemplateKHR',
-- 'vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplateKHR',
-- 'vkDestroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplateKHR',
-- 'vkUpdateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplateKHR'
type VkDescriptorUpdateTemplate = Ptr VkDescriptorUpdateTemplate_T

data VkDescriptorUpdateTemplateCreateFlags

data VkDescriptorUpdateTemplateCreateInfo

data VkDescriptorUpdateTemplateEntry

data VkDescriptorUpdateTemplateType

type FN_vkCreateDescriptorUpdateTemplate = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult
type PFN_vkCreateDescriptorUpdateTemplate = FunPtr FN_vkCreateDescriptorUpdateTemplate

type FN_vkDestroyDescriptorUpdateTemplate = ("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDescriptorUpdateTemplate = FunPtr FN_vkDestroyDescriptorUpdateTemplate

type FN_vkUpdateDescriptorSetWithTemplate = ("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()
type PFN_vkUpdateDescriptorSetWithTemplate = FunPtr FN_vkUpdateDescriptorSetWithTemplate
