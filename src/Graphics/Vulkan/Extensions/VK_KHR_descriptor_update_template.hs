{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template
  ( pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION
  , pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
  , vkCreateDescriptorUpdateTemplateKHR
  , vkDestroyDescriptorUpdateTemplateKHR
  , vkUpdateDescriptorSetWithTemplateKHR
  , VkDescriptorUpdateTemplateTypeKHR
  , VkDescriptorUpdateTemplateCreateFlagsKHR
  , VkDescriptorUpdateTemplateKHR
  , VkDescriptorUpdateTemplateEntryKHR
  , pattern VkDescriptorUpdateTemplateEntryKHR
  , VkDescriptorUpdateTemplateCreateInfoKHR
  , pattern VkDescriptorUpdateTemplateCreateInfoKHR
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorType(..)
  , VkDescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Pass
  ( VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkPipelineLayout
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateFlags(..)
  , VkDescriptorUpdateTemplateCreateInfo(..)
  , VkDescriptorUpdateTemplateEntry(..)
  , VkDescriptorUpdateTemplateType(..)
  , VkDescriptorUpdateTemplate
  , vkCreateDescriptorUpdateTemplate
  , vkDestroyDescriptorUpdateTemplate
  , vkUpdateDescriptorSetWithTemplate
  , pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( VkDebugReportObjectTypeEXT(..)
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  )


-- No documentation found for TopLevel "VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION"
pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME"
pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME = "VK_KHR_descriptor_update_template"
-- No documentation found for TopLevel "vkCreateDescriptorUpdateTemplateKHR"
vkCreateDescriptorUpdateTemplateKHR :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult
vkCreateDescriptorUpdateTemplateKHR = vkCreateDescriptorUpdateTemplate
-- No documentation found for TopLevel "vkDestroyDescriptorUpdateTemplateKHR"
vkDestroyDescriptorUpdateTemplateKHR :: ("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyDescriptorUpdateTemplateKHR = vkDestroyDescriptorUpdateTemplate
-- No documentation found for TopLevel "vkUpdateDescriptorSetWithTemplateKHR"
vkUpdateDescriptorSetWithTemplateKHR :: ("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()
vkUpdateDescriptorSetWithTemplateKHR = vkUpdateDescriptorSetWithTemplate
-- No documentation found for TopLevel "VkDescriptorUpdateTemplateTypeKHR"
type VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateType
-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateFlagsKHR"
type VkDescriptorUpdateTemplateCreateFlagsKHR = VkDescriptorUpdateTemplateCreateFlags
-- No documentation found for TopLevel "VkDescriptorUpdateTemplateKHR"
type VkDescriptorUpdateTemplateKHR = VkDescriptorUpdateTemplate
-- No documentation found for TopLevel "VkDescriptorUpdateTemplateEntryKHR"
type VkDescriptorUpdateTemplateEntryKHR = VkDescriptorUpdateTemplateEntry


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateEntryKHR"
pattern VkDescriptorUpdateTemplateEntryKHR :: ("dstBinding" ::: Word32) -> ("dstArrayElement" ::: Word32) -> ("descriptorCount" ::: Word32) -> ("descriptorType" ::: VkDescriptorType) -> ("offset" ::: CSize) -> ("stride" ::: CSize) -> VkDescriptorUpdateTemplateEntryKHR
pattern VkDescriptorUpdateTemplateEntryKHR vkDstBinding vkDstArrayElement vkDescriptorCount vkDescriptorType vkOffset vkStride = VkDescriptorUpdateTemplateEntry vkDstBinding vkDstArrayElement vkDescriptorCount vkDescriptorType vkOffset vkStride
-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateInfoKHR"
type VkDescriptorUpdateTemplateCreateInfoKHR = VkDescriptorUpdateTemplateCreateInfo


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateInfoKHR"
pattern VkDescriptorUpdateTemplateCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("flags" ::: VkDescriptorUpdateTemplateCreateFlags) -> ("descriptorUpdateEntryCount" ::: Word32) -> ("pDescriptorUpdateEntries" ::: Ptr VkDescriptorUpdateTemplateEntry) -> ("templateType" ::: VkDescriptorUpdateTemplateType) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> VkDescriptorUpdateTemplateCreateInfoKHR
pattern VkDescriptorUpdateTemplateCreateInfoKHR vkSType vkPNext vkFlags vkDescriptorUpdateEntryCount vkPDescriptorUpdateEntries vkTemplateType vkDescriptorSetLayout vkPipelineBindPoint vkPipelineLayout vkSet = VkDescriptorUpdateTemplateCreateInfo vkSType vkPNext vkFlags vkDescriptorUpdateEntryCount vkPDescriptorUpdateEntries vkTemplateType vkDescriptorSetLayout vkPipelineBindPoint vkPipelineLayout vkSet
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
-- No documentation found for TopLevel "VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR"
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR = VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
-- No documentation found for TopLevel "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR"
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR :: VkDescriptorUpdateTemplateType
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR = VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT"
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
