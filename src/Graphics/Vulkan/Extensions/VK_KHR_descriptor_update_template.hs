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
  , VkStructureType(..)
  , VkResult(..)
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
  ( pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  , VkDescriptorUpdateTemplateEntry(..)
  , VkDescriptorUpdateTemplateCreateFlags(..)
  , VkDescriptorUpdateTemplateType(..)
  , vkUpdateDescriptorSetWithTemplate
  , vkDestroyDescriptorUpdateTemplate
  , VkDescriptorUpdateTemplate
  , VkDescriptorUpdateTemplateCreateInfo(..)
  , vkCreateDescriptorUpdateTemplate
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( VkDebugReportObjectTypeEXT(..)
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
  )


pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1
pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME = "VK_KHR_descriptor_update_template"
vkCreateDescriptorUpdateTemplateKHR :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult
vkCreateDescriptorUpdateTemplateKHR = vkCreateDescriptorUpdateTemplate
vkDestroyDescriptorUpdateTemplateKHR :: ("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyDescriptorUpdateTemplateKHR = vkDestroyDescriptorUpdateTemplate
vkUpdateDescriptorSetWithTemplateKHR :: ("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()
vkUpdateDescriptorSetWithTemplateKHR = vkUpdateDescriptorSetWithTemplate
type VkDescriptorUpdateTemplateTypeKHR = VkDescriptorUpdateTemplateType
type VkDescriptorUpdateTemplateCreateFlagsKHR = VkDescriptorUpdateTemplateCreateFlags
type VkDescriptorUpdateTemplateKHR = VkDescriptorUpdateTemplate
type VkDescriptorUpdateTemplateEntryKHR = VkDescriptorUpdateTemplateEntry


pattern VkDescriptorUpdateTemplateEntryKHR :: ("dstBinding" ::: Word32) -> ("dstArrayElement" ::: Word32) -> ("descriptorCount" ::: Word32) -> ("descriptorType" ::: VkDescriptorType) -> ("offset" ::: CSize) -> ("stride" ::: CSize) -> VkDescriptorUpdateTemplateEntryKHR
pattern VkDescriptorUpdateTemplateEntryKHR vkDstBinding vkDstArrayElement vkDescriptorCount vkDescriptorType vkOffset vkStride = VkDescriptorUpdateTemplateEntry vkDstBinding vkDstArrayElement vkDescriptorCount vkDescriptorType vkOffset vkStride
type VkDescriptorUpdateTemplateCreateInfoKHR = VkDescriptorUpdateTemplateCreateInfo


pattern VkDescriptorUpdateTemplateCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("flags" ::: VkDescriptorUpdateTemplateCreateFlags) -> ("descriptorUpdateEntryCount" ::: Word32) -> ("pDescriptorUpdateEntries" ::: Ptr VkDescriptorUpdateTemplateEntry) -> ("templateType" ::: VkDescriptorUpdateTemplateType) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> VkDescriptorUpdateTemplateCreateInfoKHR
pattern VkDescriptorUpdateTemplateCreateInfoKHR vkSType vkNext vkFlags vkDescriptorUpdateEntryCount vkDescriptorUpdateEntries vkTemplateType vkDescriptorSetLayout vkPipelineBindPoint vkPipelineLayout vkSet = VkDescriptorUpdateTemplateCreateInfo vkSType vkNext vkFlags vkDescriptorUpdateEntryCount vkDescriptorUpdateEntries vkTemplateType vkDescriptorSetLayout vkPipelineBindPoint vkPipelineLayout vkSet
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR = VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR :: VkDescriptorUpdateTemplateType
pattern VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR = VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT :: VkDebugReportObjectTypeEXT
pattern VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT = VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
