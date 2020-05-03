{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_descriptor_update_template  ( pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR
                                                            , pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR
                                                            , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR
                                                            , pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
                                                            , createDescriptorUpdateTemplateKHR
                                                            , destroyDescriptorUpdateTemplateKHR
                                                            , updateDescriptorSetWithTemplateKHR
                                                            , DescriptorUpdateTemplateCreateFlagsKHR
                                                            , DescriptorUpdateTemplateKHR
                                                            , DescriptorUpdateTemplateTypeKHR
                                                            , DescriptorUpdateTemplateEntryKHR
                                                            , DescriptorUpdateTemplateCreateInfoKHR
                                                            , KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION
                                                            , pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION
                                                            , KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
                                                            , pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME
                                                            , cmdPushDescriptorSetWithTemplateKHR
                                                            , DebugReportObjectTypeEXT(..)
                                                            ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (createDescriptorUpdateTemplate)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (destroyDescriptorUpdateTemplate)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (updateDescriptorSetWithTemplate)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate)
import Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags (DescriptorUpdateTemplateCreateFlags)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (DescriptorUpdateTemplateCreateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (DescriptorUpdateTemplateEntry)
import Vulkan.Core11.Enums.DescriptorUpdateTemplateType (DescriptorUpdateTemplateType)
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT))
import Vulkan.Core11.Enums.DescriptorUpdateTemplateType (DescriptorUpdateTemplateType(DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO))
import Vulkan.Extensions.VK_KHR_push_descriptor (cmdPushDescriptorSetWithTemplateKHR)
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR = STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO


-- No documentation found for TopLevel "VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR"
pattern OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR = OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE


-- No documentation found for TopLevel "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR"
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR = DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET


-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT = DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT


-- No documentation found for TopLevel "vkCreateDescriptorUpdateTemplateKHR"
createDescriptorUpdateTemplateKHR = createDescriptorUpdateTemplate


-- No documentation found for TopLevel "vkDestroyDescriptorUpdateTemplateKHR"
destroyDescriptorUpdateTemplateKHR = destroyDescriptorUpdateTemplate


-- No documentation found for TopLevel "vkUpdateDescriptorSetWithTemplateKHR"
updateDescriptorSetWithTemplateKHR = updateDescriptorSetWithTemplate


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateFlagsKHR"
type DescriptorUpdateTemplateCreateFlagsKHR = DescriptorUpdateTemplateCreateFlags


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateKHR"
type DescriptorUpdateTemplateKHR = DescriptorUpdateTemplate


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateTypeKHR"
type DescriptorUpdateTemplateTypeKHR = DescriptorUpdateTemplateType


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateEntryKHR"
type DescriptorUpdateTemplateEntryKHR = DescriptorUpdateTemplateEntry


-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateInfoKHR"
type DescriptorUpdateTemplateCreateInfoKHR = DescriptorUpdateTemplateCreateInfo


type KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION"
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION = 1


type KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME = "VK_KHR_descriptor_update_template"

-- No documentation found for TopLevel "VK_KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME"
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME = "VK_KHR_descriptor_update_template"

