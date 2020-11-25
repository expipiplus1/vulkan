{-# language CPP #-}
-- | = Name
--
-- VK_KHR_descriptor_update_template - device extension
--
-- == VK_KHR_descriptor_update_template
--
-- [__Name String__]
--     @VK_KHR_descriptor_update_template@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     86
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Markus Tavenrath
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_descriptor_update_template:%20&body=@mtavenrath%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_push_descriptor@
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Michael Worcester, Imagination Technologies
--
-- == Description
--
-- Applications may wish to update a fixed set of descriptors in a large
-- number of descriptors sets very frequently, i.e. during initializaton
-- phase or if it is required to rebuild descriptor sets for each frame.
-- For those cases it is also not unlikely that all information required to
-- update a single descriptor set is stored in a single struct. This
-- extension provides a way to update a fixed set of descriptors in a
-- single 'Vulkan.Core10.Handles.DescriptorSet' with a pointer to a user
-- defined data structure describing the new descriptors.
--
-- == Promotion to Vulkan 1.1
--
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetWithTemplateKHR'
-- is included as an interaction with @VK_KHR_push_descriptor@. If Vulkan
-- 1.1 and @VK_KHR_push_descriptor@ are supported, this is included by
-- @VK_KHR_push_descriptor@.
--
-- The base functionality in this extension is included in core Vulkan 1.1,
-- with the KHR suffix omitted. The original type, enum and command names
-- are still available as aliases of the core functionality.
--
-- == New Object Types
--
-- -   'DescriptorUpdateTemplateKHR'
--
-- == New Commands
--
-- -   'createDescriptorUpdateTemplateKHR'
--
-- -   'destroyDescriptorUpdateTemplateKHR'
--
-- -   'updateDescriptorSetWithTemplateKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetWithTemplateKHR'
--
-- == New Structures
--
-- -   'DescriptorUpdateTemplateCreateInfoKHR'
--
-- -   'DescriptorUpdateTemplateEntryKHR'
--
-- == New Enums
--
-- -   'DescriptorUpdateTemplateTypeKHR'
--
-- == New Bitmasks
--
-- -   'DescriptorUpdateTemplateCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DESCRIPTOR_UPDATE_TEMPLATE_EXTENSION_NAME'
--
-- -   'KHR_DESCRIPTOR_UPDATE_TEMPLATE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType':
--
--     -   'DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_report VK_EXT_debug_report>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_push_descriptor VK_KHR_push_descriptor>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType':
--
--     -   'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
--
-- == Version History
--
-- -   Revision 1, 2016-01-11 (Markus Tavenrath)
--
--     -   Initial draft
--
-- = See Also
--
-- 'DescriptorUpdateTemplateCreateFlagsKHR',
-- 'DescriptorUpdateTemplateCreateInfoKHR',
-- 'DescriptorUpdateTemplateEntryKHR', 'DescriptorUpdateTemplateKHR',
-- 'DescriptorUpdateTemplateTypeKHR', 'createDescriptorUpdateTemplateKHR',
-- 'destroyDescriptorUpdateTemplateKHR',
-- 'updateDescriptorSetWithTemplateKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_descriptor_update_template Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

