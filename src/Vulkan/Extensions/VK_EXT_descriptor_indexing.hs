{-# language CPP #-}
-- | = Name
--
-- VK_EXT_descriptor_indexing - device extension
--
-- == VK_EXT_descriptor_indexing
--
-- [__Name String__]
--     @VK_EXT_descriptor_indexing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     162
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
--     -   Requires @VK_KHR_maintenance3@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_descriptor_indexing] @jeffbolznv%0A<<Here describe the issue or question you have about the VK_EXT_descriptor_indexing extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-10-02
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_descriptor_indexing.html SPV_EXT_descriptor_indexing>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_nonuniform_qualifier.txt GL_EXT_nonuniform_qualifier>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Slawomir Grajewski, Intel
--
--     -   Tobias Hector, Imagination Technologies
--
-- == Description
--
-- This extension adds several small features which together enable
-- applications to create large descriptor sets containing substantially
-- all of their resources, and selecting amongst those resources with
-- dynamic (non-uniform) indexes in the shader. There are feature enables
-- and SPIR-V capabilities for non-uniform descriptor indexing in the
-- shader, and non-uniform indexing in the shader requires use of a new
-- @NonUniformEXT@ decoration defined in the @SPV_EXT_descriptor_indexing@
-- SPIR-V extension. There are descriptor set layout binding creation flags
-- enabling several features:
--
-- -   Descriptors can be updated after they are bound to a command buffer,
--     such that the execution of the command buffer reflects the most
--     recent update to the descriptors.
--
-- -   Descriptors that are not used by any pending command buffers can be
--     updated, which enables writing new descriptors for frame N+1 while
--     frame N is executing.
--
-- -   Relax the requirement that all descriptors in a binding that is
--     “statically used” must be valid, such that descriptors that are not
--     accessed by a submission need not be valid and can be updated while
--     that submission is executing.
--
-- -   The final binding in a descriptor set layout can have a variable
--     size (and unsized arrays of resources are allowed in the
--     @GL_EXT_nonuniform_qualifier@ and @SPV_EXT_descriptor_indexing@
--     extensions).
--
-- Note that it is valid for multiple descriptor arrays in a shader to use
-- the same set and binding number, as long as they are all compatible with
-- the descriptor type in the pipeline layout. This means a single array
-- binding in the descriptor set can serve multiple texture
-- dimensionalities, or an array of buffer descriptors can be used with
-- multiple different block layouts.
--
-- There are new descriptor set layout and descriptor pool creation flags
-- that are required to opt in to the update-after-bind functionality, and
-- there are separate @maxPerStage@* and @maxDescriptorSet@* limits that
-- apply to these descriptor set layouts which /may/ be much higher than
-- the pre-existing limits. The old limits only count descriptors in
-- non-updateAfterBind descriptor set layouts, and the new limits count
-- descriptors in all descriptor set layouts in the pipeline layout.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.DescriptorSet.DescriptorSetAllocateInfo':
--
--     -   'DescriptorSetVariableDescriptorCountAllocateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo':
--
--     -   'DescriptorSetLayoutBindingFlagsCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.DescriptorSetLayoutSupport':
--
--     -   'DescriptorSetVariableDescriptorCountLayoutSupportEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorIndexingFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDescriptorIndexingPropertiesEXT'
--
-- == New Enums
--
-- -   'DescriptorBindingFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'DescriptorBindingFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME'
--
-- -   'EXT_DESCRIPTOR_INDEXING_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DescriptorBindingFlagBits':
--
--     -   'DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT'
--
--     -   'DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--
--     -   'DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
--
--     -   'DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlagBits':
--
--     -   'DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'ERROR_FRAGMENTATION_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT'
--
-- == Promotion to Vulkan 1.2
--
-- Functionality in this extension is included in core Vulkan 1.2, with the
-- EXT suffix omitted. However, if Vulkan 1.2 is supported and this
-- extension is not, the @descriptorIndexing@ capability is optional. The
-- original type, enum and command names are still available as aliases of
-- the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2017-07-26 (Jeff Bolz)
--
--     -   Internal revisions
--
-- -   Revision 2, 2017-10-02 (Jeff Bolz)
--
--     -   ???
--
-- == See Also
--
-- 'DescriptorBindingFlagBitsEXT', 'DescriptorBindingFlagsEXT',
-- 'DescriptorSetLayoutBindingFlagsCreateInfoEXT',
-- 'DescriptorSetVariableDescriptorCountAllocateInfoEXT',
-- 'DescriptorSetVariableDescriptorCountLayoutSupportEXT',
-- 'PhysicalDeviceDescriptorIndexingFeaturesEXT',
-- 'PhysicalDeviceDescriptorIndexingPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_descriptor_indexing Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_descriptor_indexing  ( pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
                                                     , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
                                                     , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
                                                     , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
                                                     , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
                                                     , pattern DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT
                                                     , pattern DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
                                                     , pattern DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT
                                                     , pattern DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT
                                                     , pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
                                                     , pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
                                                     , pattern ERROR_FRAGMENTATION_EXT
                                                     , DescriptorBindingFlagsEXT
                                                     , DescriptorBindingFlagBitsEXT
                                                     , PhysicalDeviceDescriptorIndexingFeaturesEXT
                                                     , PhysicalDeviceDescriptorIndexingPropertiesEXT
                                                     , DescriptorSetLayoutBindingFlagsCreateInfoEXT
                                                     , DescriptorSetVariableDescriptorCountAllocateInfoEXT
                                                     , DescriptorSetVariableDescriptorCountLayoutSupportEXT
                                                     , EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
                                                     , pattern EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
                                                     , EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                                                     , pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlagBits)
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlags)
import Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetLayoutBindingFlagsCreateInfo)
import Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountAllocateInfo)
import Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountLayoutSupport)
import Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingFeatures)
import Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingProperties)
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlags)
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlagBits(DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT))
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlags)
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlagBits(DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT))
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlags)
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlagBits(DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT))
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlags)
import Vulkan.Core12.Enums.DescriptorBindingFlagBits (DescriptorBindingFlagBits(DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT))
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlags)
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlagBits(DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT))
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlagBits(DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT))
import Vulkan.Core10.Enums.Result (Result(ERROR_FRAGMENTATION))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT = STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT = STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT = STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT


-- No documentation found for TopLevel "VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT"
pattern DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT


-- No documentation found for TopLevel "VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT"
pattern DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT


-- No documentation found for TopLevel "VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT"
pattern DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT


-- No documentation found for TopLevel "VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT"
pattern DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT


-- No documentation found for TopLevel "VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT"
pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT = DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT


-- No documentation found for TopLevel "VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT"
pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT = DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT


-- No documentation found for TopLevel "VK_ERROR_FRAGMENTATION_EXT"
pattern ERROR_FRAGMENTATION_EXT = ERROR_FRAGMENTATION


-- No documentation found for TopLevel "VkDescriptorBindingFlagsEXT"
type DescriptorBindingFlagsEXT = DescriptorBindingFlags


-- No documentation found for TopLevel "VkDescriptorBindingFlagBitsEXT"
type DescriptorBindingFlagBitsEXT = DescriptorBindingFlagBits


-- No documentation found for TopLevel "VkPhysicalDeviceDescriptorIndexingFeaturesEXT"
type PhysicalDeviceDescriptorIndexingFeaturesEXT = PhysicalDeviceDescriptorIndexingFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceDescriptorIndexingPropertiesEXT"
type PhysicalDeviceDescriptorIndexingPropertiesEXT = PhysicalDeviceDescriptorIndexingProperties


-- No documentation found for TopLevel "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT"
type DescriptorSetLayoutBindingFlagsCreateInfoEXT = DescriptorSetLayoutBindingFlagsCreateInfo


-- No documentation found for TopLevel "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT"
type DescriptorSetVariableDescriptorCountAllocateInfoEXT = DescriptorSetVariableDescriptorCountAllocateInfo


-- No documentation found for TopLevel "VkDescriptorSetVariableDescriptorCountLayoutSupportEXT"
type DescriptorSetVariableDescriptorCountLayoutSupportEXT = DescriptorSetVariableDescriptorCountLayoutSupport


type EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION"
pattern EXT_DESCRIPTOR_INDEXING_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2


type EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME = "VK_EXT_descriptor_indexing"

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME"
pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME = "VK_EXT_descriptor_indexing"

