{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_descriptor_indexing"
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

