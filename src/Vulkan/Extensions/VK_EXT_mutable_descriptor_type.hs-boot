{-# language CPP #-}
-- | = Name
--
-- VK_EXT_mutable_descriptor_type - device extension
--
-- == VK_EXT_mutable_descriptor_type
--
-- [__Name String__]
--     @VK_EXT_mutable_descriptor_type@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     495
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance3 VK_KHR_maintenance3>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_mutable_descriptor_type] @Joshua-Ashton%0A*Here describe the issue or question you have about the VK_EXT_mutable_descriptor_type extension* >
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_mutable_descriptor_type] @HansKristian-Work%0A*Here describe the issue or question you have about the VK_EXT_mutable_descriptor_type extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_mutable_descriptor_type.adoc VK_EXT_mutable_descriptor_type>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-08-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- This extension allows applications to reduce descriptor memory footprint
-- by allowing a descriptor to be able to mutate to a given list of
-- descriptor types depending on which descriptor types are written into,
-- or copied into a descriptor set.
--
-- The main use case this extension intends to address is descriptor
-- indexing with
-- 'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT'
-- where the descriptor types are completely generic, as this means
-- applications can allocate one large descriptor set, rather than having
-- one large descriptor set per descriptor type, which significantly bloats
-- descriptor memory usage and causes performance issues.
--
-- This extension also adds a mechanism to declare that a descriptor pool,
-- and therefore the descriptor sets that are allocated from it, reside
-- only in host memory; as such these descriptors can only be
-- updated\/copied, but not bound.
--
-- These features together allow much more efficient emulation of the raw
-- D3D12 binding model. This extension is primarily intended to be useful
-- for API layering efforts.
--
-- == New Structures
--
-- -   'MutableDescriptorTypeListEXT'
--
-- -   Extending
--     'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo':
--
--     -   'MutableDescriptorTypeCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMutableDescriptorTypeFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME'
--
-- -   'EXT_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-08-22 (Jon Leech)
--
--     -   Initial version, promoted from
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_mutable_descriptor_type VK_VALVE_mutable_descriptor_type>.
--
-- == See Also
--
-- 'MutableDescriptorTypeCreateInfoEXT', 'MutableDescriptorTypeListEXT',
-- 'PhysicalDeviceMutableDescriptorTypeFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_mutable_descriptor_type Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_mutable_descriptor_type  ( MutableDescriptorTypeCreateInfoEXT
                                                         , MutableDescriptorTypeListEXT
                                                         , PhysicalDeviceMutableDescriptorTypeFeaturesEXT
                                                         ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MutableDescriptorTypeCreateInfoEXT

instance ToCStruct MutableDescriptorTypeCreateInfoEXT
instance Show MutableDescriptorTypeCreateInfoEXT

instance FromCStruct MutableDescriptorTypeCreateInfoEXT


data MutableDescriptorTypeListEXT

instance ToCStruct MutableDescriptorTypeListEXT
instance Show MutableDescriptorTypeListEXT

instance FromCStruct MutableDescriptorTypeListEXT


data PhysicalDeviceMutableDescriptorTypeFeaturesEXT

instance ToCStruct PhysicalDeviceMutableDescriptorTypeFeaturesEXT
instance Show PhysicalDeviceMutableDescriptorTypeFeaturesEXT

instance FromCStruct PhysicalDeviceMutableDescriptorTypeFeaturesEXT

