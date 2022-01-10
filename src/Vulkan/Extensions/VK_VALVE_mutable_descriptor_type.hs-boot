{-# language CPP #-}
-- | = Name
--
-- VK_VALVE_mutable_descriptor_type - device extension
--
-- == VK_VALVE_mutable_descriptor_type
--
-- [__Name String__]
--     @VK_VALVE_mutable_descriptor_type@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     352
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_maintenance3@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_mutable_descriptor_type] @Joshua-Ashton%0A<<Here describe the issue or question you have about the VK_VALVE_mutable_descriptor_type extension>> >
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_mutable_descriptor_type] @HansKristian-Work%0A<<Here describe the issue or question you have about the VK_VALVE_mutable_descriptor_type extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-12-02
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
-- -   'MutableDescriptorTypeListVALVE'
--
-- -   Extending
--     'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo',
--     'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo':
--
--     -   'MutableDescriptorTypeCreateInfoVALVE'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'
--
-- == New Enum Constants
--
-- -   'VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME'
--
-- -   'VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE'
--
-- == Version History
--
-- -   Revision 1, 2020-12-01 (Joshua Ashton, Hans-Kristian Arntzen)
--
--     -   Initial specification, squashed from public draft.
--
-- == See Also
--
-- 'MutableDescriptorTypeCreateInfoVALVE',
-- 'MutableDescriptorTypeListVALVE',
-- 'PhysicalDeviceMutableDescriptorTypeFeaturesVALVE'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VALVE_mutable_descriptor_type Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_mutable_descriptor_type  ( MutableDescriptorTypeCreateInfoVALVE
                                                           , MutableDescriptorTypeListVALVE
                                                           , PhysicalDeviceMutableDescriptorTypeFeaturesVALVE
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MutableDescriptorTypeCreateInfoVALVE

instance ToCStruct MutableDescriptorTypeCreateInfoVALVE
instance Show MutableDescriptorTypeCreateInfoVALVE

instance FromCStruct MutableDescriptorTypeCreateInfoVALVE


data MutableDescriptorTypeListVALVE

instance ToCStruct MutableDescriptorTypeListVALVE
instance Show MutableDescriptorTypeListVALVE

instance FromCStruct MutableDescriptorTypeListVALVE


data PhysicalDeviceMutableDescriptorTypeFeaturesVALVE

instance ToCStruct PhysicalDeviceMutableDescriptorTypeFeaturesVALVE
instance Show PhysicalDeviceMutableDescriptorTypeFeaturesVALVE

instance FromCStruct PhysicalDeviceMutableDescriptorTypeFeaturesVALVE

