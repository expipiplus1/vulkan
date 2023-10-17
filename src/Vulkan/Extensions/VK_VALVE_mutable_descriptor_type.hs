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
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance3 VK_KHR_maintenance3>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to @VK_EXT_mutable_descriptor_type@ extension
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_mutable_descriptor_type] @Joshua-Ashton%0A*Here describe the issue or question you have about the VK_VALVE_mutable_descriptor_type extension* >
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_mutable_descriptor_type] @HansKristian-Work%0A*Here describe the issue or question you have about the VK_VALVE_mutable_descriptor_type extension* >
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
--     -   'DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'DESCRIPTOR_TYPE_MUTABLE_VALVE'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE'
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_VALVE_mutable_descriptor_type Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_mutable_descriptor_type  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE
                                                           , pattern STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE
                                                           , pattern DESCRIPTOR_TYPE_MUTABLE_VALVE
                                                           , pattern DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE
                                                           , pattern DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE
                                                           , PhysicalDeviceMutableDescriptorTypeFeaturesVALVE
                                                           , MutableDescriptorTypeListVALVE
                                                           , MutableDescriptorTypeCreateInfoVALVE
                                                           , VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION
                                                           , pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION
                                                           , VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME
                                                           , pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME
                                                           , PhysicalDeviceMutableDescriptorTypeFeaturesEXT(..)
                                                           , MutableDescriptorTypeListEXT(..)
                                                           , MutableDescriptorTypeCreateInfoEXT(..)
                                                           ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_EXT_mutable_descriptor_type (MutableDescriptorTypeCreateInfoEXT)
import Vulkan.Extensions.VK_EXT_mutable_descriptor_type (MutableDescriptorTypeListEXT)
import Vulkan.Extensions.VK_EXT_mutable_descriptor_type (PhysicalDeviceMutableDescriptorTypeFeaturesEXT)
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlags)
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits (DescriptorPoolCreateFlagBits(DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_EXT))
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlagBits(DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_EXT))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_MUTABLE_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT))
import Vulkan.Extensions.VK_EXT_mutable_descriptor_type (MutableDescriptorTypeCreateInfoEXT(..))
import Vulkan.Extensions.VK_EXT_mutable_descriptor_type (MutableDescriptorTypeListEXT(..))
import Vulkan.Extensions.VK_EXT_mutable_descriptor_type (PhysicalDeviceMutableDescriptorTypeFeaturesEXT(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_VALVE = STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE"
pattern STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_VALVE = STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT


-- No documentation found for TopLevel "VK_DESCRIPTOR_TYPE_MUTABLE_VALVE"
pattern DESCRIPTOR_TYPE_MUTABLE_VALVE = DESCRIPTOR_TYPE_MUTABLE_EXT


-- No documentation found for TopLevel "VK_DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE"
pattern DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE = DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_EXT


-- No documentation found for TopLevel "VK_DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE"
pattern DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_VALVE = DESCRIPTOR_SET_LAYOUT_CREATE_HOST_ONLY_POOL_BIT_EXT


-- No documentation found for TopLevel "VkPhysicalDeviceMutableDescriptorTypeFeaturesVALVE"
type PhysicalDeviceMutableDescriptorTypeFeaturesVALVE = PhysicalDeviceMutableDescriptorTypeFeaturesEXT


-- No documentation found for TopLevel "VkMutableDescriptorTypeListVALVE"
type MutableDescriptorTypeListVALVE = MutableDescriptorTypeListEXT


-- No documentation found for TopLevel "VkMutableDescriptorTypeCreateInfoVALVE"
type MutableDescriptorTypeCreateInfoVALVE = MutableDescriptorTypeCreateInfoEXT


type VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION"
pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION :: forall a . Integral a => a
pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_SPEC_VERSION = 1


type VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME = "VK_VALVE_mutable_descriptor_type"

-- No documentation found for TopLevel "VK_VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME"
pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern VALVE_MUTABLE_DESCRIPTOR_TYPE_EXTENSION_NAME = "VK_VALVE_mutable_descriptor_type"

