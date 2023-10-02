{-# language CPP #-}
-- | = Name
--
-- VK_VALVE_descriptor_set_host_mapping - device extension
--
-- == VK_VALVE_descriptor_set_host_mapping
--
-- [__Name String__]
--     @VK_VALVE_descriptor_set_host_mapping@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     421
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_descriptor_set_host_mapping] @HansKristian-Work%0A*Here describe the issue or question you have about the VK_VALVE_descriptor_set_host_mapping extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-02-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- This extension allows applications to directly query a host pointer for
-- a 'Vulkan.Core10.Handles.DescriptorSet' which /can/ be used to copy
-- descriptors between descriptor sets without the use of an API command.
-- Memory offsets and sizes for descriptors /can/ be queried from a
-- 'Vulkan.Core10.Handles.DescriptorSetLayout' as well.
--
-- Note
--
-- There is currently no specification language written for this extension.
-- The links to APIs defined by the extension are to stubs that only
-- include generated content such as API declarations and implicit valid
-- usage statements.
--
-- Note
--
-- This extension is only intended for use in specific embedded
-- environments with known implementation details, and is therefore
-- undocumented.
--
-- == New Commands
--
-- -   'getDescriptorSetHostMappingVALVE'
--
-- -   'getDescriptorSetLayoutHostMappingInfoVALVE'
--
-- == New Structures
--
-- -   'DescriptorSetBindingReferenceVALVE'
--
-- -   'DescriptorSetLayoutHostMappingInfoVALVE'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE'
--
-- == New Enum Constants
--
-- -   'VALVE_DESCRIPTOR_SET_HOST_MAPPING_EXTENSION_NAME'
--
-- -   'VALVE_DESCRIPTOR_SET_HOST_MAPPING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_BINDING_REFERENCE_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_HOST_MAPPING_INFO_VALVE'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE'
--
-- == Stub API References
--
-- There is currently no specification language written for this command.
-- This section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > void vkGetDescriptorSetLayoutHostMappingInfoVALVE(
-- >     VkDevice                                    device,
-- >     const VkDescriptorSetBindingReferenceVALVE* pBindingReference,
-- >     VkDescriptorSetLayoutHostMappingInfoVALVE*  pHostMapping);
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-vkGetDescriptorSetLayoutHostMappingInfoVALVE-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDescriptorSetLayoutHostMappingInfoVALVE-pBindingReference-parameter#
--     @pBindingReference@ /must/ be a valid pointer to a valid
--     'DescriptorSetBindingReferenceVALVE' structure
--
-- -   #VUID-vkGetDescriptorSetLayoutHostMappingInfoVALVE-pHostMapping-parameter#
--     @pHostMapping@ /must/ be a valid pointer to a
--     'DescriptorSetLayoutHostMappingInfoVALVE' structure
--
-- There is currently no specification language written for this command.
-- This section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > void vkGetDescriptorSetHostMappingVALVE(
-- >     VkDevice                                    device,
-- >     VkDescriptorSet                             descriptorSet,
-- >     void**                                      ppData);
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-vkGetDescriptorSetHostMappingVALVE-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDescriptorSetHostMappingVALVE-descriptorSet-parameter#
--     @descriptorSet@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSet' handle
--
-- -   #VUID-vkGetDescriptorSetHostMappingVALVE-ppData-parameter# @ppData@
--     /must/ be a valid pointer to a pointer value
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > typedef struct VkPhysicalDeviceDescriptorSetHostMappingFeaturesVALVE {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     VkBool32           descriptorSetHostMapping;
-- > } VkPhysicalDeviceDescriptorSetHostMappingFeaturesVALVE;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceDescriptorSetHostMappingFeaturesVALVE-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE'
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > typedef struct VkDescriptorSetBindingReferenceVALVE {
-- >     VkStructureType          sType;
-- >     const void*              pNext;
-- >     VkDescriptorSetLayout    descriptorSetLayout;
-- >     uint32_t                 binding;
-- > } VkDescriptorSetBindingReferenceVALVE;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorSetBindingReferenceVALVE-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_BINDING_REFERENCE_VALVE'
--
-- -   #VUID-VkDescriptorSetBindingReferenceVALVE-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkDescriptorSetBindingReferenceVALVE-descriptorSetLayout-parameter#
--     @descriptorSetLayout@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSetLayout' handle
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_VALVE_descriptor_set_host_mapping
-- > typedef struct VkDescriptorSetLayoutHostMappingInfoVALVE {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     size_t             descriptorOffset;
-- >     uint32_t           descriptorSize;
-- > } VkDescriptorSetLayoutHostMappingInfoVALVE;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorSetLayoutHostMappingInfoVALVE-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_HOST_MAPPING_INFO_VALVE'
--
-- -   #VUID-VkDescriptorSetLayoutHostMappingInfoVALVE-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- == Version History
--
-- -   Revision 1, 2022-02-22 (Hans-Kristian Arntzen)
--
--     -   Initial specification
--
-- == See Also
--
-- 'DescriptorSetBindingReferenceVALVE',
-- 'DescriptorSetLayoutHostMappingInfoVALVE',
-- 'PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE',
-- 'getDescriptorSetHostMappingVALVE',
-- 'getDescriptorSetLayoutHostMappingInfoVALVE'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_VALVE_descriptor_set_host_mapping Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping  ( DescriptorSetBindingReferenceVALVE
                                                               , DescriptorSetLayoutHostMappingInfoVALVE
                                                               , PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE
                                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DescriptorSetBindingReferenceVALVE

instance ToCStruct DescriptorSetBindingReferenceVALVE
instance Show DescriptorSetBindingReferenceVALVE

instance FromCStruct DescriptorSetBindingReferenceVALVE


data DescriptorSetLayoutHostMappingInfoVALVE

instance ToCStruct DescriptorSetLayoutHostMappingInfoVALVE
instance Show DescriptorSetLayoutHostMappingInfoVALVE

instance FromCStruct DescriptorSetLayoutHostMappingInfoVALVE


data PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE

instance ToCStruct PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE
instance Show PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE

instance FromCStruct PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE

