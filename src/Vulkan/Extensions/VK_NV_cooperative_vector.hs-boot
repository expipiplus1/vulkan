{-# language CPP #-}
-- | = Name
--
-- VK_NV_cooperative_vector - device extension
--
-- = VK_NV_cooperative_vector
--
-- [__Name String__]
--     @VK_NV_cooperative_vector@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     492
--
-- [__Revision__]
--     4
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_vector.html SPV_NV_cooperative_vector>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_cooperative_vector] @jeffbolznv%0A*Here describe the issue or question you have about the VK_NV_cooperative_vector extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_cooperative_vector.adoc VK_NV_cooperative_vector>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-05-23
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_vector.html SPV_NV_cooperative_vector>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_cooperative_vector.txt GL_NV_cooperative_vector>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds support for using cooperative vector types in
-- SPIR-V. Unlike cooperative matrix types, a variable with a cooperative
-- vector type is logically stored in the invocation it belongs to, but
-- they can cooperate behind the scenes when performing matrix-vector
-- multiplies. Cooperative vectors do not require a fully occupied subgroup
-- or uniform control flow like cooperative matrices, although these do
-- increase the likelihood of being on the fast path. And unlike normal
-- vector types, they have arbitrary length and support a relatively
-- limited set of operations. These types are intended to help accelerate
-- the evaluation of small neural networks, where each invocation is
-- performing its own independent evaluation of the network.
--
-- Cooperative vector types are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_vector.html SPV_NV_cooperative_vector>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_cooperative_vector.txt GL_NV_cooperative_vector>
-- GLSL extension.
--
-- This extension includes support for enumerating the combinations of
-- types that are supported by the implementation, and for converting
-- matrix data to and from an optimized opaque layout.
--
-- == New Commands
--
-- -   'cmdConvertCooperativeVectorMatrixNV'
--
-- -   'convertCooperativeVectorMatrixNV'
--
-- -   'getPhysicalDeviceCooperativeVectorPropertiesNV'
--
-- == New Structures
--
-- -   'ConvertCooperativeVectorMatrixInfoNV'
--
-- -   'CooperativeVectorPropertiesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeVectorFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCooperativeVectorPropertiesNV'
--
-- == New Unions
--
-- -   'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--
-- -   'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR'
--
-- == New Enums
--
-- -   'ComponentTypeKHR'
--
-- -   'CooperativeVectorMatrixLayoutNV'
--
-- == New Enum Constants
--
-- -   'NV_COOPERATIVE_VECTOR_EXTENSION_NAME'
--
-- -   'NV_COOPERATIVE_VECTOR_SPEC_VERSION'
--
-- -   Extending 'ComponentTypeKHR':
--
--     -   'COMPONENT_TYPE_FLOAT_E4M3_NV'
--
--     -   'COMPONENT_TYPE_FLOAT_E5M2_NV'
--
--     -   'COMPONENT_TYPE_SINT8_PACKED_NV'
--
--     -   'COMPONENT_TYPE_UINT8_PACKED_NV'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONVERT_COOPERATIVE_VECTOR_MATRIX_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CONVERT_COOPERATIVE_VECTOR_MATRIX_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COOPERATIVE_VECTOR_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_VECTOR_PROPERTIES_NV'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-CooperativeVectorNV CooperativeVectorNV>
--
-- == Version History
--
-- -   Revision 4, 2024-05-23 (Jeff Bolz)
--
--     -   Add maxCooperativeVectorComponents
--
-- -   Revision 3, 2024-05-23 (Jeff Bolz)
--
--     -   Add training functions
--
-- -   Revision 2, 2024-02-10 (Jeff Bolz)
--
--     -   Add device-side matrix conversion
--
-- -   Revision 1, 2023-12-13 (Jeff Bolz)
--
--     -   Initial revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_cooperative_vector Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_cooperative_vector  ( ConvertCooperativeVectorMatrixInfoNV
                                                   , CooperativeVectorPropertiesNV
                                                   , PhysicalDeviceCooperativeVectorFeaturesNV
                                                   , PhysicalDeviceCooperativeVectorPropertiesNV
                                                   , ComponentTypeKHR
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ConvertCooperativeVectorMatrixInfoNV

instance ToCStruct ConvertCooperativeVectorMatrixInfoNV
instance Show ConvertCooperativeVectorMatrixInfoNV


data CooperativeVectorPropertiesNV

instance ToCStruct CooperativeVectorPropertiesNV
instance Show CooperativeVectorPropertiesNV

instance FromCStruct CooperativeVectorPropertiesNV


data PhysicalDeviceCooperativeVectorFeaturesNV

instance ToCStruct PhysicalDeviceCooperativeVectorFeaturesNV
instance Show PhysicalDeviceCooperativeVectorFeaturesNV

instance FromCStruct PhysicalDeviceCooperativeVectorFeaturesNV


data PhysicalDeviceCooperativeVectorPropertiesNV

instance ToCStruct PhysicalDeviceCooperativeVectorPropertiesNV
instance Show PhysicalDeviceCooperativeVectorPropertiesNV

instance FromCStruct PhysicalDeviceCooperativeVectorPropertiesNV


data ComponentTypeKHR

