{-# language CPP #-}
-- | = Name
--
-- VK_ARM_tensors - device extension
--
-- = VK_ARM_tensors
--
-- [__Name String__]
--     @VK_ARM_tensors@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     461
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_descriptor_buffer
--
--     -   Interacts with VK_EXT_frame_boundary
--
--     -   Interacts with VK_EXT_shader_float8
--
--     -   Interacts with VK_KHR_shader_bfloat16
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/ARM/SPV_ARM_tensors.html SPV_ARM_tensors>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_tensors] @kpet%0A*Here describe the issue or question you have about the VK_ARM_tensors extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ARM_tensors.adoc VK_ARM_tensors>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-01-07
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/ARM/SPV_ARM_tensors.html SPV_ARM_tensors>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/arm/GL_ARM_tensors.txt GL_ARM_tensors>
--
--     -   This extension interacts with @VK_EXT_mutable_descriptor_type@
--
--     -   This extension interacts with @VK_EXT_descriptor_buffer@
--
--     -   This extension interacts with @VK_EXT_frame_boundary@
--
--     -   This extension interacts with @VK_EXT_robustness2@
--
--     -   This extension interacts with @VK_KHR_unified_image_layouts@
--
--     -   This extension interacts with @VK_KHR_shader_bfloat16@
--
--     -   This extension interacts with @VK_EXT_shader_float8@
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Einar Hov, Arm Ltd.
--
--     -   Dominic Symes, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Marco Cattani, Arm Ltd.
--
--     -   Lisa Wu, Arm Ltd.
--
--     -   Robert Hughes, Arm Ltd.
--
--     -   David Garbett, Arm Ltd.
--
--     -   Oualid Khelifi, Arm Ltd.
--
-- == Description
--
-- This extension adds support for tensors.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.TensorARM'
--
-- -   'Vulkan.Extensions.Handles.TensorViewARM'
--
-- == New Commands
--
-- -   'bindTensorMemoryARM'
--
-- -   'cmdCopyTensorARM'
--
-- -   'createTensorARM'
--
-- -   'createTensorViewARM'
--
-- -   'destroyTensorARM'
--
-- -   'destroyTensorViewARM'
--
-- -   'getDeviceTensorMemoryRequirementsARM'
--
-- -   'getPhysicalDeviceExternalTensorPropertiesARM'
--
-- -   'getTensorMemoryRequirementsARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   'getTensorOpaqueCaptureDescriptorDataARM'
--
-- -   'getTensorViewOpaqueCaptureDescriptorDataARM'
--
-- == New Structures
--
-- -   'BindTensorMemoryInfoARM'
--
-- -   'CopyTensorInfoARM'
--
-- -   'DeviceTensorMemoryRequirementsARM'
--
-- -   'ExternalTensorPropertiesARM'
--
-- -   'PhysicalDeviceExternalTensorInfoARM'
--
-- -   'TensorCopyARM'
--
-- -   'TensorCreateInfoARM'
--
-- -   'TensorMemoryRequirementsInfoARM'
--
-- -   'TensorViewCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineResourceInfoARM',
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineConstantARM':
--
--     -   'TensorDescriptionARM'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.DependencyInfo':
--
--     -   'TensorDependencyInfoARM'
--
--     -   'TensorMemoryBarrierARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2':
--
--     -   'TensorFormatPropertiesARM'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'MemoryDedicatedAllocateInfoTensorARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTensorFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTensorPropertiesARM'
--
-- -   Extending 'TensorCreateInfoARM':
--
--     -   'ExternalMemoryTensorCreateInfoARM'
--
-- -   Extending 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet':
--
--     -   'WriteDescriptorSetTensorARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   'TensorCaptureDescriptorDataInfoARM'
--
-- -   'TensorViewCaptureDescriptorDataInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.DescriptorGetInfoEXT':
--
--     -   'DescriptorGetTensorInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorBufferTensorFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDescriptorBufferTensorPropertiesARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.SubmitInfo2',
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR',
--     'Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo':
--
--     -   'FrameBoundaryTensorsARM'
--
-- == New Enums
--
-- -   'TensorCreateFlagBitsARM'
--
-- -   'TensorTilingARM'
--
-- -   'TensorUsageFlagBitsARM'
--
-- -   'TensorViewCreateFlagBitsARM'
--
-- == New Bitmasks
--
-- -   'TensorCreateFlagsARM'
--
-- -   'TensorUsageFlagsARM'
--
-- -   'TensorViewCreateFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_TENSORS_EXTENSION_NAME'
--
-- -   'ARM_TENSORS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R8_BOOL_ARM'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_TENSOR_IMAGE_ALIASING_BIT_ARM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_TENSOR_SHADER_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TENSOR_ALIASING_ARM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TENSOR_ALIASING_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_TENSOR_ARM'
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_TENSOR_VIEW_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_TENSOR_MEMORY_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_TENSOR_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_TENSOR_MEMORY_REQUIREMENTS_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_MEMORY_TENSOR_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_TENSOR_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_TENSOR_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_TENSOR_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TENSOR_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_COPY_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_DEPENDENCY_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_DESCRIPTION_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_FORMAT_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_MEMORY_BARRIER_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_MEMORY_REQUIREMENTS_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_VIEW_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_TENSOR_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_GET_TENSOR_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_TENSOR_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_CAPTURE_DESCRIPTOR_DATA_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TENSOR_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_ARM'
--
-- -   Extending 'TensorCreateFlagBitsARM':
--
--     -   'TENSOR_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM'
--
-- -   Extending 'TensorViewCreateFlagBitsARM':
--
--     -   'TENSOR_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_frame_boundary VK_EXT_frame_boundary>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAME_BOUNDARY_TENSORS_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_float8 VK_EXT_shader_float8>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R8_SFLOAT_FPENCODING_FLOAT8E4M3_ARM'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R8_SFLOAT_FPENCODING_FLOAT8E5M2_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_bfloat16 VK_KHR_shader_bfloat16>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R16_SFLOAT_FPENCODING_BFLOAT16_ARM'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-TensorsARM TensorsARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-StorageTensorArrayDynamicIndexingARM StorageTensorArrayDynamicIndexingARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-StorageTensorArrayNonUniformIndexingARM StorageTensorArrayNonUniformIndexingARM>
--
-- == Issues
--
-- 1) Should tensor strides be passed in elements or in bytes?
--
-- __RESOLVED__: Strides are passed in bytes but are required to be a
-- multiple of the tensor element size. Passing strides in bytes makes it
-- possible to relax this requirement in the future without an interface
-- change. It also makes it easier to describe memory alignment
-- requirements.
--
-- 2) Should there be commands to copy data between tensors and
-- buffers\/images?
--
-- __RESOLVED__: Adding these commands would result in a rather large API
-- surface and not insignificant implementation and validation cost. The
-- same outcome can be achieved with memory aliasing and tensor to tensor
-- copy operations.
--
-- 3) Should this extension define transpose and\/or other data
-- reorganization operations?
--
-- __RESOLVED__: These operations are useful to expose but this extension
-- is only meant to add base support for tensors. Additional operations
-- should be layered on top and defined in other extensions.
--
-- 4) Why are tensor strides described using signed integers?
--
-- __RESOLVED__: Negative strides make it possible to describe different
-- linear data layouts. While this extension does not allow negative
-- strides, it uses signed integers for strides to make it possible to
-- relax this limitation in future extensions.
--
-- == Version History
--
-- -   Revision 2, 2026-01-07 (Kévin Petit)
--
--     -   Add interactions with VK_KHR_unified_image_layouts,
--         VK_KHR_shader_bfloat16, and VK_EXT_shader_float8
--
-- -   Revision 1, 2025-06-03 (Kévin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_tensors Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_tensors  ( BindTensorMemoryInfoARM
                                         , CopyTensorInfoARM
                                         , DescriptorGetTensorInfoARM
                                         , DeviceTensorMemoryRequirementsARM
                                         , ExternalMemoryTensorCreateInfoARM
                                         , ExternalTensorPropertiesARM
                                         , FrameBoundaryTensorsARM
                                         , MemoryDedicatedAllocateInfoTensorARM
                                         , PhysicalDeviceDescriptorBufferTensorFeaturesARM
                                         , PhysicalDeviceDescriptorBufferTensorPropertiesARM
                                         , PhysicalDeviceExternalTensorInfoARM
                                         , PhysicalDeviceTensorFeaturesARM
                                         , PhysicalDeviceTensorPropertiesARM
                                         , TensorCaptureDescriptorDataInfoARM
                                         , TensorCopyARM
                                         , TensorCreateInfoARM
                                         , TensorDependencyInfoARM
                                         , TensorDescriptionARM
                                         , TensorFormatPropertiesARM
                                         , TensorMemoryBarrierARM
                                         , TensorMemoryRequirementsInfoARM
                                         , TensorViewCaptureDescriptorDataInfoARM
                                         , TensorViewCreateInfoARM
                                         , WriteDescriptorSetTensorARM
                                         ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BindTensorMemoryInfoARM

instance ToCStruct BindTensorMemoryInfoARM
instance Show BindTensorMemoryInfoARM

instance FromCStruct BindTensorMemoryInfoARM


data CopyTensorInfoARM

instance ToCStruct CopyTensorInfoARM
instance Show CopyTensorInfoARM

instance FromCStruct CopyTensorInfoARM


data DescriptorGetTensorInfoARM

instance ToCStruct DescriptorGetTensorInfoARM
instance Show DescriptorGetTensorInfoARM

instance FromCStruct DescriptorGetTensorInfoARM


data DeviceTensorMemoryRequirementsARM

instance ToCStruct DeviceTensorMemoryRequirementsARM
instance Show DeviceTensorMemoryRequirementsARM

instance FromCStruct DeviceTensorMemoryRequirementsARM


data ExternalMemoryTensorCreateInfoARM

instance ToCStruct ExternalMemoryTensorCreateInfoARM
instance Show ExternalMemoryTensorCreateInfoARM

instance FromCStruct ExternalMemoryTensorCreateInfoARM


data ExternalTensorPropertiesARM

instance ToCStruct ExternalTensorPropertiesARM
instance Show ExternalTensorPropertiesARM

instance FromCStruct ExternalTensorPropertiesARM


data FrameBoundaryTensorsARM

instance ToCStruct FrameBoundaryTensorsARM
instance Show FrameBoundaryTensorsARM

instance FromCStruct FrameBoundaryTensorsARM


data MemoryDedicatedAllocateInfoTensorARM

instance ToCStruct MemoryDedicatedAllocateInfoTensorARM
instance Show MemoryDedicatedAllocateInfoTensorARM

instance FromCStruct MemoryDedicatedAllocateInfoTensorARM


data PhysicalDeviceDescriptorBufferTensorFeaturesARM

instance ToCStruct PhysicalDeviceDescriptorBufferTensorFeaturesARM
instance Show PhysicalDeviceDescriptorBufferTensorFeaturesARM

instance FromCStruct PhysicalDeviceDescriptorBufferTensorFeaturesARM


data PhysicalDeviceDescriptorBufferTensorPropertiesARM

instance ToCStruct PhysicalDeviceDescriptorBufferTensorPropertiesARM
instance Show PhysicalDeviceDescriptorBufferTensorPropertiesARM

instance FromCStruct PhysicalDeviceDescriptorBufferTensorPropertiesARM


data PhysicalDeviceExternalTensorInfoARM

instance ToCStruct PhysicalDeviceExternalTensorInfoARM
instance Show PhysicalDeviceExternalTensorInfoARM

instance FromCStruct PhysicalDeviceExternalTensorInfoARM


data PhysicalDeviceTensorFeaturesARM

instance ToCStruct PhysicalDeviceTensorFeaturesARM
instance Show PhysicalDeviceTensorFeaturesARM

instance FromCStruct PhysicalDeviceTensorFeaturesARM


data PhysicalDeviceTensorPropertiesARM

instance ToCStruct PhysicalDeviceTensorPropertiesARM
instance Show PhysicalDeviceTensorPropertiesARM

instance FromCStruct PhysicalDeviceTensorPropertiesARM


data TensorCaptureDescriptorDataInfoARM

instance ToCStruct TensorCaptureDescriptorDataInfoARM
instance Show TensorCaptureDescriptorDataInfoARM

instance FromCStruct TensorCaptureDescriptorDataInfoARM


data TensorCopyARM

instance ToCStruct TensorCopyARM
instance Show TensorCopyARM

instance FromCStruct TensorCopyARM


type role TensorCreateInfoARM nominal
data TensorCreateInfoARM (es :: [Type])

instance ( Extendss TensorCreateInfoARM es
         , PokeChain es ) => ToCStruct (TensorCreateInfoARM es)
instance Show (Chain es) => Show (TensorCreateInfoARM es)

instance ( Extendss TensorCreateInfoARM es
         , PeekChain es ) => FromCStruct (TensorCreateInfoARM es)


data TensorDependencyInfoARM

instance ToCStruct TensorDependencyInfoARM
instance Show TensorDependencyInfoARM

instance FromCStruct TensorDependencyInfoARM


data TensorDescriptionARM

instance ToCStruct TensorDescriptionARM
instance Show TensorDescriptionARM

instance FromCStruct TensorDescriptionARM


data TensorFormatPropertiesARM

instance ToCStruct TensorFormatPropertiesARM
instance Show TensorFormatPropertiesARM

instance FromCStruct TensorFormatPropertiesARM


data TensorMemoryBarrierARM

instance ToCStruct TensorMemoryBarrierARM
instance Show TensorMemoryBarrierARM

instance FromCStruct TensorMemoryBarrierARM


data TensorMemoryRequirementsInfoARM

instance ToCStruct TensorMemoryRequirementsInfoARM
instance Show TensorMemoryRequirementsInfoARM

instance FromCStruct TensorMemoryRequirementsInfoARM


data TensorViewCaptureDescriptorDataInfoARM

instance ToCStruct TensorViewCaptureDescriptorDataInfoARM
instance Show TensorViewCaptureDescriptorDataInfoARM

instance FromCStruct TensorViewCaptureDescriptorDataInfoARM


type role TensorViewCreateInfoARM nominal
data TensorViewCreateInfoARM (es :: [Type])

instance ( Extendss TensorViewCreateInfoARM es
         , PokeChain es ) => ToCStruct (TensorViewCreateInfoARM es)
instance Show (Chain es) => Show (TensorViewCreateInfoARM es)

instance ( Extendss TensorViewCreateInfoARM es
         , PeekChain es ) => FromCStruct (TensorViewCreateInfoARM es)


data WriteDescriptorSetTensorARM

instance ToCStruct WriteDescriptorSetTensorARM
instance Show WriteDescriptorSetTensorARM

instance FromCStruct WriteDescriptorSetTensorARM

