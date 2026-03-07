{-# language CPP #-}
-- | = Name
--
-- VK_EXT_descriptor_heap - device extension
--
-- = VK_EXT_descriptor_heap
--
-- [__Name String__]
--     @VK_EXT_descriptor_heap@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     136
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_ARM_tensors
--
--     -   Interacts with VK_EXT_custom_border_color
--
--     -   Interacts with VK_EXT_device_generated_commands
--
--     -   Interacts with VK_EXT_fragment_density_map
--
--     -   Interacts with VK_EXT_shader_object
--
--     -   Interacts with VK_KHR_ray_tracing_pipeline
--
--     -   Interacts with VK_NV_device_generated_commands
--
--     -   Interacts with VK_NV_ray_tracing
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_descriptor_heap.html SPV_EXT_descriptor_heap>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_descriptor_heap] @tobski%0A*Here describe the issue or question you have about the VK_EXT_descriptor_heap extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_descriptor_heap.adoc VK_EXT_descriptor_heap>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-06-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Daniel Story, Nintendo
--
--     -   Connor Abbot, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Alyssa Rosenzweig, Valve
--
--     -   Lionel Landerwerlin, Intel
--
--     -   Baldur Karlsson, Valve
--
--     -   Faith Ekstrand, Collabora
--
--     -   Slawomir Grajewski, Intel
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Yiwei Zhang, Google
--
--     -   Stu Smith, AMD
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jon Leech, Khronos
--
--     -   Rodrigo Locatti, NVIDIA
--
--     -   Krzysztof Niski, NVIDIA
--
--     -   Alan Baker, Google
--
--     -   James Fitzpatrick, Imagination
--
--     -   Samuel (Sheng-Wen) Huang, Mediatek
--
--     -   Hai Nguyen, Google
--
--     -   Jeff Leger, Qualcomm
--
--     -   Marty Johnson, Khronos
--
--     -   Tom Olson, Arm
--
--     -   Chris Glover, Google
--
--     -   Daniel Koch, NVIDIA
--
--     -   Robert Simpson, Qualcomm
--
--     -   Dan Ginsburg, Valve
--
--     -   Andreas Süßenbach, NVIDIA
--
--     -   Anna Maniscalco, Valve
--
--     -   Artem Kharytoniuk, LunarG
--
--     -   Younggwan Kim, Arm
--
--     -   Konstantin Seurer, Valve
--
--     -   Catarina Shablia, Collabora
--
--     -   Spencer Fricke, LunarG
--
--     -   Chris Bieneman, Microsoft
--
--     -   Ting Wei, Arm
--
--     -   Boris Zanin, AMD
--
--     -   Samuel Pitoiset, Valve
--
--     -   Erik Hogeman, Arm
--
--     -   Jesse Natalie, Microsoft
--
--     -   Guang Xu, AMD
--
--     -   Embla Flatlandsmo, Arm
--
-- [__Interactions and External Dependencies__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_untyped_pointers VK_KHR_shader_untyped_pointers>
--         must be supported, but it does not need to be enabled for
--         applications using only the
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps-bindings binding interface>.
--
-- == Description
--
-- This extension allows explicit management of descriptors, and the memory
-- used to store descriptors, conceptualised as descriptor heaps.
-- Descriptor heap memory can be accessed as any other memory, enabling
-- management of descriptors on both CPU and the GPU.
--
-- This extension was developed based on issues discovered with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>.
-- There are more constraints on how it is implemented, to provide more
-- portable guarantees and more predictable performance characteristics.
-- For instance, rather than an arbitrary number of sampler or resource
-- buffers, there is exactly one sampler heap and exactly one resource
-- heap.
--
-- This extension also eliminates descriptor sets and pipeline layouts
-- completely; instead applications can look descriptors up solely by their
-- offset into a heap. Shaders still using descriptor set and binding
-- decorations can still be mapped to heaps through an API that maps set
-- and binding decorations to heap offsets, either as constants or through
-- push data. This new mapping API also enables a much more straightforward
-- mapping to HLSL shaders using the root signature and descriptor table
-- interfaces.
--
-- The push constant API has also been replaced with the /push data/
-- interface, which aims to provide much more clarity about how data is
-- passed to the shader, without requiring any mapping information to be
-- provided during pipeline or shader creation. Mappings are still
-- available for shaders written for the legacy interface.
--
-- There is also a much clearer path for mapping shader constant data, with
-- two recommended options for mapping constant data through push data;
-- either directly in push data, or through a device address stored in push
-- data, both of which can be mapped to shaders with set and binding
-- interfaces.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.TensorARM'
--
-- == New Commands
--
-- -   'cmdBindResourceHeapEXT'
--
-- -   'cmdBindSamplerHeapEXT'
--
-- -   'cmdPushDataEXT'
--
-- -   'getImageOpaqueCaptureDataEXT'
--
-- -   'getPhysicalDeviceDescriptorSizeEXT'
--
-- -   'writeResourceDescriptorsEXT'
--
-- -   'writeSamplerDescriptorsEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>
-- is supported:
--
-- -   'getTensorOpaqueCaptureDataARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_border_color VK_EXT_custom_border_color>
-- is supported:
--
-- -   'registerCustomBorderColorEXT'
--
-- -   'unregisterCustomBorderColorEXT'
--
-- == New Structures
--
-- -   'BindHeapInfoEXT'
--
-- -   'DescriptorMappingSourceConstantOffsetEXT'
--
-- -   'DescriptorMappingSourceHeapDataEXT'
--
-- -   'DescriptorMappingSourceIndirectAddressEXT'
--
-- -   'DescriptorMappingSourceIndirectIndexArrayEXT'
--
-- -   'DescriptorMappingSourceIndirectIndexEXT'
--
-- -   'DescriptorMappingSourcePushIndexEXT'
--
-- -   'DescriptorMappingSourceShaderRecordIndexEXT'
--
-- -   'DescriptorSetAndBindingMappingEXT'
--
-- -   'DeviceAddressRangeEXT'
--
-- -   'HostAddressRangeConstEXT'
--
-- -   'HostAddressRangeEXT'
--
-- -   'ImageDescriptorInfoEXT'
--
-- -   'PushDataInfoEXT'
--
-- -   'ResourceDescriptorInfoEXT'
--
-- -   'Vulkan.Extensions.VK_ARM_tensors.TensorViewCreateInfoARM'
--
-- -   'TexelBufferDescriptorInfoEXT'
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'CommandBufferInheritanceDescriptorHeapInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM':
--
--     -   'OpaqueCaptureDataCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorHeapFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDescriptorHeapPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo',
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateInfoEXT':
--
--     -   'ShaderDescriptorSetAndBindingMappingInfoEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDescriptorHeapTensorPropertiesARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_border_color VK_EXT_custom_border_color>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerCustomBorderColorIndexCreateInfoEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2':
--
--     -   'SubsampledImageFormatPropertiesEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV':
--
--     -   'IndirectCommandsLayoutPushDataTokenNV'
--
-- == New Unions
--
-- -   'DescriptorMappingSourceDataEXT'
--
-- -   'ResourceDescriptorDataEXT'
--
-- == New Enums
--
-- -   'DescriptorMappingSourceEXT'
--
-- -   'SpirvResourceTypeFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'SpirvResourceTypeFlagsEXT'
--
-- -   'Vulkan.Extensions.VK_ARM_tensors.TensorViewCreateFlagsARM'
--
-- == New Enum Constants
--
-- -   'EXT_DESCRIPTOR_HEAP_EXTENSION_NAME'
--
-- -   'EXT_DESCRIPTOR_HEAP_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_RESOURCE_HEAP_READ_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SAMPLER_HEAP_READ_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_DESCRIPTOR_HEAP_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_HEAP_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_DESCRIPTOR_HEAP_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_AND_BINDING_MAPPING_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_DESCRIPTOR_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPAQUE_CAPTURE_DATA_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DATA_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RESOURCE_DESCRIPTOR_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_DESCRIPTOR_SET_AND_BINDING_MAPPING_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TEXEL_BUFFER_DESCRIPTOR_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>
-- is supported:
--
-- -   Extending 'SpirvResourceTypeFlagBitsEXT':
--
--     -   'SPIRV_RESOURCE_TYPE_TENSOR_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_TENSOR_PROPERTIES_ARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateFlagBitsARM':
--
--     -   'Vulkan.Extensions.VK_ARM_tensors.TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_border_color VK_EXT_custom_border_color>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_INDEX_CREATE_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.IndirectCommandsTokenTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_EXT'
--
--     -   'Vulkan.Extensions.VK_EXT_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_SEQUENCE_INDEX_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBSAMPLED_IMAGE_FORMAT_PROPERTIES_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagBitsEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   Extending 'DescriptorMappingSourceEXT':
--
--     -   'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT'
--
--     -   'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT'
--
--     -   'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT'
--
-- -   Extending 'SpirvResourceTypeFlagBitsEXT':
--
--     -   'SPIRV_RESOURCE_TYPE_ACCELERATION_STRUCTURE_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsTokenTypeNV':
--
--     -   'Vulkan.Extensions.VK_NV_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_PUSH_DATA_TOKEN_NV'
--
-- == Version History
--
-- -   Revision 1, 2024-06-12 (Tobias Hector)
--
--     -   Initial revision.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_descriptor_heap Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_descriptor_heap  ( BindHeapInfoEXT
                                                 , CommandBufferInheritanceDescriptorHeapInfoEXT
                                                 , DescriptorMappingSourceConstantOffsetEXT
                                                 , DescriptorMappingSourceHeapDataEXT
                                                 , DescriptorMappingSourceIndirectAddressEXT
                                                 , DescriptorMappingSourceIndirectIndexArrayEXT
                                                 , DescriptorMappingSourceIndirectIndexEXT
                                                 , DescriptorMappingSourcePushIndexEXT
                                                 , DescriptorMappingSourceShaderRecordIndexEXT
                                                 , DescriptorSetAndBindingMappingEXT
                                                 , DeviceAddressRangeEXT
                                                 , HostAddressRangeConstEXT
                                                 , HostAddressRangeEXT
                                                 , ImageDescriptorInfoEXT
                                                 , IndirectCommandsLayoutPushDataTokenNV
                                                 , OpaqueCaptureDataCreateInfoEXT
                                                 , PhysicalDeviceDescriptorHeapFeaturesEXT
                                                 , PhysicalDeviceDescriptorHeapPropertiesEXT
                                                 , PhysicalDeviceDescriptorHeapTensorPropertiesARM
                                                 , PushDataInfoEXT
                                                 , ResourceDescriptorInfoEXT
                                                 , SamplerCustomBorderColorIndexCreateInfoEXT
                                                 , ShaderDescriptorSetAndBindingMappingInfoEXT
                                                 , SubsampledImageFormatPropertiesEXT
                                                 , TexelBufferDescriptorInfoEXT
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BindHeapInfoEXT

instance ToCStruct BindHeapInfoEXT
instance Show BindHeapInfoEXT

instance FromCStruct BindHeapInfoEXT


data CommandBufferInheritanceDescriptorHeapInfoEXT

instance ToCStruct CommandBufferInheritanceDescriptorHeapInfoEXT
instance Show CommandBufferInheritanceDescriptorHeapInfoEXT

instance FromCStruct CommandBufferInheritanceDescriptorHeapInfoEXT


data DescriptorMappingSourceConstantOffsetEXT

instance ToCStruct DescriptorMappingSourceConstantOffsetEXT
instance Show DescriptorMappingSourceConstantOffsetEXT

instance FromCStruct DescriptorMappingSourceConstantOffsetEXT


data DescriptorMappingSourceHeapDataEXT

instance ToCStruct DescriptorMappingSourceHeapDataEXT
instance Show DescriptorMappingSourceHeapDataEXT

instance FromCStruct DescriptorMappingSourceHeapDataEXT


data DescriptorMappingSourceIndirectAddressEXT

instance ToCStruct DescriptorMappingSourceIndirectAddressEXT
instance Show DescriptorMappingSourceIndirectAddressEXT

instance FromCStruct DescriptorMappingSourceIndirectAddressEXT


data DescriptorMappingSourceIndirectIndexArrayEXT

instance ToCStruct DescriptorMappingSourceIndirectIndexArrayEXT
instance Show DescriptorMappingSourceIndirectIndexArrayEXT

instance FromCStruct DescriptorMappingSourceIndirectIndexArrayEXT


data DescriptorMappingSourceIndirectIndexEXT

instance ToCStruct DescriptorMappingSourceIndirectIndexEXT
instance Show DescriptorMappingSourceIndirectIndexEXT

instance FromCStruct DescriptorMappingSourceIndirectIndexEXT


data DescriptorMappingSourcePushIndexEXT

instance ToCStruct DescriptorMappingSourcePushIndexEXT
instance Show DescriptorMappingSourcePushIndexEXT

instance FromCStruct DescriptorMappingSourcePushIndexEXT


data DescriptorMappingSourceShaderRecordIndexEXT

instance ToCStruct DescriptorMappingSourceShaderRecordIndexEXT
instance Show DescriptorMappingSourceShaderRecordIndexEXT

instance FromCStruct DescriptorMappingSourceShaderRecordIndexEXT


type role DescriptorSetAndBindingMappingEXT nominal
data DescriptorSetAndBindingMappingEXT (es :: [Type])

instance ( Extendss DescriptorSetAndBindingMappingEXT es
         , PokeChain es ) => ToCStruct (DescriptorSetAndBindingMappingEXT es)
instance Show (Chain es) => Show (DescriptorSetAndBindingMappingEXT es)


data DeviceAddressRangeEXT

instance ToCStruct DeviceAddressRangeEXT
instance Show DeviceAddressRangeEXT

instance FromCStruct DeviceAddressRangeEXT


data HostAddressRangeConstEXT

instance ToCStruct HostAddressRangeConstEXT
instance Show HostAddressRangeConstEXT

instance FromCStruct HostAddressRangeConstEXT


data HostAddressRangeEXT

instance ToCStruct HostAddressRangeEXT
instance Show HostAddressRangeEXT

instance FromCStruct HostAddressRangeEXT


data ImageDescriptorInfoEXT

instance ToCStruct ImageDescriptorInfoEXT
instance Show ImageDescriptorInfoEXT

instance FromCStruct ImageDescriptorInfoEXT


data IndirectCommandsLayoutPushDataTokenNV

instance ToCStruct IndirectCommandsLayoutPushDataTokenNV
instance Show IndirectCommandsLayoutPushDataTokenNV

instance FromCStruct IndirectCommandsLayoutPushDataTokenNV


data OpaqueCaptureDataCreateInfoEXT

instance ToCStruct OpaqueCaptureDataCreateInfoEXT
instance Show OpaqueCaptureDataCreateInfoEXT

instance FromCStruct OpaqueCaptureDataCreateInfoEXT


data PhysicalDeviceDescriptorHeapFeaturesEXT

instance ToCStruct PhysicalDeviceDescriptorHeapFeaturesEXT
instance Show PhysicalDeviceDescriptorHeapFeaturesEXT

instance FromCStruct PhysicalDeviceDescriptorHeapFeaturesEXT


data PhysicalDeviceDescriptorHeapPropertiesEXT

instance ToCStruct PhysicalDeviceDescriptorHeapPropertiesEXT
instance Show PhysicalDeviceDescriptorHeapPropertiesEXT

instance FromCStruct PhysicalDeviceDescriptorHeapPropertiesEXT


data PhysicalDeviceDescriptorHeapTensorPropertiesARM

instance ToCStruct PhysicalDeviceDescriptorHeapTensorPropertiesARM
instance Show PhysicalDeviceDescriptorHeapTensorPropertiesARM

instance FromCStruct PhysicalDeviceDescriptorHeapTensorPropertiesARM


type role PushDataInfoEXT nominal
data PushDataInfoEXT (es :: [Type])

instance ( Extendss PushDataInfoEXT es
         , PokeChain es ) => ToCStruct (PushDataInfoEXT es)
instance Show (Chain es) => Show (PushDataInfoEXT es)

instance ( Extendss PushDataInfoEXT es
         , PeekChain es ) => FromCStruct (PushDataInfoEXT es)


type role ResourceDescriptorInfoEXT nominal
data ResourceDescriptorInfoEXT (es :: [Type])

instance ( Extendss ResourceDescriptorInfoEXT es
         , PokeChain es ) => ToCStruct (ResourceDescriptorInfoEXT es)
instance Show (Chain es) => Show (ResourceDescriptorInfoEXT es)


data SamplerCustomBorderColorIndexCreateInfoEXT

instance ToCStruct SamplerCustomBorderColorIndexCreateInfoEXT
instance Show SamplerCustomBorderColorIndexCreateInfoEXT

instance FromCStruct SamplerCustomBorderColorIndexCreateInfoEXT


data ShaderDescriptorSetAndBindingMappingInfoEXT

instance ToCStruct ShaderDescriptorSetAndBindingMappingInfoEXT
instance Show ShaderDescriptorSetAndBindingMappingInfoEXT


data SubsampledImageFormatPropertiesEXT

instance ToCStruct SubsampledImageFormatPropertiesEXT
instance Show SubsampledImageFormatPropertiesEXT

instance FromCStruct SubsampledImageFormatPropertiesEXT


data TexelBufferDescriptorInfoEXT

instance ToCStruct TexelBufferDescriptorInfoEXT
instance Show TexelBufferDescriptorInfoEXT

instance FromCStruct TexelBufferDescriptorInfoEXT

