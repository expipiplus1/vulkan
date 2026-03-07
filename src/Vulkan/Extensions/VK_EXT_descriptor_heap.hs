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
module Vulkan.Extensions.VK_EXT_descriptor_heap  ( writeSamplerDescriptorsEXT
                                                 , writeResourceDescriptorsEXT
                                                 , cmdBindSamplerHeapEXT
                                                 , cmdBindResourceHeapEXT
                                                 , cmdPushDataEXT
                                                 , registerCustomBorderColorEXT
                                                 , unregisterCustomBorderColorEXT
                                                 , getImageOpaqueCaptureDataEXT
                                                 , getPhysicalDeviceDescriptorSizeEXT
                                                 , getTensorOpaqueCaptureDataARM
                                                 , HostAddressRangeEXT(..)
                                                 , HostAddressRangeConstEXT(..)
                                                 , DeviceAddressRangeEXT(..)
                                                 , TexelBufferDescriptorInfoEXT(..)
                                                 , ImageDescriptorInfoEXT(..)
                                                 , ResourceDescriptorInfoEXT(..)
                                                 , BindHeapInfoEXT(..)
                                                 , PushDataInfoEXT(..)
                                                 , DescriptorMappingSourceConstantOffsetEXT(..)
                                                 , DescriptorMappingSourcePushIndexEXT(..)
                                                 , DescriptorMappingSourceIndirectIndexEXT(..)
                                                 , DescriptorMappingSourceIndirectIndexArrayEXT(..)
                                                 , DescriptorMappingSourceHeapDataEXT(..)
                                                 , DescriptorMappingSourceShaderRecordIndexEXT(..)
                                                 , DescriptorMappingSourceIndirectAddressEXT(..)
                                                 , DescriptorSetAndBindingMappingEXT(..)
                                                 , ShaderDescriptorSetAndBindingMappingInfoEXT(..)
                                                 , SamplerCustomBorderColorIndexCreateInfoEXT(..)
                                                 , OpaqueCaptureDataCreateInfoEXT(..)
                                                 , IndirectCommandsLayoutPushDataTokenNV(..)
                                                 , SubsampledImageFormatPropertiesEXT(..)
                                                 , PhysicalDeviceDescriptorHeapFeaturesEXT(..)
                                                 , PhysicalDeviceDescriptorHeapPropertiesEXT(..)
                                                 , CommandBufferInheritanceDescriptorHeapInfoEXT(..)
                                                 , PhysicalDeviceDescriptorHeapTensorPropertiesARM(..)
                                                 , ResourceDescriptorDataEXT(..)
                                                 , DescriptorMappingSourceDataEXT(..)
                                                 , DescriptorMappingSourceEXT( DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT
                                                                             , DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT
                                                                             , ..
                                                                             )
                                                 , SpirvResourceTypeFlagsEXT
                                                 , SpirvResourceTypeFlagBitsEXT( SPIRV_RESOURCE_TYPE_ALL_EXT
                                                                               , SPIRV_RESOURCE_TYPE_SAMPLER_BIT_EXT
                                                                               , SPIRV_RESOURCE_TYPE_SAMPLED_IMAGE_BIT_EXT
                                                                               , SPIRV_RESOURCE_TYPE_READ_ONLY_IMAGE_BIT_EXT
                                                                               , SPIRV_RESOURCE_TYPE_READ_WRITE_IMAGE_BIT_EXT
                                                                               , SPIRV_RESOURCE_TYPE_COMBINED_SAMPLED_IMAGE_BIT_EXT
                                                                               , SPIRV_RESOURCE_TYPE_UNIFORM_BUFFER_BIT_EXT
                                                                               , SPIRV_RESOURCE_TYPE_READ_ONLY_STORAGE_BUFFER_BIT_EXT
                                                                               , SPIRV_RESOURCE_TYPE_READ_WRITE_STORAGE_BUFFER_BIT_EXT
                                                                               , SPIRV_RESOURCE_TYPE_TENSOR_BIT_ARM
                                                                               , SPIRV_RESOURCE_TYPE_ACCELERATION_STRUCTURE_BIT_EXT
                                                                               , ..
                                                                               )
                                                 , EXT_DESCRIPTOR_HEAP_SPEC_VERSION
                                                 , pattern EXT_DESCRIPTOR_HEAP_SPEC_VERSION
                                                 , EXT_DESCRIPTOR_HEAP_EXTENSION_NAME
                                                 , pattern EXT_DESCRIPTOR_HEAP_EXTENSION_NAME
                                                 , TensorARM(..)
                                                 , SamplerCustomBorderColorCreateInfoEXT(..)
                                                 , TensorViewCreateInfoARM(..)
                                                 , IndirectCommandsTokenTypeNV(..)
                                                 , IndirectCommandsTokenTypeEXT(..)
                                                 , ShaderCreateFlagBitsEXT(..)
                                                 , ShaderCreateFlagsEXT
                                                 , TensorCreateFlagBitsARM(..)
                                                 , TensorCreateFlagsARM
                                                 , TensorViewCreateFlagBitsARM(..)
                                                 , TensorViewCreateFlagsARM
                                                 ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsObjectNameInfoEXT)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindResourceHeapEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindSamplerHeapEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageOpaqueCaptureDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetTensorOpaqueCaptureDataARM))
import Vulkan.Dynamic (DeviceCmds(pVkRegisterCustomBorderColorEXT))
import Vulkan.Dynamic (DeviceCmds(pVkUnregisterCustomBorderColorEXT))
import Vulkan.Dynamic (DeviceCmds(pVkWriteResourceDescriptorsEXT))
import Vulkan.Dynamic (DeviceCmds(pVkWriteSamplerDescriptorsEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.ImageView (ImageViewCreateInfo)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceDescriptorSizeEXT))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_push_constant_bank (PushConstantBankInfoNV)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Sampler (SamplerCreateInfo)
import Vulkan.Extensions.VK_EXT_custom_border_color (SamplerCustomBorderColorCreateInfoEXT)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (TensorARM)
import Vulkan.Extensions.Handles (TensorARM(..))
import Vulkan.Extensions.VK_ARM_tensors (TensorViewCreateInfoARM)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_HEAP_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_DESCRIPTOR_HEAP_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_AND_BINDING_MAPPING_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_DESCRIPTOR_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_PUSH_DATA_TOKEN_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_OPAQUE_CAPTURE_DATA_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_TENSOR_PROPERTIES_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PUSH_DATA_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RESOURCE_DESCRIPTOR_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_INDEX_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHADER_DESCRIPTOR_SET_AND_BINDING_MAPPING_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBSAMPLED_IMAGE_FORMAT_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_TEXEL_BUFFER_DESCRIPTOR_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_EXT_device_generated_commands (IndirectCommandsTokenTypeEXT(..))
import Vulkan.Extensions.VK_NV_device_generated_commands (IndirectCommandsTokenTypeNV(..))
import Vulkan.Extensions.VK_EXT_custom_border_color (SamplerCustomBorderColorCreateInfoEXT(..))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagsEXT)
import Vulkan.Extensions.Handles (TensorARM(..))
import Vulkan.Extensions.VK_ARM_tensors (TensorCreateFlagBitsARM(..))
import Vulkan.Extensions.VK_ARM_tensors (TensorCreateFlagsARM)
import Vulkan.Extensions.VK_ARM_tensors (TensorViewCreateFlagBitsARM(..))
import Vulkan.Extensions.VK_ARM_tensors (TensorViewCreateFlagsARM)
import Vulkan.Extensions.VK_ARM_tensors (TensorViewCreateInfoARM(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWriteSamplerDescriptorsEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct SamplerCreateInfo) -> Ptr HostAddressRangeEXT -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct SamplerCreateInfo) -> Ptr HostAddressRangeEXT -> IO Result

-- | vkWriteSamplerDescriptorsEXT - Write sampler descriptors to memory
--
-- = Description
--
-- Each descriptor will be written to @pDescriptors@[i].@address@ where i
-- is the index of its create info in @pSamplers@.
--
-- Descriptors written using a fully identical
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structure on the same
-- 'Vulkan.Core10.Handles.Device' will always return the same bit pattern.
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorHeapCaptureReplay descriptorHeapCaptureReplay>
-- feature is enabled, descriptors written using a fully identical
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structure on a
-- 'Vulkan.Core10.Handles.Device' created from the same
-- 'Vulkan.Core10.Handles.PhysicalDevice' with identical parameters will
-- always return the same bit pattern.
--
-- YCBCR samplers must be embedded in a shader by using
-- 'ShaderDescriptorSetAndBindingMappingInfoEXT', they cannot be specified
-- here.
--
-- == Valid Usage
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-descriptorHeap-11202# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorHeap descriptorHeap>
--     feature /must/ be enabled
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-size-11203# The @size@ member of
--     each element of @pDescriptors@ /must/ be greater than or equal to
--     the value returned by 'getPhysicalDeviceDescriptorSizeEXT' with a
--     @descriptorType@ equal to
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLER'
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-pSamplers-11204# Elements of
--     @pSamplers@ /must/ not include
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     structures in their @pNext@ chains
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-borderColor-11444# If the
--     @borderColor@ of any element of @pSamplers@ is
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT',
--     'SamplerCustomBorderColorIndexCreateInfoEXT' /must/ be included in
--     the @pNext@ chain of that element
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-borderColor-11205# If the
--     @borderColor@ of any element of @pSamplers@ is
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT',
--     'SamplerCustomBorderColorIndexCreateInfoEXT'::@index@ /must/ be a
--     value less than
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxCustomBorderColorSamplers maxCustomBorderColorSamplers>
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-pNext-11400# If there is a
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
--     structure in the @pNext@ chain of any element of @pSamplers@, its
--     @objectType@ /must/ be
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-pSamplers-parameter# @pSamplers@
--     /must/ be a valid pointer to an array of @samplerCount@ valid
--     'Vulkan.Core10.Sampler.SamplerCreateInfo' structures
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-pDescriptors-parameter#
--     @pDescriptors@ /must/ be a valid pointer to an array of
--     @samplerCount@ valid 'HostAddressRangeEXT' structures
--
-- -   #VUID-vkWriteSamplerDescriptorsEXT-samplerCount-arraylength#
--     @samplerCount@ /must/ be greater than @0@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Handles.Device', 'HostAddressRangeEXT',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
writeSamplerDescriptorsEXT :: forall io
                            . (MonadIO io)
                           => -- | @device@ is the logical device that the descriptors are for.
                              Device
                           -> -- | @pSamplers@ is a pointer to an array of
                              -- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structures defining properties
                              -- of the sampler descriptors that will be written.
                              ("samplers" ::: Vector (SomeStruct SamplerCreateInfo))
                           -> -- | @pDescriptors@ is a pointer to an array of 'HostAddressRangeEXT'
                              -- structures defining the host address ranges that will be written to for
                              -- each descriptor.
                              ("descriptors" ::: Vector HostAddressRangeEXT)
                           -> io ()
writeSamplerDescriptorsEXT device samplers descriptors = liftIO . evalContT $ do
  let vkWriteSamplerDescriptorsEXTPtr = pVkWriteSamplerDescriptorsEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkWriteSamplerDescriptorsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWriteSamplerDescriptorsEXT is null" Nothing Nothing
  let vkWriteSamplerDescriptorsEXT' = mkVkWriteSamplerDescriptorsEXT vkWriteSamplerDescriptorsEXTPtr
  let pSamplersLength = Data.Vector.length $ (samplers)
  lift $ unless ((Data.Vector.length $ (descriptors)) == pSamplersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pDescriptors and pSamplers must have the same length" Nothing Nothing
  pPSamplers <- ContT $ allocaBytes @(SamplerCreateInfo _) ((Data.Vector.length (samplers)) * 80)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPSamplers `plusPtr` (80 * (i)) :: Ptr (SamplerCreateInfo _))) (e) . ($ ())) (samplers)
  pPDescriptors <- ContT $ allocaBytes @HostAddressRangeEXT ((Data.Vector.length (descriptors)) * 16)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptors `plusPtr` (16 * (i)) :: Ptr HostAddressRangeEXT) (e)) (descriptors)
  r <- lift $ traceAroundEvent "vkWriteSamplerDescriptorsEXT" (vkWriteSamplerDescriptorsEXT'
                                                                 (deviceHandle (device))
                                                                 ((fromIntegral pSamplersLength :: Word32))
                                                                 (forgetExtensions (pPSamplers))
                                                                 (pPDescriptors))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWriteResourceDescriptorsEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct ResourceDescriptorInfoEXT) -> Ptr HostAddressRangeEXT -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct ResourceDescriptorInfoEXT) -> Ptr HostAddressRangeEXT -> IO Result

-- | vkWriteResourceDescriptorsEXT - Write resource descriptors to memory
--
-- = Description
--
-- Each descriptor will be written to @pDescriptors@[i].address where i is
-- the index of its create info in @pResources@.
--
-- If any image descriptor written by this command includes a
-- 'Vulkan.Core11.Handles.SamplerYcbcrConversion', multiple descriptors
-- will be written adjacent to each other for that descriptor, equal to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionImageFormatProperties'::combinedImageSamplerDescriptorCount
-- for the image.
--
-- If any image descriptor written by this command is for an image created
-- with @flags@ containing
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
-- multiple descriptors will be written adjacent to each other for that
-- descriptor, equal to
-- 'SubsampledImageFormatPropertiesEXT'::subsampledImageDescriptorCount for
-- the image.
--
-- Descriptors using the same @type@ and written using a fully identical
-- 'TexelBufferDescriptorInfoEXT' or 'DeviceAddressRangeEXT' structure on
-- the same 'Vulkan.Core10.Handles.Device' will always return the same bit
-- pattern. If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorHeapCaptureReplay descriptorHeapCaptureReplay>
-- feature is enabled, this applies to any 'Vulkan.Core10.Handles.Device'
-- created with identical parameters from the same
-- 'Vulkan.Core10.Handles.PhysicalDevice'.
--
-- Recreating the same buffer descriptor during replay of a prior capture
-- requires that the device address is the same, which requires additional
-- data to be captured and provided during replay when creating a buffer
-- and allocating memory for it.
--
-- Image descriptors using the same @type@ and written using a fully
-- identical 'ImageDescriptorInfoEXT' other than
-- 'ImageDescriptorInfoEXT'::@pView->image@, where image was successfully
-- created with
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_EXT'
-- and a 'OpaqueCaptureDataCreateInfoEXT' with data captured via
-- 'getImageOpaqueCaptureDataEXT' from an image used previously, will write
-- a descriptor with the same bit pattern if possible; if the same bit
-- pattern cannot be generated,
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS' will
-- be returned instead.
--
-- Tensor descriptors using the same @type@ and written using a fully
-- identical 'Vulkan.Extensions.VK_ARM_tensors.TensorViewCreateInfoARM'
-- other than
-- 'Vulkan.Extensions.VK_ARM_tensors.TensorViewCreateInfoARM'::@tensor@,
-- where tensor was successfully created with
-- 'OpaqueCaptureDataCreateInfoEXT' with
-- 'Vulkan.Extensions.VK_ARM_tensors.TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM'
-- and a 'OpaqueCaptureDataCreateInfoEXT' with data captured via
-- 'getTensorOpaqueCaptureDataARM' from a tensor used previously, will
-- write a descriptor with the same bit pattern if possible; if the same
-- bit pattern cannot be generated,
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS' will
-- be returned instead.
--
-- Image creation is sufficiently complex that it may not be possible to
-- recreate all possible descriptors from an image during replay, even if
-- the image itself was successfully recreated. The conditions for this
-- happening will be largely the same as those which could cause allocating
-- a buffer with the same device address during replay to fail. Replay
-- tools are advised to recreate captured descriptors for an image
-- immediately after recreating the image itself wherever possible. The
-- same is true for tensors.
--
-- == Valid Usage
--
-- -   #VUID-vkWriteResourceDescriptorsEXT-descriptorHeap-11206# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorHeap descriptorHeap>
--     feature /must/ be enabled
--
-- -   #VUID-vkWriteResourceDescriptorsEXT-size-11207# The @size@ member of
--     each element of @pDescriptors@ /must/ be greater than or equal to
--     the value returned by 'getPhysicalDeviceDescriptorSizeEXT' with a
--     @descriptorType@ equal to @type@
--
-- -   #VUID-vkWriteResourceDescriptorsEXT-pResources-11208# If any element
--     of @pResources@ specifies a
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo' structure with a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo'
--     structure in its @pNext@ chain, the corresponding element of
--     @pDescriptors@ /must/ have a @size@ member that is greater than or
--     equal to the product of the value returned by
--     'getPhysicalDeviceDescriptorSizeEXT' with a @descriptorType@ equal
--     to @type@ and
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionImageFormatProperties'::combinedImageSamplerDescriptorCount,
--     as queried from
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--     with image format info equivalent to the image view the descriptor
--     is being created for
--
-- -   #VUID-vkWriteResourceDescriptorsEXT-pResources-11209# If any element
--     of @pResources@ specifies a
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo' structure with an
--     @image@ created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT',
--     the corresponding element of @pDescriptors@ /must/ have a @size@
--     member that is greater than or equal to the product of the value
--     returned by 'getPhysicalDeviceDescriptorSizeEXT' with a
--     @descriptorType@ equal to @type@ and
--     'SubsampledImageFormatPropertiesEXT'::subsampledImageDescriptorCount,
--     as queried from
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
--     with image format info equivalent to the image view the descriptor
--     is being created for
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkWriteResourceDescriptorsEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkWriteResourceDescriptorsEXT-pResources-parameter#
--     @pResources@ /must/ be a valid pointer to an array of
--     @resourceCount@ valid 'ResourceDescriptorInfoEXT' structures
--
-- -   #VUID-vkWriteResourceDescriptorsEXT-pDescriptors-parameter#
--     @pDescriptors@ /must/ be a valid pointer to an array of
--     @resourceCount@ valid 'HostAddressRangeEXT' structures
--
-- -   #VUID-vkWriteResourceDescriptorsEXT-resourceCount-arraylength#
--     @resourceCount@ /must/ be greater than @0@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Handles.Device', 'HostAddressRangeEXT',
-- 'ResourceDescriptorInfoEXT'
writeResourceDescriptorsEXT :: forall io
                             . (MonadIO io)
                            => -- | @device@ is the logical device that the descriptors are for.
                               Device
                            -> -- | @pResources@ is a pointer to an array of 'ResourceDescriptorInfoEXT'
                               -- structures defining properties of the resource descriptors that will be
                               -- written.
                               ("resources" ::: Vector (SomeStruct ResourceDescriptorInfoEXT))
                            -> -- | @pDescriptors@ is a pointer to an array of 'HostAddressRangeEXT'
                               -- structures defining the host address ranges that will be written to for
                               -- each descriptor.
                               ("descriptors" ::: Vector HostAddressRangeEXT)
                            -> io ()
writeResourceDescriptorsEXT device
                              resources
                              descriptors = liftIO . evalContT $ do
  let vkWriteResourceDescriptorsEXTPtr = pVkWriteResourceDescriptorsEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkWriteResourceDescriptorsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWriteResourceDescriptorsEXT is null" Nothing Nothing
  let vkWriteResourceDescriptorsEXT' = mkVkWriteResourceDescriptorsEXT vkWriteResourceDescriptorsEXTPtr
  let pResourcesLength = Data.Vector.length $ (resources)
  lift $ unless ((Data.Vector.length $ (descriptors)) == pResourcesLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pDescriptors and pResources must have the same length" Nothing Nothing
  pPResources <- ContT $ allocaBytes @(ResourceDescriptorInfoEXT _) ((Data.Vector.length (resources)) * 32)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPResources `plusPtr` (32 * (i)) :: Ptr (ResourceDescriptorInfoEXT _))) (e) . ($ ())) (resources)
  pPDescriptors <- ContT $ allocaBytes @HostAddressRangeEXT ((Data.Vector.length (descriptors)) * 16)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptors `plusPtr` (16 * (i)) :: Ptr HostAddressRangeEXT) (e)) (descriptors)
  r <- lift $ traceAroundEvent "vkWriteResourceDescriptorsEXT" (vkWriteResourceDescriptorsEXT'
                                                                  (deviceHandle (device))
                                                                  ((fromIntegral pResourcesLength :: Word32))
                                                                  (forgetExtensions (pPResources))
                                                                  (pPDescriptors))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindSamplerHeapEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr BindHeapInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr BindHeapInfoEXT -> IO ()

-- | vkCmdBindSamplerHeapEXT - Binds a sampler heap to a command buffer
--
-- = Description
--
-- Addresses in the range defined by @pBindInfo->heapRange@ are bound as
-- the sampler heap. The application /can/ access samplers and data through
-- this heap anywhere except for the reserved range specified by
-- @pBindInfo->reservedRangeOffset@. Addresses in the range
-- [@pBindInfo->reservedRangeOffset@, @pBindInfo->reservedRangeOffset@ +
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minSamplerHeapReservedRange minSamplerHeapReservedRange>),
-- or in the range [@pBindInfo->reservedRangeOffset@,
-- @pBindInfo->reservedRangeOffset@ +
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minSamplerHeapReservedRangeWithEmbedded minSamplerHeapReservedRangeWithEmbedded>)
-- if embedded samplers will be used, are reserved for the implementation
-- and /must/ not be accessed by the application at any time from when this
-- command is recorded until all command buffers with that range bound
-- (even invalid ones) have been reset or freed.
--
-- Implementations may require a larger sampler heap reservation to store
-- embedded sampler descriptors when used in a mapping, as advertised by
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minSamplerHeapReservedRangeWithEmbedded minSamplerHeapReservedRangeWithEmbedded>.
--
-- Shaders executed by commands recorded after this command /can/ use the
-- specified sampler heap to access resources.
-- @pBindInfo->heapRange.address@ will be available to shaders to access
-- samplers and data through the @SamplerHeapEXT@ @BuiltIn@ or via
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps-bindings shader bindings>.
--
-- When 'cmdBindSamplerHeapEXT' is recorded, it
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps-invalidate-sets immediately invalidates all non-heap descriptor state>.
-- Similarly, recording any non-heap descriptor state commands immediately
-- invalidates state set by this command.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-pBindInfo-11223# The sum of
--     @pBindInfo->reservedRangeOffset@ and @pBindInfo->reservedRangeSize@
--     /must/ be less than or equal to @pBindInfo->heapRange.size@
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-pBindInfo-11224#
--     @pBindInfo->reservedRangeSize@ /must/ be greater than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minSamplerHeapReservedRange minSamplerHeapReservedRange>
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-pBindInfo-11225#
--     @pBindInfo->heapRange.size@ /must/ less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxSamplerHeapSize maxSamplerHeapSize>
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-pBindInfo-11226#
--     @pBindInfo->heapRange.address@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-samplerHeapAlignment samplerHeapAlignment>
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-pBindInfo-11434#
--     @pBindInfo->reservedRangeOffset@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-samplerDescriptorAlignment samplerDescriptorAlignment>
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-pBindInfo-11228# Memory bound to
--     addresses in the range [@pBindInfo->heapRange.address@ +
--     @pBindInfo->reservedRangeOffset@, @pBindInfo->heapRange.address@ +
--     @pBindInfo->reservedRangeOffset@ + @pBindInfo->reservedRangeSize@)
--     /must/ not be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps-reservedranges bound to any other command buffer as a reserved range>
--     for any heap unless the reserved range matches exactly and it is the
--     same heap type
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-heapRange-11230# @heapRange@ /must/ be
--     a device address range allocated to the application from a buffer
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT'
--     usage flag set
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-commandBuffer-11231# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'CommandBufferInheritanceDescriptorHeapInfoEXT'::@pSamplerHeapBindInfo@
--     equal to @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-pBindInfo-parameter# @pBindInfo@
--     /must/ be a valid pointer to a valid 'BindHeapInfoEXT' structure
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBindSamplerHeapEXT-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBindSamplerHeapEXT is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'BindHeapInfoEXT', 'Vulkan.Core10.Handles.CommandBuffer'
cmdBindSamplerHeapEXT :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer that the sampler heap will be
                         -- bound to.
                         CommandBuffer
                      -> -- | @pBindInfo@ is a 'BindHeapInfoEXT' specifying the device address range
                         -- used for the heap and any implementation reservations.
                         ("bindInfo" ::: BindHeapInfoEXT)
                      -> io ()
cmdBindSamplerHeapEXT commandBuffer bindInfo = liftIO . evalContT $ do
  let vkCmdBindSamplerHeapEXTPtr = pVkCmdBindSamplerHeapEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindSamplerHeapEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindSamplerHeapEXT is null" Nothing Nothing
  let vkCmdBindSamplerHeapEXT' = mkVkCmdBindSamplerHeapEXT vkCmdBindSamplerHeapEXTPtr
  pBindInfo <- ContT $ withCStruct (bindInfo)
  lift $ traceAroundEvent "vkCmdBindSamplerHeapEXT" (vkCmdBindSamplerHeapEXT'
                                                       (commandBufferHandle (commandBuffer))
                                                       pBindInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindResourceHeapEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr BindHeapInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr BindHeapInfoEXT -> IO ()

-- | vkCmdBindResourceHeapEXT - Binds a resource heap to a command buffer
--
-- = Description
--
-- Addresses in the range defined by @pBindInfo->heapRange@ are bound as
-- the resource heap. The application /can/ access resources and data
-- through this heap anywhere except for the reserved range specified by
-- @pBindInfo->reservedRangeOffset@. Addresses in the range
-- [@pBindInfo->reservedRangeOffset@, @pBindInfo->reservedRangeOffset@ +
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minResourceHeapReservedRange minResourceHeapReservedRange>)
-- are reserved for the implementation and /must/ not be accessed by the
-- application at any time from when this command is recorded until there
-- are no command buffers with that range bound.
--
-- Shaders executed by commands recorded after this command /can/ use the
-- specified resource heap to access resources.
-- @pBindInfo->heapRange.address@ will be available to shaders to access
-- resources through the @ResourceHeapEXT@ @BuiltIn@ or via
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps-bindings shader bindings>.
--
-- When 'cmdBindResourceHeapEXT' is recorded, it
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps-invalidate-sets immediately invalidates all non-heap descriptor state>.
-- Similarly, recording any non-heap descriptor state commands immediately
-- invalidates state set by this command.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindResourceHeapEXT-pBindInfo-11232# The sum of
--     @pBindInfo->reservedRangeOffset@ and @pBindInfo->reservedRangeSize@
--     /must/ be less than or equal to @pBindInfo->heapRange.size@
--
-- -   #VUID-vkCmdBindResourceHeapEXT-pBindInfo-11233#
--     @pBindInfo->reservedRangeSize@ /must/ be greater than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minResourceHeapReservedRange minResourceHeapReservedRange>
--
-- -   #VUID-vkCmdBindResourceHeapEXT-pBindInfo-11234#
--     @pBindInfo->heapRange.size@ /must/ less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxResourceHeapSize maxResourceHeapSize>
--
-- -   #VUID-vkCmdBindResourceHeapEXT-pBindInfo-11235#
--     @pBindInfo->heapRange.address@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-resourceHeapAlignment resourceHeapAlignment>
--
-- -   #VUID-vkCmdBindResourceHeapEXT-pBindInfo-11435#
--     @pBindInfo->reservedRangeOffset@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-bufferDescriptorAlignment bufferDescriptorAlignment>
--
-- -   #VUID-vkCmdBindResourceHeapEXT-pBindInfo-11436#
--     @pBindInfo->reservedRangeOffset@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-imageDescriptorAlignment imageDescriptorAlignment>
--
-- -   #VUID-vkCmdBindResourceHeapEXT-pBindInfo-11236# Memory bound to
--     addresses in the range [@pBindInfo->heapRange.address@ +
--     @pBindInfo->reservedRangeOffset@, @pBindInfo->heapRange.address@ +
--     @pBindInfo->reservedRangeOffset@ + @pBindInfo->reservedRangeSize@)
--     /must/ not be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps-reservedranges bound to any other command buffer as a reserved range>
--     for any heap unless the reserved range matches exactly and it is the
--     same heap type
--
-- -   #VUID-vkCmdBindResourceHeapEXT-heapRange-11237# @heapRange@ /must/
--     be a device address range allocated to the application from a buffer
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT'
--     usage flag set
--
-- -   #VUID-vkCmdBindResourceHeapEXT-commandBuffer-11238# If
--     @commandBuffer@ is a secondary command buffer, it /must/ have begun
--     with
--     'CommandBufferInheritanceDescriptorHeapInfoEXT'::@pResourceHeapBindInfo@
--     equal to @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindResourceHeapEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindResourceHeapEXT-pBindInfo-parameter# @pBindInfo@
--     /must/ be a valid pointer to a valid 'BindHeapInfoEXT' structure
--
-- -   #VUID-vkCmdBindResourceHeapEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindResourceHeapEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBindResourceHeapEXT-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBindResourceHeapEXT is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'BindHeapInfoEXT', 'Vulkan.Core10.Handles.CommandBuffer'
cmdBindResourceHeapEXT :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer that the resource heap will be
                          -- bound to.
                          CommandBuffer
                       -> -- | @pBindInfo@ is a 'BindHeapInfoEXT' specifying the device address range
                          -- used for the heap and any implementation reservations.
                          ("bindInfo" ::: BindHeapInfoEXT)
                       -> io ()
cmdBindResourceHeapEXT commandBuffer bindInfo = liftIO . evalContT $ do
  let vkCmdBindResourceHeapEXTPtr = pVkCmdBindResourceHeapEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindResourceHeapEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindResourceHeapEXT is null" Nothing Nothing
  let vkCmdBindResourceHeapEXT' = mkVkCmdBindResourceHeapEXT vkCmdBindResourceHeapEXTPtr
  pBindInfo <- ContT $ withCStruct (bindInfo)
  lift $ traceAroundEvent "vkCmdBindResourceHeapEXT" (vkCmdBindResourceHeapEXT'
                                                        (commandBufferHandle (commandBuffer))
                                                        pBindInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDataEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct PushDataInfoEXT) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct PushDataInfoEXT) -> IO ()

-- | vkCmdPushDataEXT - Update the values of push data
--
-- = Description
--
-- When 'cmdPushDataEXT' is recorded, it
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps-invalidate-sets immediately invalidates all non-heap descriptor state>.
-- Similarly, recording any non-heap descriptor state commands immediately
-- invalidates state set by this command.
--
-- All push data is available to all shaders using the existing
-- @PushConstant@ @Storage@ @Class@.
--
-- Device addresses in push data are intended as the fast path for
-- shader-constant data that does not fit into push data directly. In order
-- to maximize performance of constant data inputs, addresses should be
-- aligned to
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minUniformBufferOffsetAlignment minUniformBufferOffsetAlignment>,
-- and decorated with @Alignment@ and @NonWritable@ in the shader when
-- using physical pointers.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushDataEXT-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushDataEXT-pPushDataInfo-parameter# @pPushDataInfo@
--     /must/ be a valid pointer to a valid 'PushDataInfoEXT' structure
--
-- -   #VUID-vkCmdPushDataEXT-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushDataEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdPushDataEXT-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdPushDataEXT is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'PushDataInfoEXT'
cmdPushDataEXT :: forall a io
                . (Extendss PushDataInfoEXT a, PokeChain a, MonadIO io)
               => -- | @commandBuffer@ is the command buffer in which the push data update will
                  -- be recorded.
                  CommandBuffer
               -> -- | @pPushDataInfo@ is a pointer to a 'PushDataInfoEXT' structure.
                  (PushDataInfoEXT a)
               -> io ()
cmdPushDataEXT commandBuffer pushDataInfo = liftIO . evalContT $ do
  let vkCmdPushDataEXTPtr = pVkCmdPushDataEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPushDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDataEXT is null" Nothing Nothing
  let vkCmdPushDataEXT' = mkVkCmdPushDataEXT vkCmdPushDataEXTPtr
  pPushDataInfo <- ContT $ withCStruct (pushDataInfo)
  lift $ traceAroundEvent "vkCmdPushDataEXT" (vkCmdPushDataEXT'
                                                (commandBufferHandle (commandBuffer))
                                                (forgetExtensions pPushDataInfo))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterCustomBorderColorEXT
  :: FunPtr (Ptr Device_T -> Ptr SamplerCustomBorderColorCreateInfoEXT -> Bool32 -> Ptr Word32 -> IO Result) -> Ptr Device_T -> Ptr SamplerCustomBorderColorCreateInfoEXT -> Bool32 -> Ptr Word32 -> IO Result

-- | vkRegisterCustomBorderColorEXT - Register a custom border color
--
-- = Description
--
-- If @requestIndex@ is 'Vulkan.Core10.FundamentalTypes.TRUE', the value
-- present in @pIndex@ when passed to the command is a requested index, and
-- rather than returning a new index, the implementation will attempt to
-- register that index, leaving the value intact. If the implementation is
-- unable to register a requested index,
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS' will
-- be returned. If an index has not been registered (either explicitly or
-- implicitly by creating a sampler object), or if it has been subsequently
-- unregistered, the implementation /must/ register that index
-- successfully.
--
-- If @requestIndex@ is 'Vulkan.Core10.FundamentalTypes.FALSE', the value
-- stored in @pIndex@ is ignored, and a new index will be returned if the
-- implementation is able to register a new index. If the implementation is
-- unable to register a new index,
-- 'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS' will be returned.
--
-- If an index is successfully registered, it /can/ be used when writing a
-- sampler descriptor or creating a sampler object to use with the custom
-- border color, via 'SamplerCustomBorderColorIndexCreateInfoEXT'.
--
-- The type of border color is not specified by this command
-- ('Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' vs.
-- 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'); the data
-- will be interpreted at the point the border color is sampled with an
-- actual sampler. Implementations are expected to store the data as raw
-- bytes if they do not need the format to be specified.
--
-- == Valid Usage
--
-- -   #VUID-vkRegisterCustomBorderColorEXT-requestIndex-11287# If
--     @requestIndex@ is 'Vulkan.Core10.FundamentalTypes.TRUE', the value
--     stored in @pIndex@ /must/ be less than
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxCustomBorderColorSamplers maxCustomBorderColorSamplers>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkRegisterCustomBorderColorEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkRegisterCustomBorderColorEXT-pBorderColor-parameter#
--     @pBorderColor@ /must/ be a valid pointer to a valid
--     'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'
--     structure
--
-- -   #VUID-vkRegisterCustomBorderColorEXT-pIndex-parameter# @pIndex@
--     /must/ be a valid pointer to a @uint32_t@ value
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_border_color VK_EXT_custom_border_color>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'
registerCustomBorderColorEXT :: forall io
                              . (MonadIO io)
                             => -- | @device@ is the logical device where the border color is registered.
                                Device
                             -> -- | @pBorderColor@ is a pointer to a
                                -- 'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT'
                                -- structure specifying the custom border color value to register.
                                SamplerCustomBorderColorCreateInfoEXT
                             -> -- | @requestIndex@ is a Boolean value indicating if a specific index is
                                -- requested or not.
                                ("requestIndex" ::: Bool)
                             -> io (("index" ::: Word32))
registerCustomBorderColorEXT device
                               borderColor
                               requestIndex = liftIO . evalContT $ do
  let vkRegisterCustomBorderColorEXTPtr = pVkRegisterCustomBorderColorEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkRegisterCustomBorderColorEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkRegisterCustomBorderColorEXT is null" Nothing Nothing
  let vkRegisterCustomBorderColorEXT' = mkVkRegisterCustomBorderColorEXT vkRegisterCustomBorderColorEXTPtr
  pBorderColor <- ContT $ withCStruct (borderColor)
  pPIndex <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkRegisterCustomBorderColorEXT" (vkRegisterCustomBorderColorEXT'
                                                                   (deviceHandle (device))
                                                                   pBorderColor
                                                                   (boolToBool32 (requestIndex))
                                                                   (pPIndex))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pIndex <- lift $ peek @Word32 pPIndex
  pure $ (pIndex)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUnregisterCustomBorderColorEXT
  :: FunPtr (Ptr Device_T -> Word32 -> IO ()) -> Ptr Device_T -> Word32 -> IO ()

-- | vkUnregisterCustomBorderColorEXT - Unregister a custom border color
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_border_color VK_EXT_custom_border_color>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Handles.Device'
unregisterCustomBorderColorEXT :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device where the border color is registered.
                                  --
                                  -- #VUID-vkUnregisterCustomBorderColorEXT-device-parameter# @device@ /must/
                                  -- be a valid 'Vulkan.Core10.Handles.Device' handle
                                  Device
                               -> -- | @index@ is the @uint32_t@ index value to unregister.
                                  --
                                  -- #VUID-vkUnregisterCustomBorderColorEXT-index-11288# @index@ /must/ be
                                  -- less than
                                  -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxCustomBorderColorSamplers maxCustomBorderColorSamplers>
                                  ("index" ::: Word32)
                               -> io ()
unregisterCustomBorderColorEXT device index = liftIO $ do
  let vkUnregisterCustomBorderColorEXTPtr = pVkUnregisterCustomBorderColorEXT (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkUnregisterCustomBorderColorEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUnregisterCustomBorderColorEXT is null" Nothing Nothing
  let vkUnregisterCustomBorderColorEXT' = mkVkUnregisterCustomBorderColorEXT vkUnregisterCustomBorderColorEXTPtr
  traceAroundEvent "vkUnregisterCustomBorderColorEXT" (vkUnregisterCustomBorderColorEXT'
                                                         (deviceHandle (device))
                                                         (index))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageOpaqueCaptureDataEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr Image -> Ptr HostAddressRangeEXT -> IO Result) -> Ptr Device_T -> Word32 -> Ptr Image -> Ptr HostAddressRangeEXT -> IO Result

-- | vkGetImageOpaqueCaptureDataEXT - Get image opaque capture descriptor
-- data for descriptor heap replay
--
-- == Valid Usage
--
-- -   #VUID-vkGetImageOpaqueCaptureDataEXT-descriptorHeapCaptureReplay-11282#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorHeapCaptureReplay descriptorHeapCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetImageOpaqueCaptureDataEXT-size-11283# The @size@ member
--     of each element of @pDatas@ /must/ be equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-imageCaptureReplayOpaqueDataSize imageCaptureReplayOpaqueDataSize>
--
-- -   #VUID-vkGetImageOpaqueCaptureDataEXT-device-11284# If @device@ was
--     created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetImageOpaqueCaptureDataEXT-pImages-11285# Each element of
--     @pImages@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_EXT'
--     set in 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetImageOpaqueCaptureDataEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetImageOpaqueCaptureDataEXT-pImages-parameter# @pImages@
--     /must/ be a valid pointer to an array of @imageCount@ valid
--     'Vulkan.Core10.Handles.Image' handles
--
-- -   #VUID-vkGetImageOpaqueCaptureDataEXT-pDatas-parameter# @pDatas@
--     /must/ be a valid pointer to an array of @imageCount@
--     'HostAddressRangeEXT' structures
--
-- -   #VUID-vkGetImageOpaqueCaptureDataEXT-imageCount-arraylength#
--     @imageCount@ /must/ be greater than @0@
--
-- -   #VUID-vkGetImageOpaqueCaptureDataEXT-pImages-parent# Each element of
--     @pImages@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Handles.Device', 'HostAddressRangeEXT',
-- 'Vulkan.Core10.Handles.Image'
getImageOpaqueCaptureDataEXT :: forall io
                              . (MonadIO io)
                             => -- | @device@ is the logical device that gets the data.
                                Device
                             -> -- | @pImages@ is a pointer to an array of 'Vulkan.Core10.Handles.Image'
                                -- objects to retrieve the opaque capture data from.
                                ("images" ::: Vector Image)
                             -> io (("datas" ::: Vector HostAddressRangeEXT))
getImageOpaqueCaptureDataEXT device images = liftIO . evalContT $ do
  let vkGetImageOpaqueCaptureDataEXTPtr = pVkGetImageOpaqueCaptureDataEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageOpaqueCaptureDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageOpaqueCaptureDataEXT is null" Nothing Nothing
  let vkGetImageOpaqueCaptureDataEXT' = mkVkGetImageOpaqueCaptureDataEXT vkGetImageOpaqueCaptureDataEXTPtr
  pPImages <- ContT $ allocaBytes @Image ((Data.Vector.length (images)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPImages `plusPtr` (8 * (i)) :: Ptr Image) (e)) (images)
  pPDatas <- ContT $ bracket (callocBytes @HostAddressRangeEXT ((fromIntegral ((fromIntegral (Data.Vector.length $ (images)) :: Word32))) * 16)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPDatas `advancePtrBytes` (i * 16) :: Ptr HostAddressRangeEXT) . ($ ())) [0..(fromIntegral ((fromIntegral (Data.Vector.length $ (images)) :: Word32))) - 1]
  r <- lift $ traceAroundEvent "vkGetImageOpaqueCaptureDataEXT" (vkGetImageOpaqueCaptureDataEXT'
                                                                   (deviceHandle (device))
                                                                   ((fromIntegral (Data.Vector.length $ (images)) :: Word32))
                                                                   (pPImages)
                                                                   ((pPDatas)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDatas <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (images)) :: Word32))) (\i -> peekCStruct @HostAddressRangeEXT (((pPDatas) `advancePtrBytes` (16 * (i)) :: Ptr HostAddressRangeEXT)))
  pure $ (pDatas)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDescriptorSizeEXT
  :: FunPtr (Ptr PhysicalDevice_T -> DescriptorType -> IO DeviceSize) -> Ptr PhysicalDevice_T -> DescriptorType -> IO DeviceSize

-- | vkGetPhysicalDeviceDescriptorSizeEXT - Report specific descriptor sizes
-- for each descriptor type
--
-- = Description
--
-- The return value of this function will be a
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize' indicating the size in bytes
-- (N) of a heap descriptor with a type equal to @descriptorType@. When a
-- descriptor of this type is written by 'writeResourceDescriptorsEXT' or
-- 'writeSamplerDescriptorsEXT', only the first N bytes are written; the
-- rest will not be accessed and /can/ be safely discarded when copying
-- descriptors around. Additionally, those first N bytes are the only bytes
-- that will be accessed when the descriptor is accessed in the shader. N
-- will never be larger than the applicable limits in
-- 'PhysicalDeviceDescriptorHeapTensorPropertiesARM' or
-- 'PhysicalDeviceDescriptorHeapPropertiesEXT'.
--
-- Values returned by this function have other requirements, so for example
-- may not be power-of-two values.
--
-- This command is not intended for general use, and is for tools that
-- already take advantage of tighter packing with other similar features
-- (e.g. @VK_EXT_descriptor_buffer@) to optimize accesses in some cases.
-- Applications can safely ignore this function and are advised to do so,
-- to avoid depending on non-portable packing.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
getPhysicalDeviceDescriptorSizeEXT :: forall io
                                    . (MonadIO io)
                                   => -- | @physicalDevice@ is the physical device from which to query the
                                      -- descriptor sizes.
                                      --
                                      -- #VUID-vkGetPhysicalDeviceDescriptorSizeEXT-physicalDevice-parameter#
                                      -- @physicalDevice@ /must/ be a valid
                                      -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                      PhysicalDevice
                                   -> -- | @descriptorType@ is a
                                      -- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' specifying the type
                                      -- of heap descriptor to query the size for.
                                      --
                                      -- #VUID-vkGetPhysicalDeviceDescriptorSizeEXT-descriptorType-parameter#
                                      -- @descriptorType@ /must/ be a valid
                                      -- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' value
                                      DescriptorType
                                   -> io (DeviceSize)
getPhysicalDeviceDescriptorSizeEXT physicalDevice descriptorType = liftIO $ do
  let vkGetPhysicalDeviceDescriptorSizeEXTPtr = pVkGetPhysicalDeviceDescriptorSizeEXT (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkGetPhysicalDeviceDescriptorSizeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceDescriptorSizeEXT is null" Nothing Nothing
  let vkGetPhysicalDeviceDescriptorSizeEXT' = mkVkGetPhysicalDeviceDescriptorSizeEXT vkGetPhysicalDeviceDescriptorSizeEXTPtr
  r <- traceAroundEvent "vkGetPhysicalDeviceDescriptorSizeEXT" (vkGetPhysicalDeviceDescriptorSizeEXT'
                                                                  (physicalDeviceHandle (physicalDevice))
                                                                  (descriptorType))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetTensorOpaqueCaptureDataARM
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr TensorARM -> Ptr HostAddressRangeEXT -> IO Result) -> Ptr Device_T -> Word32 -> Ptr TensorARM -> Ptr HostAddressRangeEXT -> IO Result

-- | vkGetTensorOpaqueCaptureDataARM - Get tensor opaque capture descriptor
-- data for descriptor heap replay
--
-- == Valid Usage
--
-- -   #VUID-vkGetTensorOpaqueCaptureDataARM-descriptorHeapCaptureReplay-11391#
--     The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorHeapCaptureReplay descriptorHeapCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetTensorOpaqueCaptureDataARM-size-11392# The @size@ member
--     of each element of @pDatas@ /must/ be equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-tensorCaptureReplayOpaqueDataSize tensorCaptureReplayOpaqueDataSize>
--
-- -   #VUID-vkGetTensorOpaqueCaptureDataARM-device-11393# If @device@ was
--     created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetTensorOpaqueCaptureDataARM-pTensors-11394# Each element
--     of @pTensors@ /must/ have been created with
--     'Vulkan.Extensions.VK_ARM_tensors.TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM'
--     set in
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM'::@flags@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetTensorOpaqueCaptureDataARM-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetTensorOpaqueCaptureDataARM-pTensors-parameter# @pTensors@
--     /must/ be a valid pointer to an array of @tensorCount@ valid
--     'Vulkan.Extensions.Handles.TensorARM' handles
--
-- -   #VUID-vkGetTensorOpaqueCaptureDataARM-pDatas-parameter# @pDatas@
--     /must/ be a valid pointer to an array of @tensorCount@
--     'HostAddressRangeEXT' structures
--
-- -   #VUID-vkGetTensorOpaqueCaptureDataARM-tensorCount-arraylength#
--     @tensorCount@ /must/ be greater than @0@
--
-- -   #VUID-vkGetTensorOpaqueCaptureDataARM-pTensors-parent# Each element
--     of @pTensors@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Handles.Device', 'HostAddressRangeEXT',
-- 'Vulkan.Extensions.Handles.TensorARM'
getTensorOpaqueCaptureDataARM :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the logical device that gets the data.
                                 Device
                              -> -- | @pTensors@ is a pointer to an array of
                                 -- 'Vulkan.Extensions.Handles.TensorARM' objects to retrieve the opaque
                                 -- capture data from.
                                 ("tensors" ::: Vector TensorARM)
                              -> io (("datas" ::: Vector HostAddressRangeEXT))
getTensorOpaqueCaptureDataARM device tensors = liftIO . evalContT $ do
  let vkGetTensorOpaqueCaptureDataARMPtr = pVkGetTensorOpaqueCaptureDataARM (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetTensorOpaqueCaptureDataARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetTensorOpaqueCaptureDataARM is null" Nothing Nothing
  let vkGetTensorOpaqueCaptureDataARM' = mkVkGetTensorOpaqueCaptureDataARM vkGetTensorOpaqueCaptureDataARMPtr
  pPTensors <- ContT $ allocaBytes @TensorARM ((Data.Vector.length (tensors)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPTensors `plusPtr` (8 * (i)) :: Ptr TensorARM) (e)) (tensors)
  pPDatas <- ContT $ bracket (callocBytes @HostAddressRangeEXT ((fromIntegral ((fromIntegral (Data.Vector.length $ (tensors)) :: Word32))) * 16)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPDatas `advancePtrBytes` (i * 16) :: Ptr HostAddressRangeEXT) . ($ ())) [0..(fromIntegral ((fromIntegral (Data.Vector.length $ (tensors)) :: Word32))) - 1]
  r <- lift $ traceAroundEvent "vkGetTensorOpaqueCaptureDataARM" (vkGetTensorOpaqueCaptureDataARM'
                                                                    (deviceHandle (device))
                                                                    ((fromIntegral (Data.Vector.length $ (tensors)) :: Word32))
                                                                    (pPTensors)
                                                                    ((pPDatas)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDatas <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (tensors)) :: Word32))) (\i -> peekCStruct @HostAddressRangeEXT (((pPDatas) `advancePtrBytes` (16 * (i)) :: Ptr HostAddressRangeEXT)))
  pure $ (pDatas)


-- | VkHostAddressRangeEXT - Structure specifying a host address range
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'getImageOpaqueCaptureDataEXT', 'getTensorOpaqueCaptureDataARM',
-- 'writeResourceDescriptorsEXT', 'writeSamplerDescriptorsEXT'
data HostAddressRangeEXT = HostAddressRangeEXT
  { -- | @address@ is a host memory address.
    --
    -- #VUID-VkHostAddressRangeEXT-address-parameter# @address@ /must/ be a
    -- valid pointer to an array of @size@ bytes
    address :: Ptr ()
  , -- | @size@ is the size of the range.
    --
    -- #VUID-VkHostAddressRangeEXT-size-arraylength# @size@ /must/ be greater
    -- than @0@
    size :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HostAddressRangeEXT)
#endif
deriving instance Show HostAddressRangeEXT

instance ToCStruct HostAddressRangeEXT where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HostAddressRangeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr ()))) (address)
    poke ((p `plusPtr` 8 :: Ptr CSize)) (CSize (size))
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr ()))) (zero)
    poke ((p `plusPtr` 8 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct HostAddressRangeEXT where
  peekCStruct p = do
    address <- peek @(Ptr ()) ((p `plusPtr` 0 :: Ptr (Ptr ())))
    size <- peek @CSize ((p `plusPtr` 8 :: Ptr CSize))
    pure $ HostAddressRangeEXT
             address (coerce @CSize @Word64 size)

instance Storable HostAddressRangeEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HostAddressRangeEXT where
  zero = HostAddressRangeEXT
           zero
           zero


-- | VkHostAddressRangeConstEXT - Structure specifying a constant host
-- address range
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'OpaqueCaptureDataCreateInfoEXT', 'PushDataInfoEXT'
data HostAddressRangeConstEXT = HostAddressRangeConstEXT
  { -- | @address@ is a read-only host memory address.
    --
    -- #VUID-VkHostAddressRangeConstEXT-address-parameter# @address@ /must/ be
    -- a valid pointer to an array of @size@ bytes
    address :: Ptr ()
  , -- | @size@ is the size of the range.
    --
    -- #VUID-VkHostAddressRangeConstEXT-size-arraylength# @size@ /must/ be
    -- greater than @0@
    size :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HostAddressRangeConstEXT)
#endif
deriving instance Show HostAddressRangeConstEXT

instance ToCStruct HostAddressRangeConstEXT where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HostAddressRangeConstEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr ()))) (address)
    poke ((p `plusPtr` 8 :: Ptr CSize)) (CSize (size))
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr ()))) (zero)
    poke ((p `plusPtr` 8 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct HostAddressRangeConstEXT where
  peekCStruct p = do
    address <- peek @(Ptr ()) ((p `plusPtr` 0 :: Ptr (Ptr ())))
    size <- peek @CSize ((p `plusPtr` 8 :: Ptr CSize))
    pure $ HostAddressRangeConstEXT
             address (coerce @CSize @Word64 size)

instance Storable HostAddressRangeConstEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HostAddressRangeConstEXT where
  zero = HostAddressRangeConstEXT
           zero
           zero


-- | VkDeviceAddressRangeEXT - Structure specifying a device address range
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceAddressRangeEXT-size-11411# If @size@ is not 0,
--     @address@ /must/ not be 0
--
-- -   #VUID-VkDeviceAddressRangeEXT-address-11365# The sum of @address@
--     and @size@ /must/ be less than or equal to the sum of an address
--     retrieved from a 'Vulkan.Core10.Handles.Buffer' and the value of
--     'Vulkan.Core10.Buffer.BufferCreateInfo'::@size@ used to create that
--     'Vulkan.Core10.Handles.Buffer'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceAddressRangeEXT-address-parameter# If @address@ is not
--     @0@, @address@ /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'BindHeapInfoEXT', 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'ResourceDescriptorDataEXT', 'TexelBufferDescriptorInfoEXT'
data DeviceAddressRangeEXT = DeviceAddressRangeEXT
  { -- | @address@ is a 'Vulkan.Core10.FundamentalTypes.DeviceAddress' specifying
    -- the start of the range.
    address :: DeviceAddress
  , -- | @size@ is a 'Vulkan.Core10.FundamentalTypes.DeviceSize' specifying the
    -- size of the range.
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceAddressRangeEXT)
#endif
deriving instance Show DeviceAddressRangeEXT

instance ToCStruct DeviceAddressRangeEXT where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceAddressRangeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (address)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct DeviceAddressRangeEXT where
  peekCStruct p = do
    address <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    pure $ DeviceAddressRangeEXT
             address size

instance Storable DeviceAddressRangeEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceAddressRangeEXT where
  zero = DeviceAddressRangeEXT
           zero
           zero


-- | VkTexelBufferDescriptorInfoEXT - Structure describing an image
-- descriptor created from a buffer
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'DeviceAddressRangeEXT', 'Vulkan.Core10.Enums.Format.Format',
-- 'ResourceDescriptorDataEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data TexelBufferDescriptorInfoEXT = TexelBufferDescriptorInfoEXT
  { -- | @format@ is the 'Vulkan.Core10.Enums.Format.Format' of the descriptor.
    --
    -- #VUID-VkTexelBufferDescriptorInfoEXT-format-parameter# @format@ /must/
    -- be a valid 'Vulkan.Core10.Enums.Format.Format' value
    format :: Format
  , -- | @addressRange@ is a 'DeviceAddressRangeEXT' defining the range of data
    -- backing the descriptor.
    addressRange :: DeviceAddressRangeEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TexelBufferDescriptorInfoEXT)
#endif
deriving instance Show TexelBufferDescriptorInfoEXT

instance ToCStruct TexelBufferDescriptorInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TexelBufferDescriptorInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TEXEL_BUFFER_DESCRIPTOR_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (format)
    poke ((p `plusPtr` 24 :: Ptr DeviceAddressRangeEXT)) (addressRange)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_TEXEL_BUFFER_DESCRIPTOR_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceAddressRangeEXT)) (zero)
    f

instance FromCStruct TexelBufferDescriptorInfoEXT where
  peekCStruct p = do
    format <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    addressRange <- peekCStruct @DeviceAddressRangeEXT ((p `plusPtr` 24 :: Ptr DeviceAddressRangeEXT))
    pure $ TexelBufferDescriptorInfoEXT
             format addressRange

instance Storable TexelBufferDescriptorInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TexelBufferDescriptorInfoEXT where
  zero = TexelBufferDescriptorInfoEXT
           zero
           zero


-- | VkImageDescriptorInfoEXT - Structure describing an image descriptor
-- created from an image
--
-- == Valid Usage
--
-- -   #VUID-VkImageDescriptorInfoEXT-pView-11426# @pView->viewType@ /must/
--     not be 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--     if @pView->image@ was created with an @imageType@ of
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   #VUID-VkImageDescriptorInfoEXT-pView-11427# If @pView->viewType@ is
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' and
--     @pView->image@ was created with an @imageType@ of
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', @pView->image@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT'
--     set
--
-- -   #VUID-VkImageDescriptorInfoEXT-layout-11219# @layout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read.IMAGE_LAYOUT_RENDERING_LOCAL_READ_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT',
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-VkImageDescriptorInfoEXT-layout-11221# If @layout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     then @pView->aspectMask@ /must/ not include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageDescriptorInfoEXT-pView-11430# If @pView->image@ is a
--     depth\/stencil image, @pView->subresourceRange.aspectMask@ /must/
--     include either
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     but not both
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageDescriptorInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_DESCRIPTOR_INFO_EXT'
--
-- -   #VUID-VkImageDescriptorInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkImageDescriptorInfoEXT-pView-parameter# @pView@ /must/ be a
--     valid pointer to a valid
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo' structure
--
-- -   #VUID-VkImageDescriptorInfoEXT-layout-parameter# @layout@ /must/ be
--     a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.ImageView.ImageViewCreateInfo',
-- 'ResourceDescriptorDataEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageDescriptorInfoEXT = ImageDescriptorInfoEXT
  { -- | @pView@ is an 'Vulkan.Core10.ImageView.ImageViewCreateInfo' describing
    -- the descriptor.
    view :: SomeStruct ImageViewCreateInfo
  , -- | @layout@ is the 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' that the
    -- image view will be in when accessed as a descriptor.
    layout :: ImageLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageDescriptorInfoEXT)
#endif
deriving instance Show ImageDescriptorInfoEXT

instance ToCStruct ImageDescriptorInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageDescriptorInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DESCRIPTOR_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pView'' <- ContT @_ @_ @(Ptr (ImageViewCreateInfo '[])) $ \cont -> withSomeCStruct @ImageViewCreateInfo (view) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageViewCreateInfo _)))) pView''
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (layout)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_DESCRIPTOR_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pView'' <- ContT @_ @_ @(Ptr (ImageViewCreateInfo '[])) $ \cont -> withSomeCStruct @ImageViewCreateInfo ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageViewCreateInfo _)))) pView''
    lift $ poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    lift $ f

instance FromCStruct ImageDescriptorInfoEXT where
  peekCStruct p = do
    pView <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (ImageViewCreateInfo _))))
    layout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    pure $ ImageDescriptorInfoEXT
             pView layout

instance Zero ImageDescriptorInfoEXT where
  zero = ImageDescriptorInfoEXT
           (SomeStruct zero)
           zero


-- | VkResourceDescriptorInfoEXT - Structure describing a resource descriptor
--
-- = Description
--
-- If @type@ is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
-- or
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
-- @data->pTexelBuffer@ is used to construct the descriptor.
--
-- If @type@ is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR',
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV',
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDescriptorType VK_DESCRIPTOR_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_NV>,
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER', or
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
-- @data->pAddressRange@ is used to construct the descriptor. For
-- acceleration structures, the size of the range is not used by the
-- descriptor, and /can/ be set to 0. If a non-zero size is provided
-- though, it /must/ be a valid range.
--
-- Applications may wish to provide a valid range as a way to check their
-- own assumptions about the range they are binding; but it has no bearing
-- on anything except validation. Implementations cannot make any
-- assumptions based on the size of the provided range.
--
-- If @type@ is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM',
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM',
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE', or
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
-- @data->pImage@ is used to construct the descriptor. If @type@ is
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM',
-- @data->pTensorARM@ is used to construct the descriptor.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nullDescriptor nullDescriptor>
-- feature is enabled, the corresponding element of @data@ /may/ be @NULL@
-- to generate a null descriptor.
--
-- Applications /can/ give resource descriptors a debug name in a similar
-- way to naming an object, via the
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
-- structure. However, as there is no actual object, this structure /must/
-- be passed via the @pNext@ chain of this structure, with a @objectType@
-- of 'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN' and a
-- @objectHandle@ of 'Vulkan.Core10.APIConstants.NULL_HANDLE'. The name is
-- attached to the unique set of descriptor bits written by the
-- implementation, and writing the same bits again with new debug info
-- /may/ rename the original descriptor.
--
-- Implementations are not prevented from returning the same bits for
-- different descriptors. This can result in multiple different resources
-- mapping to the same name. A common case for this might be something like
-- a uniform buffer and storage buffer with the same device address range.
--
-- If a descriptor becomes invalid due to the underlying resource becoming
-- invalid, implementations /may/ remove the name association.
--
-- == Valid Usage
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11210# @type@ /must/ be one
--     of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV',
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDescriptorType VK_DESCRIPTOR_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_NV>,
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM', or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--
-- -   #VUID-VkResourceDescriptorInfoEXT-None-11211# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, and @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     @data->pImage@ /must/ not be @NULL@
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11469# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     @data->pImage@ /must/ not be @NULL@
--
-- -   #VUID-VkResourceDescriptorInfoEXT-None-11212# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, and @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     @data->pTexelBuffer@ /must/ not be @NULL@
--
-- -   #VUID-VkResourceDescriptorInfoEXT-None-11213# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, and @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV',
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDescriptorType VK_DESCRIPTOR_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_NV>,
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     @data->pAddressRange@ /must/ not be @NULL@
--
-- -   #VUID-VkResourceDescriptorInfoEXT-None-11457# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, and @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM',
--     @data->pTensorARM@ /must/ not be @NULL@
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-12349# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     @data->pTexelBuffer->addressRange.address@ /must/ be a multiple of
--     the effective alignment requirement of @data->pTexelBuffer->format@
--     as determined by
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minTexelBufferOffsetAlignment minTexelBufferOffsetAlignment>
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-12350# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     @data->pAddressRange->address@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minUniformBufferOffsetAlignment minUniformBufferOffsetAlignment>
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-12351# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     @data->pAddressRange->address@ /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minStorageBufferOffsetAlignment minStorageBufferOffsetAlignment>
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11454# If @type@ is one of
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR'
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV'
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDescriptorType VK_DESCRIPTOR_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_NV>
--
--         @data->pAddressRange->address@ /must/ be a multiple of 256
--
-- -   #VUID-VkResourceDescriptorInfoEXT-pNext-11401# If there is a
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
--     structure in the @pNext@ chain, its @objectType@ /must/ be
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11422# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     @data.pImage->pView->image@ /must/ not have been created with an
--     @imageType@ of 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11424# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     @data.pImage->pView->viewType@ is
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D', and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-image2DViewOf3D image2DViewOf3D>
--     feature is not enabled, @data.pImage->pView->image@ /must/ not have
--     been created with an @imageType@ of
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11425# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     @data.pImage->pView->viewType@ is
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D', and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-sampler2DViewOf3D sampler2DViewOf3D>
--     feature is not enabled, @data.pImage->pView->image@ /must/ not have
--     been created with an @imageType@ of
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D'
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11433# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     @data.pAddressRange->size@ /must/ not be 0
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11458# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     and @data.pImage@ is not @NULL@, @data.pImage->pView->image@ /must/
--     have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     usage flag set
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11459# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     and @data.pImage@ is not @NULL@, @data.pImage->pView->image@ /must/
--     have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_STORAGE_BIT'
--     usage flag set
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11460# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     and @data.pImage@ is not @NULL@, @data.pImage->pView->image@ /must/
--     have been created with the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     usage flag set
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11461# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     and @data.pAddressRange@ is not @NULL@, @data.pAddressRange@ /must/
--     be a device address range allocated to the application from a buffer
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11462# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     and @data.pAddressRange@ is not @NULL@, @data.pAddressRange@ /must/
--     be a device address range allocated to the application from a buffer
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11463# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     and @data.pTexelBuffer@ is not @NULL@,
--     @data.pTexelBuffer->addressRange@ /must/ be a device address range
--     allocated to the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11464# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     and @data.pTexelBuffer@ is not @NULL@,
--     @data.pTexelBuffer->addressRange@ /must/ be a device address range
--     allocated to the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11483# If @type@ is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDescriptorType VK_DESCRIPTOR_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_NV>
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR',
--     and @data.pAddressRange@ is not @NULL@,
--     @data.pAddressRange->address@ /must/ be an acceleration structure
--     address retrieved from a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' object via
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.getAccelerationStructureDeviceAddressKHR'
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11484# If @type@ is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkDescriptorType VK_DESCRIPTOR_TYPE_PARTITIONED_ACCELERATION_STRUCTURE_NV>
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR',
--     @data.pAddressRange@ is not @NULL@, and @data.pAddressRange->size@
--     is not 0, @data.pAddressRange@ /must/ be a device address range
--     allocated to the application from the buffer used to create the
--     acceleration structure that @data.pAddressRange->address@ was
--     retrieved from, and within the buffer range bound to that
--     acceleration structure
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11467# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV'
--     and @data.pAddressRange@ is not @NULL@,
--     @data.pAddressRange->address@ /must/ be an acceleration structure
--     handle retrieved from a
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' object via
--     'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-11468# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV'
--     and @data.pAddressRange@ is not @NULL@, @data.pAddressRange->size@
--     /must/ be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkResourceDescriptorInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RESOURCE_DESCRIPTOR_INFO_EXT'
--
-- -   #VUID-VkResourceDescriptorInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
--
-- -   #VUID-VkResourceDescriptorInfoEXT-sType-unique# The @sType@ value of
--     each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkResourceDescriptorInfoEXT-type-parameter# @type@ /must/ be a
--     valid 'Vulkan.Core10.Enums.DescriptorType.DescriptorType' value
--
-- -   #VUID-VkResourceDescriptorInfoEXT-pImage-parameter# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM',
--     and if @pImage@ is not @NULL@, the @pImage@ member of @data@ /must/
--     be a valid pointer to a valid 'ImageDescriptorInfoEXT' structure
--
-- -   #VUID-VkResourceDescriptorInfoEXT-pTexelBuffer-parameter# If @type@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     and if @pTexelBuffer@ is not @NULL@, the @pTexelBuffer@ member of
--     @data@ /must/ be a valid pointer to a valid
--     'TexelBufferDescriptorInfoEXT' structure
--
-- -   #VUID-VkResourceDescriptorInfoEXT-pAddressRange-parameter# If @type@
--     is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     and if @pAddressRange@ is not @NULL@, the @pAddressRange@ member of
--     @data@ /must/ be a valid pointer to a valid 'DeviceAddressRangeEXT'
--     structure
--
-- -   #VUID-VkResourceDescriptorInfoEXT-pTensorARM-parameter# If @type@ is
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM', and
--     if @pTensorARM@ is not @NULL@, the @pTensorARM@ member of @data@
--     /must/ be a valid pointer to a valid
--     'Vulkan.Extensions.VK_ARM_tensors.TensorViewCreateInfoARM' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Enums.DescriptorType.DescriptorType',
-- 'ResourceDescriptorDataEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'writeResourceDescriptorsEXT'
data ResourceDescriptorInfoEXT (es :: [Type]) = ResourceDescriptorInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @type@ is the type of descriptor to get.
    type' :: DescriptorType
  , -- | @data@ is a 'ResourceDescriptorDataEXT' union defining the properties of
    -- a resource descriptor according to @type@
    data' :: ResourceDescriptorDataEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ResourceDescriptorInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ResourceDescriptorInfoEXT es)

instance Extensible ResourceDescriptorInfoEXT where
  extensibleTypeName = "ResourceDescriptorInfoEXT"
  setNext ResourceDescriptorInfoEXT{..} next' = ResourceDescriptorInfoEXT{next = next', ..}
  getNext ResourceDescriptorInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ResourceDescriptorInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DebugUtilsObjectNameInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss ResourceDescriptorInfoEXT es
         , PokeChain es ) => ToCStruct (ResourceDescriptorInfoEXT es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ResourceDescriptorInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOURCE_DESCRIPTOR_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorType)) (type')
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr ResourceDescriptorDataEXT)) (data') . ($ ())
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RESOURCE_DESCRIPTOR_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorType)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr ResourceDescriptorDataEXT)) (zero) . ($ ())
    lift $ f

instance es ~ '[] => Zero (ResourceDescriptorInfoEXT es) where
  zero = ResourceDescriptorInfoEXT
           ()
           zero
           zero


-- | VkBindHeapInfoEXT - Structure describing a device address range and
-- implementation reservation for a descriptor heap
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'CommandBufferInheritanceDescriptorHeapInfoEXT',
-- 'DeviceAddressRangeEXT', 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBindResourceHeapEXT', 'cmdBindSamplerHeapEXT'
data BindHeapInfoEXT = BindHeapInfoEXT
  { -- | @heapRange@ is a 'DeviceAddressRangeEXT' defining the device address
    -- range used for the heap, inclusive of the implementation reserved range.
    heapRange :: DeviceAddressRangeEXT
  , -- | @reservedRangeOffset@ is the offset within @heapRange@ to the start of
    -- the reserved range for the implementation.
    reservedRangeOffset :: DeviceSize
  , -- | @reservedRangeSize@ is the size of the reserved range for the
    -- implementation within @heapRange@.
    reservedRangeSize :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindHeapInfoEXT)
#endif
deriving instance Show BindHeapInfoEXT

instance ToCStruct BindHeapInfoEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindHeapInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_HEAP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddressRangeEXT)) (heapRange)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (reservedRangeOffset)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (reservedRangeSize)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_HEAP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceAddressRangeEXT)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BindHeapInfoEXT where
  peekCStruct p = do
    heapRange <- peekCStruct @DeviceAddressRangeEXT ((p `plusPtr` 16 :: Ptr DeviceAddressRangeEXT))
    reservedRangeOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    reservedRangeSize <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    pure $ BindHeapInfoEXT
             heapRange reservedRangeOffset reservedRangeSize

instance Storable BindHeapInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindHeapInfoEXT where
  zero = BindHeapInfoEXT
           zero
           zero
           zero


-- | VkPushDataInfoEXT - Structure specifying a push data update operation
--
-- == Valid Usage
--
-- -   #VUID-VkPushDataInfoEXT-offset-11243# The sum of @offset@ and
--     @data.size@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxPushDataSize maxPushDataSize>
--
-- -   #VUID-VkPushDataInfoEXT-offset-11418# @offset@ /must/ be a multiple
--     of 4
--
-- -   #VUID-VkPushDataInfoEXT-data-11419# @data.size@ /must/ be a multiple
--     of 4
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPushDataInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_DATA_INFO_EXT'
--
-- -   #VUID-VkPushDataInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_NV_push_constant_bank.PushConstantBankInfoNV'
--
-- -   #VUID-VkPushDataInfoEXT-sType-unique# The @sType@ value of each
--     structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPushDataInfoEXT-data-parameter# @data@ /must/ be a valid
--     'HostAddressRangeConstEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'HostAddressRangeConstEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdPushDataEXT'
data PushDataInfoEXT (es :: [Type]) = PushDataInfoEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @offset@ is the start offset of the push data range to update, in units
    -- of bytes.
    offset :: Word32
  , -- | @data@ is the host address range containing the push data to update.
    data' :: HostAddressRangeConstEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PushDataInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PushDataInfoEXT es)

instance Extensible PushDataInfoEXT where
  extensibleTypeName = "PushDataInfoEXT"
  setNext PushDataInfoEXT{..} next' = PushDataInfoEXT{next = next', ..}
  getNext PushDataInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PushDataInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PushConstantBankInfoNV = Just f
    | otherwise = Nothing

instance ( Extendss PushDataInfoEXT es
         , PokeChain es ) => ToCStruct (PushDataInfoEXT es) where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PushDataInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DATA_INFO_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (offset)
    lift $ poke ((p `plusPtr` 24 :: Ptr HostAddressRangeConstEXT)) (data')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PUSH_DATA_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr HostAddressRangeConstEXT)) (zero)
    lift $ f

instance ( Extendss PushDataInfoEXT es
         , PeekChain es ) => FromCStruct (PushDataInfoEXT es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    offset <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    data' <- peekCStruct @HostAddressRangeConstEXT ((p `plusPtr` 24 :: Ptr HostAddressRangeConstEXT))
    pure $ PushDataInfoEXT
             next offset data'

instance es ~ '[] => Zero (PushDataInfoEXT es) where
  zero = PushDataInfoEXT
           ()
           zero
           zero


-- | VkDescriptorMappingSourceConstantOffsetEXT - Structure specifying
-- mapping resources to a constant heap index
--
-- = Description
--
-- Resources using this mapping will be backed by a descriptor in the heap,
-- at an offset calculated as
--
-- -   shaderIndex = (Binding - @firstBinding@) + arrayIndex
--
-- -   offset = heapOffset + (shaderIndex * heapArrayStride)
--
-- where Binding is the binding value in the shader, arrayIndex is the
-- index into the array if the shader binding is declared as an array.
--
-- If the mapped resource is a @OpTypeSampledImage@, offset is instead
-- calculated for the sampler as
--
-- -   offset = samplerHeapOffset + (shaderIndex * samplerHeapArrayStride)
--
-- If the mapped resource is a @OpTypeSampler@ or @OpTypeSampledImage@, and
-- @pEmbeddedSampler@ is not @NULL@, the specified embedded sampler will be
-- used rather than accessing the sampler heap.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorMappingSourceConstantOffsetEXT-pEmbeddedSampler-11445#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', its @borderColor@ /must/
--     not be
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- -   #VUID-VkDescriptorMappingSourceConstantOffsetEXT-pEmbeddedSampler-11415#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', and there is a
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
--     structure in its @pNext@ chain, its @objectType@ /must/ be
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorMappingSourceConstantOffsetEXT-pEmbeddedSampler-parameter#
--     If @pEmbeddedSampler@ is not @NULL@, @pEmbeddedSampler@ /must/ be a
--     valid pointer to a valid 'Vulkan.Core10.Sampler.SamplerCreateInfo'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'DescriptorMappingSourceDataEXT',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
data DescriptorMappingSourceConstantOffsetEXT = DescriptorMappingSourceConstantOffsetEXT
  { -- | @heapOffset@ is a constant byte offset added to the heap address for the
    -- mapped resource or sampler.
    heapOffset :: Word32
  , -- | @heapArrayStride@ is a constant byte stride that multiplies the shader
    -- binding and array index.
    heapArrayStride :: Word32
  , -- | @pEmbeddedSampler@ is an optional
    -- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structure specifying a sampler
    -- to embed into the shader, in place of looking the sampler up in a heap.
    embeddedSampler :: Maybe (SomeStruct SamplerCreateInfo)
  , -- | @samplerHeapOffset@ is used only when mapping a combined image sampler,
    -- used in place of @heapOffset@ to retrieve the sampler.
    samplerHeapOffset :: Word32
  , -- | @samplerHeapArrayStride@ is used only when mapping a combined image
    -- sampler, used in place of @heapArrayStride@ to retrieve the sampler.
    samplerHeapArrayStride :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorMappingSourceConstantOffsetEXT)
#endif
deriving instance Show DescriptorMappingSourceConstantOffsetEXT

instance ToCStruct DescriptorMappingSourceConstantOffsetEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorMappingSourceConstantOffsetEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (heapOffset)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (heapArrayStride)
    pEmbeddedSampler'' <- case (embeddedSampler) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (SamplerCreateInfo '[])) $ \cont -> withSomeCStruct @SamplerCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr (SamplerCreateInfo _)))) pEmbeddedSampler''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (samplerHeapOffset)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (samplerHeapArrayStride)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorMappingSourceConstantOffsetEXT where
  peekCStruct p = do
    heapOffset <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    heapArrayStride <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pEmbeddedSampler <- peek @(Ptr (SamplerCreateInfo _)) ((p `plusPtr` 8 :: Ptr (Ptr (SamplerCreateInfo _))))
    pEmbeddedSampler' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pEmbeddedSampler
    samplerHeapOffset <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    samplerHeapArrayStride <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ DescriptorMappingSourceConstantOffsetEXT
             heapOffset
             heapArrayStride
             pEmbeddedSampler'
             samplerHeapOffset
             samplerHeapArrayStride

instance Zero DescriptorMappingSourceConstantOffsetEXT where
  zero = DescriptorMappingSourceConstantOffsetEXT
           zero
           zero
           Nothing
           zero
           zero


-- | VkDescriptorMappingSourcePushIndexEXT - Structure specifying mapping
-- resources to a heap index in push data
--
-- = Description
--
-- Resources using this mapping will be backed by a descriptor in the heap,
-- at an offset calculated as
--
-- -   pushIndex = ((uint32_t*)pPushData)[pushOffset\/4]
--
-- -   shaderIndex = (Binding - firstBinding) + arrayIndex
--
-- -   offset = heapOffset + (pushIndex × heapIndexStride) + (shaderIndex ×
--     heapArrayStride)
--
-- where Binding is the binding value in the shader, arrayIndex is the
-- index into the array if the shader binding is declared as an array, and
-- pPushData is the total set of push data specified by 'cmdPushDataEXT'.
--
-- If the mapped resource is a @OpTypeSampledImage@, offset is instead
-- calculated for the sampler as
--
-- -   samplerPushIndex = ((uint32_t*)pPushData)[samplerPushOffset\/4]
--
-- -   offset = samplerHeapOffset + (samplerPushIndex ×
--     samplerHeapIndexStride) + (shaderIndex × samplerHeapArrayStride)
--
-- If @useCombinedImageSamplerIndex@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', and the mapped resource is a
-- @OpTypeSampledImage@, pushIndex and samplerPushIndex in the above
-- equations are instead calculated as
--
-- -   pushIndex = ((uint32_t*)pPushData)[pushOffset\/4] & 0xFFFFF
--
-- -   samplerPushIndex = (((uint32_t*)pPushData)[pushOffset\/4] >> 20) &
--     0xFFF
--
-- If the mapped resource is a @OpTypeSampler@ or @OpTypeSampledImage@, and
-- @pEmbeddedSampler@ is not @NULL@, the specified embedded sampler will be
-- used rather than accessing the sampler heap.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorMappingSourcePushIndexEXT-pushOffset-11258#
--     @pushOffset@ /must/ be a multiple of 4
--
-- -   #VUID-VkDescriptorMappingSourcePushIndexEXT-pushOffset-11259#
--     @pushOffset@ /must/ be less than or equal to @maxPushDataSize@ - 4
--
-- -   #VUID-VkDescriptorMappingSourcePushIndexEXT-pEmbeddedSampler-11446#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', its @borderColor@ /must/
--     not be
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- -   #VUID-VkDescriptorMappingSourcePushIndexEXT-pEmbeddedSampler-11402#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', and there is a
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
--     structure in its @pNext@ chain, its @objectType@ /must/ be
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorMappingSourcePushIndexEXT-pEmbeddedSampler-parameter#
--     If @pEmbeddedSampler@ is not @NULL@, @pEmbeddedSampler@ /must/ be a
--     valid pointer to a valid 'Vulkan.Core10.Sampler.SamplerCreateInfo'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'DescriptorMappingSourceDataEXT',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
data DescriptorMappingSourcePushIndexEXT = DescriptorMappingSourcePushIndexEXT
  { -- | @heapOffset@ is a constant byte offset added to the heap address for the
    -- mapped resource or sampler.
    heapOffset :: Word32
  , -- | @pushOffset@ is an index into push data where an index into the heap for
    -- the mapped resource will be retrieved.
    pushOffset :: Word32
  , -- | @heapIndexStride@ is a constant byte stride that multiplies the index in
    -- push data.
    heapIndexStride :: Word32
  , -- | @heapArrayStride@ is a constant byte stride that multiplies the shader
    -- binding and array index.
    heapArrayStride :: Word32
  , -- | @pEmbeddedSampler@ is an optional
    -- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structure specifying a sampler
    -- to embed into the shader, in place of looking the sampler up in a heap.
    embeddedSampler :: Maybe (SomeStruct SamplerCreateInfo)
  , -- | @useCombinedImageSamplerIndex@ specifies whether the generated index
    -- value will be decoded as two packed indices if the mapped resource is an
    -- @OpTypeSampledImage@.
    useCombinedImageSamplerIndex :: Bool
  , -- | @samplerHeapOffset@ is used only when mapping a combined image sampler,
    -- used in place of @heapOffset@ to retrieve the sampler.
    samplerHeapOffset :: Word32
  , -- | @samplerPushOffset@ is used only when mapping a combined image sampler,
    -- used in place of @pushOffset@ to retrieve the sampler.
    samplerPushOffset :: Word32
  , -- | @samplerHeapIndexStride@ is used only when mapping a combined image
    -- sampler, used in place of @heapIndexStride@ to retrieve the sampler.
    samplerHeapIndexStride :: Word32
  , -- | @samplerHeapArrayStride@ is used only when mapping a combined image
    -- sampler, used in place of @heapArrayStride@ to retrieve the sampler.
    samplerHeapArrayStride :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorMappingSourcePushIndexEXT)
#endif
deriving instance Show DescriptorMappingSourcePushIndexEXT

instance ToCStruct DescriptorMappingSourcePushIndexEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorMappingSourcePushIndexEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (heapOffset)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (pushOffset)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) (heapIndexStride)
    lift $ poke ((p `plusPtr` 12 :: Ptr Word32)) (heapArrayStride)
    pEmbeddedSampler'' <- case (embeddedSampler) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (SamplerCreateInfo '[])) $ \cont -> withSomeCStruct @SamplerCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (SamplerCreateInfo _)))) pEmbeddedSampler''
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (useCombinedImageSamplerIndex))
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (samplerHeapOffset)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (samplerPushOffset)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (samplerHeapIndexStride)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (samplerHeapArrayStride)
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorMappingSourcePushIndexEXT where
  peekCStruct p = do
    heapOffset <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pushOffset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    heapIndexStride <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    heapArrayStride <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pEmbeddedSampler <- peek @(Ptr (SamplerCreateInfo _)) ((p `plusPtr` 16 :: Ptr (Ptr (SamplerCreateInfo _))))
    pEmbeddedSampler' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pEmbeddedSampler
    useCombinedImageSamplerIndex <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    samplerHeapOffset <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    samplerPushOffset <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    samplerHeapIndexStride <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    samplerHeapArrayStride <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pure $ DescriptorMappingSourcePushIndexEXT
             heapOffset
             pushOffset
             heapIndexStride
             heapArrayStride
             pEmbeddedSampler'
             (bool32ToBool useCombinedImageSamplerIndex)
             samplerHeapOffset
             samplerPushOffset
             samplerHeapIndexStride
             samplerHeapArrayStride

instance Zero DescriptorMappingSourcePushIndexEXT where
  zero = DescriptorMappingSourcePushIndexEXT
           zero
           zero
           zero
           zero
           Nothing
           zero
           zero
           zero
           zero
           zero


-- | VkDescriptorMappingSourceIndirectIndexEXT - Structure specifying mapping
-- resources to a heap index in indirect data
--
-- = Description
--
-- Resources using this mapping will be backed by a descriptor in the heap,
-- at an offset calculated as
--
-- -   uint32_t *indirectAddress =
--     ((VkDeviceAddress*)pPushData)[pushOffset\/8]
--
-- -   indirectIndex = indirectAddress[(addressOffset \/ 4)]
--
-- -   shaderIndex = (Binding - firstBinding) + arrayIndex
--
-- -   offset = heapOffset + (indirectIndex × heapIndexStride) +
--     (shaderIndex × heapArrayStride)
--
-- where Binding is the binding value in the shader, arrayIndex is the
-- index into the array if the shader binding is declared as an array, and
-- pPushData is the total set of push data specified by 'cmdPushDataEXT'.
-- The value of the address in push data /must/ be a multiple of 4. Index
-- reads through indirectAddress are performed as non-volatile uniform
-- buffer reads, and can be synchronized using
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_UNIFORM_READ_BIT'. The value
-- in memory /must/ remain static while any shader invocation using this
-- mapping is in flight to avoid a data race.
--
-- If the mapped resource is a @OpTypeSampledImage@, offset is instead
-- calculated for the sampler as
--
-- -   uint32_t *samplerIndirectAddress =
--     ((VkDeviceAddress*)pPushData)[samplerPushOffset\/8]
--
-- -   samplerIndirectIndex = samplerIndirectAddress[(samplerAddressOffset
--     \/ 4)]
--
-- -   offset = samplerHeapOffset + (samplerIndirectIndex ×
--     samplerHeapIndexStride) + (shaderIndex × samplerHeapArrayStride)
--
-- If @useCombinedImageSamplerIndex@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', and the mapped resource is a
-- @OpTypeSampledImage@, indirectIndex and samplerIndirectIndex in the
-- above equations are instead calculated as
--
-- -   indirectIndex = indirectAddress[addressOffset\/4] & 0xFFFFF
--
-- -   samplerIndirectIndex = indirectAddress[addressOffset\/4] >> 20) &
--     0xFFF
--
-- If the mapped resource is a @OpTypeSampler@ or @OpTypeSampledImage@, and
-- @pEmbeddedSampler@ is not @NULL@, the specified embedded sampler will be
-- used rather than accessing the sampler heap.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexEXT-pushOffset-11260#
--     @pushOffset@ /must/ be a multiple of 8
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexEXT-pushOffset-11261#
--     @pushOffset@ /must/ be less than or equal to @maxPushDataSize@ - 8
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexEXT-addressOffset-11262#
--     @addressOffset@ /must/ be a multiple of 4
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexEXT-pEmbeddedSampler-11447#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', its @borderColor@ /must/
--     not be
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexEXT-pEmbeddedSampler-11403#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', and there is a
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
--     structure in its @pNext@ chain, its @objectType@ /must/ be
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexEXT-pEmbeddedSampler-parameter#
--     If @pEmbeddedSampler@ is not @NULL@, @pEmbeddedSampler@ /must/ be a
--     valid pointer to a valid 'Vulkan.Core10.Sampler.SamplerCreateInfo'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'DescriptorMappingSourceDataEXT',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
data DescriptorMappingSourceIndirectIndexEXT = DescriptorMappingSourceIndirectIndexEXT
  { -- | @heapOffset@ is a constant byte offset added to the heap address for the
    -- mapped resource or sampler.
    heapOffset :: Word32
  , -- | @pushOffset@ is an offset into push data where an the indirect address
    -- will be.
    pushOffset :: Word32
  , -- | @addressOffset@ is an index into the address in push data where an index
    -- into the heap for the mapped resource will be retrieved.
    addressOffset :: Word32
  , -- | @heapIndexStride@ is a constant byte stride that multiplies the index in
    -- indirect data.
    heapIndexStride :: Word32
  , -- | @heapArrayStride@ is a constant byte stride that multiplies the shader
    -- binding and array index.
    heapArrayStride :: Word32
  , -- | @pEmbeddedSampler@ is an optional
    -- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structure specifying a sampler
    -- to embed into the shader, in place of looking the sampler up in a heap.
    embeddedSampler :: Maybe (SomeStruct SamplerCreateInfo)
  , -- | @useCombinedImageSamplerIndex@ specifies whether the generated index
    -- value will be decoded as two packed indices if the mapped resource is an
    -- @OpTypeSampledImage@.
    useCombinedImageSamplerIndex :: Bool
  , -- | @samplerHeapOffset@ is used only when mapping a combined image sampler,
    -- used in place of @heapOffset@ to retrieve the sampler.
    samplerHeapOffset :: Word32
  , -- | @samplerPushOffset@ is used only when mapping a combined image sampler,
    -- used in place of @pushOffset@ to retrieve the sampler.
    samplerPushOffset :: Word32
  , -- | @samplerAddressOffset@ is used only when mapping a combined image
    -- sampler, used in place of @addressOffset@ to retrieve the sampler.
    samplerAddressOffset :: Word32
  , -- | @samplerHeapIndexStride@ is used only when mapping a combined image
    -- sampler, used in place of @heapIndexStride@ to retrieve the sampler.
    samplerHeapIndexStride :: Word32
  , -- | @samplerHeapArrayStride@ is used only when mapping a combined image
    -- sampler, used in place of @heapArrayStride@ to retrieve the sampler.
    samplerHeapArrayStride :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorMappingSourceIndirectIndexEXT)
#endif
deriving instance Show DescriptorMappingSourceIndirectIndexEXT

instance ToCStruct DescriptorMappingSourceIndirectIndexEXT where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorMappingSourceIndirectIndexEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (heapOffset)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (pushOffset)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) (addressOffset)
    lift $ poke ((p `plusPtr` 12 :: Ptr Word32)) (heapIndexStride)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (heapArrayStride)
    pEmbeddedSampler'' <- case (embeddedSampler) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (SamplerCreateInfo '[])) $ \cont -> withSomeCStruct @SamplerCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (SamplerCreateInfo _)))) pEmbeddedSampler''
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (useCombinedImageSamplerIndex))
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (samplerHeapOffset)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (samplerPushOffset)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) (samplerAddressOffset)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (samplerHeapIndexStride)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (samplerHeapArrayStride)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorMappingSourceIndirectIndexEXT where
  peekCStruct p = do
    heapOffset <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pushOffset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    addressOffset <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    heapIndexStride <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    heapArrayStride <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pEmbeddedSampler <- peek @(Ptr (SamplerCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (SamplerCreateInfo _))))
    pEmbeddedSampler' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pEmbeddedSampler
    useCombinedImageSamplerIndex <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    samplerHeapOffset <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    samplerPushOffset <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    samplerAddressOffset <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    samplerHeapIndexStride <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    samplerHeapArrayStride <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    pure $ DescriptorMappingSourceIndirectIndexEXT
             heapOffset
             pushOffset
             addressOffset
             heapIndexStride
             heapArrayStride
             pEmbeddedSampler'
             (bool32ToBool useCombinedImageSamplerIndex)
             samplerHeapOffset
             samplerPushOffset
             samplerAddressOffset
             samplerHeapIndexStride
             samplerHeapArrayStride

instance Zero DescriptorMappingSourceIndirectIndexEXT where
  zero = DescriptorMappingSourceIndirectIndexEXT
           zero
           zero
           zero
           zero
           zero
           Nothing
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDescriptorMappingSourceIndirectIndexArrayEXT - Structure specifying
-- mapping resources to a heap index array in indirect data
--
-- = Description
--
-- Resources using this mapping will be backed by a descriptor in the heap,
-- at an offset calculated as
--
-- -   uint32_t *indirectAddress =
--     ((VkDeviceAddress*)pPushData)[pushOffset\/8]
--
-- -   shaderIndex = (Binding - firstBinding) + arrayIndex
--
-- -   indirectIndex = indirectAddress[(addressOffset \/ 4) + shaderIndex]
--
-- -   offset = heapOffset + (indirectIndex × heapIndexStride)
--
-- where Binding is the binding value in the shader, arrayIndex is the
-- index into the array if the shader binding is declared as an array, and
-- pPushData is the total set of push data specified by 'cmdPushDataEXT'.
-- The value of the address in push data /must/ be a multiple of 4. Index
-- reads through indirectAddress are performed as non-volatile uniform
-- buffer reads, and can be synchronized using
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_UNIFORM_READ_BIT'. The value
-- in memory /must/ remain static while any shader invocation using this
-- mapping is in flight to avoid a data race.
--
-- If the mapped resource is a @OpTypeSampledImage@, offset is instead
-- calculated for the sampler as
--
-- -   uint32_t *samplerIndirectAddress =
--     ((VkDeviceAddress*)pPushData)[samplerPushOffset\/8]
--
-- -   samplerIndirectIndex = samplerAddr[(samplerAddressOffset \/ 4) +
--     shaderIndex]
--
-- -   offset = samplerHeapOffset + (samplerIndirectIndex ×
--     samplerHeapIndexStride)
--
-- If @useCombinedImageSamplerIndex@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', and the mapped resource is a
-- @OpTypeSampledImage@, indirectIndex and samplerIndirectIndex in the
-- above equations are instead calculated as
--
-- -   indirectIndex = indirectAddress[addressOffset\/4 + shaderIndex] &
--     0xFFFFF
--
-- -   samplerIndirectIndex = indirectAddress[addressOffset\/4 +
--     shaderIndex] >> 20) & 0xFFF
--
-- If the mapped resource is a @OpTypeSampler@ or @OpTypeSampledImage@, and
-- @pEmbeddedSampler@ is not @NULL@, the specified embedded sampler will be
-- used rather than accessing the sampler heap.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexArrayEXT-pushOffset-11359#
--     @pushOffset@ /must/ be a multiple of 8
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexArrayEXT-pushOffset-11360#
--     @pushOffset@ /must/ be less than or equal to @maxPushDataSize@ - 8
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexArrayEXT-addressOffset-11361#
--     @addressOffset@ /must/ be a multiple of 4
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexArrayEXT-pEmbeddedSampler-11448#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', its @borderColor@ /must/
--     not be
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexArrayEXT-pEmbeddedSampler-11404#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', and there is a
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
--     structure in its @pNext@ chain, its @objectType@ /must/ be
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorMappingSourceIndirectIndexArrayEXT-pEmbeddedSampler-parameter#
--     If @pEmbeddedSampler@ is not @NULL@, @pEmbeddedSampler@ /must/ be a
--     valid pointer to a valid 'Vulkan.Core10.Sampler.SamplerCreateInfo'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'DescriptorMappingSourceDataEXT',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
data DescriptorMappingSourceIndirectIndexArrayEXT = DescriptorMappingSourceIndirectIndexArrayEXT
  { -- | @heapOffset@ is a constant byte offset added to the heap address for the
    -- mapped resource or sampler.
    heapOffset :: Word32
  , -- | @pushOffset@ is an offset into push data where an the indirect address
    -- will be.
    pushOffset :: Word32
  , -- | @addressOffset@ is an index into the address in push data where an index
    -- into the heap for the mapped resource will be retrieved.
    addressOffset :: Word32
  , -- | @heapIndexStride@ is a constant byte stride that multiplies the index in
    -- indirect data.
    heapIndexStride :: Word32
  , -- | @pEmbeddedSampler@ is an optional
    -- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structure specifying a sampler
    -- to embed into the shader, in place of looking the sampler up in a heap.
    embeddedSampler :: Maybe (SomeStruct SamplerCreateInfo)
  , -- | @useCombinedImageSamplerIndex@ specifies whether the generated index
    -- value will be decoded as two packed indices if the mapped resource is an
    -- @OpTypeSampledImage@.
    useCombinedImageSamplerIndex :: Bool
  , -- | @samplerHeapOffset@ is used only when mapping a combined image sampler,
    -- used in place of @heapOffset@ to retrieve the sampler.
    samplerHeapOffset :: Word32
  , -- | @samplerPushOffset@ is used only when mapping a combined image sampler,
    -- used in place of @pushOffset@ to retrieve the sampler.
    samplerPushOffset :: Word32
  , -- | @samplerAddressOffset@ is used only when mapping a combined image
    -- sampler, used in place of @addressOffset@ to retrieve the sampler.
    samplerAddressOffset :: Word32
  , -- | @samplerHeapIndexStride@ is used only when mapping a combined image
    -- sampler, used in place of @heapIndexStride@ to retrieve the sampler.
    samplerHeapIndexStride :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorMappingSourceIndirectIndexArrayEXT)
#endif
deriving instance Show DescriptorMappingSourceIndirectIndexArrayEXT

instance ToCStruct DescriptorMappingSourceIndirectIndexArrayEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorMappingSourceIndirectIndexArrayEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (heapOffset)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (pushOffset)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) (addressOffset)
    lift $ poke ((p `plusPtr` 12 :: Ptr Word32)) (heapIndexStride)
    pEmbeddedSampler'' <- case (embeddedSampler) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (SamplerCreateInfo '[])) $ \cont -> withSomeCStruct @SamplerCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (SamplerCreateInfo _)))) pEmbeddedSampler''
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (useCombinedImageSamplerIndex))
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (samplerHeapOffset)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (samplerPushOffset)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (samplerAddressOffset)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (samplerHeapIndexStride)
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorMappingSourceIndirectIndexArrayEXT where
  peekCStruct p = do
    heapOffset <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pushOffset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    addressOffset <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    heapIndexStride <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pEmbeddedSampler <- peek @(Ptr (SamplerCreateInfo _)) ((p `plusPtr` 16 :: Ptr (Ptr (SamplerCreateInfo _))))
    pEmbeddedSampler' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pEmbeddedSampler
    useCombinedImageSamplerIndex <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    samplerHeapOffset <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    samplerPushOffset <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    samplerAddressOffset <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    samplerHeapIndexStride <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pure $ DescriptorMappingSourceIndirectIndexArrayEXT
             heapOffset
             pushOffset
             addressOffset
             heapIndexStride
             pEmbeddedSampler'
             (bool32ToBool useCombinedImageSamplerIndex)
             samplerHeapOffset
             samplerPushOffset
             samplerAddressOffset
             samplerHeapIndexStride

instance Zero DescriptorMappingSourceIndirectIndexArrayEXT where
  zero = DescriptorMappingSourceIndirectIndexArrayEXT
           zero
           zero
           zero
           zero
           Nothing
           zero
           zero
           zero
           zero
           zero


-- | VkDescriptorMappingSourceHeapDataEXT - Structure specifying mapping a
-- uniform buffer to heap data
--
-- = Description
--
-- Uniform buffers using this mapping will be backed directly by data in
-- the heap. Accessing data in the uniform buffer at an offset of
-- shaderOffset in the shader will access heap data at an offset equal to
--
-- -   offset = shaderOffset + heapOffset +
--     ((uint32_t*)pPushData)[pushOffset\/4]
--
-- where pPushData is the total set of push data specified by
-- 'cmdPushDataEXT'. Shader reads through the heap mapped in this way are
-- performed according to the mapped resource.
--
-- == Valid Usage
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'DescriptorMappingSourceDataEXT'
data DescriptorMappingSourceHeapDataEXT = DescriptorMappingSourceHeapDataEXT
  { -- | @heapOffset@ is a constant byte offset added to the heap address for the
    -- mapped buffer.
    --
    -- #VUID-VkDescriptorMappingSourceHeapDataEXT-heapOffset-11263#
    -- @heapOffset@ /must/ be a multiple of
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-minUniformBufferOffsetAlignment minUniformBufferOffsetAlignment>
    heapOffset :: Word32
  , -- | @pushOffset@ is an index into push data where an additional offset into
    -- the heap for the mapped resource will be retrieved.
    --
    -- #VUID-VkDescriptorMappingSourceHeapDataEXT-pushOffset-11264#
    -- @pushOffset@ /must/ be a multiple of 4
    --
    -- #VUID-VkDescriptorMappingSourceHeapDataEXT-pushOffset-11265#
    -- @pushOffset@ /must/ be less than or equal to @maxPushDataSize@ - 4
    pushOffset :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorMappingSourceHeapDataEXT)
#endif
deriving instance Show DescriptorMappingSourceHeapDataEXT

instance ToCStruct DescriptorMappingSourceHeapDataEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorMappingSourceHeapDataEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (heapOffset)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (pushOffset)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorMappingSourceHeapDataEXT where
  peekCStruct p = do
    heapOffset <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pushOffset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ DescriptorMappingSourceHeapDataEXT
             heapOffset pushOffset

instance Storable DescriptorMappingSourceHeapDataEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorMappingSourceHeapDataEXT where
  zero = DescriptorMappingSourceHeapDataEXT
           zero
           zero


-- | VkDescriptorMappingSourceShaderRecordIndexEXT - Structure specifying
-- mapping resources to a heap index in shader record data
--
-- = Description
--
-- Resources using this mapping will be backed by a descriptor in the heap,
-- at an offset calculated as
--
-- -   shaderRecordIndex =
--     ((uint32_t*)pShaderRecordData)[shaderRecordOffset\/4]
--
-- -   shaderIndex = (Binding - firstBinding) + arrayIndex
--
-- -   offset = heapOffset + (shaderRecordIndex × heapIndexStride) +
--     (shaderIndex × heapArrayStride)
--
-- where Binding is the binding value in the shader, arrayIndex is the
-- index into the array if the shader binding is declared as an array, and
-- pShaderRecordData is the set of shader record data accessible to the
-- shader.
--
-- If the mapped resource is a @OpTypeSampledImage@, offset is instead
-- calculated for the sampler as
--
-- -   samplerShaderRecordIndex =
--     ((uint32_t*)pShaderRecordData)[samplerShaderRecordOffset\/4]
--
-- -   offset = samplerHeapOffset + (samplerShaderRecordIndex ×
--     samplerHeapIndexStride) + (shaderIndex × samplerHeapArrayStride)
--
-- If @useCombinedImageSamplerIndex@ is
-- 'Vulkan.Core10.FundamentalTypes.TRUE', and the mapped resource is a
-- @OpTypeSampledImage@, shaderRecordIndex and samplerShaderRecordIndex in
-- the above equations are instead calculated as
--
-- -   shaderRecordIndex =
--     ((uint32_t*)pShaderRecordData)[shaderRecordOffset\/4] & 0xFFFFF
--
-- -   samplerShaderRecordIndex =
--     (((uint32_t*)pShaderRecordData)[shaderRecordOffset\/4] >> 20) &
--     0xFFF
--
-- If the mapped resource is a @OpTypeSampler@ or @OpTypeSampledImage@, and
-- @pEmbeddedSampler@ is not @NULL@, the specified embedded sampler will be
-- used rather than accessing the sampler heap.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorMappingSourceShaderRecordIndexEXT-shaderRecordOffset-11269#
--     @shaderRecordOffset@ /must/ be a multiple of 4
--
-- -   #VUID-VkDescriptorMappingSourceShaderRecordIndexEXT-shaderRecordOffset-11270#
--     @shaderRecordOffset@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxShaderGroupStride maxShaderGroupStride>
--     - 4
--
-- -   #VUID-VkDescriptorMappingSourceShaderRecordIndexEXT-pEmbeddedSampler-11449#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', its @borderColor@ /must/
--     not be
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
--     'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- -   #VUID-VkDescriptorMappingSourceShaderRecordIndexEXT-pEmbeddedSampler-11405#
--     If @pEmbeddedSampler@ is a valid pointer to a
--     'Vulkan.Core10.Sampler.SamplerCreateInfo', and there is a
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT'
--     structure in its @pNext@ chain, its @objectType@ /must/ be
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorMappingSourceShaderRecordIndexEXT-pEmbeddedSampler-parameter#
--     If @pEmbeddedSampler@ is not @NULL@, @pEmbeddedSampler@ /must/ be a
--     valid pointer to a valid 'Vulkan.Core10.Sampler.SamplerCreateInfo'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'DescriptorMappingSourceDataEXT',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
data DescriptorMappingSourceShaderRecordIndexEXT = DescriptorMappingSourceShaderRecordIndexEXT
  { -- | @heapOffset@ is a constant byte offset added to the heap address for the
    -- mapped resource or sampler.
    heapOffset :: Word32
  , -- | @shaderRecordOffset@ is an index into shader record data where an index
    -- into the heap for the mapped resource will be retrieved.
    shaderRecordOffset :: Word32
  , -- | @heapIndexStride@ is a constant byte stride that multiplies the index in
    -- shader record data.
    heapIndexStride :: Word32
  , -- | @heapArrayStride@ is a constant byte stride that multiplies the shader
    -- binding and array index.
    heapArrayStride :: Word32
  , -- | @pEmbeddedSampler@ is an optional
    -- 'Vulkan.Core10.Sampler.SamplerCreateInfo' structure specifying a sampler
    -- to embed into the shader, in place of looking the sampler up in a heap.
    embeddedSampler :: Maybe (SomeStruct SamplerCreateInfo)
  , -- | @useCombinedImageSamplerIndex@ specifies whether the generated index
    -- value will be decoded as two packed indices if the mapped resource is an
    -- @OpTypeSampledImage@.
    useCombinedImageSamplerIndex :: Bool
  , -- | @samplerHeapOffset@ is used only when mapping a combined image sampler,
    -- used in place of @heapOffset@ to retrieve the sampler.
    samplerHeapOffset :: Word32
  , -- | @samplerShaderRecordOffset@ is used only when mapping a combined image
    -- sampler, used in place of @shaderRecordOffset@ to retrieve the sampler.
    samplerShaderRecordOffset :: Word32
  , -- | @samplerHeapIndexStride@ is used only when mapping a combined image
    -- sampler, used in place of @heapIndexStride@ to retrieve the sampler.
    samplerHeapIndexStride :: Word32
  , -- | @samplerHeapArrayStride@ is used only when mapping a combined image
    -- sampler, used in place of @heapArrayStride@ to retrieve the sampler.
    samplerHeapArrayStride :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorMappingSourceShaderRecordIndexEXT)
#endif
deriving instance Show DescriptorMappingSourceShaderRecordIndexEXT

instance ToCStruct DescriptorMappingSourceShaderRecordIndexEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorMappingSourceShaderRecordIndexEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (heapOffset)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (shaderRecordOffset)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) (heapIndexStride)
    lift $ poke ((p `plusPtr` 12 :: Ptr Word32)) (heapArrayStride)
    pEmbeddedSampler'' <- case (embeddedSampler) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (SamplerCreateInfo '[])) $ \cont -> withSomeCStruct @SamplerCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (SamplerCreateInfo _)))) pEmbeddedSampler''
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (useCombinedImageSamplerIndex))
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (samplerHeapOffset)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (samplerShaderRecordOffset)
    lift $ poke ((p `plusPtr` 36 :: Ptr Word32)) (samplerHeapIndexStride)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (samplerHeapArrayStride)
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorMappingSourceShaderRecordIndexEXT where
  peekCStruct p = do
    heapOffset <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    shaderRecordOffset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    heapIndexStride <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    heapArrayStride <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pEmbeddedSampler <- peek @(Ptr (SamplerCreateInfo _)) ((p `plusPtr` 16 :: Ptr (Ptr (SamplerCreateInfo _))))
    pEmbeddedSampler' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pEmbeddedSampler
    useCombinedImageSamplerIndex <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    samplerHeapOffset <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    samplerShaderRecordOffset <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    samplerHeapIndexStride <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    samplerHeapArrayStride <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pure $ DescriptorMappingSourceShaderRecordIndexEXT
             heapOffset
             shaderRecordOffset
             heapIndexStride
             heapArrayStride
             pEmbeddedSampler'
             (bool32ToBool useCombinedImageSamplerIndex)
             samplerHeapOffset
             samplerShaderRecordOffset
             samplerHeapIndexStride
             samplerHeapArrayStride

instance Zero DescriptorMappingSourceShaderRecordIndexEXT where
  zero = DescriptorMappingSourceShaderRecordIndexEXT
           zero
           zero
           zero
           zero
           Nothing
           zero
           zero
           zero
           zero
           zero


-- | VkDescriptorMappingSourceIndirectAddressEXT - Structure specifying
-- mapping a uniform buffer to an address specified indirectly
--
-- = Description
--
-- Accessing data via the mapped resource in the shader will access data
-- backing the address specified in the indirect address at the supplied
-- offset:
--
-- -   indirectAddress = ((VkDeviceAddress*)pPushData)[pushOffset\/8]
--
-- -   resourceAddress =
--     ((VkDeviceAddress*)indirectAddress)[addressOffset\/8]
--
-- where pPushData is the total set of push data specified by
-- 'cmdPushDataEXT'. Reads through indirectAddress are performed as
-- non-volatile uniform buffer reads, and can be synchronized using
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_UNIFORM_READ_BIT'. Shader
-- reads through resourceAddress are performed according to the mapped
-- resource. If the shader resource is an acceleration structure, the
-- address /must/ be a valid acceleration structure address.
--
-- == Valid Usage
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'DescriptorMappingSourceDataEXT'
data DescriptorMappingSourceIndirectAddressEXT = DescriptorMappingSourceIndirectAddressEXT
  { -- | @pushOffset@ is a byte offset into push data where an indirect address
    -- containing the address for the mapped resource will be retrieved.
    --
    -- #VUID-VkDescriptorMappingSourceIndirectAddressEXT-pushOffset-11266#
    -- @pushOffset@ /must/ be a multiple of 8
    --
    -- #VUID-VkDescriptorMappingSourceIndirectAddressEXT-pushOffset-11267#
    -- @pushOffset@ /must/ be less than or equal to @maxPushDataSize@ - 8
    pushOffset :: Word32
  , -- | @addressOffset@ is a byte offset into the indirect address where the
    -- address for the mapped resource will be retrieved.
    --
    -- #VUID-VkDescriptorMappingSourceIndirectAddressEXT-addressOffset-11268#
    -- @addressOffset@ /must/ be a multiple of 8
    addressOffset :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorMappingSourceIndirectAddressEXT)
#endif
deriving instance Show DescriptorMappingSourceIndirectAddressEXT

instance ToCStruct DescriptorMappingSourceIndirectAddressEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorMappingSourceIndirectAddressEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (pushOffset)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (addressOffset)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct DescriptorMappingSourceIndirectAddressEXT where
  peekCStruct p = do
    pushOffset <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    addressOffset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ DescriptorMappingSourceIndirectAddressEXT
             pushOffset addressOffset

instance Storable DescriptorMappingSourceIndirectAddressEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorMappingSourceIndirectAddressEXT where
  zero = DescriptorMappingSourceIndirectAddressEXT
           zero
           zero


-- | VkDescriptorSetAndBindingMappingEXT - Structure specifying mappings from
-- a set of shader resources to a descriptor heap
--
-- = Description
--
-- Resources specified in a shader with a
-- 'Vulkan.Core10.Handles.DescriptorSet' decoration set to @descriptorSet@,
-- a @Binding@ decoration greater than or equal to @firstBinding@ and less
-- than the sum of @firstBinding@ and @bindingCount@, and a resource type
-- matching one of the bits in @resourceMask@ will be mapped according to
-- @source@ and @sourceData@.
--
-- Applications are free to overspecify bindings that are not present;
-- allowing reuse of the same mapping structures with multiple shaders,
-- even when those shaders only partially reuse those mappings. This
-- includes things like setting binding counts higher than the number used
-- in the shader, specifying bindings that are not present in the shader,
-- and setting @resourceMask@ to all possible resources that may be
-- encountered.
--
-- If @source@ selects an element of @sourceData@ defined by a structure,
-- the description of that structure defines how resources are mapped.
-- Source mappings using a single base type are defined here.
--
-- If @source@ is
-- 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT', the resource
-- will be backed by heap data as specified by
-- <VkDescriptorMappingSourceConstantOffsetEXT.html constantOffset>.
--
-- If @source@ is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT', the
-- resource will be backed by heap data as specified by
-- <VkDescriptorMappingSourcePushIndexEXT.html pushIndex>.
--
-- If @source@ is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
-- the resource will be backed by heap data as specified by
-- <VkDescriptorMappingSourceIndirectIndexEXT.html indirectIndex>.
--
-- If @source@ is
-- 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT', the
-- resource will be backed by heap data as specified by
-- <VkDescriptorMappingSourceIndirectIndexEXT.html indirectIndexArray>.
--
-- If @source@ is 'DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT', the
-- resource will be backed by heap data as specified by
-- <VkDescriptorMappingSourceHeapDataEXT.html heapData>.
--
-- If @source@ is 'DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT', the resource
-- will be backed by push data at a range from @pushDataOffset@ to the size
-- of the resource, allowing a uniform buffer to be backed by push data
-- access push data. Accessing data in the uniform buffer at an offset of
-- shaderOffset in the shader will access push data at an offset equal to
--
-- -   offset = shaderOffset + pushDataOffset.
--
-- If @source@ is 'DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT', the
-- resource will be backed by data pointed to by a device address in push
-- data at an offset of @pushAddressOffset@. Accessing data via the mapped
-- resource in the shader will access data backing the address specified in
-- push data:
--
-- -   address = ((VkDeviceAddress*)pPushData)[pushAddressOffset\/8]
--
-- where pPushData is the total set of push data specified by
-- 'cmdPushDataEXT'. If the shader resource is an acceleration structure,
-- the address /must/ be a valid acceleration structure address.
--
-- If @source@ is 'DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT', the
-- resource will be backed by heap data as specified by
-- <VkDescriptorMappingSourceIndirectAddressEXT.html indirectAddress>.
--
-- Accesses to resources using mappings to anything that is not a
-- descriptor in a heap are not subject to robustness guarantees; resources
-- for such mappings must not be accessed out of bounds.
--
-- If @source@ is
-- 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT', the
-- resource will be backed by heap data as specified by
-- <VkDescriptorMappingSourceShaderRecordIndexEXT.html shaderRecordIndex>.
--
-- If @source@ is 'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT', the
-- resource will be backed by shader record data at a range from
-- @shaderRecordDataOffset@ to the size of the resource, allowing a uniform
-- buffer to be used as a way to access shader record data. Accessing data
-- in the uniform buffer at an offset shaderOffset in the shader will
-- access shader record data at an offset equal to
--
-- -   offset = shaderOffset + shaderRecordDataOffset.
--
-- If @source@ is 'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
-- the resource will be backed by data pointed to by a device address in
-- the shader record at @shaderRecordAddressOffset@. Accessing data via the
-- mapped resource in the shader will access data backing the address
-- specified in shader record data:
--
-- -   address =
--     ((VkDeviceAddress*)pShaderRecordData)[shaderRecordAddressOffset\/8]
--
-- where pShaderRecord is the memory associated with a given shader as its
-- shader record. If the shader resource is an acceleration structure, the
-- address /must/ be a valid acceleration structure address.
--
-- Accesses to resources using
-- 'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT' mappings are not
-- subject to robustness guarantees; data must not be accessed outside of
-- the allocated memory range.
--
-- Mappings must be declared for all variables with a
-- 'Vulkan.Core10.Handles.DescriptorSet' and @Binding@ in the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-resources shader resource interface>.
--
-- == Valid Usage
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11245# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT', or
--     'DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT', @bindingCount@
--     /must/ be 1
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11246# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT',
--     @sourceData.pushDataOffset@ /must/ be a multiple of 4
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11247# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     @sourceData.pushAddressOffset@ /must/ be a multiple of 8
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11248# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT' or
--     'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     @bindingCount@ /must/ be 1
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11249# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT',
--     @sourceData.shaderRecordDataOffset@ /must/ be a multiple of 4
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11250# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     @sourceData.shaderRecordAddressOffset@ /must/ be a multiple of 8
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11251# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT', or
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT', and
--     @descriptorSet@, @firstBinding@, and @bindingCount@ identify any
--     @OpTypeImage@ variables, any @heapOffset@, and @heapArrayStride@
--     members of the corresponding member of @sourceData@ /must/ be 0 or a
--     multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-imageDescriptorAlignment imageDescriptorAlignment>
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11252# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT', or
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT', and
--     @descriptorSet@, @firstBinding@, and @bindingCount@ identify any
--     @OpTypeStruct@ variables, any @heapOffset@, and @heapArrayStride@
--     members of the corresponding member of @sourceData@ /must/ be 0 or a
--     multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-bufferDescriptorAlignment bufferDescriptorAlignment>
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11253# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT', or
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT', and
--     @descriptorSet@, @firstBinding@, and @bindingCount@ identify any
--     @OpTypeSampler@ variables, any @heapOffset@ and @heapArrayStride@
--     members of the corresponding member of @sourceData@ /must/ be 0 or a
--     multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-samplerDescriptorAlignment samplerDescriptorAlignment>
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11254# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT', or
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT', and
--     @descriptorSet@, @firstBinding@, and @bindingCount@ identify any
--     @OpTypeSampledImage@ variables, any @samplerHeapOffset@ and
--     @samplerHeapArrayStride@ members of the corresponding member of
--     @sourceData@ /must/ be 0 or a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-samplerDescriptorAlignment samplerDescriptorAlignment>
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11356# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT', or
--     'DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT', @resourceMask@ /must/
--     include 'SPIRV_RESOURCE_TYPE_UNIFORM_BUFFER_BIT_EXT'
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11357# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT' or
--     'DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT', @resourceMask@ /must/
--     include at least one of
--     'SPIRV_RESOURCE_TYPE_UNIFORM_BUFFER_BIT_EXT',
--     'SPIRV_RESOURCE_TYPE_READ_ONLY_STORAGE_BUFFER_BIT_EXT',
--     'SPIRV_RESOURCE_TYPE_READ_WRITE_STORAGE_BUFFER_BIT_EXT', or
--     'SPIRV_RESOURCE_TYPE_ACCELERATION_STRUCTURE_BIT_EXT'
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11358# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT', or
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT', and
--     the mapping sets @useCombinedImageSamplerIndex@ to
--     'Vulkan.Core10.FundamentalTypes.TRUE', @resourceMask@ /must/ include
--     at least one of
--     'SPIRV_RESOURCE_TYPE_COMBINED_SAMPLED_IMAGE_BIT_EXT',
--     'SPIRV_RESOURCE_TYPE_SAMPLED_IMAGE_BIT_EXT', or
--     'SPIRV_RESOURCE_TYPE_SAMPLER_BIT_EXT'
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11389# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT', or
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT', and
--     @bindingCount@ is not @1@, the @pEmbeddedSampler@ member of the
--     corresponding mapping structure /must/ be @NULL@
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-11390# If @source@
--     is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT', or
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT', and
--     @descriptorSet@, @firstBinding@, and @bindingCount@ identify any
--     @OpTypeTensorARM@ variables, the @heapOffset@, and @heapArrayStride@
--     members of the corresponding member of @sourceData@ /must/ be 0 or a
--     multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-tensorDescriptorAlignment tensorDescriptorAlignment>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_SET_AND_BINDING_MAPPING_EXT'
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-pNext-pNext# @pNext@
--     /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_NV_push_constant_bank.PushConstantBankInfoNV'
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-resourceMask-parameter#
--     @resourceMask@ /must/ be a valid combination of
--     'SpirvResourceTypeFlagBitsEXT' values
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-resourceMask-requiredbitmask#
--     @resourceMask@ /must/ not be @0@
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-source-parameter# @source@
--     /must/ be a valid 'DescriptorMappingSourceEXT' value
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-constantOffset-parameter#
--     If @source@ is
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT', the
--     @constantOffset@ member of @sourceData@ /must/ be a valid
--     'DescriptorMappingSourceConstantOffsetEXT' structure
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-pushIndex-parameter# If
--     @source@ is 'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     the @pushIndex@ member of @sourceData@ /must/ be a valid
--     'DescriptorMappingSourcePushIndexEXT' structure
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-indirectIndex-parameter#
--     If @source@ is
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT', the
--     @indirectIndex@ member of @sourceData@ /must/ be a valid
--     'DescriptorMappingSourceIndirectIndexEXT' structure
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-indirectIndexArray-parameter#
--     If @source@ is
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT', the
--     @indirectIndexArray@ member of @sourceData@ /must/ be a valid
--     'DescriptorMappingSourceIndirectIndexArrayEXT' structure
--
-- -   #VUID-VkDescriptorSetAndBindingMappingEXT-shaderRecordIndex-parameter#
--     If @source@ is
--     'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT', the
--     @shaderRecordIndex@ member of @sourceData@ /must/ be a valid
--     'DescriptorMappingSourceShaderRecordIndexEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'DescriptorMappingSourceDataEXT', 'DescriptorMappingSourceEXT',
-- 'ShaderDescriptorSetAndBindingMappingInfoEXT',
-- 'SpirvResourceTypeFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DescriptorSetAndBindingMappingEXT (es :: [Type]) = DescriptorSetAndBindingMappingEXT
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @descriptorSet@ is the value of 'Vulkan.Core10.Handles.DescriptorSet'
    -- for resources that this mapping affects.
    descriptorSet :: Word32
  , -- | @firstBinding@ is the first value of @Binding@ of resources that this
    -- mapping affects.
    firstBinding :: Word32
  , -- | @bindingCount@ is the number of consecutive @Binding@ values of
    -- resources that this mapping affects.
    bindingCount :: Word32
  , -- | @resourceMask@ is a mask of 'SpirvResourceTypeFlagBitsEXT' values
    -- indicating which resource types are specified by this mapping.
    resourceMask :: SpirvResourceTypeFlagsEXT
  , -- | @source@ is a 'DescriptorMappingSourceEXT' value specifying the method
    -- of mapping specified for the affected resources.
    source :: DescriptorMappingSourceEXT
  , -- | @sourceData@ is a 'DescriptorMappingSourceDataEXT' that provides the
    -- details of how each mapping is specified according to @source@.
    sourceData :: DescriptorMappingSourceDataEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorSetAndBindingMappingEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DescriptorSetAndBindingMappingEXT es)

instance Extensible DescriptorSetAndBindingMappingEXT where
  extensibleTypeName = "DescriptorSetAndBindingMappingEXT"
  setNext DescriptorSetAndBindingMappingEXT{..} next' = DescriptorSetAndBindingMappingEXT{next = next', ..}
  getNext DescriptorSetAndBindingMappingEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DescriptorSetAndBindingMappingEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PushConstantBankInfoNV = Just f
    | otherwise = Nothing

instance ( Extendss DescriptorSetAndBindingMappingEXT es
         , PokeChain es ) => ToCStruct (DescriptorSetAndBindingMappingEXT es) where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorSetAndBindingMappingEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_AND_BINDING_MAPPING_EXT)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (descriptorSet)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (firstBinding)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (bindingCount)
    lift $ poke ((p `plusPtr` 28 :: Ptr SpirvResourceTypeFlagsEXT)) (resourceMask)
    lift $ poke ((p `plusPtr` 32 :: Ptr DescriptorMappingSourceEXT)) (source)
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr DescriptorMappingSourceDataEXT)) (sourceData) . ($ ())
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_SET_AND_BINDING_MAPPING_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr SpirvResourceTypeFlagsEXT)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DescriptorMappingSourceEXT)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr DescriptorMappingSourceDataEXT)) (zero) . ($ ())
    lift $ f

instance es ~ '[] => Zero (DescriptorSetAndBindingMappingEXT es) where
  zero = DescriptorSetAndBindingMappingEXT
           ()
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkShaderDescriptorSetAndBindingMappingInfoEXT - Structure specifying
-- mappings from shader resources to descriptor heaps
--
-- = Description
--
-- Including this structure in the @pNext@ chain of
-- 'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo' will set
-- mappings for the shader defined by that structure. Similarly, including
-- this structure in the @pNext@ chain of a
-- 'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateInfoEXT' with a
-- @codeType@ of
-- 'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CODE_TYPE_SPIRV_EXT',
-- will set mappings for that shader.
--
-- If this structure is not present, it is equivalent to setting
-- @mappingCount@ to 0.
--
-- == Valid Usage
--
-- -   #VUID-VkShaderDescriptorSetAndBindingMappingInfoEXT-pMappings-11244#
--     Any two elements of @pMappings@ /must/ not have the same value of
--     @descriptorSet@, an overlapping range specified by @firstBinding@
--     and @bindingCount@, and any overlapping bits in @resourceMask@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkShaderDescriptorSetAndBindingMappingInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_DESCRIPTOR_SET_AND_BINDING_MAPPING_INFO_EXT'
--
-- -   #VUID-VkShaderDescriptorSetAndBindingMappingInfoEXT-pMappings-parameter#
--     If @mappingCount@ is not @0@, @pMappings@ /must/ be a valid pointer
--     to an array of @mappingCount@ valid
--     'DescriptorSetAndBindingMappingEXT' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'DescriptorSetAndBindingMappingEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ShaderDescriptorSetAndBindingMappingInfoEXT = ShaderDescriptorSetAndBindingMappingInfoEXT
  { -- | @pMappings@ is a pointer to an array of
    -- 'DescriptorSetAndBindingMappingEXT' structures specifying mappings for a
    -- set of descriptors
    mappings :: Vector (SomeStruct DescriptorSetAndBindingMappingEXT) }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderDescriptorSetAndBindingMappingInfoEXT)
#endif
deriving instance Show ShaderDescriptorSetAndBindingMappingInfoEXT

instance ToCStruct ShaderDescriptorSetAndBindingMappingInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderDescriptorSetAndBindingMappingInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_DESCRIPTOR_SET_AND_BINDING_MAPPING_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (mappings)) :: Word32))
    pPMappings' <- ContT $ allocaBytes @(DescriptorSetAndBindingMappingEXT _) ((Data.Vector.length (mappings)) * 96)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPMappings' `plusPtr` (96 * (i)) :: Ptr (DescriptorSetAndBindingMappingEXT _))) (e) . ($ ())) (mappings)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (DescriptorSetAndBindingMappingEXT _)))) (pPMappings')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_DESCRIPTOR_SET_AND_BINDING_MAPPING_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance Zero ShaderDescriptorSetAndBindingMappingInfoEXT where
  zero = ShaderDescriptorSetAndBindingMappingInfoEXT
           mempty


-- | VkSamplerCustomBorderColorIndexCreateInfoEXT - Structure specifying the
-- custom border color index for a sampler
--
-- = Description
--
-- If this structure is included in the @pNext@ chain of
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo', the value of @index@ will be
-- used for the custom border color registration. @index@ does not need to
-- be registered at the point that a sampler object is created or a sampler
-- descriptor is written; as long as it is registered when any use of the
-- sampler is recorded to a command, and remains registered while the
-- sampler is in use. The color registered with the index and the color
-- specified in the sampler /must/ be identically defined.
--
-- If this structure is not provided when creating a sampler object with a
-- custom border color, it is equivalent to registering a new custom border
-- color by calling 'registerCustomBorderColorEXT' with that custom border
-- color value, and using that value as @index@ in this structure. This
-- implicit registration will be implicitly unregistered when the sampler
-- is destroyed.
--
-- If this structure is not provided when creating a sampler object without
-- a custom border color, it is equivalent to setting @index@ to 0.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_custom_border_color VK_EXT_custom_border_color>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SamplerCustomBorderColorIndexCreateInfoEXT = SamplerCustomBorderColorIndexCreateInfoEXT
  { -- | @index@ is the @uint32_t@ index value to use with the sampler
    --
    -- #VUID-VkSamplerCustomBorderColorIndexCreateInfoEXT-index-11289# @index@
    -- /must/ be less than
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxCustomBorderColorSamplers maxCustomBorderColorSamplers>
    index :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SamplerCustomBorderColorIndexCreateInfoEXT)
#endif
deriving instance Show SamplerCustomBorderColorIndexCreateInfoEXT

instance ToCStruct SamplerCustomBorderColorIndexCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SamplerCustomBorderColorIndexCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_INDEX_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (index)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_INDEX_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct SamplerCustomBorderColorIndexCreateInfoEXT where
  peekCStruct p = do
    index <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ SamplerCustomBorderColorIndexCreateInfoEXT
             index

instance Storable SamplerCustomBorderColorIndexCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SamplerCustomBorderColorIndexCreateInfoEXT where
  zero = SamplerCustomBorderColorIndexCreateInfoEXT
           zero


-- | VkOpaqueCaptureDataCreateInfoEXT - Structure specifying opaque capture
-- data
--
-- = Description
--
-- When an image is created with
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_EXT'
-- set in 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@, if the @pNext@
-- chain of 'Vulkan.Core10.Image.ImageCreateInfo' includes this structure,
-- and @pData@ is not @NULL@, the implementation will attempt to recreate
-- the image such that descriptors written with
-- 'writeResourceDescriptorsEXT' will be reproduced with the same bit
-- pattern as during capture if possible. If the implementation is unable
-- to recreate the image based on this data, image creation will fail and
-- return
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'.
--
-- When a tensor is created with
-- 'Vulkan.Extensions.VK_ARM_tensors.TENSOR_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_ARM'
-- set in 'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM'::@flags@,
-- if the @pNext@ chain of
-- 'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM' includes this
-- structure, and @pData@ is not @NULL@, the implementation will attempt to
-- recreate the tensor such that descriptors written with
-- 'writeResourceDescriptorsEXT' will be reproduced with the same bit
-- pattern as during capture if possible. If the implementation is unable
-- to recreate the tensor based on this data, tensor creation will fail and
-- return
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'.
--
-- If this structure is not present, it is equivalent to setting @pData@ to
-- @NULL@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkOpaqueCaptureDataCreateInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPAQUE_CAPTURE_DATA_CREATE_INFO_EXT'
--
-- -   #VUID-VkOpaqueCaptureDataCreateInfoEXT-pData-parameter# If @pData@
--     is not @NULL@, @pData@ /must/ be a valid pointer to a valid
--     'HostAddressRangeConstEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'HostAddressRangeConstEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data OpaqueCaptureDataCreateInfoEXT = OpaqueCaptureDataCreateInfoEXT
  { -- | @pData@ is a pointer to the range of host memory containing opaque data
    -- previously captured via 'getImageOpaqueCaptureDataEXT'.
    data' :: Maybe HostAddressRangeConstEXT }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (OpaqueCaptureDataCreateInfoEXT)
#endif
deriving instance Show OpaqueCaptureDataCreateInfoEXT

instance ToCStruct OpaqueCaptureDataCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p OpaqueCaptureDataCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPAQUE_CAPTURE_DATA_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pData'' <- case (data') of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr HostAddressRangeConstEXT))) pData''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OPAQUE_CAPTURE_DATA_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct OpaqueCaptureDataCreateInfoEXT where
  peekCStruct p = do
    pData <- peek @(Ptr HostAddressRangeConstEXT) ((p `plusPtr` 16 :: Ptr (Ptr HostAddressRangeConstEXT)))
    pData' <- maybePeek (\j -> peekCStruct @HostAddressRangeConstEXT (j)) pData
    pure $ OpaqueCaptureDataCreateInfoEXT
             pData'

instance Zero OpaqueCaptureDataCreateInfoEXT where
  zero = OpaqueCaptureDataCreateInfoEXT
           Nothing


-- | VkIndirectCommandsLayoutPushDataTokenNV - Struct specifying the details
-- of an indirect push data command layout token
--
-- = Description
--
-- If this structure is in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV',
-- and
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV'::@tokenType@
-- is set to
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_DATA_NV',
-- this structure defines a push data command layout token.
--
-- If this structure is not provided, it is equivalent to setting
-- @pushDataOffset@ and @pushDataSize@ to 0.
--
-- == Valid Usage
--
-- -   #VUID-VkIndirectCommandsLayoutPushDataTokenNV-pushDataOffset-11335#
--     The sum of @pushDataOffset@ and @pushDataSize@ /must/ be less than
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxPushDataSize maxPushDataSize>
--
-- -   #VUID-VkIndirectCommandsLayoutPushDataTokenNV-pushDataOffset-11420#
--     @pushDataOffset@ /must/ be a multiple of 4
--
-- -   #VUID-VkIndirectCommandsLayoutPushDataTokenNV-pushDataSize-11421#
--     @pushDataSize@ /must/ be a multiple of 4
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkIndirectCommandsLayoutPushDataTokenNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_PUSH_DATA_TOKEN_NV'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data IndirectCommandsLayoutPushDataTokenNV = IndirectCommandsLayoutPushDataTokenNV
  { -- | @pushDataOffset@ is the offset used for the push data command.
    pushDataOffset :: Word32
  , -- | @pushDataSize@ is the size used for the push data command.
    pushDataSize :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsLayoutPushDataTokenNV)
#endif
deriving instance Show IndirectCommandsLayoutPushDataTokenNV

instance ToCStruct IndirectCommandsLayoutPushDataTokenNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutPushDataTokenNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_PUSH_DATA_TOKEN_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (pushDataOffset)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (pushDataSize)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_PUSH_DATA_TOKEN_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct IndirectCommandsLayoutPushDataTokenNV where
  peekCStruct p = do
    pushDataOffset <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pushDataSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ IndirectCommandsLayoutPushDataTokenNV
             pushDataOffset pushDataSize

instance Storable IndirectCommandsLayoutPushDataTokenNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsLayoutPushDataTokenNV where
  zero = IndirectCommandsLayoutPushDataTokenNV
           zero
           zero


-- | VkSubsampledImageFormatPropertiesEXT - Structure specifying image
-- descriptor count for subsampled images
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SubsampledImageFormatPropertiesEXT = SubsampledImageFormatPropertiesEXT
  { -- | @subsampledImageDescriptorCount@ is the number of image descriptors that
    -- the implementation uses to access the image.
    subsampledImageDescriptorCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubsampledImageFormatPropertiesEXT)
#endif
deriving instance Show SubsampledImageFormatPropertiesEXT

instance ToCStruct SubsampledImageFormatPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubsampledImageFormatPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBSAMPLED_IMAGE_FORMAT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (subsampledImageDescriptorCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBSAMPLED_IMAGE_FORMAT_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct SubsampledImageFormatPropertiesEXT where
  peekCStruct p = do
    subsampledImageDescriptorCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ SubsampledImageFormatPropertiesEXT
             subsampledImageDescriptorCount

instance Storable SubsampledImageFormatPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubsampledImageFormatPropertiesEXT where
  zero = SubsampledImageFormatPropertiesEXT
           zero


-- | VkPhysicalDeviceDescriptorHeapFeaturesEXT - Structure describing support
-- for descriptor heaps
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorHeapFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDescriptorHeapFeaturesEXT', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorHeapFeaturesEXT = PhysicalDeviceDescriptorHeapFeaturesEXT
  { -- | #features-descriptorHeap# @descriptorHeap@ specifies whether
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps descriptor heaps>
    -- /can/ be used.
    descriptorHeap :: Bool
  , -- | #features-descriptorHeapCaptureReplay# @descriptorHeapCaptureReplay@
    -- specifies whether
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorheaps-writing heap descriptors>
    -- /can/ be captured and replayed.
    descriptorHeapCaptureReplay :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorHeapFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDescriptorHeapFeaturesEXT

instance ToCStruct PhysicalDeviceDescriptorHeapFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorHeapFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (descriptorHeap))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (descriptorHeapCaptureReplay))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorHeapFeaturesEXT where
  peekCStruct p = do
    descriptorHeap <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    descriptorHeapCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceDescriptorHeapFeaturesEXT
             (bool32ToBool descriptorHeap)
             (bool32ToBool descriptorHeapCaptureReplay)

instance Storable PhysicalDeviceDescriptorHeapFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorHeapFeaturesEXT where
  zero = PhysicalDeviceDescriptorHeapFeaturesEXT
           zero
           zero


-- | VkPhysicalDeviceDescriptorHeapPropertiesEXT - Structure describing
-- supported image alignments for a physical device
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorHeapPropertiesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorHeapPropertiesEXT = PhysicalDeviceDescriptorHeapPropertiesEXT
  { -- | #limits-samplerHeapAlignment# @samplerHeapAlignment@ specifies the
    -- required alignment of the @heapRange->address@ member of
    -- 'BindHeapInfoEXT' for binding sampler heaps. It must be a power-of-two
    -- value.
    samplerHeapAlignment :: DeviceSize
  , -- | #limits-resourceHeapAlignment# @resourceHeapAlignment@ specifies the
    -- required alignment of the @heapRange->address@ member of
    -- 'BindHeapInfoEXT' for binding resource heaps. It must be a power-of-two
    -- value.
    resourceHeapAlignment :: DeviceSize
  , -- | #limits-maxSamplerHeapSize# @maxSamplerHeapSize@ describes maximum value
    -- of the @size@ member of 'DeviceAddressRangeEXT' for binding sampler
    -- heaps, including the reservation, when embedded samplers are used.
    maxSamplerHeapSize :: DeviceSize
  , -- | #limits-maxResourceHeapSize# @maxResourceHeapSize@ describes maximum
    -- value of the @size@ member of 'DeviceAddressRangeEXT' for binding
    -- resource heaps, including the reservation.
    maxResourceHeapSize :: DeviceSize
  , -- | #limits-minSamplerHeapReservedRange# @minSamplerHeapReservedRange@
    -- specifies the minimum amount of data that the implementation needs to be
    -- reserved within the bound sampler heap range when embedded samplers are
    -- not used.
    minSamplerHeapReservedRange :: DeviceSize
  , -- | #limits-minSamplerHeapReservedRangeWithEmbedded#
    -- @minSamplerHeapReservedRangeWithEmbedded@ specifies the minimum amount
    -- of data that the implementation needs to be reserved within the bound
    -- sampler heap range when embedded samplers are used.
    minSamplerHeapReservedRangeWithEmbedded :: DeviceSize
  , -- | #limits-minResourceHeapReservedRange# @minResourceHeapReservedRange@
    -- specifies the minimum amount of data that the implementation needs to be
    -- reserved within the bound resource heap range.
    minResourceHeapReservedRange :: DeviceSize
  , -- | #limits-samplerDescriptorSize# @samplerDescriptorSize@ specifies the
    -- size of sampler descriptors written by 'writeSamplerDescriptorsEXT'. It
    -- /must/ be a power-of-two value.
    samplerDescriptorSize :: DeviceSize
  , -- | #limits-imageDescriptorSize# @imageDescriptorSize@ specifies the maximum
    -- size of image and texel buffer descriptors written by
    -- 'writeResourceDescriptorsEXT'. It /must/ be a power-of-two value.
    imageDescriptorSize :: DeviceSize
  , -- | #limits-bufferDescriptorSize# @bufferDescriptorSize@ specifies the
    -- maximum size of unformatted buffer descriptors or acceleration
    -- structures written by 'writeResourceDescriptorsEXT'. It /must/ be a
    -- power-of-two value.
    bufferDescriptorSize :: DeviceSize
  , -- | #limits-samplerDescriptorAlignment# @samplerDescriptorAlignment@
    -- specifies the required alignment of sampler descriptors within a sampler
    -- heap. It must be a power-of-two value, and less than or equal to
    -- @samplerDescriptorSize@.
    samplerDescriptorAlignment :: DeviceSize
  , -- | #limits-imageDescriptorAlignment# @imageDescriptorAlignment@ specifies
    -- the required alignment of image descriptors within a resource heap. It
    -- must be a power-of-two value, and less than or equal to
    -- @imageDescriptorSize@.
    imageDescriptorAlignment :: DeviceSize
  , -- | #limits-bufferDescriptorAlignment# @bufferDescriptorAlignment@ specifies
    -- the required alignment of buffer descriptors within a resource heap. It
    -- must be a power-of-two value, and less than or equal to
    -- @bufferDescriptorSize@.
    bufferDescriptorAlignment :: DeviceSize
  , -- | #limits-maxPushDataSize# @maxPushDataSize@ specifies the maximum total
    -- size of all push data.
    maxPushDataSize :: DeviceSize
  , -- | #limits-imageCaptureReplayOpaqueDataSize#
    -- @imageCaptureReplayOpaqueDataSize@ specifies the size of the opaque
    -- capture\/replay data for an image.
    imageCaptureReplayOpaqueDataSize :: Word64
  , -- | #limits-maxDescriptorHeapEmbeddedSamplers#
    -- @maxDescriptorHeapEmbeddedSamplers@ specifies the maximum number of
    -- unique embedded samplers across all pipelines.
    maxDescriptorHeapEmbeddedSamplers :: Word32
  , -- | #limits-samplerYcbcrConversionCount# @samplerYcbcrConversionCount@
    -- specifies the number of sampler descriptors required for any sampler
    -- using YCBCR conversion.
    samplerYcbcrConversionCount :: Word32
  , -- | #limits-sparseDescriptorHeaps# @sparseDescriptorHeaps@ specifies whether
    -- descriptor heaps can be backed by sparse memory or not. If this value is
    -- 'Vulkan.Core10.FundamentalTypes.FALSE', buffers cannot be specified as
    -- both sparse and having descriptor heap usage.
    sparseDescriptorHeaps :: Bool
  , -- | #limits-protectedDescriptorHeaps# @protectedDescriptorHeaps@ specifies
    -- whether descriptor heaps can be used with protected submissions or not.
    -- If this value is 'Vulkan.Core10.FundamentalTypes.FALSE', buffers cannot
    -- be specified as both protected and having descriptor heap usage.
    protectedDescriptorHeaps :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorHeapPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceDescriptorHeapPropertiesEXT

instance ToCStruct PhysicalDeviceDescriptorHeapPropertiesEXT where
  withCStruct x f = allocaBytes 152 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorHeapPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (samplerHeapAlignment)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (resourceHeapAlignment)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (maxSamplerHeapSize)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (maxResourceHeapSize)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (minSamplerHeapReservedRange)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (minSamplerHeapReservedRangeWithEmbedded)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (minResourceHeapReservedRange)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (samplerDescriptorSize)
    poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (imageDescriptorSize)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (bufferDescriptorSize)
    poke ((p `plusPtr` 96 :: Ptr DeviceSize)) (samplerDescriptorAlignment)
    poke ((p `plusPtr` 104 :: Ptr DeviceSize)) (imageDescriptorAlignment)
    poke ((p `plusPtr` 112 :: Ptr DeviceSize)) (bufferDescriptorAlignment)
    poke ((p `plusPtr` 120 :: Ptr DeviceSize)) (maxPushDataSize)
    poke ((p `plusPtr` 128 :: Ptr CSize)) (CSize (imageCaptureReplayOpaqueDataSize))
    poke ((p `plusPtr` 136 :: Ptr Word32)) (maxDescriptorHeapEmbeddedSamplers)
    poke ((p `plusPtr` 140 :: Ptr Word32)) (samplerYcbcrConversionCount)
    poke ((p `plusPtr` 144 :: Ptr Bool32)) (boolToBool32 (sparseDescriptorHeaps))
    poke ((p `plusPtr` 148 :: Ptr Bool32)) (boolToBool32 (protectedDescriptorHeaps))
    f
  cStructSize = 152
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 96 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 104 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 112 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 120 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 128 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 136 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 140 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 144 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 148 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorHeapPropertiesEXT where
  peekCStruct p = do
    samplerHeapAlignment <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    resourceHeapAlignment <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    maxSamplerHeapSize <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    maxResourceHeapSize <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    minSamplerHeapReservedRange <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    minSamplerHeapReservedRangeWithEmbedded <- peek @DeviceSize ((p `plusPtr` 56 :: Ptr DeviceSize))
    minResourceHeapReservedRange <- peek @DeviceSize ((p `plusPtr` 64 :: Ptr DeviceSize))
    samplerDescriptorSize <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    imageDescriptorSize <- peek @DeviceSize ((p `plusPtr` 80 :: Ptr DeviceSize))
    bufferDescriptorSize <- peek @DeviceSize ((p `plusPtr` 88 :: Ptr DeviceSize))
    samplerDescriptorAlignment <- peek @DeviceSize ((p `plusPtr` 96 :: Ptr DeviceSize))
    imageDescriptorAlignment <- peek @DeviceSize ((p `plusPtr` 104 :: Ptr DeviceSize))
    bufferDescriptorAlignment <- peek @DeviceSize ((p `plusPtr` 112 :: Ptr DeviceSize))
    maxPushDataSize <- peek @DeviceSize ((p `plusPtr` 120 :: Ptr DeviceSize))
    imageCaptureReplayOpaqueDataSize <- peek @CSize ((p `plusPtr` 128 :: Ptr CSize))
    maxDescriptorHeapEmbeddedSamplers <- peek @Word32 ((p `plusPtr` 136 :: Ptr Word32))
    samplerYcbcrConversionCount <- peek @Word32 ((p `plusPtr` 140 :: Ptr Word32))
    sparseDescriptorHeaps <- peek @Bool32 ((p `plusPtr` 144 :: Ptr Bool32))
    protectedDescriptorHeaps <- peek @Bool32 ((p `plusPtr` 148 :: Ptr Bool32))
    pure $ PhysicalDeviceDescriptorHeapPropertiesEXT
             samplerHeapAlignment
             resourceHeapAlignment
             maxSamplerHeapSize
             maxResourceHeapSize
             minSamplerHeapReservedRange
             minSamplerHeapReservedRangeWithEmbedded
             minResourceHeapReservedRange
             samplerDescriptorSize
             imageDescriptorSize
             bufferDescriptorSize
             samplerDescriptorAlignment
             imageDescriptorAlignment
             bufferDescriptorAlignment
             maxPushDataSize
             (coerce @CSize @Word64 imageCaptureReplayOpaqueDataSize)
             maxDescriptorHeapEmbeddedSamplers
             samplerYcbcrConversionCount
             (bool32ToBool sparseDescriptorHeaps)
             (bool32ToBool protectedDescriptorHeaps)

instance Storable PhysicalDeviceDescriptorHeapPropertiesEXT where
  sizeOf ~_ = 152
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorHeapPropertiesEXT where
  zero = PhysicalDeviceDescriptorHeapPropertiesEXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkCommandBufferInheritanceDescriptorHeapInfoEXT - Structure specifying
-- command buffer inheritance information
--
-- = Description
--
-- If this structure is not present, the behavior is as if
-- @pSamplerHeapBindInfo@ and @pResourceHeapBindInfo@ were both @NULL@.
--
-- == Valid Usage
--
-- -   #VUID-VkCommandBufferInheritanceDescriptorHeapInfoEXT-descriptorHeap-11200#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorHeap descriptorHeap>
--     feature is not enabled, @pSamplerHeapBindInfo@ /must/ be @NULL@
--
-- -   #VUID-VkCommandBufferInheritanceDescriptorHeapInfoEXT-descriptorHeap-11201#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-descriptorHeap descriptorHeap>
--     feature is not enabled, @pResourceHeapBindInfo@ /must/ be @NULL@
--
-- -   #VUID-VkCommandBufferInheritanceDescriptorHeapInfoEXT-pSamplerHeapBindInfo-11470#
--     If @pSamplerHeapBindInfo@ is not @NULL@,
--     @pSamplerHeapBindInfo->heapRange@ /must/ be a device address range
--     allocated to the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT'
--     usage flag set
--
-- -   #VUID-VkCommandBufferInheritanceDescriptorHeapInfoEXT-pResourceHeapBindInfo-11471#
--     If @pResourceHeapBindInfo@ is not @NULL@,
--     @pResourceHeapBindInfo->heapRange@ /must/ be a device address range
--     allocated to the application from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_DESCRIPTOR_HEAP_BIT_EXT'
--     usage flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCommandBufferInheritanceDescriptorHeapInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_DESCRIPTOR_HEAP_INFO_EXT'
--
-- -   #VUID-VkCommandBufferInheritanceDescriptorHeapInfoEXT-pSamplerHeapBindInfo-parameter#
--     If @pSamplerHeapBindInfo@ is not @NULL@, @pSamplerHeapBindInfo@
--     /must/ be a valid pointer to a valid 'BindHeapInfoEXT' structure
--
-- -   #VUID-VkCommandBufferInheritanceDescriptorHeapInfoEXT-pResourceHeapBindInfo-parameter#
--     If @pResourceHeapBindInfo@ is not @NULL@, @pResourceHeapBindInfo@
--     /must/ be a valid pointer to a valid 'BindHeapInfoEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'BindHeapInfoEXT', 'Vulkan.Core10.Enums.StructureType.StructureType'
data CommandBufferInheritanceDescriptorHeapInfoEXT = CommandBufferInheritanceDescriptorHeapInfoEXT
  { -- | @pSamplerHeapBindInfo@ specifies the 'BindHeapInfoEXT' of the sampler
    -- heap bound using 'cmdBindSamplerHeapEXT' in the primary. If this is
    -- @NULL@, it indicates that no sampler heap is bound.
    samplerHeapBindInfo :: Maybe BindHeapInfoEXT
  , -- | @pResourceHeapBindInfo@ specifies the 'BindHeapInfoEXT' of the resource
    -- heap bound using 'cmdBindResourceHeapEXT' in the primary. If this is
    -- @NULL@, it indicates that no resource heap is bound.
    resourceHeapBindInfo :: Maybe BindHeapInfoEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferInheritanceDescriptorHeapInfoEXT)
#endif
deriving instance Show CommandBufferInheritanceDescriptorHeapInfoEXT

instance ToCStruct CommandBufferInheritanceDescriptorHeapInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferInheritanceDescriptorHeapInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_DESCRIPTOR_HEAP_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pSamplerHeapBindInfo'' <- case (samplerHeapBindInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr BindHeapInfoEXT))) pSamplerHeapBindInfo''
    pResourceHeapBindInfo'' <- case (resourceHeapBindInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr BindHeapInfoEXT))) pResourceHeapBindInfo''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_DESCRIPTOR_HEAP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct CommandBufferInheritanceDescriptorHeapInfoEXT where
  peekCStruct p = do
    pSamplerHeapBindInfo <- peek @(Ptr BindHeapInfoEXT) ((p `plusPtr` 16 :: Ptr (Ptr BindHeapInfoEXT)))
    pSamplerHeapBindInfo' <- maybePeek (\j -> peekCStruct @BindHeapInfoEXT (j)) pSamplerHeapBindInfo
    pResourceHeapBindInfo <- peek @(Ptr BindHeapInfoEXT) ((p `plusPtr` 24 :: Ptr (Ptr BindHeapInfoEXT)))
    pResourceHeapBindInfo' <- maybePeek (\j -> peekCStruct @BindHeapInfoEXT (j)) pResourceHeapBindInfo
    pure $ CommandBufferInheritanceDescriptorHeapInfoEXT
             pSamplerHeapBindInfo' pResourceHeapBindInfo'

instance Zero CommandBufferInheritanceDescriptorHeapInfoEXT where
  zero = CommandBufferInheritanceDescriptorHeapInfoEXT
           Nothing
           Nothing


-- | VkPhysicalDeviceDescriptorHeapTensorPropertiesARM - Structure describing
-- descriptor heap tensor properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceDescriptorHeapTensorPropertiesARM' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDescriptorHeapTensorPropertiesARM = PhysicalDeviceDescriptorHeapTensorPropertiesARM
  { -- | #limits-tensorDescriptorSize# @tensorDescriptorSize@ specifies the
    -- maximum size of tensor descriptors written by
    -- 'writeResourceDescriptorsEXT'.
    tensorDescriptorSize :: DeviceSize
  , -- | #limits-tensorDescriptorAlignment# @tensorDescriptorAlignment@ specifies
    -- the required alignment of tensor descriptors within a resource heap. It
    -- must be a power-of-two value, and less than or equal to
    -- @tensorDescriptorSize@.
    tensorDescriptorAlignment :: DeviceSize
  , -- | #limits-tensorCaptureReplayOpaqueDataSize#
    -- @tensorCaptureReplayOpaqueDataSize@ specifies the size of the opaque
    -- capture\/replay data for an tensor.
    tensorCaptureReplayOpaqueDataSize :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDescriptorHeapTensorPropertiesARM)
#endif
deriving instance Show PhysicalDeviceDescriptorHeapTensorPropertiesARM

instance ToCStruct PhysicalDeviceDescriptorHeapTensorPropertiesARM where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDescriptorHeapTensorPropertiesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_TENSOR_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (tensorDescriptorSize)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (tensorDescriptorAlignment)
    poke ((p `plusPtr` 32 :: Ptr CSize)) (CSize (tensorCaptureReplayOpaqueDataSize))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_HEAP_TENSOR_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct PhysicalDeviceDescriptorHeapTensorPropertiesARM where
  peekCStruct p = do
    tensorDescriptorSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    tensorDescriptorAlignment <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    tensorCaptureReplayOpaqueDataSize <- peek @CSize ((p `plusPtr` 32 :: Ptr CSize))
    pure $ PhysicalDeviceDescriptorHeapTensorPropertiesARM
             tensorDescriptorSize
             tensorDescriptorAlignment
             (coerce @CSize @Word64 tensorCaptureReplayOpaqueDataSize)

instance Storable PhysicalDeviceDescriptorHeapTensorPropertiesARM where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDescriptorHeapTensorPropertiesARM where
  zero = PhysicalDeviceDescriptorHeapTensorPropertiesARM
           zero
           zero
           zero


data ResourceDescriptorDataEXT
  = AnImage (Maybe ImageDescriptorInfoEXT)
  | ATexelBuffer (Maybe TexelBufferDescriptorInfoEXT)
  | AnAddressRange (Maybe DeviceAddressRangeEXT)
  | ATensorARM (Maybe (SomeStruct TensorViewCreateInfoARM))
  deriving (Show)

instance ToCStruct ResourceDescriptorDataEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr ResourceDescriptorDataEXT -> ResourceDescriptorDataEXT -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    AnImage v -> do
      pImage <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT $ withCStruct (j)
      lift $ poke (castPtr @_ @(Ptr ImageDescriptorInfoEXT) p) pImage
    ATexelBuffer v -> do
      pTexelBuffer <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT $ withCStruct (j)
      lift $ poke (castPtr @_ @(Ptr TexelBufferDescriptorInfoEXT) p) pTexelBuffer
    AnAddressRange v -> do
      pAddressRange <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT $ withCStruct (j)
      lift $ poke (castPtr @_ @(Ptr DeviceAddressRangeEXT) p) pAddressRange
    ATensorARM v -> do
      pTensorARM <- case (v) of
        Nothing -> pure nullPtr
        Just j -> ContT @_ @_ @(Ptr (TensorViewCreateInfoARM '[])) $ \cont -> withSomeCStruct @TensorViewCreateInfoARM (j) (cont . castPtr)
      lift $ poke (castPtr @_ @(Ptr (TensorViewCreateInfoARM _)) p) pTensorARM
  pokeZeroCStruct :: Ptr ResourceDescriptorDataEXT -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero ResourceDescriptorDataEXT where
  zero = AnImage Nothing


data DescriptorMappingSourceDataEXT
  = ConstantOffset DescriptorMappingSourceConstantOffsetEXT
  | PushIndex DescriptorMappingSourcePushIndexEXT
  | IndirectIndex DescriptorMappingSourceIndirectIndexEXT
  | IndirectIndexArray DescriptorMappingSourceIndirectIndexArrayEXT
  | HeapData DescriptorMappingSourceHeapDataEXT
  | PushDataOffset Word32
  | PushAddressOffset Word32
  | IndirectAddress DescriptorMappingSourceIndirectAddressEXT
  | ShaderRecordIndex DescriptorMappingSourceShaderRecordIndexEXT
  | ShaderRecordDataOffset Word32
  | ShaderRecordAddressOffset Word32
  deriving (Show)

instance ToCStruct DescriptorMappingSourceDataEXT where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr DescriptorMappingSourceDataEXT -> DescriptorMappingSourceDataEXT -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    ConstantOffset v -> ContT $ pokeCStruct (castPtr @_ @DescriptorMappingSourceConstantOffsetEXT p) (v) . ($ ())
    PushIndex v -> ContT $ pokeCStruct (castPtr @_ @DescriptorMappingSourcePushIndexEXT p) (v) . ($ ())
    IndirectIndex v -> ContT $ pokeCStruct (castPtr @_ @DescriptorMappingSourceIndirectIndexEXT p) (v) . ($ ())
    IndirectIndexArray v -> ContT $ pokeCStruct (castPtr @_ @DescriptorMappingSourceIndirectIndexArrayEXT p) (v) . ($ ())
    HeapData v -> lift $ poke (castPtr @_ @DescriptorMappingSourceHeapDataEXT p) (v)
    PushDataOffset v -> lift $ poke (castPtr @_ @Word32 p) (v)
    PushAddressOffset v -> lift $ poke (castPtr @_ @Word32 p) (v)
    IndirectAddress v -> lift $ poke (castPtr @_ @DescriptorMappingSourceIndirectAddressEXT p) (v)
    ShaderRecordIndex v -> ContT $ pokeCStruct (castPtr @_ @DescriptorMappingSourceShaderRecordIndexEXT p) (v) . ($ ())
    ShaderRecordDataOffset v -> lift $ poke (castPtr @_ @Word32 p) (v)
    ShaderRecordAddressOffset v -> lift $ poke (castPtr @_ @Word32 p) (v)
  pokeZeroCStruct :: Ptr DescriptorMappingSourceDataEXT -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 56
  cStructAlignment = 8

instance Zero DescriptorMappingSourceDataEXT where
  zero = IndirectIndex zero


-- | VkDescriptorMappingSourceEXT - Specifies the mapping source for a shader
-- binding
--
-- = Description
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT' specifies
--     that the resource will be backed by a descriptor from the heap at a
--     constant index.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT' specifies that
--     the resource will be backed by a descriptor from the heap at an
--     index sourced from push data, added to a constant index.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT' specifies
--     that the resource will be backed by a descriptor from the heap at an
--     index sourced from an address in push data, added to a constant
--     index. If the mapping is an array, the array will be mapped to a
--     base offset in indirect memory, and subsequent elements are mapped
--     as offsets to that base.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT'
--     specifies that the resource will be backed by a descriptor from the
--     heap at an index sourced from an address in push data, added to a
--     constant index. If the mapping is an array, each array element will
--     be mapped to a separate index in indirect memory.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT' specifies that
--     the resource will be backed by heap data directly.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT' specifies that the
--     resource will be backed by push data directly.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT' specifies that the
--     resource will be backed by an address in push data.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT' specifies that the
--     resource will be backed by an address sourced via another address in
--     push data.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT'
--     specifies that the resource will be backed by a descriptor from the
--     heap at an index sourced from shader record data, added to a
--     constant index.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT' specifies that
--     the resource will be backed by shader record data directly.
--
-- -   'DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT' specifies that
--     the resource will be backed by an address in shader record data.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'DescriptorSetAndBindingMappingEXT'
newtype DescriptorMappingSourceEXT = DescriptorMappingSourceEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT = DescriptorMappingSourceEXT 0

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT = DescriptorMappingSourceEXT 1

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT = DescriptorMappingSourceEXT 2

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT = DescriptorMappingSourceEXT 3

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT = DescriptorMappingSourceEXT 4

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT = DescriptorMappingSourceEXT 5

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT = DescriptorMappingSourceEXT 6

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT = DescriptorMappingSourceEXT 7

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT = DescriptorMappingSourceEXT 10

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT = DescriptorMappingSourceEXT 9

-- No documentation found for Nested "VkDescriptorMappingSourceEXT" "VK_DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT"
pattern DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT = DescriptorMappingSourceEXT 8

{-# COMPLETE
  DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT
  , DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT
  , DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT
  , DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT
  , DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT
  , DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT
  , DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT
  , DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT
  , DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT
  , DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT
  , DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT ::
    DescriptorMappingSourceEXT
  #-}

conNameDescriptorMappingSourceEXT :: String
conNameDescriptorMappingSourceEXT = "DescriptorMappingSourceEXT"

enumPrefixDescriptorMappingSourceEXT :: String
enumPrefixDescriptorMappingSourceEXT = "DESCRIPTOR_MAPPING_SOURCE_"

showTableDescriptorMappingSourceEXT :: [(DescriptorMappingSourceEXT, String)]
showTableDescriptorMappingSourceEXT =
  [
    ( DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT
    , "HEAP_WITH_CONSTANT_OFFSET_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT
    , "HEAP_WITH_PUSH_INDEX_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT
    , "HEAP_WITH_INDIRECT_INDEX_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT
    , "HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT
    , "RESOURCE_HEAP_DATA_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT
    , "PUSH_DATA_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT
    , "PUSH_ADDRESS_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT
    , "INDIRECT_ADDRESS_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT
    , "SHADER_RECORD_ADDRESS_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT
    , "SHADER_RECORD_DATA_EXT"
    )
  ,
    ( DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT
    , "HEAP_WITH_SHADER_RECORD_INDEX_EXT"
    )
  ]

instance Show DescriptorMappingSourceEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixDescriptorMappingSourceEXT
      showTableDescriptorMappingSourceEXT
      conNameDescriptorMappingSourceEXT
      (\(DescriptorMappingSourceEXT x) -> x)
      (showsPrec 11)

instance Read DescriptorMappingSourceEXT where
  readPrec =
    enumReadPrec
      enumPrefixDescriptorMappingSourceEXT
      showTableDescriptorMappingSourceEXT
      conNameDescriptorMappingSourceEXT
      DescriptorMappingSourceEXT

type SpirvResourceTypeFlagsEXT = SpirvResourceTypeFlagBitsEXT

-- | VkSpirvResourceTypeFlagBitsEXT - Bitmask specifying different SPIR-V
-- resource declarations
--
-- = Description
--
-- -   'SPIRV_RESOURCE_TYPE_ALL_EXT' specifies that all resource
--     declarations are included.
--
-- -   'SPIRV_RESOURCE_TYPE_SAMPLER_BIT_EXT' specifies @OpTypeSampler@
--     variables.
--
-- -   'SPIRV_RESOURCE_TYPE_SAMPLED_IMAGE_BIT_EXT' specifies @OpTypeImage@
--     variables with a @Sampled@ parameter of 1.
--
-- -   'SPIRV_RESOURCE_TYPE_READ_ONLY_IMAGE_BIT_EXT' specifies
--     @OpTypeImage@ variables with a @Sampled@ parameter of 2 and
--     decorated with @NonWritable@.
--
-- -   'SPIRV_RESOURCE_TYPE_READ_WRITE_IMAGE_BIT_EXT' specifies
--     @OpTypeImage@ variables with a @Sampled@ parameter of 2 and not
--     decorated with @NonWritable@.
--
-- -   'SPIRV_RESOURCE_TYPE_COMBINED_SAMPLED_IMAGE_BIT_EXT' specifies
--     @OpTypeSampledImage@ variables.
--
-- -   'SPIRV_RESOURCE_TYPE_UNIFORM_BUFFER_BIT_EXT' specifies
--     @OpTypeStruct@ variables in the @Uniform@ storage class decorated
--     with @Block@
--
-- -   'SPIRV_RESOURCE_TYPE_READ_ONLY_STORAGE_BUFFER_BIT_EXT' specifies
--     @OpTypeStruct@ variables either in the @StorageBuffer@ storage class
--     decorated with @Block@ or in the @Uniform@ storage class decorated
--     with @BufferBlock@, and decorated with @NonWritable@
--
-- -   'SPIRV_RESOURCE_TYPE_READ_WRITE_STORAGE_BUFFER_BIT_EXT' specifies
--     @OpTypeStruct@ variables either in the @StorageBuffer@ storage class
--     decorated with @Block@ or in the @Uniform@ storage class decorated
--     with @BufferBlock@, but not decorated with @NonWritable@
--
-- -   'SPIRV_RESOURCE_TYPE_ACCELERATION_STRUCTURE_BIT_EXT' specifies
--     @OpTypeAccelerationStructureKHR@ variables
--
-- -   'SPIRV_RESOURCE_TYPE_TENSOR_BIT_ARM' specifies @OpTypeTensorARM@
--     variables
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>,
-- 'SpirvResourceTypeFlagsEXT'
newtype SpirvResourceTypeFlagBitsEXT = SpirvResourceTypeFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_ALL_EXT"
pattern SPIRV_RESOURCE_TYPE_ALL_EXT = SpirvResourceTypeFlagBitsEXT 0x7fffffff

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_SAMPLER_BIT_EXT"
pattern SPIRV_RESOURCE_TYPE_SAMPLER_BIT_EXT = SpirvResourceTypeFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_SAMPLED_IMAGE_BIT_EXT"
pattern SPIRV_RESOURCE_TYPE_SAMPLED_IMAGE_BIT_EXT = SpirvResourceTypeFlagBitsEXT 0x00000002

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_READ_ONLY_IMAGE_BIT_EXT"
pattern SPIRV_RESOURCE_TYPE_READ_ONLY_IMAGE_BIT_EXT = SpirvResourceTypeFlagBitsEXT 0x00000004

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_READ_WRITE_IMAGE_BIT_EXT"
pattern SPIRV_RESOURCE_TYPE_READ_WRITE_IMAGE_BIT_EXT = SpirvResourceTypeFlagBitsEXT 0x00000008

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_COMBINED_SAMPLED_IMAGE_BIT_EXT"
pattern SPIRV_RESOURCE_TYPE_COMBINED_SAMPLED_IMAGE_BIT_EXT = SpirvResourceTypeFlagBitsEXT 0x00000010

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_UNIFORM_BUFFER_BIT_EXT"
pattern SPIRV_RESOURCE_TYPE_UNIFORM_BUFFER_BIT_EXT = SpirvResourceTypeFlagBitsEXT 0x00000020

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_READ_ONLY_STORAGE_BUFFER_BIT_EXT"
pattern SPIRV_RESOURCE_TYPE_READ_ONLY_STORAGE_BUFFER_BIT_EXT = SpirvResourceTypeFlagBitsEXT 0x00000040

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_READ_WRITE_STORAGE_BUFFER_BIT_EXT"
pattern SPIRV_RESOURCE_TYPE_READ_WRITE_STORAGE_BUFFER_BIT_EXT = SpirvResourceTypeFlagBitsEXT 0x00000080

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_TENSOR_BIT_ARM"
pattern SPIRV_RESOURCE_TYPE_TENSOR_BIT_ARM = SpirvResourceTypeFlagBitsEXT 0x00000200

-- No documentation found for Nested "VkSpirvResourceTypeFlagBitsEXT" "VK_SPIRV_RESOURCE_TYPE_ACCELERATION_STRUCTURE_BIT_EXT"
pattern SPIRV_RESOURCE_TYPE_ACCELERATION_STRUCTURE_BIT_EXT = SpirvResourceTypeFlagBitsEXT 0x00000100

conNameSpirvResourceTypeFlagBitsEXT :: String
conNameSpirvResourceTypeFlagBitsEXT = "SpirvResourceTypeFlagBitsEXT"

enumPrefixSpirvResourceTypeFlagBitsEXT :: String
enumPrefixSpirvResourceTypeFlagBitsEXT = "SPIRV_RESOURCE_TYPE_"

showTableSpirvResourceTypeFlagBitsEXT :: [(SpirvResourceTypeFlagBitsEXT, String)]
showTableSpirvResourceTypeFlagBitsEXT =
  [
    ( SPIRV_RESOURCE_TYPE_ALL_EXT
    , "ALL_EXT"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_SAMPLER_BIT_EXT
    , "SAMPLER_BIT_EXT"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_SAMPLED_IMAGE_BIT_EXT
    , "SAMPLED_IMAGE_BIT_EXT"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_READ_ONLY_IMAGE_BIT_EXT
    , "READ_ONLY_IMAGE_BIT_EXT"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_READ_WRITE_IMAGE_BIT_EXT
    , "READ_WRITE_IMAGE_BIT_EXT"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_COMBINED_SAMPLED_IMAGE_BIT_EXT
    , "COMBINED_SAMPLED_IMAGE_BIT_EXT"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_UNIFORM_BUFFER_BIT_EXT
    , "UNIFORM_BUFFER_BIT_EXT"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_READ_ONLY_STORAGE_BUFFER_BIT_EXT
    , "READ_ONLY_STORAGE_BUFFER_BIT_EXT"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_READ_WRITE_STORAGE_BUFFER_BIT_EXT
    , "READ_WRITE_STORAGE_BUFFER_BIT_EXT"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_TENSOR_BIT_ARM
    , "TENSOR_BIT_ARM"
    )
  ,
    ( SPIRV_RESOURCE_TYPE_ACCELERATION_STRUCTURE_BIT_EXT
    , "ACCELERATION_STRUCTURE_BIT_EXT"
    )
  ]

instance Show SpirvResourceTypeFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixSpirvResourceTypeFlagBitsEXT
      showTableSpirvResourceTypeFlagBitsEXT
      conNameSpirvResourceTypeFlagBitsEXT
      (\(SpirvResourceTypeFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SpirvResourceTypeFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixSpirvResourceTypeFlagBitsEXT
      showTableSpirvResourceTypeFlagBitsEXT
      conNameSpirvResourceTypeFlagBitsEXT
      SpirvResourceTypeFlagBitsEXT

type EXT_DESCRIPTOR_HEAP_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_HEAP_SPEC_VERSION"
pattern EXT_DESCRIPTOR_HEAP_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DESCRIPTOR_HEAP_SPEC_VERSION = 1


type EXT_DESCRIPTOR_HEAP_EXTENSION_NAME = "VK_EXT_descriptor_heap"

-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_HEAP_EXTENSION_NAME"
pattern EXT_DESCRIPTOR_HEAP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DESCRIPTOR_HEAP_EXTENSION_NAME = "VK_EXT_descriptor_heap"

