{-# language CPP #-}
-- | = Name
--
-- VK_EXT_descriptor_buffer - device extension
--
-- == VK_EXT_descriptor_buffer
--
-- [__Name String__]
--     @VK_EXT_descriptor_buffer@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     317
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_indexing VK_EXT_descriptor_indexing>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_KHR_acceleration_structure
--
--     -   Interacts with VK_NV_ray_tracing
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_descriptor_buffer] @tobski%0A*Here describe the issue or question you have about the VK_EXT_descriptor_buffer extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_descriptor_buffer.adoc VK_EXT_descriptor_buffer>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-07
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Stu Smith, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Boris Zanin, AMD
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Connor Abbott, Valve
--
--     -   Baldur Karlsson, Valve
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Rodrigo Locatti, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jeff Leger, QUALCOMM
--
--     -   Lionel Landwerlin, Intel
--
--     -   Slawomir Grajewski, Intel
--
-- == Description
--
-- This extension introduces new commands to put shader-accessible
-- descriptors directly in memory, making the management of descriptor data
-- more explicit.
--
-- == New Commands
--
-- -   'cmdBindDescriptorBufferEmbeddedSamplersEXT'
--
-- -   'cmdBindDescriptorBuffersEXT'
--
-- -   'cmdSetDescriptorBufferOffsetsEXT'
--
-- -   'getBufferOpaqueCaptureDescriptorDataEXT'
--
-- -   'getDescriptorEXT'
--
-- -   'getDescriptorSetLayoutBindingOffsetEXT'
--
-- -   'getDescriptorSetLayoutSizeEXT'
--
-- -   'getImageOpaqueCaptureDescriptorDataEXT'
--
-- -   'getImageViewOpaqueCaptureDescriptorDataEXT'
--
-- -   'getSamplerOpaqueCaptureDescriptorDataEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   'getAccelerationStructureOpaqueCaptureDescriptorDataEXT'
--
-- == New Structures
--
-- -   'BufferCaptureDescriptorDataInfoEXT'
--
-- -   'DescriptorAddressInfoEXT'
--
-- -   'DescriptorBufferBindingInfoEXT'
--
-- -   'DescriptorGetInfoEXT'
--
-- -   'ImageCaptureDescriptorDataInfoEXT'
--
-- -   'ImageViewCaptureDescriptorDataInfoEXT'
--
-- -   'SamplerCaptureDescriptorDataInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Buffer.BufferCreateInfo',
--     'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo',
--     'Vulkan.Core10.Sampler.SamplerCreateInfo',
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR',
--     'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV':
--
--     -   'OpaqueCaptureDescriptorDataCreateInfoEXT'
--
-- -   Extending 'DescriptorBufferBindingInfoEXT':
--
--     -   'DescriptorBufferBindingPushDescriptorBufferHandleEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorBufferFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT'
--
--     -   'PhysicalDeviceDescriptorBufferPropertiesEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   'AccelerationStructureCaptureDescriptorDataInfoEXT'
--
-- == New Unions
--
-- -   'DescriptorDataEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DESCRIPTOR_BUFFER_EXTENSION_NAME'
--
-- -   'EXT_DESCRIPTOR_BUFFER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_EMBEDDED_IMMUTABLE_SAMPLERS_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_ADDRESS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_PUSH_DESCRIPTOR_BUFFER_HANDLE_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_GET_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_DENSITY_MAP_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CAPTURE_DESCRIPTOR_DATA_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-06-07 (Stu Smith)
--
--     -   Initial revision
--
-- == See Also
--
-- 'BufferCaptureDescriptorDataInfoEXT', 'DescriptorAddressInfoEXT',
-- 'DescriptorBufferBindingInfoEXT',
-- 'DescriptorBufferBindingPushDescriptorBufferHandleEXT',
-- 'DescriptorDataEXT', 'DescriptorGetInfoEXT',
-- 'ImageCaptureDescriptorDataInfoEXT',
-- 'ImageViewCaptureDescriptorDataInfoEXT',
-- 'OpaqueCaptureDescriptorDataCreateInfoEXT',
-- 'PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT',
-- 'PhysicalDeviceDescriptorBufferFeaturesEXT',
-- 'PhysicalDeviceDescriptorBufferPropertiesEXT',
-- 'SamplerCaptureDescriptorDataInfoEXT',
-- 'cmdBindDescriptorBufferEmbeddedSamplersEXT',
-- 'cmdBindDescriptorBuffersEXT', 'cmdSetDescriptorBufferOffsetsEXT',
-- 'getBufferOpaqueCaptureDescriptorDataEXT', 'getDescriptorEXT',
-- 'getDescriptorSetLayoutBindingOffsetEXT',
-- 'getDescriptorSetLayoutSizeEXT',
-- 'getImageOpaqueCaptureDescriptorDataEXT',
-- 'getImageViewOpaqueCaptureDescriptorDataEXT',
-- 'getSamplerOpaqueCaptureDescriptorDataEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_descriptor_buffer Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_descriptor_buffer  ( AccelerationStructureCaptureDescriptorDataInfoEXT
                                                   , BufferCaptureDescriptorDataInfoEXT
                                                   , DescriptorAddressInfoEXT
                                                   , DescriptorBufferBindingInfoEXT
                                                   , DescriptorBufferBindingPushDescriptorBufferHandleEXT
                                                   , DescriptorGetInfoEXT
                                                   , ImageCaptureDescriptorDataInfoEXT
                                                   , ImageViewCaptureDescriptorDataInfoEXT
                                                   , OpaqueCaptureDescriptorDataCreateInfoEXT
                                                   , PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT
                                                   , PhysicalDeviceDescriptorBufferFeaturesEXT
                                                   , PhysicalDeviceDescriptorBufferPropertiesEXT
                                                   , SamplerCaptureDescriptorDataInfoEXT
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data AccelerationStructureCaptureDescriptorDataInfoEXT

instance ToCStruct AccelerationStructureCaptureDescriptorDataInfoEXT
instance Show AccelerationStructureCaptureDescriptorDataInfoEXT

instance FromCStruct AccelerationStructureCaptureDescriptorDataInfoEXT


data BufferCaptureDescriptorDataInfoEXT

instance ToCStruct BufferCaptureDescriptorDataInfoEXT
instance Show BufferCaptureDescriptorDataInfoEXT

instance FromCStruct BufferCaptureDescriptorDataInfoEXT


data DescriptorAddressInfoEXT

instance ToCStruct DescriptorAddressInfoEXT
instance Show DescriptorAddressInfoEXT

instance FromCStruct DescriptorAddressInfoEXT


type role DescriptorBufferBindingInfoEXT nominal
data DescriptorBufferBindingInfoEXT (es :: [Type])

instance ( Extendss DescriptorBufferBindingInfoEXT es
         , PokeChain es ) => ToCStruct (DescriptorBufferBindingInfoEXT es)
instance Show (Chain es) => Show (DescriptorBufferBindingInfoEXT es)

instance ( Extendss DescriptorBufferBindingInfoEXT es
         , PeekChain es ) => FromCStruct (DescriptorBufferBindingInfoEXT es)


data DescriptorBufferBindingPushDescriptorBufferHandleEXT

instance ToCStruct DescriptorBufferBindingPushDescriptorBufferHandleEXT
instance Show DescriptorBufferBindingPushDescriptorBufferHandleEXT

instance FromCStruct DescriptorBufferBindingPushDescriptorBufferHandleEXT


data DescriptorGetInfoEXT

instance ToCStruct DescriptorGetInfoEXT
instance Show DescriptorGetInfoEXT

instance FromCStruct DescriptorGetInfoEXT


data ImageCaptureDescriptorDataInfoEXT

instance ToCStruct ImageCaptureDescriptorDataInfoEXT
instance Show ImageCaptureDescriptorDataInfoEXT

instance FromCStruct ImageCaptureDescriptorDataInfoEXT


data ImageViewCaptureDescriptorDataInfoEXT

instance ToCStruct ImageViewCaptureDescriptorDataInfoEXT
instance Show ImageViewCaptureDescriptorDataInfoEXT

instance FromCStruct ImageViewCaptureDescriptorDataInfoEXT


data OpaqueCaptureDescriptorDataCreateInfoEXT

instance ToCStruct OpaqueCaptureDescriptorDataCreateInfoEXT
instance Show OpaqueCaptureDescriptorDataCreateInfoEXT

instance FromCStruct OpaqueCaptureDescriptorDataCreateInfoEXT


data PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT

instance ToCStruct PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT
instance Show PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT

instance FromCStruct PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT


data PhysicalDeviceDescriptorBufferFeaturesEXT

instance ToCStruct PhysicalDeviceDescriptorBufferFeaturesEXT
instance Show PhysicalDeviceDescriptorBufferFeaturesEXT

instance FromCStruct PhysicalDeviceDescriptorBufferFeaturesEXT


data PhysicalDeviceDescriptorBufferPropertiesEXT

instance ToCStruct PhysicalDeviceDescriptorBufferPropertiesEXT
instance Show PhysicalDeviceDescriptorBufferPropertiesEXT

instance FromCStruct PhysicalDeviceDescriptorBufferPropertiesEXT


data SamplerCaptureDescriptorDataInfoEXT

instance ToCStruct SamplerCaptureDescriptorDataInfoEXT
instance Show SamplerCaptureDescriptorDataInfoEXT

instance FromCStruct SamplerCaptureDescriptorDataInfoEXT

