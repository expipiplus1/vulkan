{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance11 - device extension
--
-- = VK_KHR_maintenance11
--
-- [__Name String__]
--     @VK_KHR_maintenance11@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     658
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_mesh_shader
--
--     -   Interacts with VK_EXT_shader_object
--
--     -   Interacts with VK_NV_mesh_shader
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance11] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance11 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance11.adoc VK_KHR_maintenance11>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-09-09
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Caterina Shablia, Collabora
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance11 VK_KHR_maintenance11>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   Add D3D compatibility for mismatch between @Arrayed@ in shaders and
--     the arrayness of the underlying descriptor when the descriptor
--     contains a single array layer
--
-- -   Clarify the pipeline depth clipping state when the pipeline is
--     created without
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
--     being set and the
--     'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'
--     struct is not present
--
-- -   Add
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INDEPENDENT_SETS_BIT_KHR'
--     to enable shader object functionality to mimic
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--     used for graphics pipeline libraries, including a new pipeline
--     layout creation flag
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_NO_TASK_SHADER_BIT_KHR'
--     to ensure pipeline layouts used with shader objects also created
--     with
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_NO_TASK_SHADER_BIT_EXT'
--     to be compatible
--
-- -   Allow @queueFamilyIndexCount@ of 1 in
--     'Vulkan.Core10.Buffer.BufferCreateInfo',
--     'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT',
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM' when
--     @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT'.
--
-- -   Require @minImageTransferGranularity@ to be (1,1,1) even on
--     transfer-only queues and add @optimalImageTransferGranularity@ queue
--     family property to communicate the performance bump for copies not
--     aligned to the optimal granularity.
--
-- -   When copying between a buffer and an image on a transfer-only queue,
--     do not require @bufferOffset@ to be a multiple of 4.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance11FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyOptimalImageTransferGranularityPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_11_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_11_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_ALIAS_SINGLE_LAYER_DESCRIPTOR_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_11_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_OPTIMAL_IMAGE_TRANSFER_GRANULARITY_PROPERTIES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagBitsEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INDEPENDENT_SETS_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_mesh_shader VK_EXT_mesh_shader>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_mesh_shader VK_NV_mesh_shader>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PipelineLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_NO_TASK_SHADER_BIT_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-09-09 (Mike Blumenkrantz)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance11 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance11  ( PhysicalDeviceMaintenance11FeaturesKHR
                                               , QueueFamilyOptimalImageTransferGranularityPropertiesKHR
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceMaintenance11FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance11FeaturesKHR
instance Show PhysicalDeviceMaintenance11FeaturesKHR

instance FromCStruct PhysicalDeviceMaintenance11FeaturesKHR


data QueueFamilyOptimalImageTransferGranularityPropertiesKHR

instance ToCStruct QueueFamilyOptimalImageTransferGranularityPropertiesKHR
instance Show QueueFamilyOptimalImageTransferGranularityPropertiesKHR

instance FromCStruct QueueFamilyOptimalImageTransferGranularityPropertiesKHR

