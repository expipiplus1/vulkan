{-# language CPP #-}
-- | = Name
--
-- VK_KHR_synchronization2 - device extension
--
-- == VK_KHR_synchronization2
--
-- [__Name String__]
--     @VK_KHR_synchronization2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     315
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_synchronization2] @tobski%0A<<Here describe the issue or question you have about the VK_KHR_synchronization2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-12-03
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_create_renderpass2@
--
-- [__Contributors__]
--
--     -   Tobias Hector
--
-- == Description
--
-- This extension modifies the original core synchronization APIs to
-- simplify the interface and improve usability of these APIs. It also adds
-- new pipeline stage and access flag types that extend into the 64-bit
-- range, as we have run out within the 32-bit range. The new flags are
-- identical to the old values within the 32-bit range, with new stages and
-- bits beyond that.
--
-- Pipeline stages and access flags are now specified together in memory
-- barrier structures, making the connection between the two more obvious.
-- Additionally, scoping the pipeline stages into the barrier structs
-- allows the use of the @MEMORY_READ@ and @MEMORY_WRITE@ flags without
-- sacrificing precision. The per-stage access flags should be used to
-- disambiguate specific accesses in a given stage or set of stages - for
-- instance, between uniform reads and sampling operations.
--
-- Layout transitions have been simplified as well; rather than requiring a
-- different set of layouts for depth\/stencil\/color attachments, there
-- are generic
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
-- and 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
-- layouts which are contextually applied based on the image format. For
-- example, for a depth format image,
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR' is
-- equivalent to
-- 'Vulkan.Extensions.VK_KHR_separate_depth_stencil_layouts.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR'.
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
-- also functionally replaces
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'.
--
-- Events are now more efficient, because they include memory dependency
-- information when you set them on the device. Previously, this
-- information was only known when waiting on an event, so the dependencies
-- could not be satisfied until the wait occurred. That sometimes meant
-- stalling the pipeline when the wait occurred. The new API provides
-- enough information for implementations to satisfy these dependencies in
-- parallel with other tasks.
--
-- Queue submission has been changed to wrap command buffers and semaphores
-- in extensible structures, which incorporate changes from Vulkan 1.1,
-- @VK_KHR_device_group@, and @VK_KHR_timeline_semaphore@. This also adds a
-- pipeline stage to the semaphore signal operation, mirroring the existing
-- pipeline stage specification for wait operations.
--
-- Other miscellaneous changes include:
--
-- -   Events can now be specified as interacting only with the device,
--     allowing more efficient access to the underlying object.
--
-- -   Image memory barriers that do not perform an image layout transition
--     can be specified by setting @oldLayout@ equal to @newLayout@.
--
--     -   E.g. the old and new layout can both be set to
--         'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
--         without discarding data in the image.
--
-- -   Queue family ownership transfer parameters are simplified in some
--     cases.
--
-- -   Where two synchronization commands need to be matched up (queue
--     transfer operations, events), the dependency information specified
--     in each place must now match completely for consistency.
--
-- -   Extensions with commands or functions with a
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags' or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     parameter have had those APIs replaced with equivalents using
--     'PipelineStageFlags2KHR'.
--
-- -   The new event and barrier interfaces are now more extensible for
--     future changes.
--
-- -   Relevant pipeline stage masks can now be specified as empty with the
--     new
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_NONE_KHR'
--     and 'PIPELINE_STAGE_2_NONE_KHR' values.
--
-- -   'MemoryBarrier2KHR' can be chained to
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDependency2',
--     overriding the original 32-bit stage and access masks.
--
-- == New Commands
--
-- -   'cmdPipelineBarrier2KHR'
--
-- -   'cmdResetEvent2KHR'
--
-- -   'cmdSetEvent2KHR'
--
-- -   'cmdWaitEvents2KHR'
--
-- -   'cmdWriteTimestamp2KHR'
--
-- -   'queueSubmit2KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_buffer_marker VK_AMD_buffer_marker>
-- is supported:
--
-- -   'cmdWriteBufferMarker2AMD'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostic_checkpoints VK_NV_device_diagnostic_checkpoints>
-- is supported:
--
-- -   'getQueueCheckpointData2NV'
--
-- == New Structures
--
-- -   'BufferMemoryBarrier2KHR'
--
-- -   'CommandBufferSubmitInfoKHR'
--
-- -   'DependencyInfoKHR'
--
-- -   'ImageMemoryBarrier2KHR'
--
-- -   'SemaphoreSubmitInfoKHR'
--
-- -   'SubmitInfo2KHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSynchronization2FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDependency2':
--
--     -   'MemoryBarrier2KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostic_checkpoints VK_NV_device_diagnostic_checkpoints>
-- is supported:
--
-- -   'CheckpointData2NV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyCheckpointProperties2NV'
--
-- == New Enums
--
-- -   'AccessFlagBits2KHR'
--
-- -   'PipelineStageFlagBits2KHR'
--
-- -   'SubmitFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'AccessFlags2KHR'
--
-- -   'PipelineStageFlags2KHR'
--
-- -   'SubmitFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SYNCHRONIZATION_2_EXTENSION_NAME'
--
-- -   'KHR_SYNCHRONIZATION_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_NONE_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.EventCreateFlagBits.EventCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.EventCreateFlagBits.EVENT_CREATE_DEVICE_ONLY_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_NONE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEPENDENCY_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBMIT_INFO_2_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_blend_operation_advanced VK_EXT_blend_operation_advanced>
-- is supported:
--
-- -   Extending 'AccessFlagBits2KHR':
--
--     -   'ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>
-- is supported:
--
-- -   Extending 'AccessFlagBits2KHR':
--
--     -   'ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT'
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending 'AccessFlagBits2KHR':
--
--     -   'ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT'
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>
-- is supported:
--
-- -   Extending 'AccessFlagBits2KHR':
--
--     -   'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT'
--
--     -   'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT'
--
--     -   'ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT'
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- is supported:
--
-- -   Extending 'AccessFlagBits2KHR':
--
--     -   'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR'
--
--     -   'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>
-- is supported:
--
-- -   Extending 'AccessFlagBits2KHR':
--
--     -   'ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR'
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
-- is supported:
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostic_checkpoints VK_NV_device_diagnostic_checkpoints>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
-- is supported:
--
-- -   Extending 'AccessFlagBits2KHR':
--
--     -   'ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV'
--
--     -   'ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV'
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_mesh_shader VK_NV_mesh_shader>
-- is supported:
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
--     -   'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   Extending 'AccessFlagBits2KHR':
--
--     -   'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV'
--
--     -   'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV'
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV'
--
--     -   'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shading_rate_image VK_NV_shading_rate_image>
-- is supported:
--
-- -   Extending 'AccessFlagBits2KHR':
--
--     -   'ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV'
--
-- -   Extending 'PipelineStageFlagBits2KHR':
--
--     -   'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- == Examples
--
-- See
-- <https://github.com/KhronosGroup/Vulkan-Docs/wiki/Synchronization-Examples>
--
-- == Version History
--
-- -   Revision 1, 2020-12-03 (Tobias Hector)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'AccessFlagBits2KHR', 'AccessFlags2KHR', 'BufferMemoryBarrier2KHR',
-- 'CommandBufferSubmitInfoKHR', 'DependencyInfoKHR',
-- 'ImageMemoryBarrier2KHR', 'MemoryBarrier2KHR',
-- 'PhysicalDeviceSynchronization2FeaturesKHR',
-- 'PipelineStageFlagBits2KHR', 'PipelineStageFlags2KHR',
-- 'SemaphoreSubmitInfoKHR', 'SubmitFlagBitsKHR', 'SubmitFlagsKHR',
-- 'SubmitInfo2KHR', 'cmdPipelineBarrier2KHR', 'cmdResetEvent2KHR',
-- 'cmdSetEvent2KHR', 'cmdWaitEvents2KHR', 'cmdWriteTimestamp2KHR',
-- 'queueSubmit2KHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_synchronization2  ( BufferMemoryBarrier2KHR
                                                  , CheckpointData2NV
                                                  , CommandBufferSubmitInfoKHR
                                                  , DependencyInfoKHR
                                                  , ImageMemoryBarrier2KHR
                                                  , MemoryBarrier2KHR
                                                  , PhysicalDeviceSynchronization2FeaturesKHR
                                                  , QueueFamilyCheckpointProperties2NV
                                                  , SemaphoreSubmitInfoKHR
                                                  , SubmitInfo2KHR
                                                  , PipelineStageFlags2KHR
                                                  , PipelineStageFlagBits2KHR
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BufferMemoryBarrier2KHR

instance ToCStruct BufferMemoryBarrier2KHR
instance Show BufferMemoryBarrier2KHR

instance FromCStruct BufferMemoryBarrier2KHR


data CheckpointData2NV

instance ToCStruct CheckpointData2NV
instance Show CheckpointData2NV

instance FromCStruct CheckpointData2NV


data CommandBufferSubmitInfoKHR

instance ToCStruct CommandBufferSubmitInfoKHR
instance Show CommandBufferSubmitInfoKHR

instance FromCStruct CommandBufferSubmitInfoKHR


data DependencyInfoKHR

instance ToCStruct DependencyInfoKHR
instance Show DependencyInfoKHR

instance FromCStruct DependencyInfoKHR


type role ImageMemoryBarrier2KHR nominal
data ImageMemoryBarrier2KHR (es :: [Type])

instance (Extendss ImageMemoryBarrier2KHR es, PokeChain es) => ToCStruct (ImageMemoryBarrier2KHR es)
instance Show (Chain es) => Show (ImageMemoryBarrier2KHR es)

instance (Extendss ImageMemoryBarrier2KHR es, PeekChain es) => FromCStruct (ImageMemoryBarrier2KHR es)


data MemoryBarrier2KHR

instance ToCStruct MemoryBarrier2KHR
instance Show MemoryBarrier2KHR

instance FromCStruct MemoryBarrier2KHR


data PhysicalDeviceSynchronization2FeaturesKHR

instance ToCStruct PhysicalDeviceSynchronization2FeaturesKHR
instance Show PhysicalDeviceSynchronization2FeaturesKHR

instance FromCStruct PhysicalDeviceSynchronization2FeaturesKHR


data QueueFamilyCheckpointProperties2NV

instance ToCStruct QueueFamilyCheckpointProperties2NV
instance Show QueueFamilyCheckpointProperties2NV

instance FromCStruct QueueFamilyCheckpointProperties2NV


data SemaphoreSubmitInfoKHR

instance ToCStruct SemaphoreSubmitInfoKHR
instance Show SemaphoreSubmitInfoKHR

instance FromCStruct SemaphoreSubmitInfoKHR


type role SubmitInfo2KHR nominal
data SubmitInfo2KHR (es :: [Type])

instance (Extendss SubmitInfo2KHR es, PokeChain es) => ToCStruct (SubmitInfo2KHR es)
instance Show (Chain es) => Show (SubmitInfo2KHR es)

instance (Extendss SubmitInfo2KHR es, PeekChain es) => FromCStruct (SubmitInfo2KHR es)


type PipelineStageFlags2KHR = PipelineStageFlagBits2KHR

data PipelineStageFlagBits2KHR

