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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_synchronization2:%20&body=@tobski%20 >
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
-- <VK_KHR_device_group.html VK_KHR_device_group>, and
-- <VK_KHR_timeline_semaphore.html VK_KHR_timeline_semaphore>. This also
-- adds a pipeline stage to the semaphore signal operation, mirroring the
-- existing pipeline stage specification for wait operations.
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
module Vulkan.Extensions.VK_KHR_synchronization2  ( cmdSetEvent2KHR
                                                  , cmdResetEvent2KHR
                                                  , cmdWaitEvents2KHR
                                                  , cmdWaitEvents2KHRSafe
                                                  , cmdPipelineBarrier2KHR
                                                  , queueSubmit2KHR
                                                  , cmdWriteTimestamp2KHR
                                                  , cmdWriteBufferMarker2AMD
                                                  , getQueueCheckpointData2NV
                                                  , pattern PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV
                                                  , pattern ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV
                                                  , pattern PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV
                                                  , pattern PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV
                                                  , pattern ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV
                                                  , pattern ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV
                                                  , pattern PIPELINE_STAGE_2_TRANSFER_BIT_KHR
                                                  , MemoryBarrier2KHR(..)
                                                  , ImageMemoryBarrier2KHR(..)
                                                  , BufferMemoryBarrier2KHR(..)
                                                  , DependencyInfoKHR(..)
                                                  , SemaphoreSubmitInfoKHR(..)
                                                  , CommandBufferSubmitInfoKHR(..)
                                                  , SubmitInfo2KHR(..)
                                                  , QueueFamilyCheckpointProperties2NV(..)
                                                  , CheckpointData2NV(..)
                                                  , PhysicalDeviceSynchronization2FeaturesKHR(..)
                                                  , AccessFlags2KHR
                                                  , AccessFlagBits2KHR( ACCESS_2_NONE_KHR
                                                                      , ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR
                                                                      , ACCESS_2_INDEX_READ_BIT_KHR
                                                                      , ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR
                                                                      , ACCESS_2_UNIFORM_READ_BIT_KHR
                                                                      , ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR
                                                                      , ACCESS_2_SHADER_READ_BIT_KHR
                                                                      , ACCESS_2_SHADER_WRITE_BIT_KHR
                                                                      , ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR
                                                                      , ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR
                                                                      , ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR
                                                                      , ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR
                                                                      , ACCESS_2_TRANSFER_READ_BIT_KHR
                                                                      , ACCESS_2_TRANSFER_WRITE_BIT_KHR
                                                                      , ACCESS_2_HOST_READ_BIT_KHR
                                                                      , ACCESS_2_HOST_WRITE_BIT_KHR
                                                                      , ACCESS_2_MEMORY_READ_BIT_KHR
                                                                      , ACCESS_2_MEMORY_WRITE_BIT_KHR
                                                                      , ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR
                                                                      , ACCESS_2_SHADER_STORAGE_READ_BIT_KHR
                                                                      , ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR
                                                                      , ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI
                                                                      , ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
                                                                      , ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
                                                                      , ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR
                                                                      , ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR
                                                                      , ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR
                                                                      , ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV
                                                                      , ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV
                                                                      , ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT
                                                                      , ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
                                                                      , ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
                                                                      , ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
                                                                      , ..
                                                                      )
                                                  , PipelineStageFlags2KHR
                                                  , PipelineStageFlagBits2KHR( PIPELINE_STAGE_2_NONE_KHR
                                                                             , PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR
                                                                             , PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR
                                                                             , PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR
                                                                             , PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR
                                                                             , PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR
                                                                             , PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR
                                                                             , PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR
                                                                             , PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR
                                                                             , PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR
                                                                             , PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR
                                                                             , PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR
                                                                             , PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR
                                                                             , PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR
                                                                             , PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR
                                                                             , PIPELINE_STAGE_2_HOST_BIT_KHR
                                                                             , PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR
                                                                             , PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR
                                                                             , PIPELINE_STAGE_2_COPY_BIT_KHR
                                                                             , PIPELINE_STAGE_2_RESOLVE_BIT_KHR
                                                                             , PIPELINE_STAGE_2_BLIT_BIT_KHR
                                                                             , PIPELINE_STAGE_2_CLEAR_BIT_KHR
                                                                             , PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR
                                                                             , PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR
                                                                             , PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR
                                                                             , PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI
                                                                             , PIPELINE_STAGE_2_MESH_SHADER_BIT_NV
                                                                             , PIPELINE_STAGE_2_TASK_SHADER_BIT_NV
                                                                             , PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT
                                                                             , PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR
                                                                             , PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR
                                                                             , PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                             , PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV
                                                                             , PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT
                                                                             , PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT
                                                                             , ..
                                                                             )
                                                  , SubmitFlagsKHR
                                                  , SubmitFlagBitsKHR( SUBMIT_PROTECTED_BIT_KHR
                                                                     , ..
                                                                     )
                                                  , KHR_SYNCHRONIZATION_2_SPEC_VERSION
                                                  , pattern KHR_SYNCHRONIZATION_2_SPEC_VERSION
                                                  , KHR_SYNCHRONIZATION_2_EXTENSION_NAME
                                                  , pattern KHR_SYNCHRONIZATION_2_EXTENSION_NAME
                                                  , Flags64
                                                  ) where

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
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Dynamic (DeviceCmds(pVkCmdPipelineBarrier2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResetEvent2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetEvent2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWaitEvents2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteBufferMarker2AMD))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteTimestamp2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetQueueCheckpointData2NV))
import Vulkan.Dynamic (DeviceCmds(pVkQueueSubmit2KHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Event)
import Vulkan.Core10.Handles (Event(..))
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.ImageView (ImageSubresourceRange)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PerformanceQuerySubmitInfoKHR)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationsInfoEXT)
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoNV)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEPENDENCY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBMIT_INFO_2_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.FundamentalTypes (Flags64)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetEvent2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Event -> Ptr DependencyInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Event -> Ptr DependencyInfoKHR -> IO ()

-- | vkCmdSetEvent2KHR - Set an event object to signaled state
--
-- = Description
--
-- When 'cmdSetEvent2KHR' is submitted to a queue, it defines the first
-- half of memory dependencies defined by @pDependencyInfo@, as well as an
-- event signal operation which sets the event to the signaled state. A
-- memory dependency is defined between the event signal operation and
-- commands that occur earlier in submission order.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- are defined by the union of all the memory dependencies defined by
-- @pDependencyInfo@, and are applied to all operations that occur earlier
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers Queue family ownership transfers>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>
-- defined by @pDependencyInfo@ are also included in the first scopes.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the event signal operation, and any
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfers>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>
-- defined by @pDependencyInfo@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- includes only
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfers>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>.
--
-- Future 'cmdWaitEvents2KHR' commands rely on all values of each element
-- in @pDependencyInfo@ matching exactly with those used to signal the
-- corresponding event. 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents'
-- /must/ not be used to wait on the result of a signal operation defined
-- by 'cmdSetEvent2KHR'.
--
-- Note
--
-- The extra information provided by 'cmdSetEvent2KHR' compared to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent' allows implementations
-- to more efficiently schedule the operations required to satisfy the
-- requested dependencies. With
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent', the full dependency
-- information is not known until
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' is recorded, forcing
-- implementations to insert the required operations at that point and not
-- before.
--
-- If @event@ is already in the signaled state when 'cmdSetEvent2KHR' is
-- executed on the device, then 'cmdSetEvent2KHR' has no effect, no event
-- signal operation occurs, and no dependency is generated.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetEvent2KHR-synchronization2-03824# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetEvent2KHR-dependencyFlags-03825# The @dependencyFlags@
--     member of @pDependencyInfo@ /must/ be @0@
--
-- -   #VUID-vkCmdSetEvent2KHR-commandBuffer-03826# The current device mask
--     of @commandBuffer@ /must/ include exactly one physical device.
--
-- -   #VUID-vkCmdSetEvent2KHR-srcStageMask-03827# The @srcStageMask@
--     member of any element of the @pMemoryBarriers@,
--     @pBufferMemoryBarriers@, or @pImageMemoryBarriers@ members of
--     @pDependencyInfo@ /must/ only include pipeline stages valid for the
--     queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdSetEvent2KHR-dstStageMask-03828# The @dstStageMask@
--     member of any element of the @pMemoryBarriers@,
--     @pBufferMemoryBarriers@, or @pImageMemoryBarriers@ members of
--     @pDependencyInfo@ /must/ only include pipeline stages valid for the
--     queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetEvent2KHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetEvent2KHR-event-parameter# @event@ /must/ be a valid
--     'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-vkCmdSetEvent2KHR-pDependencyInfo-parameter# @pDependencyInfo@
--     /must/ be a valid pointer to a valid 'DependencyInfoKHR' structure
--
-- -   #VUID-vkCmdSetEvent2KHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetEvent2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdSetEvent2KHR-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdSetEvent2KHR-commonparent# Both of @commandBuffer@, and
--     @event@ /must/ have been created, allocated, or retrieved from the
--     same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DependencyInfoKHR',
-- 'Vulkan.Core10.Handles.Event'
cmdSetEvent2KHR :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command is
                   -- recorded.
                   CommandBuffer
                -> -- | @event@ is the event that will be signaled.
                   Event
                -> -- | @pDependencyInfo@ is a pointer to a 'DependencyInfoKHR' structure
                   -- defining the first scopes of this operation.
                   DependencyInfoKHR
                -> io ()
cmdSetEvent2KHR commandBuffer event dependencyInfo = liftIO . evalContT $ do
  let vkCmdSetEvent2KHRPtr = pVkCmdSetEvent2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetEvent2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetEvent2KHR is null" Nothing Nothing
  let vkCmdSetEvent2KHR' = mkVkCmdSetEvent2KHR vkCmdSetEvent2KHRPtr
  pDependencyInfo <- ContT $ withCStruct (dependencyInfo)
  lift $ traceAroundEvent "vkCmdSetEvent2KHR" (vkCmdSetEvent2KHR' (commandBufferHandle (commandBuffer)) (event) pDependencyInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetEvent2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Event -> PipelineStageFlags2KHR -> IO ()) -> Ptr CommandBuffer_T -> Event -> PipelineStageFlags2KHR -> IO ()

-- | vkCmdResetEvent2KHR - Reset an event object to non-signaled state
--
-- = Description
--
-- When 'cmdResetEvent2KHR' is submitted to a queue, it defines an
-- execution dependency on commands that were submitted before it, and
-- defines an event unsignal operation which resets the event to the
-- unsignaled state.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations by @stageMask@ or
-- stages that are
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>
-- than @stageMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the event unsignal operation.
--
-- If @event@ is already in the unsignaled state when 'cmdResetEvent2KHR'
-- is executed on the device, then this command has no effect, no event
-- unsignal operation occurs, and no execution dependency is generated.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-vkCmdResetEvent2KHR-synchronization2-03829# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-03830# @stageMask@ /must/ not
--     include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-vkCmdResetEvent2KHR-event-03831# There /must/ be an execution
--     dependency between 'cmdResetEvent2KHR' and the execution of any
--     'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' that includes
--     @event@ in its @pEvents@ parameter
--
-- -   #VUID-vkCmdResetEvent2KHR-event-03832# There /must/ be an execution
--     dependency between 'cmdResetEvent2KHR' and the execution of any
--     'cmdWaitEvents2KHR' that includes @event@ in its @pEvents@ parameter
--
-- -   #VUID-vkCmdResetEvent2KHR-commandBuffer-03833# @commandBuffer@’s
--     current device mask /must/ include exactly one physical device.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdResetEvent2KHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdResetEvent2KHR-event-parameter# @event@ /must/ be a valid
--     'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-parameter# @stageMask@ /must/ be
--     a valid combination of 'PipelineStageFlagBits2KHR' values
--
-- -   #VUID-vkCmdResetEvent2KHR-stageMask-requiredbitmask# @stageMask@
--     /must/ not be @0@
--
-- -   #VUID-vkCmdResetEvent2KHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdResetEvent2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdResetEvent2KHR-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdResetEvent2KHR-commonparent# Both of @commandBuffer@, and
--     @event@ /must/ have been created, allocated, or retrieved from the
--     same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Event',
-- 'PipelineStageFlags2KHR'
cmdResetEvent2KHR :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which the command is
                     -- recorded.
                     CommandBuffer
                  -> -- | @event@ is the event that will be unsignaled.
                     Event
                  -> -- | @stageMask@ is a 'PipelineStageFlags2KHR' mask of pipeline stages used
                     -- to determine the first
                     -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
                     ("stageMask" ::: PipelineStageFlags2KHR)
                  -> io ()
cmdResetEvent2KHR commandBuffer event stageMask = liftIO $ do
  let vkCmdResetEvent2KHRPtr = pVkCmdResetEvent2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdResetEvent2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResetEvent2KHR is null" Nothing Nothing
  let vkCmdResetEvent2KHR' = mkVkCmdResetEvent2KHR vkCmdResetEvent2KHRPtr
  traceAroundEvent "vkCmdResetEvent2KHR" (vkCmdResetEvent2KHR' (commandBufferHandle (commandBuffer)) (event) (stageMask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWaitEvents2KHRUnsafe
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfoKHR -> IO ()

foreign import ccall
  "dynamic" mkVkCmdWaitEvents2KHRSafe
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfoKHR -> IO ()

-- | cmdWaitEvents2KHR with selectable safeness
cmdWaitEvents2KHRSafeOrUnsafe :: forall io
                               . (MonadIO io)
                              => (FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfoKHR -> IO ())
                              -> -- | @commandBuffer@ is the command buffer into which the command is
                                 -- recorded.
                                 CommandBuffer
                              -> -- | @pEvents@ is a pointer to an array of @eventCount@ events to wait on.
                                 ("events" ::: Vector Event)
                              -> -- | @pDependencyInfos@ is a pointer to an array of @eventCount@
                                 -- 'DependencyInfoKHR' structures, defining the second
                                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
                                 ("dependencyInfos" ::: Vector DependencyInfoKHR)
                              -> io ()
cmdWaitEvents2KHRSafeOrUnsafe mkVkCmdWaitEvents2KHR commandBuffer events dependencyInfos = liftIO . evalContT $ do
  let vkCmdWaitEvents2KHRPtr = pVkCmdWaitEvents2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdWaitEvents2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWaitEvents2KHR is null" Nothing Nothing
  let vkCmdWaitEvents2KHR' = mkVkCmdWaitEvents2KHR vkCmdWaitEvents2KHRPtr
  let pEventsLength = Data.Vector.length $ (events)
  lift $ unless ((Data.Vector.length $ (dependencyInfos)) == pEventsLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pDependencyInfos and pEvents must have the same length" Nothing Nothing
  pPEvents <- ContT $ allocaBytes @Event ((Data.Vector.length (events)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPEvents `plusPtr` (8 * (i)) :: Ptr Event) (e)) (events)
  pPDependencyInfos <- ContT $ allocaBytes @DependencyInfoKHR ((Data.Vector.length (dependencyInfos)) * 64)
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPDependencyInfos `plusPtr` (64 * (i)) :: Ptr DependencyInfoKHR) (e) . ($ ())) (dependencyInfos)
  lift $ traceAroundEvent "vkCmdWaitEvents2KHR" (vkCmdWaitEvents2KHR' (commandBufferHandle (commandBuffer)) ((fromIntegral pEventsLength :: Word32)) (pPEvents) (pPDependencyInfos))
  pure $ ()

-- | vkCmdWaitEvents2KHR - Wait for one or more events
--
-- = Description
--
-- When 'cmdWaitEvents2KHR' is submitted to a queue, it inserts memory
-- dependencies according to the elements of @pDependencyInfos@ and each
-- corresponding element of @pEvents@. 'cmdWaitEvents2KHR' /must/ not be
-- used to wait on event signal operations occurring on other queues, or
-- signal operations execyted by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent'.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- of each memory dependency defined by any element i of @pDependencyInfos@
-- are applied to operations that occurred earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- than the last event signal operation on element i of @pEvents@.
--
-- Signal operations for an event at index i are only included if:
--
-- -   The event was signaled by a 'cmdSetEvent2KHR' command that occurred
--     earlier in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>
--     with a @dependencyInfo@ parameter exactly equal to the element of
--     @pDependencyInfos@ at index i ; or
--
-- -   The event was created without
--     'Vulkan.Core10.Enums.EventCreateFlagBits.EVENT_CREATE_DEVICE_ONLY_BIT_KHR',
--     and the first
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
--     defined by the element of @pDependencyInfos@ at index i only
--     includes host operations ('PIPELINE_STAGE_2_HOST_BIT_KHR').
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- of each memory dependency defined by any element i of @pDependencyInfos@
-- are applied to operations that occurred later in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- than 'cmdWaitEvents2KHR'.
--
-- Note
--
-- 'cmdWaitEvents2KHR' is used with 'cmdSetEvent2KHR' to define a memory
-- dependency between two sets of action commands, roughly in the same way
-- as pipeline barriers, but split into two commands such that work between
-- the two /may/ execute unhindered.
--
-- Note
--
-- Applications should be careful to avoid race conditions when using
-- events. There is no direct ordering guarantee between 'cmdSetEvent2KHR'
-- and 'cmdResetEvent2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResetEvent', or
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent'. Another execution
-- dependency (e.g. a pipeline barrier or semaphore with
-- 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR') is needed to prevent such a
-- race condition.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWaitEvents2KHR-synchronization2-03836# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdWaitEvents2KHR-pEvents-03837# Members of @pEvents@ /must/
--     not have been signaled by
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent'
--
-- -   #VUID-vkCmdWaitEvents2KHR-pEvents-03838# For any element i of
--     @pEvents@, if that event is signaled by 'cmdSetEvent2KHR', that
--     command’s @dependencyInfo@ parameter /must/ be exactly equal to the
--     ith element of @pDependencyInfos@
--
-- -   #VUID-vkCmdWaitEvents2KHR-pEvents-03839# For any element i of
--     @pEvents@, if that event is signaled by
--     'Vulkan.Core10.Event.setEvent', barriers in the ith element of
--     @pDependencyInfos@ /must/ include only host operations in their
--     first
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
--
-- -   #VUID-vkCmdWaitEvents2KHR-pEvents-03840# For any element i of
--     @pEvents@, if barriers in the ith element of @pDependencyInfos@
--     include only host operations, the ith element of @pEvents@ /must/ be
--     signaled before 'cmdWaitEvents2KHR' is executed
--
-- -   #VUID-vkCmdWaitEvents2KHR-pEvents-03841# For any element i of
--     @pEvents@, if barriers in the ith element of @pDependencyInfos@ do
--     not include host operations, the ith element of @pEvents@ /must/ be
--     signaled by a corresponding 'cmdSetEvent2KHR' that occurred earlier
--     in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>
--
-- -   #VUID-vkCmdWaitEvents2KHR-srcStageMask-03842# The @srcStageMask@
--     member of any element of the @pMemoryBarriers@,
--     @pBufferMemoryBarriers@, or @pImageMemoryBarriers@ members of
--     @pDependencyInfos@ /must/ either include only pipeline stages valid
--     for the queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from, or include only
--     'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-vkCmdWaitEvents2KHR-dstStageMask-03843# The @dstStageMask@
--     member of any element of the @pMemoryBarriers@,
--     @pBufferMemoryBarriers@, or @pImageMemoryBarriers@ members of
--     @pDependencyInfos@ /must/ only include pipeline stages valid for the
--     queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdWaitEvents2KHR-dependencyFlags-03844# The
--     @dependencyFlags@ member of any element of @pDependencyInfo@ /must/
--     be @0@
--
-- -   #VUID-vkCmdWaitEvents2KHR-pEvents-03845# If @pEvents@ includes one
--     or more events that will be signaled by
--     'Vulkan.Core10.Event.setEvent' after @commandBuffer@ has been
--     submitted to a queue, then 'cmdWaitEvents2KHR' /must/ not be called
--     inside a render pass instance
--
-- -   #VUID-vkCmdWaitEvents2KHR-commandBuffer-03846# @commandBuffer@’s
--     current device mask /must/ include exactly one physical device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWaitEvents2KHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWaitEvents2KHR-pEvents-parameter# @pEvents@ /must/ be a
--     valid pointer to an array of @eventCount@ valid
--     'Vulkan.Core10.Handles.Event' handles
--
-- -   #VUID-vkCmdWaitEvents2KHR-pDependencyInfos-parameter#
--     @pDependencyInfos@ /must/ be a valid pointer to an array of
--     @eventCount@ valid 'DependencyInfoKHR' structures
--
-- -   #VUID-vkCmdWaitEvents2KHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWaitEvents2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdWaitEvents2KHR-eventCount-arraylength# @eventCount@
--     /must/ be greater than @0@
--
-- -   #VUID-vkCmdWaitEvents2KHR-commonparent# Both of @commandBuffer@, and
--     the elements of @pEvents@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DependencyInfoKHR',
-- 'Vulkan.Core10.Handles.Event'
cmdWaitEvents2KHR :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which the command is
                     -- recorded.
                     CommandBuffer
                  -> -- | @pEvents@ is a pointer to an array of @eventCount@ events to wait on.
                     ("events" ::: Vector Event)
                  -> -- | @pDependencyInfos@ is a pointer to an array of @eventCount@
                     -- 'DependencyInfoKHR' structures, defining the second
                     -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
                     ("dependencyInfos" ::: Vector DependencyInfoKHR)
                  -> io ()
cmdWaitEvents2KHR = cmdWaitEvents2KHRSafeOrUnsafe mkVkCmdWaitEvents2KHRUnsafe

-- | A variant of 'cmdWaitEvents2KHR' which makes a *safe* FFI call
cmdWaitEvents2KHRSafe :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command is
                         -- recorded.
                         CommandBuffer
                      -> -- | @pEvents@ is a pointer to an array of @eventCount@ events to wait on.
                         ("events" ::: Vector Event)
                      -> -- | @pDependencyInfos@ is a pointer to an array of @eventCount@
                         -- 'DependencyInfoKHR' structures, defining the second
                         -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
                         ("dependencyInfos" ::: Vector DependencyInfoKHR)
                      -> io ()
cmdWaitEvents2KHRSafe = cmdWaitEvents2KHRSafeOrUnsafe mkVkCmdWaitEvents2KHRSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPipelineBarrier2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DependencyInfoKHR -> IO ()) -> Ptr CommandBuffer_T -> Ptr DependencyInfoKHR -> IO ()

-- | vkCmdPipelineBarrier2KHR - Insert a memory dependency
--
-- = Description
--
-- When 'cmdPipelineBarrier2KHR' is submitted to a queue, it defines memory
-- dependencies between commands that were submitted before it, and those
-- submitted after it.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- of each memory dependency defined by any element i of @pDependencyInfos@
-- are applied to operations that occurred earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- of each memory dependency defined by any element i of @pDependencyInfos@
-- are applied to operations that occurred later in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- If 'cmdPipelineBarrier2KHR' is recorded within a render pass instance,
-- the synchronization scopes are
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-barriers-subpass-self-dependencies limited to operations within the same subpass>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-pDependencies-02285# If
--     'cmdPipelineBarrier2KHR' is called within a render pass instance,
--     the render pass /must/ have been created with at least one
--     'Vulkan.Core10.Pass.SubpassDependency' instance in
--     'Vulkan.Core10.Pass.RenderPassCreateInfo'::@pDependencies@ that
--     expresses a dependency from the current subpass to itself, with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scopes>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
--     that are all supersets of the scopes defined in this command
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-bufferMemoryBarrierCount-01178# If
--     'cmdPipelineBarrier2KHR' is called within a render pass instance, it
--     /must/ not include any buffer memory barriers
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-image-04073# If
--     'cmdPipelineBarrier2KHR' is called within a render pass instance,
--     the @image@ member of any image memory barrier included in this
--     command /must/ be an attachment used in the current subpass both as
--     an input attachment, and as either a color or depth\/stencil
--     attachment
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-oldLayout-01181# If
--     'cmdPipelineBarrier2KHR' is called within a render pass instance,
--     the @oldLayout@ and @newLayout@ members of any image memory barrier
--     included in this command /must/ be equal
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-srcQueueFamilyIndex-01182# If
--     'cmdPipelineBarrier2KHR' is called within a render pass instance,
--     the @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members of any
--     image memory barrier included in this command /must/ be equal
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-dependencyFlags-01186# If
--     'cmdPipelineBarrier2KHR' is called outside of a render pass
--     instance,
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--     /must/ not be included in the dependency flags
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-synchronization2-03848# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-srcStageMask-03849# The
--     @srcStageMask@ member of any element of the @pMemoryBarriers@,
--     @pBufferMemoryBarriers@, or @pImageMemoryBarriers@ members of
--     @pDependencyInfo@ /must/ only include pipeline stages valid for the
--     queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-dstStageMask-03850# The
--     @dstStageMask@ member of any element of the @pMemoryBarriers@,
--     @pBufferMemoryBarriers@, or @pImageMemoryBarriers@ members of
--     @pDependencyInfo@ /must/ only include pipeline stages valid for the
--     queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-pDependencyInfo-parameter#
--     @pDependencyInfo@ /must/ be a valid pointer to a valid
--     'DependencyInfoKHR' structure
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPipelineBarrier2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Transfer                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DependencyInfoKHR'
cmdPipelineBarrier2KHR :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command is
                          -- recorded.
                          CommandBuffer
                       -> -- | @pDependencyInfo@ is a pointer to a 'DependencyInfoKHR' structure
                          -- defining the scopes of this operation.
                          DependencyInfoKHR
                       -> io ()
cmdPipelineBarrier2KHR commandBuffer dependencyInfo = liftIO . evalContT $ do
  let vkCmdPipelineBarrier2KHRPtr = pVkCmdPipelineBarrier2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdPipelineBarrier2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPipelineBarrier2KHR is null" Nothing Nothing
  let vkCmdPipelineBarrier2KHR' = mkVkCmdPipelineBarrier2KHR vkCmdPipelineBarrier2KHRPtr
  pDependencyInfo <- ContT $ withCStruct (dependencyInfo)
  lift $ traceAroundEvent "vkCmdPipelineBarrier2KHR" (vkCmdPipelineBarrier2KHR' (commandBufferHandle (commandBuffer)) pDependencyInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueSubmit2KHR
  :: FunPtr (Ptr Queue_T -> Word32 -> Ptr (SomeStruct SubmitInfo2KHR) -> Fence -> IO Result) -> Ptr Queue_T -> Word32 -> Ptr (SomeStruct SubmitInfo2KHR) -> Fence -> IO Result

-- | vkQueueSubmit2KHR - Submits command buffers to a queue
--
-- = Description
--
-- 'queueSubmit2KHR' is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-submission queue submission command>,
-- with each batch defined by an element of @pSubmits@ as an instance of
-- the 'SubmitInfo2KHR' structure.
--
-- Semaphore operations submitted with 'queueSubmit2KHR' have additional
-- ordering constraints compared to other submission commands, with
-- dependencies involving previous and subsequent queue operations.
-- Information about these additional constraints can be found in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores semaphore>
-- section of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- If any command buffer submitted to this queue is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>,
-- it is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
-- Once execution of all submissions of a command buffer complete, it moves
-- from the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>,
-- back to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>.
-- If a command buffer was recorded with the
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT'
-- flag, it instead moves back to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle invalid state>.
--
-- If 'queueSubmit2KHR' fails, it /may/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' or
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'. If it does, the
-- implementation /must/ ensure that the state and contents of any
-- resources or synchronization primitives referenced by the submitted
-- command buffers and any semaphores referenced by @pSubmits@ is
-- unaffected by the call or its failure. If 'queueSubmit2KHR' fails in
-- such a way that the implementation is unable to make that guarantee, the
-- implementation /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'. See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>.
--
-- == Valid Usage
--
-- -   #VUID-vkQueueSubmit2KHR-fence-04894# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be
--     unsignaled
--
-- -   #VUID-vkQueueSubmit2KHR-fence-04895# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ not be
--     associated with any other queue command that has not yet completed
--     execution on that queue
--
-- -   #VUID-vkQueueSubmit2KHR-synchronization2-03866# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkQueueSubmit2KHR-commandBuffer-03867# If a command recorded
--     into the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ referenced
--     an 'Vulkan.Core10.Handles.Event', that event /must/ not be
--     referenced by a command that has been submitted to another queue and
--     is still in the /pending state/
--
-- -   #VUID-vkQueueSubmit2KHR-semaphore-03868# The @semaphore@ member of
--     any binary semaphore element of the @pSignalSemaphoreInfos@ member
--     of any element of @pSubmits@ /must/ be unsignaled when the semaphore
--     signal operation it defines is executed on the device
--
-- -   #VUID-vkQueueSubmit2KHR-stageMask-03869# The @stageMask@ member of
--     any element of the @pSignalSemaphoreInfos@ member of any element of
--     @pSubmits@ /must/ only include pipeline stages that are supported by
--     the queue family which @queue@ belongs to
--
-- -   #VUID-vkQueueSubmit2KHR-stageMask-03870# The @stageMask@ member of
--     any element of the @pWaitSemaphoreInfos@ member of any element of
--     @pSubmits@ /must/ only include pipeline stages that are supported by
--     the queue family which @queue@ belongs to
--
-- -   #VUID-vkQueueSubmit2KHR-semaphore-03871# When a semaphore wait
--     operation for a binary semaphore is executed, as defined by the
--     @semaphore@ member of any element of the @pWaitSemaphoreInfos@
--     member of any element of @pSubmits@, there /must/ be no other queues
--     waiting on the same semaphore
--
-- -   #VUID-vkQueueSubmit2KHR-semaphore-03872# The @semaphore@ member of
--     any element of the @pWaitSemaphoreInfos@ member of any element of
--     @pSubmits@ /must/ be semaphores that are signaled, or have
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>
--     previously submitted for execution
--
-- -   #VUID-vkQueueSubmit2KHR-semaphore-03873# Any @semaphore@ member of
--     any element of the @pWaitSemaphoreInfos@ member of any element of
--     @pSubmits@ that was created with a
--     'Vulkan.Extensions.VK_KHR_timeline_semaphore.SemaphoreTypeKHR' of
--     'Vulkan.Extensions.VK_KHR_timeline_semaphore.SEMAPHORE_TYPE_BINARY_KHR'
--     /must/ reference a semaphore signal operation that has been
--     submitted for execution and any semaphore signal operations on which
--     it depends (if any) /must/ have also been submitted for execution
--
-- -   #VUID-vkQueueSubmit2KHR-commandBuffer-03874# The @commandBuffer@
--     member of any element of the @pCommandBufferInfos@ member of any
--     element of @pSubmits@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   #VUID-vkQueueSubmit2KHR-commandBuffer-03875# If a command recorded
--     into the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ was not
--     recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   #VUID-vkQueueSubmit2KHR-commandBuffer-03876# Any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ /must/ be
--     in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   #VUID-vkQueueSubmit2KHR-commandBuffer-03877# If any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ was not
--     recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   #VUID-vkQueueSubmit2KHR-commandBuffer-03878# The @commandBuffer@
--     member of any element of the @pCommandBufferInfos@ member of any
--     element of @pSubmits@ /must/ have been allocated from a
--     'Vulkan.Core10.Handles.CommandPool' that was created for the same
--     queue family @queue@ belongs to
--
-- -   #VUID-vkQueueSubmit2KHR-commandBuffer-03879# If a command recorded
--     into the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ includes a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire Queue Family Transfer Acquire Operation>,
--     there /must/ exist a previously submitted
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-release Queue Family Transfer Release Operation>
--     on a queue in the queue family identified by the acquire operation,
--     with parameters matching the acquire operation as defined in the
--     definition of such
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire acquire operations>,
--     and which happens before the acquire operation
--
-- -   #VUID-vkQueueSubmit2KHR-commandBuffer-03880# If a command recorded
--     into the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ was a
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery' whose
--     @queryPool@ was created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#profiling-lock profiling lock>
--     /must/ have been held continuously on the
--     'Vulkan.Core10.Handles.Device' that @queue@ was retrieved from,
--     throughout recording of those command buffers
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkQueueSubmit2KHR-queue-parameter# @queue@ /must/ be a valid
--     'Vulkan.Core10.Handles.Queue' handle
--
-- -   #VUID-vkQueueSubmit2KHR-pSubmits-parameter# If @submitCount@ is not
--     @0@, @pSubmits@ /must/ be a valid pointer to an array of
--     @submitCount@ valid 'SubmitInfo2KHR' structures
--
-- -   #VUID-vkQueueSubmit2KHR-fence-parameter# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be a valid
--     'Vulkan.Core10.Handles.Fence' handle
--
-- -   #VUID-vkQueueSubmit2KHR-commonparent# Both of @fence@, and @queue@
--     that are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Fence', 'Vulkan.Core10.Handles.Queue',
-- 'SubmitInfo2KHR'
queueSubmit2KHR :: forall io
                 . (MonadIO io)
                => -- | @queue@ is the queue that the command buffers will be submitted to.
                   Queue
                -> -- | @pSubmits@ is a pointer to an array of 'SubmitInfo2KHR' structures, each
                   -- specifying a command buffer submission batch.
                   ("submits" ::: Vector (SomeStruct SubmitInfo2KHR))
                -> -- | @fence@ is an /optional/ handle to a fence to be signaled once all
                   -- submitted command buffers have completed execution. If @fence@ is not
                   -- 'Vulkan.Core10.APIConstants.NULL_HANDLE', it defines a
                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>.
                   Fence
                -> io ()
queueSubmit2KHR queue submits fence = liftIO . evalContT $ do
  let vkQueueSubmit2KHRPtr = pVkQueueSubmit2KHR (deviceCmds (queue :: Queue))
  lift $ unless (vkQueueSubmit2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueSubmit2KHR is null" Nothing Nothing
  let vkQueueSubmit2KHR' = mkVkQueueSubmit2KHR vkQueueSubmit2KHRPtr
  pPSubmits <- ContT $ allocaBytes @(SubmitInfo2KHR _) ((Data.Vector.length (submits)) * 64)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPSubmits `plusPtr` (64 * (i)) :: Ptr (SubmitInfo2KHR _))) (e) . ($ ())) (submits)
  r <- lift $ traceAroundEvent "vkQueueSubmit2KHR" (vkQueueSubmit2KHR' (queueHandle (queue)) ((fromIntegral (Data.Vector.length $ (submits)) :: Word32)) (forgetExtensions (pPSubmits)) (fence))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteTimestamp2KHR
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags2KHR -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlags2KHR -> QueryPool -> Word32 -> IO ()

-- | vkCmdWriteTimestamp2KHR - Write a device timestamp into a query object
--
-- = Description
--
-- When 'cmdWriteTimestamp2KHR' is submitted to a queue, it defines an
-- execution dependency on commands that were submitted before it, and
-- writes a timestamp to a query pool.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations on the pipeline stage
-- specified by @stage@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the timestamp write operation.
--
-- When the timestamp value is written, the availability status of the
-- query is set to available.
--
-- Note
--
-- If an implementation is unable to detect completion and latch the timer
-- at any specific stage of the pipeline, it /may/ instead do so at any
-- logically later stage.
--
-- Comparisons between timestamps are not meaningful if the timestamps are
-- written by commands submitted to different queues.
--
-- Note
--
-- An example of such a comparison is subtracting an older timestamp from a
-- newer one to determine the execution time of a sequence of commands.
--
-- If 'cmdWriteTimestamp2KHR' is called while executing a render pass
-- instance that has multiview enabled, the timestamp uses N consecutive
-- query indices in the query pool (starting at @query@) where N is the
-- number of bits set in the view mask of the subpass the command is
-- executed in. The resulting query values are determined by an
-- implementation-dependent choice of one of the following behaviors:
--
-- -   The first query is a timestamp value and (if more than one bit is
--     set in the view mask) zero is written to the remaining queries. If
--     two timestamps are written in the same subpass, the sum of the
--     execution time of all views between those commands is the difference
--     between the first query written by each command.
--
-- -   All N queries are timestamp values. If two timestamps are written in
--     the same subpass, the sum of the execution time of all views between
--     those commands is the sum of the difference between corresponding
--     queries written by each command. The difference between
--     corresponding queries /may/ be the execution time of a single view.
--
-- In either case, the application /can/ sum the differences between all N
-- queries to determine the total execution time.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-synchronization2-03858# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-03859# @stage@ /must/ only
--     include a single pipeline stage
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-03860# @stage@ /must/ only
--     include stages valid for the queue family that was used to create
--     the command pool that @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-queryPool-03861# @queryPool@ /must/
--     have been created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-queryPool-03862# The query identified
--     by @queryPool@ and @query@ /must/ be /unavailable/
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-timestampValidBits-03863# The command
--     pool’s queue family /must/ support a non-zero @timestampValidBits@
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-query-04903# @query@ /must/ be less
--     than the number of queries in @queryPool@
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-None-03864# All queries used by the
--     command /must/ be unavailable
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-query-03865# If
--     'cmdWriteTimestamp2KHR' is called within a render pass instance, the
--     sum of @query@ and the number of bits set in the current subpass’s
--     view mask /must/ be less than or equal to the number of queries in
--     @queryPool@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-parameter# @stage@ /must/ be a
--     valid combination of 'PipelineStageFlagBits2KHR' values
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-stage-requiredbitmask# @stage@ /must/
--     not be @0@
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-queryPool-parameter# @queryPool@
--     /must/ be a valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdWriteTimestamp2KHR-commonparent# Both of @commandBuffer@,
--     and @queryPool@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Transfer                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'PipelineStageFlags2KHR',
-- 'Vulkan.Core10.Handles.QueryPool'
cmdWriteTimestamp2KHR :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer into which the command will be
                         -- recorded.
                         CommandBuffer
                      -> -- | @stage@ specifies a stage of the pipeline.
                         PipelineStageFlags2KHR
                      -> -- | @queryPool@ is the query pool that will manage the timestamp.
                         QueryPool
                      -> -- | @query@ is the query within the query pool that will contain the
                         -- timestamp.
                         ("query" ::: Word32)
                      -> io ()
cmdWriteTimestamp2KHR commandBuffer stage queryPool query = liftIO $ do
  let vkCmdWriteTimestamp2KHRPtr = pVkCmdWriteTimestamp2KHR (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdWriteTimestamp2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteTimestamp2KHR is null" Nothing Nothing
  let vkCmdWriteTimestamp2KHR' = mkVkCmdWriteTimestamp2KHR vkCmdWriteTimestamp2KHRPtr
  traceAroundEvent "vkCmdWriteTimestamp2KHR" (vkCmdWriteTimestamp2KHR' (commandBufferHandle (commandBuffer)) (stage) (queryPool) (query))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteBufferMarker2AMD
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags2KHR -> Buffer -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlags2KHR -> Buffer -> DeviceSize -> Word32 -> IO ()

-- | vkCmdWriteBufferMarker2AMD - Execute a pipelined write of a marker value
-- into a buffer
--
-- = Description
--
-- The command will write the 32-bit marker value into the buffer only
-- after all preceding commands have finished executing up to at least the
-- specified pipeline stage. This includes the completion of other
-- preceding 'cmdWriteBufferMarker2AMD' commands so long as their specified
-- pipeline stages occur either at the same time or earlier than this
-- command’s specified @stage@.
--
-- While consecutive buffer marker writes with the same @stage@ parameter
-- implicitly complete in submission order, memory and execution
-- dependencies between buffer marker writes and other operations /must/
-- still be explicitly ordered using synchronization commands. The access
-- scope for buffer marker writes falls under the
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_WRITE_BIT', and the
-- pipeline stages for identifying the synchronization scope /must/ include
-- both @stage@ and
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFER_BIT'.
--
-- Note
--
-- Similar to 'cmdWriteTimestamp2KHR', if an implementation is unable to
-- write a marker at any specific pipeline stage, it /may/ instead do so at
-- any logically later stage.
--
-- Note
--
-- Implementations /may/ only support a limited number of pipelined marker
-- write operations in flight at a given time. Thus an excessive number of
-- marker write operations /may/ degrade command execution performance.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @stage@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-synchronization2-03893# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03894# @stage@ /must/ include
--     only a single pipeline stage
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03895# @stage@ /must/ include
--     only stages that are valid for the queue family that was used to
--     create the command pool that @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstOffset-03896# @dstOffset@ /must/
--     be less than or equal to the size of @dstBuffer@ minus @4@.
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstBuffer-03897# @dstBuffer@ /must/
--     have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstBuffer-03898# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstOffset-03899# @dstOffset@ /must/
--     be a multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-parameter# @stage@ /must/ be
--     a valid combination of 'PipelineStageFlagBits2KHR' values
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-requiredbitmask# @stage@
--     /must/ not be @0@
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstBuffer-parameter# @dstBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-commonparent# Both of
--     @commandBuffer@, and @dstBuffer@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Transfer                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'PipelineStageFlags2KHR'
cmdWriteBufferMarker2AMD :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @stage@ specifies the pipeline stage whose completion triggers the
                            -- marker write.
                            PipelineStageFlags2KHR
                         -> -- | @dstBuffer@ is the buffer where the marker will be written.
                            ("dstBuffer" ::: Buffer)
                         -> -- | @dstOffset@ is the byte offset into the buffer where the marker will be
                            -- written.
                            ("dstOffset" ::: DeviceSize)
                         -> -- | @marker@ is the 32-bit value of the marker.
                            ("marker" ::: Word32)
                         -> io ()
cmdWriteBufferMarker2AMD commandBuffer stage dstBuffer dstOffset marker = liftIO $ do
  let vkCmdWriteBufferMarker2AMDPtr = pVkCmdWriteBufferMarker2AMD (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdWriteBufferMarker2AMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteBufferMarker2AMD is null" Nothing Nothing
  let vkCmdWriteBufferMarker2AMD' = mkVkCmdWriteBufferMarker2AMD vkCmdWriteBufferMarker2AMDPtr
  traceAroundEvent "vkCmdWriteBufferMarker2AMD" (vkCmdWriteBufferMarker2AMD' (commandBufferHandle (commandBuffer)) (stage) (dstBuffer) (dstOffset) (marker))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetQueueCheckpointData2NV
  :: FunPtr (Ptr Queue_T -> Ptr Word32 -> Ptr CheckpointData2NV -> IO ()) -> Ptr Queue_T -> Ptr Word32 -> Ptr CheckpointData2NV -> IO ()

-- | vkGetQueueCheckpointData2NV - retrieve diagnostic checkpoint data
--
-- = Description
--
-- If @pCheckpointData@ is @NULL@, then the number of checkpoint markers
-- available is returned in @pCheckpointDataCount@. Otherwise,
-- @pCheckpointDataCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pCheckpointData@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pCheckpointData@.
--
-- If @pCheckpointDataCount@ is less than the number of checkpoint markers
-- available, at most @pCheckpointDataCount@ structures will be written.
--
-- == Valid Usage
--
-- -   #VUID-vkGetQueueCheckpointData2NV-queue-03892# The device that
--     @queue@ belongs to /must/ be in the lost state
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetQueueCheckpointData2NV-queue-parameter# @queue@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Queue' handle
--
-- -   #VUID-vkGetQueueCheckpointData2NV-pCheckpointDataCount-parameter#
--     @pCheckpointDataCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   #VUID-vkGetQueueCheckpointData2NV-pCheckpointData-parameter# If the
--     value referenced by @pCheckpointDataCount@ is not @0@, and
--     @pCheckpointData@ is not @NULL@, @pCheckpointData@ /must/ be a valid
--     pointer to an array of @pCheckpointDataCount@ 'CheckpointData2NV'
--     structures
--
-- = See Also
--
-- 'CheckpointData2NV', 'Vulkan.Core10.Handles.Queue'
getQueueCheckpointData2NV :: forall io
                           . (MonadIO io)
                          => -- | @queue@ is the 'Vulkan.Core10.Handles.Queue' object the caller would
                             -- like to retrieve checkpoint data for
                             Queue
                          -> io (("checkpointData" ::: Vector CheckpointData2NV))
getQueueCheckpointData2NV queue = liftIO . evalContT $ do
  let vkGetQueueCheckpointData2NVPtr = pVkGetQueueCheckpointData2NV (deviceCmds (queue :: Queue))
  lift $ unless (vkGetQueueCheckpointData2NVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetQueueCheckpointData2NV is null" Nothing Nothing
  let vkGetQueueCheckpointData2NV' = mkVkGetQueueCheckpointData2NV vkGetQueueCheckpointData2NVPtr
  let queue' = queueHandle (queue)
  pPCheckpointDataCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetQueueCheckpointData2NV" (vkGetQueueCheckpointData2NV' queue' (pPCheckpointDataCount) (nullPtr))
  pCheckpointDataCount <- lift $ peek @Word32 pPCheckpointDataCount
  pPCheckpointData <- ContT $ bracket (callocBytes @CheckpointData2NV ((fromIntegral (pCheckpointDataCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCheckpointData `advancePtrBytes` (i * 32) :: Ptr CheckpointData2NV) . ($ ())) [0..(fromIntegral (pCheckpointDataCount)) - 1]
  lift $ traceAroundEvent "vkGetQueueCheckpointData2NV" (vkGetQueueCheckpointData2NV' queue' (pPCheckpointDataCount) ((pPCheckpointData)))
  pCheckpointDataCount' <- lift $ peek @Word32 pPCheckpointDataCount
  pCheckpointData' <- lift $ generateM (fromIntegral (pCheckpointDataCount')) (\i -> peekCStruct @CheckpointData2NV (((pPCheckpointData) `advancePtrBytes` (32 * (i)) :: Ptr CheckpointData2NV)))
  pure $ (pCheckpointData')


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV"
pattern PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV = PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR


-- No documentation found for TopLevel "VK_ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV"
pattern ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV = ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV"
pattern PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV = PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV"
pattern PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV = PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR


-- No documentation found for TopLevel "VK_ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV"
pattern ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV = ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR


-- No documentation found for TopLevel "VK_ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV"
pattern ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV = ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_TRANSFER_BIT_KHR"
pattern PIPELINE_STAGE_2_TRANSFER_BIT_KHR = PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR


-- | VkMemoryBarrier2KHR - Structure specifying a global memory barrier
--
-- = Description
--
-- This structure defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependency>
-- affecting all device memory.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @srcStageMask@ and @srcAccessMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @dstStageMask@ and @dstAccessMask@.
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03900# If @srcAccessMask@
--     includes 'ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03901# If @srcAccessMask@
--     includes 'ACCESS_2_INDEX_READ_BIT_KHR', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03902# If @srcAccessMask@
--     includes 'ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03903# If @srcAccessMask@
--     includes 'ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR',
--     @VK_PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI@,
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03904# If @srcAccessMask@
--     includes 'ACCESS_2_UNIFORM_READ_BIT_KHR', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03905# If @srcAccessMask@
--     includes 'ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03906# If @srcAccessMask@
--     includes 'ACCESS_2_SHADER_STORAGE_READ_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03907# If @srcAccessMask@
--     includes 'ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03908# If @srcAccessMask@
--     includes 'ACCESS_2_SHADER_READ_BIT_KHR', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or one of
--     the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03909# If @srcAccessMask@
--     includes 'ACCESS_2_SHADER_WRITE_BIT_KHR', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03910# If @srcAccessMask@
--     includes 'ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03911# If @srcAccessMask@
--     includes 'ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03912# If @srcAccessMask@
--     includes 'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03913# If @srcAccessMask@
--     includes 'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03914# If @srcAccessMask@
--     includes 'ACCESS_2_TRANSFER_READ_BIT_KHR', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03915# If @srcAccessMask@
--     includes 'ACCESS_2_TRANSFER_WRITE_BIT_KHR', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_CLEAR_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03916# If @srcAccessMask@
--     includes 'ACCESS_2_HOST_READ_BIT_KHR', @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03917# If @srcAccessMask@
--     includes 'ACCESS_2_HOST_WRITE_BIT_KHR', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03918# If @srcAccessMask@
--     includes 'ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03919# If @srcAccessMask@
--     includes 'ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03920# If @srcAccessMask@
--     includes 'ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-04747# If @srcAccessMask@
--     includes 'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03922# If @srcAccessMask@
--     includes 'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03923# If @srcAccessMask@
--     includes 'ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-04994# If @srcAccessMask@
--     includes 'ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03924# If @srcAccessMask@
--     includes 'ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03925# If @srcAccessMask@
--     includes 'ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03926# If @srcAccessMask@
--     includes 'ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03927# If @srcAccessMask@
--     includes 'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-03928# If @srcAccessMask@
--     includes 'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03900# If @dstAccessMask@
--     includes 'ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03901# If @dstAccessMask@
--     includes 'ACCESS_2_INDEX_READ_BIT_KHR', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03902# If @dstAccessMask@
--     includes 'ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03903# If @dstAccessMask@
--     includes 'ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR',
--     @VK_PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI@,
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03904# If @dstAccessMask@
--     includes 'ACCESS_2_UNIFORM_READ_BIT_KHR', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03905# If @dstAccessMask@
--     includes 'ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03906# If @dstAccessMask@
--     includes 'ACCESS_2_SHADER_STORAGE_READ_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03907# If @dstAccessMask@
--     includes 'ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03908# If @dstAccessMask@
--     includes 'ACCESS_2_SHADER_READ_BIT_KHR', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or one of
--     the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03909# If @dstAccessMask@
--     includes 'ACCESS_2_SHADER_WRITE_BIT_KHR', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03910# If @dstAccessMask@
--     includes 'ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03911# If @dstAccessMask@
--     includes 'ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03912# If @dstAccessMask@
--     includes 'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03913# If @dstAccessMask@
--     includes 'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03914# If @dstAccessMask@
--     includes 'ACCESS_2_TRANSFER_READ_BIT_KHR', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03915# If @dstAccessMask@
--     includes 'ACCESS_2_TRANSFER_WRITE_BIT_KHR', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_CLEAR_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03916# If @dstAccessMask@
--     includes 'ACCESS_2_HOST_READ_BIT_KHR', @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03917# If @dstAccessMask@
--     includes 'ACCESS_2_HOST_WRITE_BIT_KHR', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03918# If @dstAccessMask@
--     includes 'ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03919# If @dstAccessMask@
--     includes 'ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03920# If @dstAccessMask@
--     includes 'ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-04747# If @dstAccessMask@
--     includes 'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03922# If @dstAccessMask@
--     includes 'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03923# If @dstAccessMask@
--     includes 'ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-04994# If @dstAccessMask@
--     includes 'ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03924# If @dstAccessMask@
--     includes 'ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03925# If @dstAccessMask@
--     includes 'ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03926# If @dstAccessMask@
--     includes 'ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03927# If @dstAccessMask@
--     includes 'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-03928# If @dstAccessMask@
--     includes 'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryBarrier2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR'
--
-- -   #VUID-VkMemoryBarrier2KHR-srcStageMask-parameter# @srcStageMask@
--     /must/ be a valid combination of 'PipelineStageFlagBits2KHR' values
--
-- -   #VUID-VkMemoryBarrier2KHR-srcAccessMask-parameter# @srcAccessMask@
--     /must/ be a valid combination of 'AccessFlagBits2KHR' values
--
-- -   #VUID-VkMemoryBarrier2KHR-dstStageMask-parameter# @dstStageMask@
--     /must/ be a valid combination of 'PipelineStageFlagBits2KHR' values
--
-- -   #VUID-VkMemoryBarrier2KHR-dstAccessMask-parameter# @dstAccessMask@
--     /must/ be a valid combination of 'AccessFlagBits2KHR' values
--
-- = See Also
--
-- 'AccessFlags2KHR', 'DependencyInfoKHR', 'PipelineStageFlags2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryBarrier2KHR = MemoryBarrier2KHR
  { -- | @srcStageMask@ is a 'PipelineStageFlags2KHR' mask of pipeline stages to
    -- be included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes first synchronization scope>.
    srcStageMask :: PipelineStageFlags2KHR
  , -- | @srcAccessMask@ is a 'AccessFlags2KHR' mask of access flags to be
    -- included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes first access scope>.
    srcAccessMask :: AccessFlags2KHR
  , -- | @dstStageMask@ is a 'PipelineStageFlags2KHR' mask of pipeline stages to
    -- be included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes second synchronization scope>.
    dstStageMask :: PipelineStageFlags2KHR
  , -- | @dstAccessMask@ is a 'AccessFlags2KHR' mask of access flags to be
    -- included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes second access scope>.
    dstAccessMask :: AccessFlags2KHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryBarrier2KHR)
#endif
deriving instance Show MemoryBarrier2KHR

instance ToCStruct MemoryBarrier2KHR where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryBarrier2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR)) (srcStageMask)
    poke ((p `plusPtr` 24 :: Ptr AccessFlags2KHR)) (srcAccessMask)
    poke ((p `plusPtr` 32 :: Ptr PipelineStageFlags2KHR)) (dstStageMask)
    poke ((p `plusPtr` 40 :: Ptr AccessFlags2KHR)) (dstAccessMask)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MemoryBarrier2KHR where
  peekCStruct p = do
    srcStageMask <- peek @PipelineStageFlags2KHR ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR))
    srcAccessMask <- peek @AccessFlags2KHR ((p `plusPtr` 24 :: Ptr AccessFlags2KHR))
    dstStageMask <- peek @PipelineStageFlags2KHR ((p `plusPtr` 32 :: Ptr PipelineStageFlags2KHR))
    dstAccessMask <- peek @AccessFlags2KHR ((p `plusPtr` 40 :: Ptr AccessFlags2KHR))
    pure $ MemoryBarrier2KHR
             srcStageMask srcAccessMask dstStageMask dstAccessMask

instance Storable MemoryBarrier2KHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryBarrier2KHR where
  zero = MemoryBarrier2KHR
           zero
           zero
           zero
           zero


-- | VkImageMemoryBarrier2KHR - Structure specifying an image memory barrier
--
-- = Description
--
-- This structure defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependency>
-- limited to an image subresource range, and /can/ define a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>
-- for that subresource range.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @srcStageMask@ and @srcAccessMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @dstStageMask@ and @dstAccessMask@.
--
-- Both
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
-- are limited to only memory accesses to @image@ in the subresource range
-- defined by @subresourceRange@.
--
-- If @image@ was created with
-- 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
-- @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, this memory
-- barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>.
-- When executed on a queue in the family identified by
-- @srcQueueFamilyIndex@, this barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified image subresource range, and the second
-- synchronization and access scopes do not synchronize operations on that
-- queue. When executed on a queue in the family identified by
-- @dstQueueFamilyIndex@, this barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified image subresource range, and the first synchronization
-- and access scopes do not synchronize operations on that queue.
--
-- A
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>
-- is also defined if the values are not equal, and either is one of the
-- special queue family values reserved for external memory ownership
-- transfers, as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers>.
-- A
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- is defined when @dstQueueFamilyIndex@ is one of those values, and a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- is defined when @srcQueueFamilyIndex@ is one of those values.
--
-- If @oldLayout@ is not equal to @newLayout@, then the memory barrier
-- defines an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>
-- for the specified image subresource range. If this memory barrier
-- defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>,
-- the layout transition is only executed once between the queues.
--
-- Note
--
-- When the old and new layout are equal, the layout values are ignored -
-- data is preserved no matter what values are specified, or what layout
-- the image is currently in.
--
-- If @image@ has a multi-planar format and the image is /disjoint/, then
-- including
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT' in the
-- @aspectMask@ member of @subresourceRange@ is equivalent to including
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', and
-- (for three-plane formats only)
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
--
-- == Valid Usage
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03900# If
--     @srcAccessMask@ includes 'ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03901# If
--     @srcAccessMask@ includes 'ACCESS_2_INDEX_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03902# If
--     @srcAccessMask@ includes 'ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03903# If
--     @srcAccessMask@ includes 'ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR',
--     @VK_PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI@,
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03904# If
--     @srcAccessMask@ includes 'ACCESS_2_UNIFORM_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03905# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03906# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_STORAGE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03907# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03908# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or one of
--     the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03909# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03910# If
--     @srcAccessMask@ includes 'ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03911# If
--     @srcAccessMask@ includes 'ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03912# If
--     @srcAccessMask@ includes
--     'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03913# If
--     @srcAccessMask@ includes
--     'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03914# If
--     @srcAccessMask@ includes 'ACCESS_2_TRANSFER_READ_BIT_KHR',
--     @srcStageMask@ /must/ include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03915# If
--     @srcAccessMask@ includes 'ACCESS_2_TRANSFER_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_CLEAR_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03916# If
--     @srcAccessMask@ includes 'ACCESS_2_HOST_READ_BIT_KHR',
--     @srcStageMask@ /must/ include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03917# If
--     @srcAccessMask@ includes 'ACCESS_2_HOST_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03918# If
--     @srcAccessMask@ includes
--     'ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03919# If
--     @srcAccessMask@ includes
--     'ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03920# If
--     @srcAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-04747# If
--     @srcAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03922# If
--     @srcAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03923# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-04994# If
--     @srcAccessMask@ includes 'ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03924# If
--     @srcAccessMask@ includes 'ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03925# If
--     @srcAccessMask@ includes 'ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03926# If
--     @srcAccessMask@ includes
--     'ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03927# If
--     @srcAccessMask@ includes
--     'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR', @srcStageMask@
--     /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-03928# If
--     @srcAccessMask@ includes
--     'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR', @srcStageMask@
--     /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03900# If
--     @dstAccessMask@ includes 'ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03901# If
--     @dstAccessMask@ includes 'ACCESS_2_INDEX_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03902# If
--     @dstAccessMask@ includes 'ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03903# If
--     @dstAccessMask@ includes 'ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR',
--     @VK_PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI@,
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03904# If
--     @dstAccessMask@ includes 'ACCESS_2_UNIFORM_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03905# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03906# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_STORAGE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03907# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03908# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or one of
--     the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03909# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03910# If
--     @dstAccessMask@ includes 'ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03911# If
--     @dstAccessMask@ includes 'ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03912# If
--     @dstAccessMask@ includes
--     'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03913# If
--     @dstAccessMask@ includes
--     'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03914# If
--     @dstAccessMask@ includes 'ACCESS_2_TRANSFER_READ_BIT_KHR',
--     @dstStageMask@ /must/ include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03915# If
--     @dstAccessMask@ includes 'ACCESS_2_TRANSFER_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_CLEAR_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03916# If
--     @dstAccessMask@ includes 'ACCESS_2_HOST_READ_BIT_KHR',
--     @dstStageMask@ /must/ include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03917# If
--     @dstAccessMask@ includes 'ACCESS_2_HOST_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03918# If
--     @dstAccessMask@ includes
--     'ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03919# If
--     @dstAccessMask@ includes
--     'ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03920# If
--     @dstAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-04747# If
--     @dstAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03922# If
--     @dstAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03923# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-04994# If
--     @dstAccessMask@ includes 'ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03924# If
--     @dstAccessMask@ includes 'ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03925# If
--     @dstAccessMask@ includes 'ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03926# If
--     @dstAccessMask@ includes
--     'ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03927# If
--     @dstAccessMask@ includes
--     'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR', @dstStageMask@
--     /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-03928# If
--     @dstAccessMask@ includes
--     'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR', @dstStageMask@
--     /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-subresourceRange-01486#
--     @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was
--     created
--
-- -   #VUID-VkImageMemoryBarrier2KHR-subresourceRange-01724# If
--     @subresourceRange.levelCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkImageMemoryBarrier2KHR-subresourceRange-01488#
--     @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @image@ was created
--
-- -   #VUID-VkImageMemoryBarrier2KHR-subresourceRange-01725# If
--     @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkImageMemoryBarrier2KHR-image-01932# If @image@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-01208# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-01209# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-01210# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-01211# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT' or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-01212# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-01213# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-01197# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     @oldLayout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or the
--     current layout of the image subresources affected by the barrier
--
-- -   #VUID-VkImageMemoryBarrier2KHR-newLayout-01198# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     @newLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-01658# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-01659# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcQueueFamilyIndex-04065# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcQueueFamilyIndex-04066# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcQueueFamilyIndex-04067# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcQueueFamilyIndex-04068# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcQueueFamilyIndex-03938# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR',
--     @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcQueueFamilyIndex-03939# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR',
--     @image@ /must/ have been created with at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-02088# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     set
--
-- -   #VUID-VkImageMemoryBarrier2KHR-image-01671# If @image@ has a
--     single-plane color format or is not /disjoint/, then the
--     @aspectMask@ member of @subresourceRange@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-image-01672# If @image@ has a
--     multi-planar format and the image is /disjoint/, then the
--     @aspectMask@ member of @subresourceRange@ /must/ include either at
--     least one of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT';
--     or /must/ include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-image-01673# If @image@ has a
--     multi-planar format with only two planes, then the @aspectMask@
--     member of @subresourceRange@ /must/ not include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-image-03319# If @image@ has a
--     depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include either or both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-image-03320# If @image@ has a
--     depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcQueueFamilyIndex-04070# If
--     @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, at
--     least one /must/ not be a special queue family reserved for external
--     memory ownership transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers ???>
--
-- -   #VUID-VkImageMemoryBarrier2KHR-image-04071# If @image@ was created
--     with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal, and
--     one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ is one of the
--     special queue family values reserved for external memory transfers,
--     the other /must/ be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-image-04072# If @image@ was created
--     with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal,
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ both be valid
--     queue families, or one of the special queue family values reserved
--     for external memory transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers ???>
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-03854# If either
--     @srcStageMask@ or @dstStageMask@ includes
--     'PIPELINE_STAGE_2_HOST_BIT_KHR', @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ /must/ be equal
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-03855# If @srcStageMask@
--     includes 'PIPELINE_STAGE_2_HOST_BIT_KHR', and @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     @oldLayout@ /must/ be one of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageMemoryBarrier2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT'
--
-- -   #VUID-VkImageMemoryBarrier2KHR-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcStageMask-parameter#
--     @srcStageMask@ /must/ be a valid combination of
--     'PipelineStageFlagBits2KHR' values
--
-- -   #VUID-VkImageMemoryBarrier2KHR-srcAccessMask-parameter#
--     @srcAccessMask@ /must/ be a valid combination of
--     'AccessFlagBits2KHR' values
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstStageMask-parameter#
--     @dstStageMask@ /must/ be a valid combination of
--     'PipelineStageFlagBits2KHR' values
--
-- -   #VUID-VkImageMemoryBarrier2KHR-dstAccessMask-parameter#
--     @dstAccessMask@ /must/ be a valid combination of
--     'AccessFlagBits2KHR' values
--
-- -   #VUID-VkImageMemoryBarrier2KHR-oldLayout-parameter# @oldLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkImageMemoryBarrier2KHR-newLayout-parameter# @newLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-VkImageMemoryBarrier2KHR-image-parameter# @image@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkImageMemoryBarrier2KHR-subresourceRange-parameter#
--     @subresourceRange@ /must/ be a valid
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' structure
--
-- = See Also
--
-- 'AccessFlags2KHR', 'DependencyInfoKHR', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.ImageView.ImageSubresourceRange',
-- 'PipelineStageFlags2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageMemoryBarrier2KHR (es :: [Type]) = ImageMemoryBarrier2KHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @srcStageMask@ is a 'PipelineStageFlags2KHR' mask of pipeline stages to
    -- be included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes first synchronization scope>.
    srcStageMask :: PipelineStageFlags2KHR
  , -- | @srcAccessMask@ is a 'AccessFlags2KHR' mask of access flags to be
    -- included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes first access scope>.
    srcAccessMask :: AccessFlags2KHR
  , -- | @dstStageMask@ is a 'PipelineStageFlags2KHR' mask of pipeline stages to
    -- be included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes second synchronization scope>.
    dstStageMask :: PipelineStageFlags2KHR
  , -- | @dstAccessMask@ is a 'AccessFlags2KHR' mask of access flags to be
    -- included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes second access scope>.
    dstAccessMask :: AccessFlags2KHR
  , -- | @oldLayout@ is the old layout in an
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    oldLayout :: ImageLayout
  , -- | @newLayout@ is the new layout in an
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    newLayout :: ImageLayout
  , -- | @srcQueueFamilyIndex@ is the source queue family for a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    srcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    dstQueueFamilyIndex :: Word32
  , -- | @image@ is a handle to the image affected by this barrier.
    image :: Image
  , -- | @subresourceRange@ describes the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views image subresource range>
    -- within @image@ that is affected by this barrier.
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageMemoryBarrier2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageMemoryBarrier2KHR es)

instance Extensible ImageMemoryBarrier2KHR where
  extensibleTypeName = "ImageMemoryBarrier2KHR"
  setNext x next = x{next = next}
  getNext ImageMemoryBarrier2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageMemoryBarrier2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SampleLocationsInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss ImageMemoryBarrier2KHR es, PokeChain es) => ToCStruct (ImageMemoryBarrier2KHR es) where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageMemoryBarrier2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR)) (srcStageMask)
    lift $ poke ((p `plusPtr` 24 :: Ptr AccessFlags2KHR)) (srcAccessMask)
    lift $ poke ((p `plusPtr` 32 :: Ptr PipelineStageFlags2KHR)) (dstStageMask)
    lift $ poke ((p `plusPtr` 40 :: Ptr AccessFlags2KHR)) (dstAccessMask)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (oldLayout)
    lift $ poke ((p `plusPtr` 52 :: Ptr ImageLayout)) (newLayout)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (srcQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (dstQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 64 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 72 :: Ptr ImageSubresourceRange)) (subresourceRange)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr ImageSubresourceRange)) (zero)
    lift $ f

instance (Extendss ImageMemoryBarrier2KHR es, PeekChain es) => FromCStruct (ImageMemoryBarrier2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    srcStageMask <- peek @PipelineStageFlags2KHR ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR))
    srcAccessMask <- peek @AccessFlags2KHR ((p `plusPtr` 24 :: Ptr AccessFlags2KHR))
    dstStageMask <- peek @PipelineStageFlags2KHR ((p `plusPtr` 32 :: Ptr PipelineStageFlags2KHR))
    dstAccessMask <- peek @AccessFlags2KHR ((p `plusPtr` 40 :: Ptr AccessFlags2KHR))
    oldLayout <- peek @ImageLayout ((p `plusPtr` 48 :: Ptr ImageLayout))
    newLayout <- peek @ImageLayout ((p `plusPtr` 52 :: Ptr ImageLayout))
    srcQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    dstQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    image <- peek @Image ((p `plusPtr` 64 :: Ptr Image))
    subresourceRange <- peekCStruct @ImageSubresourceRange ((p `plusPtr` 72 :: Ptr ImageSubresourceRange))
    pure $ ImageMemoryBarrier2KHR
             next srcStageMask srcAccessMask dstStageMask dstAccessMask oldLayout newLayout srcQueueFamilyIndex dstQueueFamilyIndex image subresourceRange

instance es ~ '[] => Zero (ImageMemoryBarrier2KHR es) where
  zero = ImageMemoryBarrier2KHR
           ()
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


-- | VkBufferMemoryBarrier2KHR - Structure specifying a buffer memory barrier
--
-- = Description
--
-- This structure defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependency>
-- limited to a range of a buffer, and /can/ define a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>
-- for that range.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @srcStageMask@ and @srcAccessMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @dstStageMask@ and @dstAccessMask@.
--
-- Both
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
-- are limited to only memory accesses to @buffer@ in the range defined by
-- @offset@ and @size@.
--
-- If @buffer@ was created with
-- 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
-- @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, this memory
-- barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>.
-- When executed on a queue in the family identified by
-- @srcQueueFamilyIndex@, this barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified buffer range, and the second synchronization and
-- access scopes do not synchronize operations on that queue. When executed
-- on a queue in the family identified by @dstQueueFamilyIndex@, this
-- barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified buffer range, and the first synchronization and access
-- scopes do not synchronize operations on that queue.
--
-- A
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>
-- is also defined if the values are not equal, and either is one of the
-- special queue family values reserved for external memory ownership
-- transfers, as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers>.
-- A
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- is defined when @dstQueueFamilyIndex@ is one of those values, and a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- is defined when @srcQueueFamilyIndex@ is one of those values.
--
-- == Valid Usage
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03900# If
--     @srcAccessMask@ includes 'ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03901# If
--     @srcAccessMask@ includes 'ACCESS_2_INDEX_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03902# If
--     @srcAccessMask@ includes 'ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03903# If
--     @srcAccessMask@ includes 'ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR',
--     @VK_PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI@,
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03904# If
--     @srcAccessMask@ includes 'ACCESS_2_UNIFORM_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03905# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03906# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_STORAGE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03907# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03908# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or one of
--     the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03909# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADER_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03910# If
--     @srcAccessMask@ includes 'ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03911# If
--     @srcAccessMask@ includes 'ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03912# If
--     @srcAccessMask@ includes
--     'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03913# If
--     @srcAccessMask@ includes
--     'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03914# If
--     @srcAccessMask@ includes 'ACCESS_2_TRANSFER_READ_BIT_KHR',
--     @srcStageMask@ /must/ include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03915# If
--     @srcAccessMask@ includes 'ACCESS_2_TRANSFER_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_CLEAR_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03916# If
--     @srcAccessMask@ includes 'ACCESS_2_HOST_READ_BIT_KHR',
--     @srcStageMask@ /must/ include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03917# If
--     @srcAccessMask@ includes 'ACCESS_2_HOST_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03918# If
--     @srcAccessMask@ includes
--     'ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03919# If
--     @srcAccessMask@ includes
--     'ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03920# If
--     @srcAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT', @srcStageMask@ /must/
--     include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-04747# If
--     @srcAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03922# If
--     @srcAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03923# If
--     @srcAccessMask@ includes 'ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-04994# If
--     @srcAccessMask@ includes 'ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03924# If
--     @srcAccessMask@ includes 'ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03925# If
--     @srcAccessMask@ includes 'ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @srcStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03926# If
--     @srcAccessMask@ includes
--     'ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT', @srcStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03927# If
--     @srcAccessMask@ includes
--     'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR', @srcStageMask@
--     /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-03928# If
--     @srcAccessMask@ includes
--     'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR', @srcStageMask@
--     /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03900# If
--     @dstAccessMask@ includes 'ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03901# If
--     @dstAccessMask@ includes 'ACCESS_2_INDEX_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03902# If
--     @dstAccessMask@ includes 'ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03903# If
--     @dstAccessMask@ includes 'ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR',
--     @VK_PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI@,
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03904# If
--     @dstAccessMask@ includes 'ACCESS_2_UNIFORM_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03905# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03906# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_STORAGE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03907# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03908# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or one of
--     the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03909# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADER_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR', or one of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03910# If
--     @dstAccessMask@ includes 'ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03911# If
--     @dstAccessMask@ includes 'ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03912# If
--     @dstAccessMask@ includes
--     'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03913# If
--     @dstAccessMask@ includes
--     'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03914# If
--     @dstAccessMask@ includes 'ACCESS_2_TRANSFER_READ_BIT_KHR',
--     @dstStageMask@ /must/ include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03915# If
--     @dstAccessMask@ includes 'ACCESS_2_TRANSFER_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include 'PIPELINE_STAGE_2_COPY_BIT_KHR',
--     'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR',
--     'PIPELINE_STAGE_2_CLEAR_BIT_KHR',
--     'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR',
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03916# If
--     @dstAccessMask@ includes 'ACCESS_2_HOST_READ_BIT_KHR',
--     @dstStageMask@ /must/ include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03917# If
--     @dstAccessMask@ includes 'ACCESS_2_HOST_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include 'PIPELINE_STAGE_2_HOST_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03918# If
--     @dstAccessMask@ includes
--     'ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03919# If
--     @dstAccessMask@ includes
--     'ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03920# If
--     @dstAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT', @dstStageMask@ /must/
--     include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-04747# If
--     @dstAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR',
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03922# If
--     @dstAccessMask@ includes
--     'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03923# If
--     @dstAccessMask@ includes 'ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-04994# If
--     @dstAccessMask@ includes 'ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03924# If
--     @dstAccessMask@ includes 'ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03925# If
--     @dstAccessMask@ includes 'ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @dstStageMask@ /must/ include
--     'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03926# If
--     @dstAccessMask@ includes
--     'ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT', @dstStageMask@
--     /must/ include 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--     'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03927# If
--     @dstAccessMask@ includes
--     'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR', @dstStageMask@
--     /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR', or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-03928# If
--     @dstAccessMask@ includes
--     'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR', @dstStageMask@
--     /must/ include
--     'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' or
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-offset-01187# @offset@ /must/ be
--     less than the size of @buffer@
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-size-01188# If @size@ is not equal
--     to 'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be greater
--     than @0@
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-size-01189# If @size@ is not equal
--     to 'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be less
--     than or equal to than the size of @buffer@ minus @offset@
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-buffer-01931# If @buffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcQueueFamilyIndex-04087# If
--     @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, at
--     least one /must/ not be a special queue family reserved for external
--     memory ownership transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers ???>
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-buffer-04088# If @buffer@ was
--     created with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal, and
--     one of @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ is one of the
--     special queue family values reserved for external memory transfers,
--     the other /must/ be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-buffer-04089# If @buffer@ was
--     created with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal,
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ both be valid
--     queue families, or one of the special queue family values reserved
--     for external memory transfers, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers ???>
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-03851# If either
--     @srcStageMask@ or @dstStageMask@ includes
--     'PIPELINE_STAGE_2_HOST_BIT_KHR', @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ /must/ be equal
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcStageMask-parameter#
--     @srcStageMask@ /must/ be a valid combination of
--     'PipelineStageFlagBits2KHR' values
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-srcAccessMask-parameter#
--     @srcAccessMask@ /must/ be a valid combination of
--     'AccessFlagBits2KHR' values
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstStageMask-parameter#
--     @dstStageMask@ /must/ be a valid combination of
--     'PipelineStageFlagBits2KHR' values
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-dstAccessMask-parameter#
--     @dstAccessMask@ /must/ be a valid combination of
--     'AccessFlagBits2KHR' values
--
-- -   #VUID-VkBufferMemoryBarrier2KHR-buffer-parameter# @buffer@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- 'AccessFlags2KHR', 'Vulkan.Core10.Handles.Buffer', 'DependencyInfoKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'PipelineStageFlags2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferMemoryBarrier2KHR = BufferMemoryBarrier2KHR
  { -- | @srcStageMask@ is a 'PipelineStageFlags2KHR' mask of pipeline stages to
    -- be included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes first synchronization scope>.
    srcStageMask :: PipelineStageFlags2KHR
  , -- | @srcAccessMask@ is a 'AccessFlags2KHR' mask of access flags to be
    -- included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes first access scope>.
    srcAccessMask :: AccessFlags2KHR
  , -- | @dstStageMask@ is a 'PipelineStageFlags2KHR' mask of pipeline stages to
    -- be included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes second synchronization scope>.
    dstStageMask :: PipelineStageFlags2KHR
  , -- | @dstAccessMask@ is a 'AccessFlags2KHR' mask of access flags to be
    -- included in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes second access scope>.
    dstAccessMask :: AccessFlags2KHR
  , -- | @srcQueueFamilyIndex@ is the source queue family for a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    srcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    dstQueueFamilyIndex :: Word32
  , -- | @buffer@ is a handle to the buffer whose backing memory is affected by
    -- the barrier.
    buffer :: Buffer
  , -- | @offset@ is an offset in bytes into the backing memory for @buffer@;
    -- this is relative to the base offset as bound to the buffer (see
    -- 'Vulkan.Core10.MemoryManagement.bindBufferMemory').
    offset :: DeviceSize
  , -- | @size@ is a size in bytes of the affected area of backing memory for
    -- @buffer@, or 'Vulkan.Core10.APIConstants.WHOLE_SIZE' to use the range
    -- from @offset@ to the end of the buffer.
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferMemoryBarrier2KHR)
#endif
deriving instance Show BufferMemoryBarrier2KHR

instance ToCStruct BufferMemoryBarrier2KHR where
  withCStruct x f = allocaBytes 80 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferMemoryBarrier2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR)) (srcStageMask)
    poke ((p `plusPtr` 24 :: Ptr AccessFlags2KHR)) (srcAccessMask)
    poke ((p `plusPtr` 32 :: Ptr PipelineStageFlags2KHR)) (dstStageMask)
    poke ((p `plusPtr` 40 :: Ptr AccessFlags2KHR)) (dstAccessMask)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (srcQueueFamilyIndex)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (dstQueueFamilyIndex)
    poke ((p `plusPtr` 56 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferMemoryBarrier2KHR where
  peekCStruct p = do
    srcStageMask <- peek @PipelineStageFlags2KHR ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR))
    srcAccessMask <- peek @AccessFlags2KHR ((p `plusPtr` 24 :: Ptr AccessFlags2KHR))
    dstStageMask <- peek @PipelineStageFlags2KHR ((p `plusPtr` 32 :: Ptr PipelineStageFlags2KHR))
    dstAccessMask <- peek @AccessFlags2KHR ((p `plusPtr` 40 :: Ptr AccessFlags2KHR))
    srcQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    dstQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    buffer <- peek @Buffer ((p `plusPtr` 56 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 64 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    pure $ BufferMemoryBarrier2KHR
             srcStageMask srcAccessMask dstStageMask dstAccessMask srcQueueFamilyIndex dstQueueFamilyIndex buffer offset size

instance Storable BufferMemoryBarrier2KHR where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferMemoryBarrier2KHR where
  zero = BufferMemoryBarrier2KHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDependencyInfoKHR - Structure specifying dependency information for a
-- synchronization command
--
-- = Description
--
-- This structure defines a set of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependencies>,
-- as well as
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operations>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>.
--
-- Each member of @pMemoryBarriers@, @pBufferMemoryBarriers@, and
-- @pImageMemoryBarriers@ defines a separate
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependency>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDependencyInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEPENDENCY_INFO_KHR'
--
-- -   #VUID-VkDependencyInfoKHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkDependencyInfoKHR-dependencyFlags-parameter#
--     @dependencyFlags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits' values
--
-- -   #VUID-VkDependencyInfoKHR-pMemoryBarriers-parameter# If
--     @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a valid
--     pointer to an array of @memoryBarrierCount@ valid
--     'MemoryBarrier2KHR' structures
--
-- -   #VUID-VkDependencyInfoKHR-pBufferMemoryBarriers-parameter# If
--     @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid 'BufferMemoryBarrier2KHR' structures
--
-- -   #VUID-VkDependencyInfoKHR-pImageMemoryBarriers-parameter# If
--     @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@ /must/
--     be a valid pointer to an array of @imageMemoryBarrierCount@ valid
--     'ImageMemoryBarrier2KHR' structures
--
-- = See Also
--
-- 'BufferMemoryBarrier2KHR',
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags',
-- 'ImageMemoryBarrier2KHR', 'MemoryBarrier2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdPipelineBarrier2KHR', 'cmdSetEvent2KHR', 'cmdWaitEvents2KHR'
data DependencyInfoKHR = DependencyInfoKHR
  { -- | @dependencyFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits' specifying
    -- how execution and memory dependencies are formed.
    dependencyFlags :: DependencyFlags
  , -- | @pMemoryBarriers@ is a pointer to an array of 'MemoryBarrier2KHR'
    -- structures that define memory dependencies between any memory accesses.
    memoryBarriers :: Vector MemoryBarrier2KHR
  , -- | @pBufferMemoryBarriers@ is a pointer to an array of
    -- 'BufferMemoryBarrier2KHR' structures that define memory dependencies
    -- between buffer ranges.
    bufferMemoryBarriers :: Vector BufferMemoryBarrier2KHR
  , -- | @pImageMemoryBarriers@ is a pointer to an array of
    -- 'ImageMemoryBarrier2KHR' structures that define memory dependencies
    -- between image subresources.
    imageMemoryBarriers :: Vector (SomeStruct ImageMemoryBarrier2KHR)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DependencyInfoKHR)
#endif
deriving instance Show DependencyInfoKHR

instance ToCStruct DependencyInfoKHR where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DependencyInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEPENDENCY_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DependencyFlags)) (dependencyFlags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (memoryBarriers)) :: Word32))
    pPMemoryBarriers' <- ContT $ allocaBytes @MemoryBarrier2KHR ((Data.Vector.length (memoryBarriers)) * 48)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryBarriers' `plusPtr` (48 * (i)) :: Ptr MemoryBarrier2KHR) (e)) (memoryBarriers)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr MemoryBarrier2KHR))) (pPMemoryBarriers')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (bufferMemoryBarriers)) :: Word32))
    pPBufferMemoryBarriers' <- ContT $ allocaBytes @BufferMemoryBarrier2KHR ((Data.Vector.length (bufferMemoryBarriers)) * 80)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferMemoryBarriers' `plusPtr` (80 * (i)) :: Ptr BufferMemoryBarrier2KHR) (e)) (bufferMemoryBarriers)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr BufferMemoryBarrier2KHR))) (pPBufferMemoryBarriers')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (imageMemoryBarriers)) :: Word32))
    pPImageMemoryBarriers' <- ContT $ allocaBytes @(ImageMemoryBarrier2KHR _) ((Data.Vector.length (imageMemoryBarriers)) * 96)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPImageMemoryBarriers' `plusPtr` (96 * (i)) :: Ptr (ImageMemoryBarrier2KHR _))) (e) . ($ ())) (imageMemoryBarriers)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (ImageMemoryBarrier2KHR _)))) (pPImageMemoryBarriers')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEPENDENCY_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DependencyInfoKHR where
  peekCStruct p = do
    dependencyFlags <- peek @DependencyFlags ((p `plusPtr` 16 :: Ptr DependencyFlags))
    memoryBarrierCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pMemoryBarriers <- peek @(Ptr MemoryBarrier2KHR) ((p `plusPtr` 24 :: Ptr (Ptr MemoryBarrier2KHR)))
    pMemoryBarriers' <- generateM (fromIntegral memoryBarrierCount) (\i -> peekCStruct @MemoryBarrier2KHR ((pMemoryBarriers `advancePtrBytes` (48 * (i)) :: Ptr MemoryBarrier2KHR)))
    bufferMemoryBarrierCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pBufferMemoryBarriers <- peek @(Ptr BufferMemoryBarrier2KHR) ((p `plusPtr` 40 :: Ptr (Ptr BufferMemoryBarrier2KHR)))
    pBufferMemoryBarriers' <- generateM (fromIntegral bufferMemoryBarrierCount) (\i -> peekCStruct @BufferMemoryBarrier2KHR ((pBufferMemoryBarriers `advancePtrBytes` (80 * (i)) :: Ptr BufferMemoryBarrier2KHR)))
    imageMemoryBarrierCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pImageMemoryBarriers <- peek @(Ptr (ImageMemoryBarrier2KHR _)) ((p `plusPtr` 56 :: Ptr (Ptr (ImageMemoryBarrier2KHR _))))
    pImageMemoryBarriers' <- generateM (fromIntegral imageMemoryBarrierCount) (\i -> peekSomeCStruct (forgetExtensions ((pImageMemoryBarriers `advancePtrBytes` (96 * (i)) :: Ptr (ImageMemoryBarrier2KHR _)))))
    pure $ DependencyInfoKHR
             dependencyFlags pMemoryBarriers' pBufferMemoryBarriers' pImageMemoryBarriers'

instance Zero DependencyInfoKHR where
  zero = DependencyInfoKHR
           zero
           mempty
           mempty
           mempty


-- | VkSemaphoreSubmitInfoKHR - Structure specifying a semaphore signal or
-- wait operation
--
-- = Description
--
-- Whether this structure defines a semaphore wait or signal operation is
-- defined by how it is used.
--
-- == Valid Usage
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' or
--     'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-04956# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocation mask image>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-device-03888# If the @device@ that
--     @semaphore@ was created on is not a device group, @deviceIndex@
--     /must/ be @0@
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-device-03889# If the @device@ that
--     @semaphore@ was created on is a device group, @deviceIndex@ /must/
--     be a valid device index
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR'
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-semaphore-parameter# @semaphore@
--     /must/ be a valid 'Vulkan.Core10.Handles.Semaphore' handle
--
-- -   #VUID-VkSemaphoreSubmitInfoKHR-stageMask-parameter# @stageMask@
--     /must/ be a valid combination of 'PipelineStageFlagBits2KHR' values
--
-- = See Also
--
-- 'PipelineStageFlags2KHR', 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'SubmitInfo2KHR'
data SemaphoreSubmitInfoKHR = SemaphoreSubmitInfoKHR
  { -- | @semaphore@ is a 'Vulkan.Core10.Handles.Semaphore' affected by this
    -- operation.
    semaphore :: Semaphore
  , -- | @value@ is either the value used to signal @semaphore@ or the value
    -- waited on by @semaphore@, if @semaphore@ is a timeline semaphore.
    -- Otherwise it is ignored.
    value :: Word64
  , -- | @stageMask@ is a 'PipelineStageFlags2KHR' mask of pipeline stages which
    -- limit the first synchronization scope of a semaphore signal operation,
    -- or second synchronization scope of a semaphore wait operation as
    -- described in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operation>
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
    -- sections of
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization the synchronization chapter>.
    stageMask :: PipelineStageFlags2KHR
  , -- | @deviceIndex@ is the index of the device within a device group that
    -- executes the semaphore wait or signal operation.
    deviceIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreSubmitInfoKHR)
#endif
deriving instance Show SemaphoreSubmitInfoKHR

instance ToCStruct SemaphoreSubmitInfoKHR where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreSubmitInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (value)
    poke ((p `plusPtr` 32 :: Ptr PipelineStageFlags2KHR)) (stageMask)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (deviceIndex)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct SemaphoreSubmitInfoKHR where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    value <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    stageMask <- peek @PipelineStageFlags2KHR ((p `plusPtr` 32 :: Ptr PipelineStageFlags2KHR))
    deviceIndex <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pure $ SemaphoreSubmitInfoKHR
             semaphore value stageMask deviceIndex

instance Storable SemaphoreSubmitInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreSubmitInfoKHR where
  zero = SemaphoreSubmitInfoKHR
           zero
           zero
           zero
           zero


-- | VkCommandBufferSubmitInfoKHR - Structure specifying a command buffer
-- submission
--
-- == Valid Usage
--
-- -   #VUID-VkCommandBufferSubmitInfoKHR-commandBuffer-03890#
--     @commandBuffer@ /must/ not have been allocated with
--     'Vulkan.Core10.Enums.CommandBufferLevel.COMMAND_BUFFER_LEVEL_SECONDARY'
--
-- -   #VUID-VkCommandBufferSubmitInfoKHR-deviceMask-03891# If @deviceMask@
--     is not @0@, it /must/ be a valid device mask
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCommandBufferSubmitInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR'
--
-- -   #VUID-VkCommandBufferSubmitInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkCommandBufferSubmitInfoKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'SubmitInfo2KHR'
data CommandBufferSubmitInfoKHR = CommandBufferSubmitInfoKHR
  { -- | @commandBuffer@ is a 'Vulkan.Core10.Handles.CommandBuffer' to be
    -- submitted for execution.
    commandBuffer :: Ptr CommandBuffer_T
  , -- | @deviceMask@ is a bitmask indicating which devices in a device group
    -- execute the command buffer. A @deviceMask@ of @0@ is equivalent to
    -- setting all bits corresponding to valid devices in the group to @1@.
    deviceMask :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferSubmitInfoKHR)
#endif
deriving instance Show CommandBufferSubmitInfoKHR

instance ToCStruct CommandBufferSubmitInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferSubmitInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr CommandBuffer_T))) (commandBuffer)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (deviceMask)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr CommandBuffer_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct CommandBufferSubmitInfoKHR where
  peekCStruct p = do
    commandBuffer <- peek @(Ptr CommandBuffer_T) ((p `plusPtr` 16 :: Ptr (Ptr CommandBuffer_T)))
    deviceMask <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ CommandBufferSubmitInfoKHR
             commandBuffer deviceMask

instance Storable CommandBufferSubmitInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandBufferSubmitInfoKHR where
  zero = CommandBufferSubmitInfoKHR
           zero
           zero


-- | VkSubmitInfo2KHR - Structure specifying a queue submit operation
--
-- == Valid Usage
--
-- -   #VUID-VkSubmitInfo2KHR-semaphore-03881# If the same semaphore is
--     used as the @semaphore@ member of both an element of
--     @pSignalSemaphoreInfos@ and @pWaitSemaphoreInfos@, and that
--     semaphore is a timeline semaphore, the @value@ member of the
--     @pSignalSemaphoreInfos@ element /must/ be greater than the @value@
--     member of the @pWaitSemaphoreInfos@ element
--
-- -   #VUID-VkSubmitInfo2KHR-semaphore-03882# If the @semaphore@ member of
--     any element of @pSignalSemaphoreInfos@ is a timeline semaphore, the
--     @value@ member of that element /must/ have a value greater than the
--     current value of the semaphore when the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
--     is executed
--
-- -   #VUID-VkSubmitInfo2KHR-semaphore-03883# If the @semaphore@ member of
--     any element of @pSignalSemaphoreInfos@ is a timeline semaphore, the
--     @value@ member of that element /must/ have a value which does not
--     differ from the current value of the semaphore or the value of any
--     outstanding semaphore wait or signal operation on that semaphore by
--     more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- -   #VUID-VkSubmitInfo2KHR-semaphore-03884# If the @semaphore@ member of
--     any element of @pWaitSemaphoreInfos@ is a timeline semaphore, the
--     @value@ member of that element /must/ have a value which does not
--     differ from the current value of the semaphore or the value of any
--     outstanding semaphore wait or signal operation on that semaphore by
--     more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- -   #VUID-VkSubmitInfo2KHR-flags-03885# If the protected memory feature
--     is not enabled, @flags@ /must/ not include
--     'SUBMIT_PROTECTED_BIT_KHR'
--
-- -   #VUID-VkSubmitInfo2KHR-flags-03886# If @flags@ includes
--     'SUBMIT_PROTECTED_BIT_KHR', all elements of @pCommandBuffers@ /must/
--     be protected command buffers
--
-- -   #VUID-VkSubmitInfo2KHR-flags-03887# If @flags@ does not include
--     'SUBMIT_PROTECTED_BIT_KHR', each element of @pCommandBuffers@ /must/
--     not be a protected command buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubmitInfo2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBMIT_INFO_2_KHR'
--
-- -   #VUID-VkSubmitInfo2KHR-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_performance_query.PerformanceQuerySubmitInfoKHR',
--     'Vulkan.Extensions.VK_KHR_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoKHR',
--     or
--     'Vulkan.Extensions.VK_NV_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoNV'
--
-- -   #VUID-VkSubmitInfo2KHR-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkSubmitInfo2KHR-flags-parameter# @flags@ /must/ be a valid
--     combination of 'SubmitFlagBitsKHR' values
--
-- -   #VUID-VkSubmitInfo2KHR-pWaitSemaphoreInfos-parameter# If
--     @waitSemaphoreInfoCount@ is not @0@, @pWaitSemaphoreInfos@ /must/ be
--     a valid pointer to an array of @waitSemaphoreInfoCount@ valid
--     'SemaphoreSubmitInfoKHR' structures
--
-- -   #VUID-VkSubmitInfo2KHR-pCommandBufferInfos-parameter# If
--     @commandBufferInfoCount@ is not @0@, @pCommandBufferInfos@ /must/ be
--     a valid pointer to an array of @commandBufferInfoCount@ valid
--     'CommandBufferSubmitInfoKHR' structures
--
-- -   #VUID-VkSubmitInfo2KHR-pSignalSemaphoreInfos-parameter# If
--     @signalSemaphoreInfoCount@ is not @0@, @pSignalSemaphoreInfos@
--     /must/ be a valid pointer to an array of @signalSemaphoreInfoCount@
--     valid 'SemaphoreSubmitInfoKHR' structures
--
-- = See Also
--
-- 'CommandBufferSubmitInfoKHR', 'SemaphoreSubmitInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'SubmitFlagsKHR',
-- 'queueSubmit2KHR'
data SubmitInfo2KHR (es :: [Type]) = SubmitInfo2KHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of 'SubmitFlagBitsKHR'.
    flags :: SubmitFlagsKHR
  , -- | @pWaitSemaphoreInfos@ is a pointer to an array of
    -- 'SemaphoreSubmitInfoKHR' structures defining
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operations>.
    waitSemaphoreInfos :: Vector SemaphoreSubmitInfoKHR
  , -- | @pCommandBufferInfos@ is a pointer to an array of
    -- 'CommandBufferSubmitInfoKHR' structures describing command buffers to
    -- execute in the batch.
    commandBufferInfos :: Vector CommandBufferSubmitInfoKHR
  , -- | @pSignalSemaphoreInfos@ is a pointer to an array of
    -- 'SemaphoreSubmitInfoKHR' describing
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>.
    signalSemaphoreInfos :: Vector SemaphoreSubmitInfoKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubmitInfo2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubmitInfo2KHR es)

instance Extensible SubmitInfo2KHR where
  extensibleTypeName = "SubmitInfo2KHR"
  setNext x next = x{next = next}
  getNext SubmitInfo2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubmitInfo2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PerformanceQuerySubmitInfoKHR = Just f
    | Just Refl <- eqT @e @Win32KeyedMutexAcquireReleaseInfoKHR = Just f
    | Just Refl <- eqT @e @Win32KeyedMutexAcquireReleaseInfoNV = Just f
    | otherwise = Nothing

instance (Extendss SubmitInfo2KHR es, PokeChain es) => ToCStruct (SubmitInfo2KHR es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubmitInfo2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBMIT_INFO_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SubmitFlagsKHR)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (waitSemaphoreInfos)) :: Word32))
    pPWaitSemaphoreInfos' <- ContT $ allocaBytes @SemaphoreSubmitInfoKHR ((Data.Vector.length (waitSemaphoreInfos)) * 48)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphoreInfos' `plusPtr` (48 * (i)) :: Ptr SemaphoreSubmitInfoKHR) (e)) (waitSemaphoreInfos)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr SemaphoreSubmitInfoKHR))) (pPWaitSemaphoreInfos')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (commandBufferInfos)) :: Word32))
    pPCommandBufferInfos' <- ContT $ allocaBytes @CommandBufferSubmitInfoKHR ((Data.Vector.length (commandBufferInfos)) * 32)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBufferInfos' `plusPtr` (32 * (i)) :: Ptr CommandBufferSubmitInfoKHR) (e)) (commandBufferInfos)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr CommandBufferSubmitInfoKHR))) (pPCommandBufferInfos')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (signalSemaphoreInfos)) :: Word32))
    pPSignalSemaphoreInfos' <- ContT $ allocaBytes @SemaphoreSubmitInfoKHR ((Data.Vector.length (signalSemaphoreInfos)) * 48)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphoreInfos' `plusPtr` (48 * (i)) :: Ptr SemaphoreSubmitInfoKHR) (e)) (signalSemaphoreInfos)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SemaphoreSubmitInfoKHR))) (pPSignalSemaphoreInfos')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBMIT_INFO_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss SubmitInfo2KHR es, PeekChain es) => FromCStruct (SubmitInfo2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @SubmitFlagsKHR ((p `plusPtr` 16 :: Ptr SubmitFlagsKHR))
    waitSemaphoreInfoCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pWaitSemaphoreInfos <- peek @(Ptr SemaphoreSubmitInfoKHR) ((p `plusPtr` 24 :: Ptr (Ptr SemaphoreSubmitInfoKHR)))
    pWaitSemaphoreInfos' <- generateM (fromIntegral waitSemaphoreInfoCount) (\i -> peekCStruct @SemaphoreSubmitInfoKHR ((pWaitSemaphoreInfos `advancePtrBytes` (48 * (i)) :: Ptr SemaphoreSubmitInfoKHR)))
    commandBufferInfoCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pCommandBufferInfos <- peek @(Ptr CommandBufferSubmitInfoKHR) ((p `plusPtr` 40 :: Ptr (Ptr CommandBufferSubmitInfoKHR)))
    pCommandBufferInfos' <- generateM (fromIntegral commandBufferInfoCount) (\i -> peekCStruct @CommandBufferSubmitInfoKHR ((pCommandBufferInfos `advancePtrBytes` (32 * (i)) :: Ptr CommandBufferSubmitInfoKHR)))
    signalSemaphoreInfoCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pSignalSemaphoreInfos <- peek @(Ptr SemaphoreSubmitInfoKHR) ((p `plusPtr` 56 :: Ptr (Ptr SemaphoreSubmitInfoKHR)))
    pSignalSemaphoreInfos' <- generateM (fromIntegral signalSemaphoreInfoCount) (\i -> peekCStruct @SemaphoreSubmitInfoKHR ((pSignalSemaphoreInfos `advancePtrBytes` (48 * (i)) :: Ptr SemaphoreSubmitInfoKHR)))
    pure $ SubmitInfo2KHR
             next flags pWaitSemaphoreInfos' pCommandBufferInfos' pSignalSemaphoreInfos'

instance es ~ '[] => Zero (SubmitInfo2KHR es) where
  zero = SubmitInfo2KHR
           ()
           zero
           mempty
           mempty
           mempty


-- | VkQueueFamilyCheckpointProperties2NV - return structure for queue family
-- checkpoint information query
--
-- = Description
--
-- Additional queue family information can be queried by setting
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2'::@pNext@
-- to point to a 'QueueFamilyCheckpointProperties2NV' structure.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineStageFlags2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyCheckpointProperties2NV = QueueFamilyCheckpointProperties2NV
  { -- | @checkpointExecutionStageMask@ is a mask indicating which pipeline
    -- stages the implementation can execute checkpoint markers in.
    checkpointExecutionStageMask :: PipelineStageFlags2KHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyCheckpointProperties2NV)
#endif
deriving instance Show QueueFamilyCheckpointProperties2NV

instance ToCStruct QueueFamilyCheckpointProperties2NV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyCheckpointProperties2NV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR)) (checkpointExecutionStageMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR)) (zero)
    f

instance FromCStruct QueueFamilyCheckpointProperties2NV where
  peekCStruct p = do
    checkpointExecutionStageMask <- peek @PipelineStageFlags2KHR ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR))
    pure $ QueueFamilyCheckpointProperties2NV
             checkpointExecutionStageMask

instance Storable QueueFamilyCheckpointProperties2NV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero QueueFamilyCheckpointProperties2NV where
  zero = QueueFamilyCheckpointProperties2NV
           zero


-- | VkCheckpointData2NV - return structure for command buffer checkpoint
-- data
--
-- == Valid Usage (Implicit)
--
-- The stages at which a checkpoint marker /can/ be executed are
-- implementation-defined and /can/ be queried by calling
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'.
--
-- = See Also
--
-- 'PipelineStageFlags2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getQueueCheckpointData2NV'
data CheckpointData2NV = CheckpointData2NV
  { -- | @stage@ indicates a single pipeline stage which the checkpoint marker
    -- data refers to.
    stage :: PipelineStageFlags2KHR
  , -- | @pCheckpointMarker@ contains the value of the last checkpoint marker
    -- executed in the stage that @stage@ refers to.
    checkpointMarker :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CheckpointData2NV)
#endif
deriving instance Show CheckpointData2NV

instance ToCStruct CheckpointData2NV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CheckpointData2NV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR)) (stage)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (checkpointMarker)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct CheckpointData2NV where
  peekCStruct p = do
    stage <- peek @PipelineStageFlags2KHR ((p `plusPtr` 16 :: Ptr PipelineStageFlags2KHR))
    pCheckpointMarker <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ CheckpointData2NV
             stage pCheckpointMarker

instance Storable CheckpointData2NV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CheckpointData2NV where
  zero = CheckpointData2NV
           zero
           zero


-- | VkPhysicalDeviceSynchronization2FeaturesKHR - Structure describing
-- whether the implementation supports v2 synchronization commands
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceSynchronization2FeaturesKHR' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceSynchronization2FeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSynchronization2FeaturesKHR = PhysicalDeviceSynchronization2FeaturesKHR
  { -- | #features-synchronization2# @synchronization2@ indicates whether the
    -- implementation supports the new set of synchronization commands
    -- introduced in <VK_KHR_synchronization2.html VK_KHR_synchronization2>.
    synchronization2 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSynchronization2FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceSynchronization2FeaturesKHR

instance ToCStruct PhysicalDeviceSynchronization2FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSynchronization2FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (synchronization2))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSynchronization2FeaturesKHR where
  peekCStruct p = do
    synchronization2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSynchronization2FeaturesKHR
             (bool32ToBool synchronization2)

instance Storable PhysicalDeviceSynchronization2FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSynchronization2FeaturesKHR where
  zero = PhysicalDeviceSynchronization2FeaturesKHR
           zero


type AccessFlags2KHR = AccessFlagBits2KHR

-- | VkAccessFlagBits2KHR - Access flags for VkAccessFlags2KHR
--
-- = Description
--
-- Note
--
-- In situations where an application wishes to select all access types for
-- a given set of pipeline stages, 'ACCESS_2_MEMORY_READ_BIT_KHR' or
-- 'ACCESS_2_MEMORY_WRITE_BIT_KHR' can be used. This is particularly useful
-- when specifying stages that only have a single access type.
--
-- Note
--
-- The 'AccessFlags2KHR' bitmask goes beyond the 31 individual bit flags
-- allowable within a C99 enum, which is how
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' is defined. The
-- first 31 values are common to both, and are interchangeable.
--
-- = See Also
--
-- No cross-references are available
newtype AccessFlagBits2KHR = AccessFlagBits2KHR Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'ACCESS_2_NONE_KHR' specifies no accesses.
pattern ACCESS_2_NONE_KHR                               = AccessFlagBits2KHR 0x0000000000000000
-- | 'ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR' specifies read access to
-- command data read from indirect buffers as part of an indirect build,
-- trace, drawing or dispatch command. Such access occurs in the
-- 'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR' pipeline stage.
pattern ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR          = AccessFlagBits2KHR 0x0000000000000001
-- | 'ACCESS_2_INDEX_READ_BIT_KHR' specifies read access to an index buffer
-- as part of an indexed drawing command, bound by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'. Such access
-- occurs in the 'PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR' pipeline stage.
pattern ACCESS_2_INDEX_READ_BIT_KHR                     = AccessFlagBits2KHR 0x0000000000000002
-- | 'ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR' specifies read access to a
-- vertex buffer as part of a drawing command, bound by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'. Such access
-- occurs in the 'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR' pipeline
-- stage.
pattern ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR          = AccessFlagBits2KHR 0x0000000000000004
-- | 'ACCESS_2_UNIFORM_READ_BIT_KHR' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer>
-- in any shader pipeline stage.
pattern ACCESS_2_UNIFORM_READ_BIT_KHR                   = AccessFlagBits2KHR 0x0000000000000008
-- | 'ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR' specifies read access to an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass input attachment>
-- within a render pass during fragment shading. Such access occurs in the
-- 'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR' pipeline stage.
pattern ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR          = AccessFlagBits2KHR 0x0000000000000010
-- | 'ACCESS_2_SHADER_READ_BIT_KHR' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shader-binding-table shader binding table>
-- in any shader pipeline. In addition, it is equivalent to the logical OR
-- of:
--
-- -   VK_ACCESS_2_UNIFORM_READ_BIT_KHR
--
-- -   VK_ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR
--
-- -   VK_ACCESS_2_SHADER_STORAGE_READ_BIT_KHR
pattern ACCESS_2_SHADER_READ_BIT_KHR                    = AccessFlagBits2KHR 0x0000000000000020
-- | 'ACCESS_2_SHADER_WRITE_BIT_KHR' is equivalent to
-- 'ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR'.
pattern ACCESS_2_SHADER_WRITE_BIT_KHR                   = AccessFlagBits2KHR 0x0000000000000040
-- | 'ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass color attachment>,
-- such as via
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blending blending>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-logicop logic operations>,
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
-- It does not include
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>.
-- Such access occurs in the
-- 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR' pipeline stage.
pattern ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR          = AccessFlagBits2KHR 0x0000000000000080
-- | 'ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR' specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass color, resolve, or depth\/stencil resolve attachment>
-- during a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass render pass>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
-- Such access occurs in the
-- 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR' pipeline stage.
pattern ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR         = AccessFlagBits2KHR 0x0000000000000100
-- | 'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR' specifies read access
-- to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
-- Such access occurs in the
-- 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR' or
-- 'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR' pipeline stages.
pattern ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR  = AccessFlagBits2KHR 0x0000000000000200
-- | 'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR' specifies write access
-- to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
-- Such access occurs in the
-- 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR' or
-- 'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR' pipeline stages.
pattern ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR = AccessFlagBits2KHR 0x0000000000000400
-- | 'ACCESS_2_TRANSFER_READ_BIT_KHR' specifies read access to an image or
-- buffer in a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies copy>
-- operation. Such access occurs in the 'PIPELINE_STAGE_2_COPY_BIT_KHR',
-- 'PIPELINE_STAGE_2_BLIT_BIT_KHR', or 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR'
-- pipeline stages.
pattern ACCESS_2_TRANSFER_READ_BIT_KHR                  = AccessFlagBits2KHR 0x0000000000000800
-- | 'ACCESS_2_TRANSFER_WRITE_BIT_KHR' specifies write access to an image or
-- buffer in a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears clear>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies copy>
-- operation. Such access occurs in the 'PIPELINE_STAGE_2_COPY_BIT_KHR',
-- 'PIPELINE_STAGE_2_BLIT_BIT_KHR', 'PIPELINE_STAGE_2_CLEAR_BIT_KHR', or
-- 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR' pipeline stages.
pattern ACCESS_2_TRANSFER_WRITE_BIT_KHR                 = AccessFlagBits2KHR 0x0000000000001000
-- | 'ACCESS_2_HOST_READ_BIT_KHR' specifies read access by a host operation.
-- Accesses of this type are not performed through a resource, but directly
-- on memory. Such access occurs in the 'PIPELINE_STAGE_2_HOST_BIT_KHR'
-- pipeline stage.
pattern ACCESS_2_HOST_READ_BIT_KHR                      = AccessFlagBits2KHR 0x0000000000002000
-- | 'ACCESS_2_HOST_WRITE_BIT_KHR' specifies write access by a host
-- operation. Accesses of this type are not performed through a resource,
-- but directly on memory. Such access occurs in the
-- 'PIPELINE_STAGE_2_HOST_BIT_KHR' pipeline stage.
pattern ACCESS_2_HOST_WRITE_BIT_KHR                     = AccessFlagBits2KHR 0x0000000000004000
-- | 'ACCESS_2_MEMORY_READ_BIT_KHR' specifies all read accesses. It is always
-- valid in any access mask, and is treated as equivalent to setting all
-- @READ@ access flags that are valid where it is used.
pattern ACCESS_2_MEMORY_READ_BIT_KHR                    = AccessFlagBits2KHR 0x0000000000008000
-- | 'ACCESS_2_MEMORY_WRITE_BIT_KHR' specifies all write accesses. It is
-- always valid in any access mask, and is treated as equivalent to setting
-- all @WRITE@ access flags that are valid where it is used.
pattern ACCESS_2_MEMORY_WRITE_BIT_KHR                   = AccessFlagBits2KHR 0x0000000000010000
-- | 'ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image>
-- in any shader pipeline stage.
pattern ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR            = AccessFlagBits2KHR 0x0000000100000000
-- | 'ACCESS_2_SHADER_STORAGE_READ_BIT_KHR' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storageimage storage image>
-- in any shader pipeline stage.
pattern ACCESS_2_SHADER_STORAGE_READ_BIT_KHR            = AccessFlagBits2KHR 0x0000000200000000
-- | 'ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR' specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storageimage storage image>
-- in any shader pipeline stage.
pattern ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR           = AccessFlagBits2KHR 0x0000000400000000
-- | 'ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI' specifies read access to a
-- invocation mask image in the
-- 'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI' pipeline stage.
pattern ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI        = AccessFlagBits2KHR 0x0000008000000000
-- | 'ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT' specifies read
-- access to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass color attachments>,
-- including
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>.
-- Such access occurs in the
-- 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR' pipeline stage.
pattern ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT = AccessFlagBits2KHR 0x0000000000080000
-- | 'ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>
-- during dynamic
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragmentdensitymapops fragment density map operations>.
-- Such access occurs in the
-- 'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT' pipeline stage.
pattern ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT      = AccessFlagBits2KHR 0x0000000001000000
-- | 'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR' specifies write access
-- to an acceleration structure or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-scratch acceleration structure scratch buffer>
-- as part of a build or copy command. Such access occurs in the
-- 'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' pipeline stage.
pattern ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR   = AccessFlagBits2KHR 0x0000000000400000
-- | 'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR' specifies read access to
-- an acceleration structure as part of a trace, build, or copy command, or
-- to an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-scratch acceleration structure scratch buffer>
-- as part of a build command. Such access occurs in the
-- 'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR' pipeline stage or
-- 'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' pipeline stage.
pattern ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR    = AccessFlagBits2KHR 0x0000000000200000
-- | 'ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR' specifies read
-- access to a fragment shading rate attachment during rasterization. Such
-- access occurs in the
-- 'PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR' pipeline
-- stage.
pattern ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR = AccessFlagBits2KHR 0x0000000000800000
-- | 'ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV' specifies writes to the
-- target command buffer preprocess outputs. Such access occurs in the
-- 'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' pipeline stage.
pattern ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV        = AccessFlagBits2KHR 0x0000000000040000
-- | 'ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV' specifies reads from buffer
-- inputs to
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'.
-- Such access occurs in the 'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
-- pipeline stage.
pattern ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV         = AccessFlagBits2KHR 0x0000000000020000
-- | 'ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT' specifies read access to a
-- predicate as part of conditional rendering. Such access occurs in the
-- 'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT' pipeline stage.
pattern ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT     = AccessFlagBits2KHR 0x0000000000100000
-- | 'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT' specifies write
-- access to a transform feedback counter buffer which is written when
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT'
-- executes. Such access occurs in the
-- 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT' pipeline stage.
pattern ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT = AccessFlagBits2KHR 0x0000000008000000
-- | 'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT' specifies read access
-- to a transform feedback counter buffer which is read when
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT'
-- executes. Such access occurs in the
-- 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT' pipeline stage.
pattern ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT = AccessFlagBits2KHR 0x0000000004000000
-- | 'ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT' specifies write access to a
-- transform feedback buffer made when transform feedback is active. Such
-- access occurs in the 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
-- pipeline stage.
pattern ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT       = AccessFlagBits2KHR 0x0000000002000000

conNameAccessFlagBits2KHR :: String
conNameAccessFlagBits2KHR = "AccessFlagBits2KHR"

enumPrefixAccessFlagBits2KHR :: String
enumPrefixAccessFlagBits2KHR = "ACCESS_2_"

showTableAccessFlagBits2KHR :: [(AccessFlagBits2KHR, String)]
showTableAccessFlagBits2KHR =
  [ (ACCESS_2_NONE_KHR                              , "NONE_KHR")
  , (ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR         , "INDIRECT_COMMAND_READ_BIT_KHR")
  , (ACCESS_2_INDEX_READ_BIT_KHR                    , "INDEX_READ_BIT_KHR")
  , (ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR         , "VERTEX_ATTRIBUTE_READ_BIT_KHR")
  , (ACCESS_2_UNIFORM_READ_BIT_KHR                  , "UNIFORM_READ_BIT_KHR")
  , (ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR         , "INPUT_ATTACHMENT_READ_BIT_KHR")
  , (ACCESS_2_SHADER_READ_BIT_KHR                   , "SHADER_READ_BIT_KHR")
  , (ACCESS_2_SHADER_WRITE_BIT_KHR                  , "SHADER_WRITE_BIT_KHR")
  , (ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR         , "COLOR_ATTACHMENT_READ_BIT_KHR")
  , (ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR        , "COLOR_ATTACHMENT_WRITE_BIT_KHR")
  , (ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR , "DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR")
  , (ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR, "DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR")
  , (ACCESS_2_TRANSFER_READ_BIT_KHR                 , "TRANSFER_READ_BIT_KHR")
  , (ACCESS_2_TRANSFER_WRITE_BIT_KHR                , "TRANSFER_WRITE_BIT_KHR")
  , (ACCESS_2_HOST_READ_BIT_KHR                     , "HOST_READ_BIT_KHR")
  , (ACCESS_2_HOST_WRITE_BIT_KHR                    , "HOST_WRITE_BIT_KHR")
  , (ACCESS_2_MEMORY_READ_BIT_KHR                   , "MEMORY_READ_BIT_KHR")
  , (ACCESS_2_MEMORY_WRITE_BIT_KHR                  , "MEMORY_WRITE_BIT_KHR")
  , (ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR           , "SHADER_SAMPLED_READ_BIT_KHR")
  , (ACCESS_2_SHADER_STORAGE_READ_BIT_KHR           , "SHADER_STORAGE_READ_BIT_KHR")
  , (ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR          , "SHADER_STORAGE_WRITE_BIT_KHR")
  , (ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI       , "INVOCATION_MASK_READ_BIT_HUAWEI")
  , (ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT, "COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT")
  , (ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT     , "FRAGMENT_DENSITY_MAP_READ_BIT_EXT")
  , (ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR  , "ACCELERATION_STRUCTURE_WRITE_BIT_KHR")
  , (ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR   , "ACCELERATION_STRUCTURE_READ_BIT_KHR")
  , (ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR, "FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR")
  , (ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV       , "COMMAND_PREPROCESS_WRITE_BIT_NV")
  , (ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV        , "COMMAND_PREPROCESS_READ_BIT_NV")
  , (ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT    , "CONDITIONAL_RENDERING_READ_BIT_EXT")
  , (ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT, "TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT")
  , (ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT, "TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT")
  , (ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT      , "TRANSFORM_FEEDBACK_WRITE_BIT_EXT")
  ]

instance Show AccessFlagBits2KHR where
  showsPrec = enumShowsPrec enumPrefixAccessFlagBits2KHR
                            showTableAccessFlagBits2KHR
                            conNameAccessFlagBits2KHR
                            (\(AccessFlagBits2KHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read AccessFlagBits2KHR where
  readPrec =
    enumReadPrec enumPrefixAccessFlagBits2KHR showTableAccessFlagBits2KHR conNameAccessFlagBits2KHR AccessFlagBits2KHR


type PipelineStageFlags2KHR = PipelineStageFlagBits2KHR

-- | VkPipelineStageFlagBits2KHR - Pipeline stage flags for
-- VkPipelineStageFlags2KHR
--
-- = Description
--
-- Note
--
-- The @TOP@ and @BOTTOM@ pipeline stages are deprecated, and applications
-- should prefer 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR' and
-- 'PIPELINE_STAGE_2_NONE_KHR'.
--
-- Note
--
-- The 'PipelineStageFlags2KHR' bitmask goes beyond the 31 individual bit
-- flags allowable within a C99 enum, which is how
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits' is
-- defined. The first 31 values are common to both, and are
-- interchangeable.
--
-- = See Also
--
-- No cross-references are available
newtype PipelineStageFlagBits2KHR = PipelineStageFlagBits2KHR Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PIPELINE_STAGE_2_NONE_KHR' specifies no stages of execution.
pattern PIPELINE_STAGE_2_NONE_KHR                               = PipelineStageFlagBits2KHR 0x0000000000000000
-- | 'PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR' is equivalent to
-- 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR' with 'AccessFlags2KHR' set to
-- @0@ when specified in the second synchronization scope, but equivalent
-- to 'PIPELINE_STAGE_2_NONE_KHR' in the first scope.
pattern PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR                    = PipelineStageFlagBits2KHR 0x0000000000000001
-- | 'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR' specifies the stage of the
-- pipeline where indirect command parameters are consumed. This stage also
-- includes reading commands written by
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'.
pattern PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR                  = PipelineStageFlagBits2KHR 0x0000000000000002
-- | 'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR' is equivalent to the logical OR
-- of:
--
-- -   'PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR'
pattern PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR                   = PipelineStageFlagBits2KHR 0x0000000000000004
-- | 'PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR' specifies the vertex shader
-- stage.
pattern PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR                  = PipelineStageFlagBits2KHR 0x0000000000000008
-- | 'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR' specifies the
-- tessellation control shader stage.
pattern PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR    = PipelineStageFlagBits2KHR 0x0000000000000010
-- | 'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR' specifies the
-- tessellation evaluation shader stage.
pattern PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR = PipelineStageFlagBits2KHR 0x0000000000000020
-- | 'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR' specifies the geometry shader
-- stage.
pattern PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR                = PipelineStageFlagBits2KHR 0x0000000000000040
-- | 'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR' specifies the fragment shader
-- stage.
pattern PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR                = PipelineStageFlagBits2KHR 0x0000000000000080
-- | 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR' specifies the stage of
-- the pipeline where early fragment tests (depth and stencil tests before
-- fragment shading) are performed. This stage also includes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>
-- for framebuffer attachments with a depth\/stencil format.
pattern PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR           = PipelineStageFlagBits2KHR 0x0000000000000100
-- | 'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR' specifies the stage of
-- the pipeline where late fragment tests (depth and stencil tests after
-- fragment shading) are performed. This stage also includes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass store operations>
-- for framebuffer attachments with a depth\/stencil format.
pattern PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR            = PipelineStageFlagBits2KHR 0x0000000000000200
-- | 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR' specifies the stage
-- of the pipeline after blending where the final color values are output
-- from the pipeline. This stage also includes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>
-- and multisample resolve operations for framebuffer attachments with a
-- color or depth\/stencil format.
pattern PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR        = PipelineStageFlagBits2KHR 0x0000000000000400
-- | 'PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR' specifies the compute shader
-- stage.
pattern PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR                 = PipelineStageFlagBits2KHR 0x0000000000000800
-- | 'PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR' is equivalent to specifying all
-- of:
--
-- -   'PIPELINE_STAGE_2_COPY_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_BLIT_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_RESOLVE_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_CLEAR_BIT_KHR'
pattern PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR                   = PipelineStageFlagBits2KHR 0x0000000000001000
-- | 'PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR' is equivalent to
-- 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR' with 'AccessFlags2KHR' set to
-- @0@ when specified in the first synchronization scope, but equivalent to
-- 'PIPELINE_STAGE_2_NONE_KHR' in the second scope.
pattern PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR                 = PipelineStageFlagBits2KHR 0x0000000000002000
-- | 'PIPELINE_STAGE_2_HOST_BIT_KHR' specifies a pseudo-stage indicating
-- execution on the host of reads\/writes of device memory. This stage is
-- not invoked by any commands recorded in a command buffer.
pattern PIPELINE_STAGE_2_HOST_BIT_KHR                           = PipelineStageFlagBits2KHR 0x0000000000004000
-- | 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR' specifies the execution of all
-- graphics pipeline stages, and is equivalent to the logical OR of:
--
-- -   'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
-- -   'PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
pattern PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR                   = PipelineStageFlagBits2KHR 0x0000000000008000
-- | 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR' specifies all operations
-- performed by all commands supported on the queue it is used with.
pattern PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR                   = PipelineStageFlagBits2KHR 0x0000000000010000
-- | 'PIPELINE_STAGE_2_COPY_BIT_KHR' specifies the execution of all
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies copy commands>,
-- including 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'.
pattern PIPELINE_STAGE_2_COPY_BIT_KHR                           = PipelineStageFlagBits2KHR 0x0000000100000000
-- | 'PIPELINE_STAGE_2_RESOLVE_BIT_KHR' specifies the execution of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage'.
pattern PIPELINE_STAGE_2_RESOLVE_BIT_KHR                        = PipelineStageFlagBits2KHR 0x0000000200000000
-- | 'PIPELINE_STAGE_2_BLIT_BIT_KHR' specifies the execution of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'.
pattern PIPELINE_STAGE_2_BLIT_BIT_KHR                           = PipelineStageFlagBits2KHR 0x0000000400000000
-- | 'PIPELINE_STAGE_2_CLEAR_BIT_KHR' specifies the execution of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears clear commands>,
-- with the exception of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments'.
pattern PIPELINE_STAGE_2_CLEAR_BIT_KHR                          = PipelineStageFlagBits2KHR 0x0000000800000000
-- | 'PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR' specifies the stage of the
-- pipeline where index buffers are consumed.
pattern PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR                    = PipelineStageFlagBits2KHR 0x0000001000000000
-- | 'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR' specifies the stage of
-- the pipeline where vertex buffers are consumed.
pattern PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR         = PipelineStageFlagBits2KHR 0x0000002000000000
-- | 'PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR' is equivalent to
-- specifying all supported
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipeline-graphics-subsets-pre-rasterization pre-rasterization shader stages>:
--
-- -   'PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- -   'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
pattern PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR      = PipelineStageFlagBits2KHR 0x0000004000000000
-- | 'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI' specifies the stage of the
-- pipeline where the invocation mask image is read by the implementation
-- to optimize the ray dispatch.
pattern PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI             = PipelineStageFlagBits2KHR 0x0000010000000000
-- | 'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV' specifies the mesh shader stage.
pattern PIPELINE_STAGE_2_MESH_SHADER_BIT_NV                     = PipelineStageFlagBits2KHR 0x0000000000100000
-- | 'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV' specifies the task shader stage.
pattern PIPELINE_STAGE_2_TASK_SHADER_BIT_NV                     = PipelineStageFlagBits2KHR 0x0000000000080000
-- | 'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT' specifies the stage
-- of the pipeline where the fragment density map is read to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragmentdensitymapops generate the fragment areas>.
pattern PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT       = PipelineStageFlagBits2KHR 0x0000000000800000
-- | 'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR' specifies the execution of
-- the ray tracing shader stages.
pattern PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR             = PipelineStageFlagBits2KHR 0x0000000000200000
-- | 'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' specifies the
-- execution of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure acceleration structure commands>.
pattern PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR   = PipelineStageFlagBits2KHR 0x0000000002000000
-- | 'PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR' specifies
-- the stage of the pipeline where the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-shading-rate-attachment shading rate image>
-- is read to determine the fragment shading rate for portions of a
-- rasterized primitive.
pattern PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PipelineStageFlagBits2KHR 0x0000000000400000
-- | 'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' specifies the stage of the
-- pipeline where device-side generation of commands via
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'
-- is handled.
pattern PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV              = PipelineStageFlagBits2KHR 0x0000000000020000
-- | 'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT' specifies the stage of
-- the pipeline where the predicate of conditional rendering is consumed.
pattern PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT          = PipelineStageFlagBits2KHR 0x0000000000040000
-- | 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT' specifies the stage of the
-- pipeline where vertex attribute output values are written to the
-- transform feedback buffers.
pattern PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT             = PipelineStageFlagBits2KHR 0x0000000001000000

conNamePipelineStageFlagBits2KHR :: String
conNamePipelineStageFlagBits2KHR = "PipelineStageFlagBits2KHR"

enumPrefixPipelineStageFlagBits2KHR :: String
enumPrefixPipelineStageFlagBits2KHR = "PIPELINE_STAGE_2_"

showTablePipelineStageFlagBits2KHR :: [(PipelineStageFlagBits2KHR, String)]
showTablePipelineStageFlagBits2KHR =
  [ (PIPELINE_STAGE_2_NONE_KHR                              , "NONE_KHR")
  , (PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR                   , "TOP_OF_PIPE_BIT_KHR")
  , (PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR                 , "DRAW_INDIRECT_BIT_KHR")
  , (PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR                  , "VERTEX_INPUT_BIT_KHR")
  , (PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR                 , "VERTEX_SHADER_BIT_KHR")
  , (PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR   , "TESSELLATION_CONTROL_SHADER_BIT_KHR")
  , (PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR, "TESSELLATION_EVALUATION_SHADER_BIT_KHR")
  , (PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR               , "GEOMETRY_SHADER_BIT_KHR")
  , (PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR               , "FRAGMENT_SHADER_BIT_KHR")
  , (PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR          , "EARLY_FRAGMENT_TESTS_BIT_KHR")
  , (PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR           , "LATE_FRAGMENT_TESTS_BIT_KHR")
  , (PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR       , "COLOR_ATTACHMENT_OUTPUT_BIT_KHR")
  , (PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR                , "COMPUTE_SHADER_BIT_KHR")
  , (PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR                  , "ALL_TRANSFER_BIT_KHR")
  , (PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR                , "BOTTOM_OF_PIPE_BIT_KHR")
  , (PIPELINE_STAGE_2_HOST_BIT_KHR                          , "HOST_BIT_KHR")
  , (PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR                  , "ALL_GRAPHICS_BIT_KHR")
  , (PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR                  , "ALL_COMMANDS_BIT_KHR")
  , (PIPELINE_STAGE_2_COPY_BIT_KHR                          , "COPY_BIT_KHR")
  , (PIPELINE_STAGE_2_RESOLVE_BIT_KHR                       , "RESOLVE_BIT_KHR")
  , (PIPELINE_STAGE_2_BLIT_BIT_KHR                          , "BLIT_BIT_KHR")
  , (PIPELINE_STAGE_2_CLEAR_BIT_KHR                         , "CLEAR_BIT_KHR")
  , (PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR                   , "INDEX_INPUT_BIT_KHR")
  , (PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR        , "VERTEX_ATTRIBUTE_INPUT_BIT_KHR")
  , (PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR     , "PRE_RASTERIZATION_SHADERS_BIT_KHR")
  , (PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI            , "INVOCATION_MASK_BIT_HUAWEI")
  , (PIPELINE_STAGE_2_MESH_SHADER_BIT_NV                    , "MESH_SHADER_BIT_NV")
  , (PIPELINE_STAGE_2_TASK_SHADER_BIT_NV                    , "TASK_SHADER_BIT_NV")
  , (PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT      , "FRAGMENT_DENSITY_PROCESS_BIT_EXT")
  , (PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR            , "RAY_TRACING_SHADER_BIT_KHR")
  , (PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR  , "ACCELERATION_STRUCTURE_BUILD_BIT_KHR")
  , (PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR, "FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR")
  , (PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV             , "COMMAND_PREPROCESS_BIT_NV")
  , (PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT         , "CONDITIONAL_RENDERING_BIT_EXT")
  , (PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT            , "TRANSFORM_FEEDBACK_BIT_EXT")
  ]

instance Show PipelineStageFlagBits2KHR where
  showsPrec = enumShowsPrec enumPrefixPipelineStageFlagBits2KHR
                            showTablePipelineStageFlagBits2KHR
                            conNamePipelineStageFlagBits2KHR
                            (\(PipelineStageFlagBits2KHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineStageFlagBits2KHR where
  readPrec = enumReadPrec enumPrefixPipelineStageFlagBits2KHR
                          showTablePipelineStageFlagBits2KHR
                          conNamePipelineStageFlagBits2KHR
                          PipelineStageFlagBits2KHR


type SubmitFlagsKHR = SubmitFlagBitsKHR

-- | VkSubmitFlagBitsKHR - Bitmask specifying behavior of a submission
--
-- = See Also
--
-- 'SubmitFlagsKHR'
newtype SubmitFlagBitsKHR = SubmitFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SUBMIT_PROTECTED_BIT_KHR' specifies that this batch is a protected
-- submission.
pattern SUBMIT_PROTECTED_BIT_KHR = SubmitFlagBitsKHR 0x00000001

conNameSubmitFlagBitsKHR :: String
conNameSubmitFlagBitsKHR = "SubmitFlagBitsKHR"

enumPrefixSubmitFlagBitsKHR :: String
enumPrefixSubmitFlagBitsKHR = "SUBMIT_PROTECTED_BIT_KHR"

showTableSubmitFlagBitsKHR :: [(SubmitFlagBitsKHR, String)]
showTableSubmitFlagBitsKHR = [(SUBMIT_PROTECTED_BIT_KHR, "")]

instance Show SubmitFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixSubmitFlagBitsKHR
                            showTableSubmitFlagBitsKHR
                            conNameSubmitFlagBitsKHR
                            (\(SubmitFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SubmitFlagBitsKHR where
  readPrec =
    enumReadPrec enumPrefixSubmitFlagBitsKHR showTableSubmitFlagBitsKHR conNameSubmitFlagBitsKHR SubmitFlagBitsKHR


type KHR_SYNCHRONIZATION_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SYNCHRONIZATION_2_SPEC_VERSION"
pattern KHR_SYNCHRONIZATION_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SYNCHRONIZATION_2_SPEC_VERSION = 1


type KHR_SYNCHRONIZATION_2_EXTENSION_NAME = "VK_KHR_synchronization2"

-- No documentation found for TopLevel "VK_KHR_SYNCHRONIZATION_2_EXTENSION_NAME"
pattern KHR_SYNCHRONIZATION_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SYNCHRONIZATION_2_EXTENSION_NAME = "VK_KHR_synchronization2"

