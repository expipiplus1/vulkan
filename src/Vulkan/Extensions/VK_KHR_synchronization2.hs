{-# language CPP #-}
-- | = Name
--
-- VK_KHR_synchronization2 - device extension
--
-- = VK_KHR_synchronization2
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
--     -   Interacts with VK_EXT_blend_operation_advanced
--
--     -   Interacts with VK_EXT_conditional_rendering
--
--     -   Interacts with VK_EXT_device_generated_commands
--
--     -   Interacts with VK_EXT_fragment_density_map
--
--     -   Interacts with VK_EXT_mesh_shader
--
--     -   Interacts with VK_EXT_transform_feedback
--
--     -   Interacts with VK_KHR_acceleration_structure
--
--     -   Interacts with VK_KHR_fragment_shading_rate
--
--     -   Interacts with VK_KHR_ray_tracing_pipeline
--
--     -   Interacts with VK_NV_device_generated_commands
--
--     -   Interacts with VK_NV_mesh_shader
--
--     -   Interacts with VK_NV_ray_tracing
--
--     -   Interacts with VK_NV_shading_rate_image
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_synchronization2] @tobski%0A*Here describe the issue or question you have about the VK_KHR_synchronization2 extension* >
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
-- are generic 'IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR' and
-- 'IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR' layouts which are contextually
-- applied based on the image format. For example, for a depth format
-- image, 'IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR' is equivalent to
-- 'Vulkan.Extensions.VK_KHR_separate_depth_stencil_layouts.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR'.
-- 'IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR' also functionally replaces
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
--     new 'PIPELINE_STAGE_NONE_KHR' and
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_NONE_KHR'
--     values.
--
-- -   'MemoryBarrier2KHR' can be chained to
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDependency2',
--     overriding the original 32-bit stage and access masks.
--
-- == New Base Types
--
-- -   'Vulkan.Core10.FundamentalTypes.Flags64'
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
--     -   'ACCESS_NONE_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.EventCreateFlagBits.EventCreateFlagBits':
--
--     -   'EVENT_CREATE_DEVICE_ONLY_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--
--     -   'IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'PIPELINE_STAGE_NONE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR'
--
--     -   'STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_DEPENDENCY_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR'
--
--     -   'STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_SUBMIT_INFO_2_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_blend_operation_advanced VK_EXT_blend_operation_advanced>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_generated_commands VK_EXT_device_generated_commands>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_READ_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_mesh_shader VK_EXT_mesh_shader>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_mesh_shader VK_NV_mesh_shader>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'PIPELINE_STAGE_2_MESH_SHADER_BIT_NV'
--
--     -   'PIPELINE_STAGE_2_TASK_SHADER_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV'
--
--     -   'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV'
--
--     -   'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shading_rate_image VK_NV_shading_rate_image>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- == Promotion to Vulkan 1.3
--
-- Vulkan APIs in this extension are included in core Vulkan 1.3, with the
-- KHR suffix omitted. External interactions defined by this extension,
-- such as SPIR-V token names, retain their original names. The original
-- Vulkan API names are still available as aliases of the core
-- functionality.
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
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_synchronization2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_synchronization2  ( pattern STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR
                                                  , pattern STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR
                                                  , pattern STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR
                                                  , pattern STRUCTURE_TYPE_DEPENDENCY_INFO_KHR
                                                  , pattern STRUCTURE_TYPE_SUBMIT_INFO_2_KHR
                                                  , pattern STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR
                                                  , pattern STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR
                                                  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR
                                                  , pattern EVENT_CREATE_DEVICE_ONLY_BIT_KHR
                                                  , pattern IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR
                                                  , pattern IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR
                                                  , pattern PIPELINE_STAGE_NONE_KHR
                                                  , pattern ACCESS_NONE_KHR
                                                  , pattern PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV
                                                  , pattern ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV
                                                  , pattern PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_NV
                                                  , pattern PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_NV
                                                  , pattern ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_NV
                                                  , pattern ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_NV
                                                  , pattern PIPELINE_STAGE_2_TASK_SHADER_BIT_NV
                                                  , pattern PIPELINE_STAGE_2_MESH_SHADER_BIT_NV
                                                  , cmdSetEvent2KHR
                                                  , cmdResetEvent2KHR
                                                  , cmdWaitEvents2KHR
                                                  , cmdPipelineBarrier2KHR
                                                  , queueSubmit2KHR
                                                  , cmdWriteTimestamp2KHR
                                                  , AccessFlags2KHR
                                                  , PipelineStageFlags2KHR
                                                  , SubmitFlagsKHR
                                                  , AccessFlagBits2KHR
                                                  , PipelineStageFlagBits2KHR
                                                  , SubmitFlagBitsKHR
                                                  , MemoryBarrier2KHR
                                                  , ImageMemoryBarrier2KHR
                                                  , BufferMemoryBarrier2KHR
                                                  , DependencyInfoKHR
                                                  , SemaphoreSubmitInfoKHR
                                                  , CommandBufferSubmitInfoKHR
                                                  , SubmitInfo2KHR
                                                  , PhysicalDeviceSynchronization2FeaturesKHR
                                                  , KHR_SYNCHRONIZATION_2_SPEC_VERSION
                                                  , pattern KHR_SYNCHRONIZATION_2_SPEC_VERSION
                                                  , KHR_SYNCHRONIZATION_2_EXTENSION_NAME
                                                  , pattern KHR_SYNCHRONIZATION_2_EXTENSION_NAME
                                                  , pattern PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_EXT
                                                  , pattern ACCESS_2_COMMAND_PREPROCESS_READ_BIT_EXT
                                                  , pattern ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_EXT
                                                  ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdPipelineBarrier2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdResetEvent2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdSetEvent2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdWaitEvents2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdWriteTimestamp2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (queueSubmit2)
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlagBits2)
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (BufferMemoryBarrier2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (CommandBufferSubmitInfo)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (DependencyInfo)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (ImageMemoryBarrier2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (MemoryBarrier2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (PhysicalDeviceSynchronization2Features)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (SemaphoreSubmitInfo)
import Vulkan.Core13.Enums.SubmitFlagBits (SubmitFlagBits)
import Vulkan.Core13.Enums.SubmitFlagBits (SubmitFlags)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (SubmitInfo2)
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlagBits2(ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR))
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlagBits2(ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR))
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlagBits2(ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_NONE))
import Vulkan.Core10.Enums.EventCreateFlagBits (EventCreateFlags)
import Vulkan.Core10.Enums.EventCreateFlagBits (EventCreateFlagBits(EVENT_CREATE_DEVICE_ONLY_BIT))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_ATTACHMENT_OPTIMAL))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_READ_ONLY_OPTIMAL))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_NONE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEPENDENCY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_BARRIER_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBMIT_INFO_2))
import Vulkan.Core13.Enums.AccessFlags2 (pattern ACCESS_2_COMMAND_PREPROCESS_READ_BIT_EXT)
import Vulkan.Core13.Enums.AccessFlags2 (pattern ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_EXT)
import Vulkan.Core13.Enums.PipelineStageFlags2 (pattern PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_EXT)
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR"
pattern STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR = STRUCTURE_TYPE_MEMORY_BARRIER_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR"
pattern STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2_KHR = STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2_KHR = STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEPENDENCY_INFO_KHR"
pattern STRUCTURE_TYPE_DEPENDENCY_INFO_KHR = STRUCTURE_TYPE_DEPENDENCY_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SUBMIT_INFO_2_KHR"
pattern STRUCTURE_TYPE_SUBMIT_INFO_2_KHR = STRUCTURE_TYPE_SUBMIT_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR"
pattern STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO_KHR = STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO_KHR = STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES


-- No documentation found for TopLevel "VK_EVENT_CREATE_DEVICE_ONLY_BIT_KHR"
pattern EVENT_CREATE_DEVICE_ONLY_BIT_KHR = EVENT_CREATE_DEVICE_ONLY_BIT


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR = IMAGE_LAYOUT_READ_ONLY_OPTIMAL


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR = IMAGE_LAYOUT_ATTACHMENT_OPTIMAL


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_NONE_KHR"
pattern PIPELINE_STAGE_NONE_KHR = PIPELINE_STAGE_NONE


-- No documentation found for TopLevel "VK_ACCESS_NONE_KHR"
pattern ACCESS_NONE_KHR = ACCESS_NONE


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


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_TASK_SHADER_BIT_NV"
pattern PIPELINE_STAGE_2_TASK_SHADER_BIT_NV = PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_MESH_SHADER_BIT_NV"
pattern PIPELINE_STAGE_2_MESH_SHADER_BIT_NV = PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT


-- No documentation found for TopLevel "vkCmdSetEvent2KHR"
cmdSetEvent2KHR = cmdSetEvent2


-- No documentation found for TopLevel "vkCmdResetEvent2KHR"
cmdResetEvent2KHR = cmdResetEvent2


-- No documentation found for TopLevel "vkCmdWaitEvents2KHR"
cmdWaitEvents2KHR = cmdWaitEvents2


-- No documentation found for TopLevel "vkCmdPipelineBarrier2KHR"
cmdPipelineBarrier2KHR = cmdPipelineBarrier2


-- No documentation found for TopLevel "vkQueueSubmit2KHR"
queueSubmit2KHR = queueSubmit2


-- No documentation found for TopLevel "vkCmdWriteTimestamp2KHR"
cmdWriteTimestamp2KHR = cmdWriteTimestamp2


-- No documentation found for TopLevel "VkAccessFlags2KHR"
type AccessFlags2KHR = AccessFlags2


-- No documentation found for TopLevel "VkPipelineStageFlags2KHR"
type PipelineStageFlags2KHR = PipelineStageFlags2


-- No documentation found for TopLevel "VkSubmitFlagsKHR"
type SubmitFlagsKHR = SubmitFlags


-- No documentation found for TopLevel "VkAccessFlagBits2KHR"
type AccessFlagBits2KHR = AccessFlagBits2


-- No documentation found for TopLevel "VkPipelineStageFlagBits2KHR"
type PipelineStageFlagBits2KHR = PipelineStageFlagBits2


-- No documentation found for TopLevel "VkSubmitFlagBitsKHR"
type SubmitFlagBitsKHR = SubmitFlagBits


-- No documentation found for TopLevel "VkMemoryBarrier2KHR"
type MemoryBarrier2KHR = MemoryBarrier2


-- No documentation found for TopLevel "VkImageMemoryBarrier2KHR"
type ImageMemoryBarrier2KHR = ImageMemoryBarrier2


-- No documentation found for TopLevel "VkBufferMemoryBarrier2KHR"
type BufferMemoryBarrier2KHR = BufferMemoryBarrier2


-- No documentation found for TopLevel "VkDependencyInfoKHR"
type DependencyInfoKHR = DependencyInfo


-- No documentation found for TopLevel "VkSemaphoreSubmitInfoKHR"
type SemaphoreSubmitInfoKHR = SemaphoreSubmitInfo


-- No documentation found for TopLevel "VkCommandBufferSubmitInfoKHR"
type CommandBufferSubmitInfoKHR = CommandBufferSubmitInfo


-- No documentation found for TopLevel "VkSubmitInfo2KHR"
type SubmitInfo2KHR = SubmitInfo2


-- No documentation found for TopLevel "VkPhysicalDeviceSynchronization2FeaturesKHR"
type PhysicalDeviceSynchronization2FeaturesKHR = PhysicalDeviceSynchronization2Features


type KHR_SYNCHRONIZATION_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SYNCHRONIZATION_2_SPEC_VERSION"
pattern KHR_SYNCHRONIZATION_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SYNCHRONIZATION_2_SPEC_VERSION = 1


type KHR_SYNCHRONIZATION_2_EXTENSION_NAME = "VK_KHR_synchronization2"

-- No documentation found for TopLevel "VK_KHR_SYNCHRONIZATION_2_EXTENSION_NAME"
pattern KHR_SYNCHRONIZATION_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SYNCHRONIZATION_2_EXTENSION_NAME = "VK_KHR_synchronization2"

