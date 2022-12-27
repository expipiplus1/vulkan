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
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Deprecation state__]
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
--     -   Promoted to Vulkan 1.3 Core
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
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- KHR suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
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
-- 'AccessFlagBits2KHR', 'AccessFlags2KHR', 'BufferMemoryBarrier2KHR',
-- 'CommandBufferSubmitInfoKHR', 'DependencyInfoKHR',
-- 'Vulkan.Core10.FundamentalTypes.Flags64', 'ImageMemoryBarrier2KHR',
-- 'MemoryBarrier2KHR', 'PhysicalDeviceSynchronization2FeaturesKHR',
-- 'PipelineStageFlagBits2KHR', 'PipelineStageFlags2KHR',
-- 'SemaphoreSubmitInfoKHR', 'SubmitFlagBitsKHR', 'SubmitFlagsKHR',
-- 'SubmitInfo2KHR', 'cmdPipelineBarrier2KHR', 'cmdResetEvent2KHR',
-- 'cmdSetEvent2KHR', 'cmdWaitEvents2KHR', 'cmdWriteTimestamp2KHR',
-- 'queueSubmit2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_synchronization2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_synchronization2  ( cmdWriteBufferMarker2AMD
                                                  , getQueueCheckpointData2NV
                                                  , pattern STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR
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
                                                  , QueueFamilyCheckpointProperties2NV(..)
                                                  , CheckpointData2NV(..)
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
                                                  ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdPipelineBarrier2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdResetEvent2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdSetEvent2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdWaitEvents2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (cmdWriteTimestamp2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (queueSubmit2)
import Vulkan.NamedType ((:::))
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlagBits2)
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (BufferMemoryBarrier2)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (CommandBufferSubmitInfo)
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (DependencyInfo)
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteBufferMarker2AMD))
import Vulkan.Dynamic (DeviceCmds(pVkGetQueueCheckpointData2NV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (ImageMemoryBarrier2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (MemoryBarrier2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (PhysicalDeviceSynchronization2Features)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(..))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue(Queue))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (SemaphoreSubmitInfo)
import Vulkan.Core10.Enums.StructureType (StructureType)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEPENDENCY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_BARRIER_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBMIT_INFO_2))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteBufferMarker2AMD
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags2 -> Buffer -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlags2 -> Buffer -> DeviceSize -> Word32 -> IO ()

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
-- Similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWriteTimestamp2',
-- if an implementation is unable to write a marker at any specific
-- pipeline stage, it /may/ instead do so at any logically later stage.
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-synchronization2-03893# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
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
--     be less than or equal to the size of @dstBuffer@ minus @4@
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
--     a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
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
-- -   #VUID-vkCmdWriteBufferMarker2AMD-videocoding# This command /must/
--     only be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_buffer_marker VK_AMD_buffer_marker>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2'
cmdWriteBufferMarker2AMD :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @stage@ specifies the pipeline stage whose completion triggers the
                            -- marker write.
                            PipelineStageFlags2
                         -> -- | @dstBuffer@ is the buffer where the marker will be written.
                            ("dstBuffer" ::: Buffer)
                         -> -- | @dstOffset@ is the byte offset into the buffer where the marker will be
                            -- written.
                            ("dstOffset" ::: DeviceSize)
                         -> -- | @marker@ is the 32-bit value of the marker.
                            ("marker" ::: Word32)
                         -> io ()
cmdWriteBufferMarker2AMD commandBuffer
                           stage
                           dstBuffer
                           dstOffset
                           marker = liftIO $ do
  let vkCmdWriteBufferMarker2AMDPtr = pVkCmdWriteBufferMarker2AMD (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdWriteBufferMarker2AMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteBufferMarker2AMD is null" Nothing Nothing
  let vkCmdWriteBufferMarker2AMD' = mkVkCmdWriteBufferMarker2AMD vkCmdWriteBufferMarker2AMDPtr
  traceAroundEvent "vkCmdWriteBufferMarker2AMD" (vkCmdWriteBufferMarker2AMD'
                                                   (commandBufferHandle (commandBuffer))
                                                   (stage)
                                                   (dstBuffer)
                                                   (dstOffset)
                                                   (marker))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetQueueCheckpointData2NV
  :: FunPtr (Ptr Queue_T -> Ptr Word32 -> Ptr CheckpointData2NV -> IO ()) -> Ptr Queue_T -> Ptr Word32 -> Ptr CheckpointData2NV -> IO ()

-- | vkGetQueueCheckpointData2NV - Retrieve diagnostic checkpoint data
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostic_checkpoints VK_NV_device_diagnostic_checkpoints>,
-- 'CheckpointData2NV', 'Vulkan.Core10.Handles.Queue'
getQueueCheckpointData2NV :: forall io
                           . (MonadIO io)
                          => -- | @queue@ is the 'Vulkan.Core10.Handles.Queue' object the caller would
                             -- like to retrieve checkpoint data for
                             Queue
                          -> io (("checkpointData" ::: Vector CheckpointData2NV))
getQueueCheckpointData2NV queue = liftIO . evalContT $ do
  let vkGetQueueCheckpointData2NVPtr = pVkGetQueueCheckpointData2NV (case queue of Queue{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetQueueCheckpointData2NVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetQueueCheckpointData2NV is null" Nothing Nothing
  let vkGetQueueCheckpointData2NV' = mkVkGetQueueCheckpointData2NV vkGetQueueCheckpointData2NVPtr
  let queue' = queueHandle (queue)
  pPCheckpointDataCount <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetQueueCheckpointData2NV" (vkGetQueueCheckpointData2NV'
                                                           queue'
                                                           (pPCheckpointDataCount)
                                                           (nullPtr))
  pCheckpointDataCount <- lift $ peek @Word32 pPCheckpointDataCount
  pPCheckpointData <- ContT $ bracket (callocBytes @CheckpointData2NV ((fromIntegral (pCheckpointDataCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCheckpointData `advancePtrBytes` (i * 32) :: Ptr CheckpointData2NV) . ($ ())) [0..(fromIntegral (pCheckpointDataCount)) - 1]
  lift $ traceAroundEvent "vkGetQueueCheckpointData2NV" (vkGetQueueCheckpointData2NV'
                                                           queue'
                                                           (pPCheckpointDataCount)
                                                           ((pPCheckpointData)))
  pCheckpointDataCount' <- lift $ peek @Word32 pPCheckpointDataCount
  pCheckpointData' <- lift $ generateM (fromIntegral (pCheckpointDataCount')) (\i -> peekCStruct @CheckpointData2NV (((pPCheckpointData) `advancePtrBytes` (32 * (i)) :: Ptr CheckpointData2NV)))
  pure $ (pCheckpointData')


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


-- | VkQueueFamilyCheckpointProperties2NV - Return structure for queue family
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostic_checkpoints VK_NV_device_diagnostic_checkpoints>,
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyCheckpointProperties2NV = QueueFamilyCheckpointProperties2NV
  { -- | @checkpointExecutionStageMask@ is a mask indicating which pipeline
    -- stages the implementation can execute checkpoint markers in.
    checkpointExecutionStageMask :: PipelineStageFlags2 }
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
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2)) (checkpointExecutionStageMask)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2)) (zero)
    f

instance FromCStruct QueueFamilyCheckpointProperties2NV where
  peekCStruct p = do
    checkpointExecutionStageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 16 :: Ptr PipelineStageFlags2))
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


-- | VkCheckpointData2NV - Return structure for command buffer checkpoint
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostic_checkpoints VK_NV_device_diagnostic_checkpoints>,
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getQueueCheckpointData2NV'
data CheckpointData2NV = CheckpointData2NV
  { -- | @stage@ indicates a single pipeline stage which the checkpoint marker
    -- data refers to.
    stage :: PipelineStageFlags2
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
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2)) (stage)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (checkpointMarker)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CHECKPOINT_DATA_2_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct CheckpointData2NV where
  peekCStruct p = do
    stage <- peek @PipelineStageFlags2 ((p `plusPtr` 16 :: Ptr PipelineStageFlags2))
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

