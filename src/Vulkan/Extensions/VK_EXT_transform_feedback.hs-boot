{-# language CPP #-}
-- | = Name
--
-- VK_EXT_transform_feedback - device extension
--
-- == VK_EXT_transform_feedback
--
-- [__Name String__]
--     @VK_EXT_transform_feedback@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     29
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
-- [__Special Uses__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_transform_feedback] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_EXT_transform_feedback extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-09
--
-- [__Contributors__]
--
--     -   Baldur Karlsson, Valve
--
--     -   Boris Zanin, Mobica
--
--     -   Daniel Rakos, AMD
--
--     -   Donald Scorgie, Imagination
--
--     -   Henri Verbeet, CodeWeavers
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jason Ekstrand, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jesse Barker, Unity
--
--     -   Jesse Hall, Google
--
--     -   Pierre-Loup Griffais, Valve
--
--     -   Philip Rebohle, DXVK
--
--     -   Ruihao Zhang, Qualcomm
--
--     -   Samuel Pitoiset, Valve
--
--     -   Slawomir Grajewski, Intel
--
--     -   Stu Smith, Imagination Technologies
--
-- == Description
--
-- This extension adds transform feedback to the Vulkan API by exposing the
-- SPIR-V @TransformFeedback@ and @GeometryStreams@ capabilities to capture
-- vertex, tessellation or geometry shader outputs to one or more buffers.
-- It adds API functionality to bind transform feedback buffers to capture
-- the primitives emitted by the graphics pipeline from SPIR-V outputs
-- decorated for transform feedback. The transform feedback capture can be
-- paused and resumed by way of storing and retrieving a byte counter. The
-- captured data can be drawn again where the vertex count is derived from
-- the byte counter without CPU intervention. If the implementation is
-- capable, a vertex stream other than zero can be rasterized.
--
-- All these features are designed to match the full capabilities of OpenGL
-- core transform feedback functionality and beyond. Many of the features
-- are optional to allow base OpenGL ES GPUs to also implement this
-- extension.
--
-- The primary purpose of the functionality exposed by this extension is to
-- support translation layers from other 3D APIs. This functionality is not
-- considered forward looking, and is not expected to be promoted to a KHR
-- extension or to core Vulkan. Unless this is needed for translation, it
-- is recommended that developers use alternative techniques of using the
-- GPU to process and capture vertex data.
--
-- == New Commands
--
-- -   'cmdBeginQueryIndexedEXT'
--
-- -   'cmdBeginTransformFeedbackEXT'
--
-- -   'cmdBindTransformFeedbackBuffersEXT'
--
-- -   'cmdDrawIndirectByteCountEXT'
--
-- -   'cmdEndQueryIndexedEXT'
--
-- -   'cmdEndTransformFeedbackEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTransformFeedbackFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTransformFeedbackPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationStateStreamCreateInfoEXT'
--
-- == New Bitmasks
--
-- -   'PipelineRasterizationStateStreamCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_TRANSFORM_FEEDBACK_EXTENSION_NAME'
--
-- -   'EXT_TRANSFORM_FEEDBACK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) Should we include pause\/resume functionality?
--
-- __RESOLVED__: Yes, this is needed to ease layering other APIs which have
-- this functionality. To pause use 'cmdEndTransformFeedbackEXT' and
-- provide valid buffer handles in the @pCounterBuffers@ array and offsets
-- in the @pCounterBufferOffsets@ array for the implementation to save the
-- resume points. Then to resume use 'cmdBeginTransformFeedbackEXT' with
-- the previous @pCounterBuffers@ and @pCounterBufferOffsets@ values.
-- Between the pause and resume there needs to be a memory barrier for the
-- counter buffers with a source access of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT'
-- at pipeline stage
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
-- to a destination access of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT'
-- at pipeline stage
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'.
--
-- 2) How does this interact with multiview?
--
-- __RESOLVED__: Transform feedback cannot be made active in a render pass
-- with multiview enabled.
--
-- 3) How should queries be done?
--
-- __RESOLVED__: There is a new query type
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'.
-- A query pool created with this type will capture 2 integers -
-- numPrimitivesWritten and numPrimitivesNeeded - for the specified vertex
-- stream output from the last
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipeline-graphics-subsets-pre-rasterization pre-rasterization shader stage>.
-- The vertex stream output queried is zero by default, but can be
-- specified with the new 'cmdBeginQueryIndexedEXT' and
-- 'cmdEndQueryIndexedEXT' commands.
--
-- == Version History
--
-- -   Revision 1, 2018-10-09 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceTransformFeedbackFeaturesEXT',
-- 'PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'PipelineRasterizationStateStreamCreateFlagsEXT',
-- 'PipelineRasterizationStateStreamCreateInfoEXT',
-- 'cmdBeginQueryIndexedEXT', 'cmdBeginTransformFeedbackEXT',
-- 'cmdBindTransformFeedbackBuffersEXT', 'cmdDrawIndirectByteCountEXT',
-- 'cmdEndQueryIndexedEXT', 'cmdEndTransformFeedbackEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_transform_feedback  ( PhysicalDeviceTransformFeedbackFeaturesEXT
                                                    , PhysicalDeviceTransformFeedbackPropertiesEXT
                                                    , PipelineRasterizationStateStreamCreateInfoEXT
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceTransformFeedbackFeaturesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackFeaturesEXT
instance Show PhysicalDeviceTransformFeedbackFeaturesEXT

instance FromCStruct PhysicalDeviceTransformFeedbackFeaturesEXT


data PhysicalDeviceTransformFeedbackPropertiesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackPropertiesEXT
instance Show PhysicalDeviceTransformFeedbackPropertiesEXT

instance FromCStruct PhysicalDeviceTransformFeedbackPropertiesEXT


data PipelineRasterizationStateStreamCreateInfoEXT

instance ToCStruct PipelineRasterizationStateStreamCreateInfoEXT
instance Show PipelineRasterizationStateStreamCreateInfoEXT

instance FromCStruct PipelineRasterizationStateStreamCreateInfoEXT

