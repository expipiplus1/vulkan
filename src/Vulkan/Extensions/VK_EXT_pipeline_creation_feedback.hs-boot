{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_creation_feedback - device extension
--
-- == VK_EXT_pipeline_creation_feedback
--
-- [__Name String__]
--     @VK_EXT_pipeline_creation_feedback@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     193
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Jean-Francois Roy
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_pipeline_creation_feedback:%20&body=@jfroy%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-03-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jean-Francois Roy, Google
--
--     -   Hai Nguyen, Google
--
--     -   Andrew Ellem, Google
--
--     -   Bob Fraser, Google
--
--     -   Sujeevan Rajayogam, Google
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Neil Henning, AMD
--
-- == Description
--
-- This extension adds a mechanism to provide feedback to an application
-- about pipeline creation, with the specific goal of allowing a feedback
-- loop between build systems and in-the-field application executions to
-- ensure effective pipeline caches are shipped to customers.
--
-- == New Structures
--
-- -   'PipelineCreationFeedbackEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
--     'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR':
--
--     -   'PipelineCreationFeedbackCreateInfoEXT'
--
-- == New Enums
--
-- -   'PipelineCreationFeedbackFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'PipelineCreationFeedbackFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME'
--
-- -   'EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-03-12 (Jean-Francois Roy)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PipelineCreationFeedbackCreateInfoEXT', 'PipelineCreationFeedbackEXT',
-- 'PipelineCreationFeedbackFlagBitsEXT',
-- 'PipelineCreationFeedbackFlagsEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_creation_feedback Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_creation_feedback  ( PipelineCreationFeedbackCreateInfoEXT
                                                            , PipelineCreationFeedbackEXT
                                                            ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PipelineCreationFeedbackCreateInfoEXT

instance ToCStruct PipelineCreationFeedbackCreateInfoEXT
instance Show PipelineCreationFeedbackCreateInfoEXT

instance FromCStruct PipelineCreationFeedbackCreateInfoEXT


data PipelineCreationFeedbackEXT

instance ToCStruct PipelineCreationFeedbackEXT
instance Show PipelineCreationFeedbackEXT

instance FromCStruct PipelineCreationFeedbackEXT

