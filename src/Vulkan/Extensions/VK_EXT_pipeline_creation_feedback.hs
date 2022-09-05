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
--     -   Requires support for Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Jean-Francois Roy
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_pipeline_creation_feedback] @jfroy%0A<<Here describe the issue or question you have about the VK_EXT_pipeline_creation_feedback extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-03-12
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
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
--     -   'STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- EXT suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2019-03-12 (Jean-Francois Roy)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PipelineCreationFeedbackCreateInfoEXT', 'PipelineCreationFeedbackEXT',
-- 'PipelineCreationFeedbackFlagBitsEXT',
-- 'PipelineCreationFeedbackFlagsEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pipeline_creation_feedback Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_creation_feedback  ( pattern STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
                                                            , PipelineCreationFeedbackFlagsEXT
                                                            , PipelineCreationFeedbackFlagBitsEXT
                                                            , PipelineCreationFeedbackEXT
                                                            , PipelineCreationFeedbackCreateInfoEXT
                                                            , EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
                                                            , pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
                                                            , EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
                                                            , pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
                                                            ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback (PipelineCreationFeedback)
import Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfo)
import Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits (PipelineCreationFeedbackFlagBits)
import Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits (PipelineCreationFeedbackFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT = STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO


-- No documentation found for TopLevel "VkPipelineCreationFeedbackFlagsEXT"
type PipelineCreationFeedbackFlagsEXT = PipelineCreationFeedbackFlags


-- No documentation found for TopLevel "VkPipelineCreationFeedbackFlagBitsEXT"
type PipelineCreationFeedbackFlagBitsEXT = PipelineCreationFeedbackFlagBits


-- No documentation found for TopLevel "VkPipelineCreationFeedbackEXT"
type PipelineCreationFeedbackEXT = PipelineCreationFeedback


-- No documentation found for TopLevel "VkPipelineCreationFeedbackCreateInfoEXT"
type PipelineCreationFeedbackCreateInfoEXT = PipelineCreationFeedbackCreateInfo


type EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION"
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = 1


type EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = "VK_EXT_pipeline_creation_feedback"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME"
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = "VK_EXT_pipeline_creation_feedback"

