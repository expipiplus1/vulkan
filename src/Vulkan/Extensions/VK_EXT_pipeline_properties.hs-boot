{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_properties - device extension
--
-- == VK_EXT_pipeline_properties
--
-- [__Name String__]
--     @VK_EXT_pipeline_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     373
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Mukund Keshava
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_pipeline_properties] @mkeshavanv%0A*Here describe the issue or question you have about the VK_EXT_pipeline_properties extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-04-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mukund Keshava, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Mark Bellamy, Arm
--
-- == Description
--
-- Vulkan SC requires offline compilation of pipelines. In order to support
-- this, the pipeline state is represented in a
-- <https://github.com/KhronosGroup/VulkanSC-Docs/wiki/JSON-schema JSON schema>
-- that is read by an offline tool for compilation.
--
-- One method of developing a Vulkan SC application is to author a Vulkan
-- application and use a layer to record and serialize the pipeline state
-- and shaders for offline compilation. Each pipeline is represented by a
-- separate JSON file, and can be identified with a @pipelineIdentifier@.
--
-- Once the pipelines have been compiled by the offline pipeline cache
-- compiler, the Vulkan SC application can then use this
-- @pipelineIdentifier@ for identifying the pipeline via Vulkan SC’s
-- @VkPipelineIdentifierInfo@ structure.
--
-- This extension allows the Vulkan application to query the
-- @pipelineIdentifier@ associated with each pipeline so that the
-- application can store this with its pipeline metadata and the Vulkan SC
-- application will then use to map the same state to an entry in the
-- Vulkan SC pipeline cache.
--
-- It is expected that this extension will initially be implemented in the
-- json generation layer, although we can envision that there might be
-- future uses for it in native Vulkan drivers as well.
--
-- == New Commands
--
-- -   'getPipelinePropertiesEXT'
--
-- == New Structures
--
-- -   'PipelineInfoEXT'
--
-- -   'PipelinePropertiesIdentifierEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelinePropertiesFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PIPELINE_PROPERTIES_EXTENSION_NAME'
--
-- -   'EXT_PIPELINE_PROPERTIES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROPERTIES_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_PIPELINE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_PROPERTIES_IDENTIFIER_EXT'
--
-- == Issues
--
-- (1) This extension does not make sense on a strict Vulkan SC
-- implementation. It may however be of potential use in a non-strict
-- Vulkan SC implementation. Should this extension be enabled as part of
-- Vulkan SC as well?
--
-- __RESOLVED__: No. This extension will not be enabled for Vulkan SC.
--
-- (2) This is intended to be a general pipeline properties query, but is
-- currently only retrieving the pipeline identifier. Should the pipeline
-- identifier query be mandatory for this extension and for all queries
-- using this entry point?
--
-- __RESOLVED__: Use 'Vulkan.CStruct.Extends.BaseOutStructure' for the
-- return parameter. Currently this is required to actually be a
-- 'PipelinePropertiesIdentifierEXT' structure, but that could be relaxed
-- in the future to allow other structure types or to allow other
-- structures to be chained in along with this one.
--
-- (3) Should there be a feature structure? Should it be required?
--
-- __RESOLVED__: Add a feature structure, and a feature for querying
-- pipeline identifier, but allow it to be optional so that this extension
-- can be used as the basis for other pipeline property queries without
-- requiring the pipeline identifier to be supported.
--
-- == Version History
--
-- -   Revision 1, 2022-04-19 (Mukund Keshava, Daniel Koch)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDevicePipelinePropertiesFeaturesEXT', 'PipelineInfoEXT',
-- 'PipelinePropertiesIdentifierEXT', 'getPipelinePropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pipeline_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_properties  ( PhysicalDevicePipelinePropertiesFeaturesEXT
                                                     , PipelinePropertiesIdentifierEXT
                                                     , PipelineInfoEXT
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineInfoKHR)
data PhysicalDevicePipelinePropertiesFeaturesEXT

instance ToCStruct PhysicalDevicePipelinePropertiesFeaturesEXT
instance Show PhysicalDevicePipelinePropertiesFeaturesEXT

instance FromCStruct PhysicalDevicePipelinePropertiesFeaturesEXT


data PipelinePropertiesIdentifierEXT

instance ToCStruct PipelinePropertiesIdentifierEXT
instance Show PipelinePropertiesIdentifierEXT

instance FromCStruct PipelinePropertiesIdentifierEXT


-- No documentation found for TopLevel "VkPipelineInfoEXT"
type PipelineInfoEXT = PipelineInfoKHR

