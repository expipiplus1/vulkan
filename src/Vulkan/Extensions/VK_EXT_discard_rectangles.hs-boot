{-# language CPP #-}
-- | = Name
--
-- VK_EXT_discard_rectangles - device extension
--
-- = Registered Extension Number
--
-- 100
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-22
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_device_group@
--
--     -   Interacts with Vulkan 1.1
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension provides additional orthogonally aligned “discard
-- rectangles” specified in framebuffer-space coordinates that restrict
-- rasterization of all points, lines and triangles.
--
-- From zero to an implementation-dependent limit (specified by
-- @maxDiscardRectangles@) number of discard rectangles can be operational
-- at once. When one or more discard rectangles are active, rasterized
-- fragments can either survive if the fragment is within any of the
-- operational discard rectangles ('DISCARD_RECTANGLE_MODE_INCLUSIVE_EXT'
-- mode) or be rejected if the fragment is within any of the operational
-- discard rectangles ('DISCARD_RECTANGLE_MODE_EXCLUSIVE_EXT' mode).
--
-- These discard rectangles operate orthogonally to the existing scissor
-- test functionality. The discard rectangles can be different for each
-- physical device in a device group by specifying the device mask and
-- setting discard rectangle dynamic state.
--
-- == New Commands
--
-- -   'cmdSetDiscardRectangleEXT'
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineDiscardRectangleStateCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDiscardRectanglePropertiesEXT'
--
-- == New Enums
--
-- -   'DiscardRectangleModeEXT'
--
-- == New Bitmasks
--
-- -   'PipelineDiscardRectangleStateCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DISCARD_RECTANGLES_EXTENSION_NAME'
--
-- -   'EXT_DISCARD_RECTANGLES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2016-12-22 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'DiscardRectangleModeEXT',
-- 'PhysicalDeviceDiscardRectanglePropertiesEXT',
-- 'PipelineDiscardRectangleStateCreateFlagsEXT',
-- 'PipelineDiscardRectangleStateCreateInfoEXT',
-- 'cmdSetDiscardRectangleEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_discard_rectangles Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_discard_rectangles  ( PhysicalDeviceDiscardRectanglePropertiesEXT
                                                    , PipelineDiscardRectangleStateCreateInfoEXT
                                                    ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceDiscardRectanglePropertiesEXT

instance ToCStruct PhysicalDeviceDiscardRectanglePropertiesEXT
instance Show PhysicalDeviceDiscardRectanglePropertiesEXT

instance FromCStruct PhysicalDeviceDiscardRectanglePropertiesEXT


data PipelineDiscardRectangleStateCreateInfoEXT

instance ToCStruct PipelineDiscardRectangleStateCreateInfoEXT
instance Show PipelineDiscardRectangleStateCreateInfoEXT

instance FromCStruct PipelineDiscardRectangleStateCreateInfoEXT

