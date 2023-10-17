{-# language CPP #-}
-- | = Name
--
-- VK_EXT_discard_rectangles - device extension
--
-- == VK_EXT_discard_rectangles
--
-- [__Name String__]
--     @VK_EXT_discard_rectangles@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     100
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_discard_rectangles] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_discard_rectangles extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-01-18
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
-- Version 2 of this extension introduces new dynamic states
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_ENABLE_EXT'
-- and
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_MODE_EXT',
-- and the corresponding functions 'cmdSetDiscardRectangleEnableEXT' and
-- 'cmdSetDiscardRectangleModeEXT'. Applications that use these dynamic
-- states must ensure the implementation advertises at least @specVersion@
-- @2@ of this extension.
--
-- == New Commands
--
-- -   'cmdSetDiscardRectangleEXT'
--
-- -   'cmdSetDiscardRectangleEnableEXT'
--
-- -   'cmdSetDiscardRectangleModeEXT'
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
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_MODE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 2, 2023-01-18 (Piers Daniell)
--
--     -   Add dynamic states for discard rectangle enable\/disable and
--         mode.
--
-- -   Revision 1, 2016-12-22 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'DiscardRectangleModeEXT',
-- 'PhysicalDeviceDiscardRectanglePropertiesEXT',
-- 'PipelineDiscardRectangleStateCreateFlagsEXT',
-- 'PipelineDiscardRectangleStateCreateInfoEXT',
-- 'cmdSetDiscardRectangleEXT', 'cmdSetDiscardRectangleEnableEXT',
-- 'cmdSetDiscardRectangleModeEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_discard_rectangles Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_discard_rectangles  ( PhysicalDeviceDiscardRectanglePropertiesEXT
                                                    , PipelineDiscardRectangleStateCreateInfoEXT
                                                    , DiscardRectangleModeEXT
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDiscardRectanglePropertiesEXT

instance ToCStruct PhysicalDeviceDiscardRectanglePropertiesEXT
instance Show PhysicalDeviceDiscardRectanglePropertiesEXT

instance FromCStruct PhysicalDeviceDiscardRectanglePropertiesEXT


data PipelineDiscardRectangleStateCreateInfoEXT

instance ToCStruct PipelineDiscardRectangleStateCreateInfoEXT
instance Show PipelineDiscardRectangleStateCreateInfoEXT

instance FromCStruct PipelineDiscardRectangleStateCreateInfoEXT


data DiscardRectangleModeEXT

