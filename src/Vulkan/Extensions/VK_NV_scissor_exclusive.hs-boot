{-# language CPP #-}
-- | = Name
--
-- VK_NV_scissor_exclusive - device extension
--
-- = Registered Extension Number
--
-- 206
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
--     2018-07-31
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--     None
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds support for an exclusive scissor test to Vulkan. The
-- exclusive scissor test behaves like the scissor test, except that the
-- exclusive scissor test fails for pixels inside the corresponding
-- rectangle and passes for pixels outside the rectangle. If the same
-- rectangle is used for both the scissor and exclusive scissor tests, the
-- exclusive scissor test will pass if and only if the scissor test fails.
--
-- == New Commands
--
-- -   'cmdSetExclusiveScissorNV'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExclusiveScissorFeaturesNV'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportExclusiveScissorStateCreateInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME'
--
-- -   'NV_SCISSOR_EXCLUSIVE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- 1) For the scissor test, the viewport state must be created with a
-- matching number of scissor and viewport rectangles. Should we have the
-- same requirement for exclusive scissors?
--
-- __RESOLVED__: For exclusive scissors, we relax this requirement and
-- allow an exclusive scissor rectangle count that is either zero or equal
-- to the number of viewport rectangles. If you pass in an exclusive
-- scissor count of zero, the exclusive scissor test is treated as
-- disabled.
--
-- == Version History
--
-- -   Revision 1, 2018-07-31 (Pat Brown)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceExclusiveScissorFeaturesNV',
-- 'PipelineViewportExclusiveScissorStateCreateInfoNV',
-- 'cmdSetExclusiveScissorNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_scissor_exclusive Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_scissor_exclusive  ( PhysicalDeviceExclusiveScissorFeaturesNV
                                                  , PipelineViewportExclusiveScissorStateCreateInfoNV
                                                  ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceExclusiveScissorFeaturesNV

instance ToCStruct PhysicalDeviceExclusiveScissorFeaturesNV
instance Show PhysicalDeviceExclusiveScissorFeaturesNV

instance FromCStruct PhysicalDeviceExclusiveScissorFeaturesNV


data PipelineViewportExclusiveScissorStateCreateInfoNV

instance ToCStruct PipelineViewportExclusiveScissorStateCreateInfoNV
instance Show PipelineViewportExclusiveScissorStateCreateInfoNV

instance FromCStruct PipelineViewportExclusiveScissorStateCreateInfoNV

