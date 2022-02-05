{-# language CPP #-}
-- | = Name
--
-- VK_NV_scissor_exclusive - device extension
--
-- == VK_NV_scissor_exclusive
--
-- [__Name String__]
--     @VK_NV_scissor_exclusive@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     206
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
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_scissor_exclusive] @nvpbrown%0A<<Here describe the issue or question you have about the VK_NV_scissor_exclusive extension>> >
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
-- == See Also
--
-- 'PhysicalDeviceExclusiveScissorFeaturesNV',
-- 'PipelineViewportExclusiveScissorStateCreateInfoNV',
-- 'cmdSetExclusiveScissorNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_scissor_exclusive Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_scissor_exclusive  ( PhysicalDeviceExclusiveScissorFeaturesNV
                                                  , PipelineViewportExclusiveScissorStateCreateInfoNV
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceExclusiveScissorFeaturesNV

instance ToCStruct PhysicalDeviceExclusiveScissorFeaturesNV
instance Show PhysicalDeviceExclusiveScissorFeaturesNV

instance FromCStruct PhysicalDeviceExclusiveScissorFeaturesNV


data PipelineViewportExclusiveScissorStateCreateInfoNV

instance ToCStruct PipelineViewportExclusiveScissorStateCreateInfoNV
instance Show PipelineViewportExclusiveScissorStateCreateInfoNV

instance FromCStruct PipelineViewportExclusiveScissorStateCreateInfoNV

