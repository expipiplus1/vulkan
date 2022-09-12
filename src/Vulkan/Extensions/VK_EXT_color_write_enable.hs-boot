{-# language CPP #-}
-- | = Name
--
-- VK_EXT_color_write_enable - device extension
--
-- == VK_EXT_color_write_enable
--
-- [__Name String__]
--     @VK_EXT_color_write_enable@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     382
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
-- [__Contact__]
--
--     -   Sharif Elcott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_color_write_enable] @selcott%0A*Here describe the issue or question you have about the VK_EXT_color_write_enable extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-02-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Sharif Elcott, Google
--
--     -   Tobias Hector, AMD
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows for selectively enabling and disabling writes to
-- output color attachments via a pipeline dynamic state.
--
-- The intended use cases for this new state are mostly identical to those
-- of colorWriteMask, such as selectively disabling writes to avoid
-- feedback loops between subpasses or bandwidth savings for unused
-- outputs. By making the state dynamic, one additional benefit is the
-- ability to reduce pipeline counts and pipeline switching via shaders
-- that write a superset of the desired data of which subsets are selected
-- dynamically. The reason for a new state, colorWriteEnable, rather than
-- making colorWriteMask dynamic is that, on many implementations, the more
-- flexible per-component semantics of the colorWriteMask state cannot be
-- made dynamic in a performant manner.
--
-- == New Commands
--
-- -   'cmdSetColorWriteEnableEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceColorWriteEnableFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo':
--
--     -   'PipelineColorWriteCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_COLOR_WRITE_ENABLE_EXTENSION_NAME'
--
-- -   'EXT_COLOR_WRITE_ENABLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COLOR_WRITE_ENABLE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_WRITE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-01-25 (Sharif Elcott)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceColorWriteEnableFeaturesEXT',
-- 'PipelineColorWriteCreateInfoEXT', 'cmdSetColorWriteEnableEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_color_write_enable Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_color_write_enable  ( PhysicalDeviceColorWriteEnableFeaturesEXT
                                                    , PipelineColorWriteCreateInfoEXT
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceColorWriteEnableFeaturesEXT

instance ToCStruct PhysicalDeviceColorWriteEnableFeaturesEXT
instance Show PhysicalDeviceColorWriteEnableFeaturesEXT

instance FromCStruct PhysicalDeviceColorWriteEnableFeaturesEXT


data PipelineColorWriteCreateInfoEXT

instance ToCStruct PipelineColorWriteCreateInfoEXT
instance Show PipelineColorWriteCreateInfoEXT

instance FromCStruct PipelineColorWriteCreateInfoEXT

