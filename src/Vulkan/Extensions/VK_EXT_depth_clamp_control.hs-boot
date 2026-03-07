{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_clamp_control - device extension
--
-- = VK_EXT_depth_clamp_control
--
-- [__Name String__]
--     @VK_EXT_depth_clamp_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     583
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Jules Blok
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_clamp_control] @jules%0A*Here describe the issue or question you have about the VK_EXT_depth_clamp_control extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_depth_clamp_control.adoc VK_EXT_depth_clamp_control>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-07-15
--
-- [__Contributors__]
--
--     -   Jules Blok, Independent
--
-- == Description
--
-- This extension allows the application to control the viewport depth
-- clamp range separately from the viewport @minDepth@ and @maxDepth@. This
-- gives the ability for the application to restrict depth values to an
-- application-defined range rather than the viewport depth range or the
-- range defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_zero_one VK_EXT_depth_clamp_zero_one>
-- extension.
--
-- It can be used to set a smaller or larger clamping range than the
-- viewport depth range without affecting the depth mapping of the viewport
-- transform. Another possible use of this extension is to restrict depth
-- values beyond the viewport depth range to a clamping range other than
-- the [0, 1] range defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clamp_zero_one VK_EXT_depth_clamp_zero_one>
-- extension.
--
-- == New Commands
--
-- -   'cmdSetDepthClampRangeEXT'
--
-- == New Structures
--
-- -   'DepthClampRangeEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthClampControlFeaturesEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportDepthClampControlCreateInfoEXT'
--
-- == New Enums
--
-- -   'DepthClampModeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_CLAMP_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_CLAMP_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_RANGE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_CONTROL_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLAMP_CONTROL_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) Should the depth clamp range be a per-viewport parameter?
--
-- __RESOLVED__: No. Because the depth clamp range was previously defined
-- to be equal to the viewport depth range, conformant runtimes are already
-- handling the depth clamp range as a per-viewport parameter. However
-- because of complexities from interactions with multiple viewports a
-- per-viewport clamp range is left to a future extensions if a use case
-- arises.
--
-- 2) Should this pipeline state be dynamic?
--
-- __RESOLVED__: Yes. Since the viewport depth range can already be a
-- dynamic state conformant runtimes are already able to handle the depth
-- clamp range as a dynamic state.
--
-- 3) Can the depth clamp range be ignored when depth clamping is disabled?
--
-- __RESOLVED__: Yes. This extension overrides the clamping range used only
-- when depth clamping is enabled. The alternative would be highly
-- unintuitive. As a consequence the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clip_enable VK_EXT_depth_clip_enable>
-- extension is required if depth clipping is desired in combination with
-- this extension.
--
-- == Version History
--
-- -   Revision 1, 2024-02-13 (Jules Blok)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_depth_clamp_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_clamp_control  ( DepthClampRangeEXT
                                                     , PhysicalDeviceDepthClampControlFeaturesEXT
                                                     , PipelineViewportDepthClampControlCreateInfoEXT
                                                     , DepthClampModeEXT
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DepthClampRangeEXT

instance ToCStruct DepthClampRangeEXT
instance Show DepthClampRangeEXT

instance FromCStruct DepthClampRangeEXT


data PhysicalDeviceDepthClampControlFeaturesEXT

instance ToCStruct PhysicalDeviceDepthClampControlFeaturesEXT
instance Show PhysicalDeviceDepthClampControlFeaturesEXT

instance FromCStruct PhysicalDeviceDepthClampControlFeaturesEXT


data PipelineViewportDepthClampControlCreateInfoEXT

instance ToCStruct PipelineViewportDepthClampControlCreateInfoEXT
instance Show PipelineViewportDepthClampControlCreateInfoEXT

instance FromCStruct PipelineViewportDepthClampControlCreateInfoEXT


data DepthClampModeEXT

