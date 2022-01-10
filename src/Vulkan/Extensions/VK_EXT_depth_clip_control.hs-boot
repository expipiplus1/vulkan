{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_clip_control - device extension
--
-- == VK_EXT_depth_clip_control
--
-- [__Name String__]
--     @VK_EXT_depth_clip_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     356
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
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_clip_control] @syoussefi%0A<<Here describe the issue or question you have about the VK_EXT_depth_clip_control extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-11-09
--
-- [__Contributors__]
--
--     -   Spencer Fricke, Samsung Electronics
--
--     -   Shahbaz Youssefi, Google
--
--     -   Ralph Potter, Samsung Electronics
--
-- == Description
--
-- This extension allows the application to use the OpenGL depth range in
-- NDC, i.e. with depth in range [-1, 1], as opposed to Vulkan’s default of
-- [0, 1]. The purpose of this extension is to allow efficient layering of
-- OpenGL over Vulkan, by avoiding emulation in the pre-rasterization
-- shader stages. This emulation, which effectively duplicates gl_Position
-- but with a different depth value, costs ALU and consumes shader output
-- components that the implementation may not have to spare to meet OpenGL
-- minimum requirements.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthClipControlFeaturesEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportDepthClipControlCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_CLIP_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_CLIP_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_CONTROL_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLIP_CONTROL_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 0, 2020-10-01 (Spencer Fricke)
--
--     -   Internal revisions
--
-- -   Revision 1, 2020-11-26 (Shahbaz Youssefi)
--
--     -   Language fixes
--
-- == Issues
--
-- 1) Should this extension include an origin control option to match
-- GL_LOWER_LEFT found in ARB_clip_control?
--
-- __RESOLVED__: Yes. The fix for porting over the origin is a simple
-- y-axis flip. The depth clip control is a much harder problem to solve
-- than what this extension is aimed to solve. Adding an equivalent to
-- GL_LOWER_LEFT would require more testing.
--
-- 2) Should this pipeline state be dynamic?
--
-- __RESOLVED__: Yes. The purpose of this extension is to emulate the
-- OpenGL depth range, which is expected to be globally fixed (in case of
-- OpenGL ES) or very infrequently changed (with @glClipControl@ in
-- OpenGL).
--
-- 3) Should the control provided in this extension be an enum that could
-- be extended in the future?
--
-- __RESOLVED__: Yes. It is highly unlikely that the depth range is changed
-- to anything other than [0, 1] in the future. Should that happen a new
-- extension will be required to extend such an enum, and that extension
-- might as well add a new struct to chain to
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@pNext@
-- instead.
--
-- == See Also
--
-- 'PhysicalDeviceDepthClipControlFeaturesEXT',
-- 'PipelineViewportDepthClipControlCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clip_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_clip_control  ( PhysicalDeviceDepthClipControlFeaturesEXT
                                                    , PipelineViewportDepthClipControlCreateInfoEXT
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDepthClipControlFeaturesEXT

instance ToCStruct PhysicalDeviceDepthClipControlFeaturesEXT
instance Show PhysicalDeviceDepthClipControlFeaturesEXT

instance FromCStruct PhysicalDeviceDepthClipControlFeaturesEXT


data PipelineViewportDepthClipControlCreateInfoEXT

instance ToCStruct PipelineViewportDepthClipControlCreateInfoEXT
instance Show PipelineViewportDepthClipControlCreateInfoEXT

instance FromCStruct PipelineViewportDepthClipControlCreateInfoEXT

