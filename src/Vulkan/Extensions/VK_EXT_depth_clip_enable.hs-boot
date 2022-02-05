{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_clip_enable - device extension
--
-- == VK_EXT_depth_clip_enable
--
-- [__Name String__]
--     @VK_EXT_depth_clip_enable@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     103
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
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_clip_enable] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_EXT_depth_clip_enable extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-20
--
-- [__Contributors__]
--
--     -   Daniel Rakos, AMD
--
--     -   Henri Verbeet, CodeWeavers
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Philip Rebohle, DXVK
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension allows the depth clipping operation, that is normally
-- implicitly controlled by
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@depthClampEnable@,
-- to instead be controlled explicitly by
-- 'PipelineRasterizationDepthClipStateCreateInfoEXT'::@depthClipEnable@.
--
-- This is useful for translating DX content which assumes depth clamping
-- is always enabled, but depth clip can be controlled by the
-- DepthClipEnable rasterization state (D3D12_RASTERIZER_DESC).
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthClipEnableFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationDepthClipStateCreateInfoEXT'
--
-- == New Bitmasks
--
-- -   'PipelineRasterizationDepthClipStateCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-12-20 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceDepthClipEnableFeaturesEXT',
-- 'PipelineRasterizationDepthClipStateCreateFlagsEXT',
-- 'PipelineRasterizationDepthClipStateCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_depth_clip_enable Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_clip_enable  ( PhysicalDeviceDepthClipEnableFeaturesEXT
                                                   , PipelineRasterizationDepthClipStateCreateInfoEXT
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDepthClipEnableFeaturesEXT

instance ToCStruct PhysicalDeviceDepthClipEnableFeaturesEXT
instance Show PhysicalDeviceDepthClipEnableFeaturesEXT

instance FromCStruct PhysicalDeviceDepthClipEnableFeaturesEXT


data PipelineRasterizationDepthClipStateCreateInfoEXT

instance ToCStruct PipelineRasterizationDepthClipStateCreateInfoEXT
instance Show PipelineRasterizationDepthClipStateCreateInfoEXT

instance FromCStruct PipelineRasterizationDepthClipStateCreateInfoEXT

