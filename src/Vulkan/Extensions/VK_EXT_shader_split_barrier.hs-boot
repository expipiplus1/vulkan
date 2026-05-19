{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_split_barrier - device extension
--
-- = VK_EXT_shader_split_barrier
--
-- [__Name String__]
--     @VK_EXT_shader_split_barrier@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     306
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_split_barrier.html SPV_EXT_split_barrier>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_split_barrier] @mnetsch%0A*Here describe the issue or question you have about the VK_EXT_shader_split_barrier extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_split_barrier.adoc VK_EXT_shader_split_barrier>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-05-08
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_split_barrier.txt GLSL_EXT_split_barrier>
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Elina Kamenetskaya, Qualcomm Technologies, Inc.
--
--     -   Wooyoung Kim, Qualcomm Technologies, Inc.
--
--     -   John Li, Qualcomm Technologies, Inc.
--
--     -   Jeff Bolz, Nvidia
--
--     -   Ben Ashbaugh, Intel
--
-- == Description
--
-- This extension splits @OpControlBarrier@ by exposing two new barrier
-- operations with
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_split_barrier.html SPV_EXT_split_barrier>:
--
-- -   @OpControlBarrierArriveEXT@ - notifies that invocation has arrived
--     here
--
-- -   @OpControlBarrierWaitEXT@ - waits on all invocations before
--     proceeding execution
--
-- In the Vulkan context, this allows apps to synchronize Subgroup
-- execution flow within a Workgroup without requiring all Subgroups to
-- wait at the arrival condition before proceeding to execute independent
-- work. It also permits synchronizing memory access like other control
-- barriers.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderSplitBarrierFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderSplitBarrierPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_SPLIT_BARRIER_EXTENSION_NAME'
--
-- -   'EXT_SHADER_SPLIT_BARRIER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SPLIT_BARRIER_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2026-05-08 (Matthew Netsch)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_split_barrier Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_split_barrier  ( PhysicalDeviceShaderSplitBarrierFeaturesEXT
                                                      , PhysicalDeviceShaderSplitBarrierPropertiesEXT
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderSplitBarrierFeaturesEXT

instance ToCStruct PhysicalDeviceShaderSplitBarrierFeaturesEXT
instance Show PhysicalDeviceShaderSplitBarrierFeaturesEXT

instance FromCStruct PhysicalDeviceShaderSplitBarrierFeaturesEXT


data PhysicalDeviceShaderSplitBarrierPropertiesEXT

instance ToCStruct PhysicalDeviceShaderSplitBarrierPropertiesEXT
instance Show PhysicalDeviceShaderSplitBarrierPropertiesEXT

instance FromCStruct PhysicalDeviceShaderSplitBarrierPropertiesEXT

