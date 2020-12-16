{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_demote_to_helper_invocation - device extension
--
-- == VK_EXT_shader_demote_to_helper_invocation
--
-- [__Name String__]
--     @VK_EXT_shader_demote_to_helper_invocation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     277
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
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_shader_demote_to_helper_invocation:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_demote_to_helper_invocation.html SPV_EXT_demote_to_helper_invocation>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_demote_to_helper_invocation.html SPV_EXT_demote_to_helper_invocation>
-- SPIR-V extension. That SPIR-V extension provides a new instruction
-- @OpDemoteToHelperInvocationEXT@ allowing shaders to \"demote\" a
-- fragment shader invocation to behave like a helper invocation for its
-- duration. The demoted invocation will have no further side effects and
-- will not output to the framebuffer, but remains active and can
-- participate in computing derivatives and in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-group-operations group operations>.
-- This is a better match for the \"discard\" instruction in HLSL.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_EXTENSION_NAME'
--
-- -   'EXT_SHADER_DEMOTE_TO_HELPER_INVOCATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT'
--
-- == New SPIR-V Capability
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DemoteToHelperInvocationEXT DemoteToHelperInvocationEXT>
--
-- == Version History
--
-- -   Revision 1, 2019-06-01 (Jeff Bolz)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_demote_to_helper_invocation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation  (PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT

instance ToCStruct PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
instance Show PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT

instance FromCStruct PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT

