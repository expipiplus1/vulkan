{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_terminate_invocation - device extension
--
-- == VK_KHR_shader_terminate_invocation
--
-- [__Name String__]
--     @VK_KHR_shader_terminate_invocation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     216
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
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_shader_terminate_invocation:%20&body=@critsec%20 >
--
-- [__Last Modified Date__]
--     2020-08-11
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires the
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_terminate_invocation.html SPV_KHR_terminate_invocation>
--         SPIR-V extension.
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jesse Hall, Google
--
--     -   Ralph Potter, Samsung
--
--     -   Tom Olson, Arm
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_terminate_invocation.html SPV_KHR_terminate_invocation>
-- SPIR-V extension. That SPIR-V extension provides a new instruction,
-- @OpTerminateInvocation@, which causes a shader invocation to immediately
-- terminate and sets the coverage of shaded samples to @0@; only
-- previously executed instructions will have observable effects. The
-- @OpTerminateInvocation@ instruction, along with the
-- @OpDemoteToHelperInvocation@ instruction from the
-- <VK_EXT_shader_demote_to_helper_invocation.html VK_EXT_shader_demote_to_helper_invocation>
-- extension, together replace the @OpKill@ instruction, which could behave
-- like either of these instructions. @OpTerminateInvocation@ provides the
-- behavior required by the GLSL @discard@ statement, and should be used
-- when available by GLSL compilers and applications that need the GLSL
-- @discard@ behavior.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderTerminateInvocationFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME'
--
-- -   'KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-08-11 (Jesse Hall)
--
-- = See Also
--
-- 'PhysicalDeviceShaderTerminateInvocationFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_terminate_invocation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_terminate_invocation  (PhysicalDeviceShaderTerminateInvocationFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderTerminateInvocationFeaturesKHR

instance ToCStruct PhysicalDeviceShaderTerminateInvocationFeaturesKHR
instance Show PhysicalDeviceShaderTerminateInvocationFeaturesKHR

instance FromCStruct PhysicalDeviceShaderTerminateInvocationFeaturesKHR

