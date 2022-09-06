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
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_terminate_invocation] @critsec%0A<<Here describe the issue or question you have about the VK_KHR_shader_terminate_invocation extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-08-11
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
--     -   Requires the
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_terminate_invocation.html SPV_KHR_terminate_invocation>
--         SPIR-V extension.
--
-- [__IP Status__]
--     No known IP claims.
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
-- @VK_EXT_shader_demote_to_helper_invocation@ extension, together replace
-- the @OpKill@ instruction, which could behave like either of these
-- instructions. @OpTerminateInvocation@ provides the behavior required by
-- the GLSL @discard@ statement, and should be used when available by GLSL
-- compilers and applications that need the GLSL @discard@ behavior.
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
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- KHR suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2020-08-11 (Jesse Hall)
--
-- == See Also
--
-- 'PhysicalDeviceShaderTerminateInvocationFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_terminate_invocation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_terminate_invocation  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR
                                                             , PhysicalDeviceShaderTerminateInvocationFeaturesKHR
                                                             , KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION
                                                             , pattern KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION
                                                             , KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME
                                                             , pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME
                                                             ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_KHR_shader_terminate_invocation (PhysicalDeviceShaderTerminateInvocationFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceShaderTerminateInvocationFeaturesKHR"
type PhysicalDeviceShaderTerminateInvocationFeaturesKHR = PhysicalDeviceShaderTerminateInvocationFeatures


type KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION"
pattern KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_TERMINATE_INVOCATION_SPEC_VERSION = 1


type KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME = "VK_KHR_shader_terminate_invocation"

-- No documentation found for TopLevel "VK_KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME"
pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_TERMINATE_INVOCATION_EXTENSION_NAME = "VK_KHR_shader_terminate_invocation"

