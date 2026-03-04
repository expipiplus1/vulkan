{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_expect_assume - device extension
--
-- == VK_KHR_shader_expect_assume
--
-- [__Name String__]
--     @VK_KHR_shader_expect_assume@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     545
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_expect_assume.html SPV_KHR_expect_assume>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_expect_assume] @kpet%0A*Here describe the issue or question you have about the VK_KHR_shader_expect_assume extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_expect_assume.adoc VK_KHR_shader_expect_assume>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-12-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kevin Petit, Arm
--
--     -   Tobias Hector, AMD
--
--     -   James Fitzpatrick, Imagination Technologies
--
-- == Description
--
-- This extension allows the use of the @SPV_KHR_expect_assume@ extension
-- in SPIR-V shader modules which enables SPIR-V producers to provide
-- optimization hints to the Vulkan implementation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderExpectAssumeFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME'
--
-- -   'KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ExpectAssumeKHR ExpectAssumeKHR>
--
-- == Version History
--
-- -   Revision 1, 2023-12-06 (Kevin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceShaderExpectAssumeFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_expect_assume Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_expect_assume  (PhysicalDeviceShaderExpectAssumeFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderExpectAssumeFeaturesKHR

instance ToCStruct PhysicalDeviceShaderExpectAssumeFeaturesKHR
instance Show PhysicalDeviceShaderExpectAssumeFeaturesKHR

instance FromCStruct PhysicalDeviceShaderExpectAssumeFeaturesKHR

