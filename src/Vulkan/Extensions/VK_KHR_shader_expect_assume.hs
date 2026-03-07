{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_expect_assume - device extension
--
-- = VK_KHR_shader_expect_assume
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
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_expect_assume.html SPV_KHR_expect_assume>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
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
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-ExpectAssumeKHR ExpectAssumeKHR>
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- KHR suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2023-12-06 (Kevin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_expect_assume Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_expect_assume  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES_KHR
                                                      , PhysicalDeviceShaderExpectAssumeFeaturesKHR
                                                      , KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION
                                                      , pattern KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION
                                                      , KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME
                                                      , pattern KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME
                                                      ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_KHR_shader_expect_assumeRoadmap (PhysicalDeviceShaderExpectAssumeFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EXPECT_ASSUME_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceShaderExpectAssumeFeaturesKHR"
type PhysicalDeviceShaderExpectAssumeFeaturesKHR = PhysicalDeviceShaderExpectAssumeFeatures


type KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION"
pattern KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_EXPECT_ASSUME_SPEC_VERSION = 1


type KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME = "VK_KHR_shader_expect_assume"

-- No documentation found for TopLevel "VK_KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME"
pattern KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_EXPECT_ASSUME_EXTENSION_NAME = "VK_KHR_shader_expect_assume"

