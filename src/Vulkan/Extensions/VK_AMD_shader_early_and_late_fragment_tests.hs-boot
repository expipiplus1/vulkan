{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_early_and_late_fragment_tests - device extension
--
-- == VK_AMD_shader_early_and_late_fragment_tests
--
-- [__Name String__]
--     @VK_AMD_shader_early_and_late_fragment_tests@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     322
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_shader_early_and_late_fragment_tests] @tobski%0A*Here describe the issue or question you have about the VK_AMD_shader_early_and_late_fragment_tests extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_AMD_shader_early_and_late_fragment_tests.adoc VK_AMD_shader_early_and_late_fragment_tests>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-14
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_shader_early_and_late_fragment_tests.html SPV_AMD_shader_early_and_late_fragment_tests>
--
--     -   This extension interacts with @VK_EXT_shader_stencil_export@
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension adds support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_shader_early_and_late_fragment_tests.html SPV_AMD_shader_early_and_late_fragment_tests>
-- extension, allowing shaders to explicitly opt in to allowing both early
-- /and/ late fragment tests with the @EarlyAndLateFragmentTestsAMD@
-- execution mode.
--
-- If @VK_EXT_shader_stencil_export@ is supported, additional execution
-- modes allowing early depth tests similar to @DepthUnchanged@,
-- @DepthLess@, and @DepthGreater@ are provided.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD'
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_EXTENSION_NAME'
--
-- -   'AMD_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_FEATURES_AMD'
--
-- == Version History
--
-- -   Revision 1, 2021-09-14 (Tobias Hector)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_shader_early_and_late_fragment_tests Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_shader_early_and_late_fragment_tests  (PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD

instance ToCStruct PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD
instance Show PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD

instance FromCStruct PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD

