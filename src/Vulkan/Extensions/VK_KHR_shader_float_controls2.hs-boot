{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_float_controls2 - device extension
--
-- == VK_KHR_shader_float_controls2
--
-- [__Name String__]
--     @VK_KHR_shader_float_controls2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     529
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_float_controls VK_KHR_shader_float_controls>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_float_controls2.html SPV_KHR_float_controls2>
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_float_controls2] @gnl21%0A*Here describe the issue or question you have about the VK_KHR_shader_float_controls2 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_float_controls2.adoc VK_KHR_shader_float_controls2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-16
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_float_controls2.html SPV_KHR_float_controls2>.
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
-- == Description
--
-- This extension enables use of the more expressive fast floating-point
-- math flags in the SPV_KHR_float_controls2 extension. These flags give
-- finer- grained control over which optimisations compilers may apply,
-- potentially speeding up execution while retaining correct results.
--
-- The extension also adds control over the fast-math modes to the GLSL
-- extended instruction set, making these operations more consistent with
-- SPIR-V and allowing their use in situations where floating-point
-- conformance is important.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderFloatControls2FeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_FLOAT_CONTROLS_2_EXTENSION_NAME'
--
-- -   'KHR_SHADER_FLOAT_CONTROLS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT_CONTROLS_2_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2023-05-16 (Graeme Leese)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceShaderFloatControls2FeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_float_controls2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_float_controls2  (PhysicalDeviceShaderFloatControls2FeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderFloatControls2FeaturesKHR

instance ToCStruct PhysicalDeviceShaderFloatControls2FeaturesKHR
instance Show PhysicalDeviceShaderFloatControls2FeaturesKHR

instance FromCStruct PhysicalDeviceShaderFloatControls2FeaturesKHR

