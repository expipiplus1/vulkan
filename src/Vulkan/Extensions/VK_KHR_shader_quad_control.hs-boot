{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_quad_control - device extension
--
-- = VK_KHR_shader_quad_control
--
-- [__Name String__]
--     @VK_KHR_shader_quad_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     236
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--              and
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vulkan_memory_model VK_KHR_vulkan_memory_model>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_maximal_reconvergence VK_KHR_shader_maximal_reconvergence>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_quad_control.html SPV_KHR_quad_control>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_quad_control] @tobski%0A*Here describe the issue or question you have about the VK_KHR_shader_quad_control extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_quad_control.adoc VK_KHR_shader_quad_control>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-11-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Bill Licea-Kane, Qualcomm
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Nicolai Hähnle, AMD
--
--     -   Jeff Bolz, NVidia
--
--     -   Alan Baker, Google
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- This extension adds new quad any\/all operations, requires that
-- derivatives are well-defined in quad-uniform control flow, and adds the
-- ability to require helper invocations participate in group operations.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderQuadControlFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_QUAD_CONTROL_EXTENSION_NAME'
--
-- -   'KHR_SHADER_QUAD_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_QUAD_CONTROL_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-QuadControlKHR QuadControlKHR>
--
-- == Version History
--
-- -   Revision 1, 2023-11-01 (Tobias Hector)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_quad_control Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_quad_control  (PhysicalDeviceShaderQuadControlFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderQuadControlFeaturesKHR

instance ToCStruct PhysicalDeviceShaderQuadControlFeaturesKHR
instance Show PhysicalDeviceShaderQuadControlFeaturesKHR

instance FromCStruct PhysicalDeviceShaderQuadControlFeaturesKHR

