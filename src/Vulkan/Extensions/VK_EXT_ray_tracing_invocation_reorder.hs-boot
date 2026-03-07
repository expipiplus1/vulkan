{-# language CPP #-}
-- | = Name
--
-- VK_EXT_ray_tracing_invocation_reorder - device extension
--
-- = VK_EXT_ray_tracing_invocation_reorder
--
-- [__Name String__]
--     @VK_EXT_ray_tracing_invocation_reorder@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     582
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_invocation_reorder.html SPV_EXT_shader_invocation_reorder>
--
-- [__Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_ray_tracing_invocation_reorder] @ewerness-nv%0A*Here describe the issue or question you have about the VK_EXT_ray_tracing_invocation_reorder extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_ray_tracing_invocation_reorder.adoc VK_EXT_ray_tracing_invocation_reorder>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-11-12
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_shader_invocation_reorder.txt GL_EXT_shader_invocation_reorder>
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Stu Smith, AMD
--
--     -   Aaron Hagan, AMD
--
--     -   Tyler Nowicki, AMD
--
--     -   Sebastian Neubauer, AMD
--
--     -   Radoslaw Drabinski, Intel
--
--     -   Sven Woop, Intel
--
--     -   Aleksandra Krstic, QUALCOMM
--
--     -   Andrew Garrard, Imagination Technologies
--
--     -   Mathieu Robart, Arm Limited
--
--     -   Tom Olson, Khronos
--
--     -   Ralph Potter, Samsung Electronics
--
--     -   Antonio Caggiano, LunarG
--
-- == Description
--
-- The ray tracing pipeline API provides some ability to reorder for
-- locality, but it is useful to have more control over how the reordering
-- happens and what information is included in the reordering. The shader
-- API provides a hit object to contain result information from the hit
-- which can be used as part of the explicit sorting plus options that
-- contain an integer for hint bits to use to add more coherency.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingInvocationReorderFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRayTracingInvocationReorderPropertiesEXT'
--
-- == New Enums
--
-- -   'RayTracingInvocationReorderModeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME'
--
-- -   'EXT_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2025-11-12 (Eric Werness)
--
--     -   Internal development - forked from NV
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_ray_tracing_invocation_reorder Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_ray_tracing_invocation_reorder  ( PhysicalDeviceRayTracingInvocationReorderFeaturesEXT
                                                                , PhysicalDeviceRayTracingInvocationReorderPropertiesEXT
                                                                , RayTracingInvocationReorderModeEXT
                                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRayTracingInvocationReorderFeaturesEXT

instance ToCStruct PhysicalDeviceRayTracingInvocationReorderFeaturesEXT
instance Show PhysicalDeviceRayTracingInvocationReorderFeaturesEXT

instance FromCStruct PhysicalDeviceRayTracingInvocationReorderFeaturesEXT


data PhysicalDeviceRayTracingInvocationReorderPropertiesEXT

instance ToCStruct PhysicalDeviceRayTracingInvocationReorderPropertiesEXT
instance Show PhysicalDeviceRayTracingInvocationReorderPropertiesEXT

instance FromCStruct PhysicalDeviceRayTracingInvocationReorderPropertiesEXT


data RayTracingInvocationReorderModeEXT

