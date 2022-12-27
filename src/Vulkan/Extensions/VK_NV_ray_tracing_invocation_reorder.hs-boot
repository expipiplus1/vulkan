{-# language CPP #-}
-- | = Name
--
-- VK_NV_ray_tracing_invocation_reorder - device extension
--
-- == VK_NV_ray_tracing_invocation_reorder
--
-- [__Name String__]
--     @VK_NV_ray_tracing_invocation_reorder@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     491
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_ray_tracing_pipeline@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_ray_tracing_invocation_reorder] @ewerness-nv%0A*Here describe the issue or question you have about the VK_NV_ray_tracing_invocation_reorder extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-11-02
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_invocation_reorder.html SPV_NV_shader_invocation_reorder>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_shader_invocation_reorder.txt GL_NV_shader_invocation_reorder>
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
-- == Description
--
-- The ray tracing pipeline API provides some ability to reorder for
-- locality, but it is useful to have more control over how the reordering
-- happens and what information is included in the reordering. The shader
-- API provides a hit object to contain result information from the hit
-- which can be used as part of the explicit sorting plus options that
-- contain an integer for hint bits to use to add more locality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingInvocationReorderFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRayTracingInvocationReorderPropertiesNV'
--
-- == New Enums
--
-- -   'RayTracingInvocationReorderModeNV'
--
-- == New Enum Constants
--
-- -   'NV_RAY_TRACING_INVOCATION_REORDER_EXTENSION_NAME'
--
-- -   'NV_RAY_TRACING_INVOCATION_REORDER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2020-09-12 (Eric Werness, Ashwin Lele)
--
--     -   Initial external release
--
-- == See Also
--
-- 'PhysicalDeviceRayTracingInvocationReorderFeaturesNV',
-- 'PhysicalDeviceRayTracingInvocationReorderPropertiesNV',
-- 'RayTracingInvocationReorderModeNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_ray_tracing_invocation_reorder Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_ray_tracing_invocation_reorder  ( PhysicalDeviceRayTracingInvocationReorderFeaturesNV
                                                               , PhysicalDeviceRayTracingInvocationReorderPropertiesNV
                                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRayTracingInvocationReorderFeaturesNV

instance ToCStruct PhysicalDeviceRayTracingInvocationReorderFeaturesNV
instance Show PhysicalDeviceRayTracingInvocationReorderFeaturesNV

instance FromCStruct PhysicalDeviceRayTracingInvocationReorderFeaturesNV


data PhysicalDeviceRayTracingInvocationReorderPropertiesNV

instance ToCStruct PhysicalDeviceRayTracingInvocationReorderPropertiesNV
instance Show PhysicalDeviceRayTracingInvocationReorderPropertiesNV

instance FromCStruct PhysicalDeviceRayTracingInvocationReorderPropertiesNV

