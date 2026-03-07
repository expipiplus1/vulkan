{-# language CPP #-}
-- | = Name
--
-- VK_NV_ray_tracing_linear_swept_spheres - device extension
--
-- = VK_NV_ray_tracing_linear_swept_spheres
--
-- [__Name String__]
--     @VK_NV_ray_tracing_linear_swept_spheres@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     430
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_linear_swept_spheres.html SPV_NV_linear_swept_spheres>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_ray_tracing_linear_swept_spheres] @vkushwaha%0A*Here describe the issue or question you have about the VK_NV_ray_tracing_linear_swept_spheres extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_ray_tracing_linear_swept_spheres.adoc VK_NV_ray_tracing_linear_swept_spheres>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-03
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_linear_swept_spheres.html SPV_NV_linear_swept_spheres>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_linear_swept_spheres.txt GL_NV_linear_swept_spheres>
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
--     -   Nathan Morrical, NVIDIA
--
-- == Description
--
-- This extension adds two new primitives for ray tracing: a sphere
-- primitive and a linear swept sphere (LSS) primitive. The purpose of the
-- LSS primitive is to enable rendering of high quality hair and fur using
-- a compact primitive representation encoded in the acceleration
-- structure. Sphere primitives are defined by a position and a radius and
-- are a subset of LSS, but are useful in their own right, for example for
-- particle systems.
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_NV_linear_swept_spheres@
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryKHR':
--
--     -   'AccelerationStructureGeometryLinearSweptSpheresDataNV'
--
--     -   'AccelerationStructureGeometrySpheresDataNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV'
--
-- == New Enums
--
-- -   'RayTracingLssIndexingModeNV'
--
-- -   'RayTracingLssPrimitiveEndCapsModeNV'
--
-- == New Enum Constants
--
-- -   'NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_EXTENSION_NAME'
--
-- -   'NV_RAY_TRACING_LINEAR_SWEPT_SPHERES_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_RADIUS_BUFFER_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryTypeKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_LINEAR_SWEPT_SPHERES_NV'
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_SPHERES_NV'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_RAY_TRACING_ALLOW_SPHERES_AND_LINEAR_SWEPT_SPHERES_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_LINEAR_SWEPT_SPHERES_DATA_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_SPHERES_DATA_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_LINEAR_SWEPT_SPHERES_FEATURES_NV'
--
-- == New or Modified Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitissphere HitIsSphereNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitislss HitIsLSSNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitsphereposition HitSpherePositionNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitsphereradius HitSphereRadiusNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitlsspositions HitLSSPositionsNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-hitlssradii HitLSSRadiiNV>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-RayTracingSpheresGeometryNV RayTracingSpheresGeometryNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-RayTracingLinearSweptSpheresGeometryNV RayTracingLinearSweptSpheresGeometryNV>
--
-- == Version History
--
-- -   Revision 1, 2025-01-03 (Vikram Kushwaha)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_ray_tracing_linear_swept_spheres Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_ray_tracing_linear_swept_spheres  ( AccelerationStructureGeometryLinearSweptSpheresDataNV
                                                                 , AccelerationStructureGeometrySpheresDataNV
                                                                 , PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV
                                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AccelerationStructureGeometryLinearSweptSpheresDataNV

instance ToCStruct AccelerationStructureGeometryLinearSweptSpheresDataNV
instance Show AccelerationStructureGeometryLinearSweptSpheresDataNV


data AccelerationStructureGeometrySpheresDataNV

instance ToCStruct AccelerationStructureGeometrySpheresDataNV
instance Show AccelerationStructureGeometrySpheresDataNV


data PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV

instance ToCStruct PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV
instance Show PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV

instance FromCStruct PhysicalDeviceRayTracingLinearSweptSpheresFeaturesNV

