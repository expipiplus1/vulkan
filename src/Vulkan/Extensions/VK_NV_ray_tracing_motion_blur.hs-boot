{-# language CPP #-}
-- | = Name
--
-- VK_NV_ray_tracing_motion_blur - device extension
--
-- == VK_NV_ray_tracing_motion_blur
--
-- [__Name String__]
--     @VK_NV_ray_tracing_motion_blur@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     328
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_ray_tracing_pipeline@
--
-- [__Contact__]
--
--     -   Eric Werness
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-16
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_ray_tracing_motion_blur.html SPV_NV_ray_tracing_motion_blur>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_ray_tracing_motion_blur.txt GL_NV_ray_tracing_motion_blur>
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
-- == Description
--
-- Ray tracing support in the API provides an efficient mechanism to
-- intersect rays against static geometry, but rendering algorithms often
-- want to support motion, which is more efficiently supported with
-- motion-specific algorithms. This extension adds a set of mechanisms to
-- support fast tracing of moving geometry:
--
-- -   A ray pipeline trace call which takes a time parameter
--
-- -   Flags to enable motion support in an acceleration structure
--
-- -   Support for time-varying vertex positions in a geometry
--
-- -   Motion instances to move existing instances over time
--
-- The motion represented here is parameterized across a normalized
-- timestep between 0.0 and 1.0. A motion trace using @OpTraceRayMotionNV@
-- provides a time within that normalized range to be used when
-- intersecting that ray with geometry. The geometry can be provided with
-- motion by a combination of adding a second vertex position for time of
-- 1.0 using 'AccelerationStructureGeometryMotionTrianglesDataNV' and
-- providing multiple transforms in the instance using
-- 'AccelerationStructureMotionInstanceNV'.
--
-- == New Structures
--
-- -   'AccelerationStructureMatrixMotionInstanceNV'
--
-- -   'AccelerationStructureMotionInstanceNV'
--
-- -   'AccelerationStructureSRTMotionInstanceNV'
--
-- -   'SRTDataNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR':
--
--     -   'AccelerationStructureMotionInfoNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR':
--
--     -   'AccelerationStructureGeometryMotionTrianglesDataNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingMotionBlurFeaturesNV'
--
-- == New Unions
--
-- -   'AccelerationStructureMotionInstanceDataNV'
--
-- == New Enums
--
-- -   'AccelerationStructureMotionInstanceTypeNV'
--
-- == New Bitmasks
--
-- -   'AccelerationStructureMotionInfoFlagsNV'
--
-- -   'AccelerationStructureMotionInstanceFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_RAY_TRACING_MOTION_BLUR_EXTENSION_NAME'
--
-- -   'NV_RAY_TRACING_MOTION_BLUR_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_CREATE_MOTION_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_MOTION_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MOTION_TRIANGLES_DATA_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MOTION_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MOTION_BLUR_FEATURES_NV'
--
-- == Issues
--
-- (1) What size is VkAccelerationStructureMotionInstanceNV?
--
-- -   Added a note on the structure size and made the stride explicit in
--     the language.
--
-- (2) Allow arrayOfPointers for motion TLAS?
--
-- -   Yes, with a packed encoding to minimize the amount of data sent for
--     metadata.
--
-- == Version History
--
-- -   Revision 1, 2020-06-16 (Eric Werness, Ashwin Lele)
--
--     -   Initial external release
--
-- == See Also
--
-- 'AccelerationStructureGeometryMotionTrianglesDataNV',
-- 'AccelerationStructureMatrixMotionInstanceNV',
-- 'AccelerationStructureMotionInfoFlagsNV',
-- 'AccelerationStructureMotionInfoNV',
-- 'AccelerationStructureMotionInstanceDataNV',
-- 'AccelerationStructureMotionInstanceFlagsNV',
-- 'AccelerationStructureMotionInstanceNV',
-- 'AccelerationStructureMotionInstanceTypeNV',
-- 'AccelerationStructureSRTMotionInstanceNV',
-- 'PhysicalDeviceRayTracingMotionBlurFeaturesNV', 'SRTDataNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_ray_tracing_motion_blur  ( AccelerationStructureGeometryMotionTrianglesDataNV
                                                        , AccelerationStructureMatrixMotionInstanceNV
                                                        , AccelerationStructureMotionInfoNV
                                                        , AccelerationStructureMotionInstanceNV
                                                        , AccelerationStructureSRTMotionInstanceNV
                                                        , PhysicalDeviceRayTracingMotionBlurFeaturesNV
                                                        , SRTDataNV
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AccelerationStructureGeometryMotionTrianglesDataNV

instance ToCStruct AccelerationStructureGeometryMotionTrianglesDataNV
instance Show AccelerationStructureGeometryMotionTrianglesDataNV


data AccelerationStructureMatrixMotionInstanceNV

instance ToCStruct AccelerationStructureMatrixMotionInstanceNV
instance Show AccelerationStructureMatrixMotionInstanceNV

instance FromCStruct AccelerationStructureMatrixMotionInstanceNV


data AccelerationStructureMotionInfoNV

instance ToCStruct AccelerationStructureMotionInfoNV
instance Show AccelerationStructureMotionInfoNV

instance FromCStruct AccelerationStructureMotionInfoNV


data AccelerationStructureMotionInstanceNV

instance ToCStruct AccelerationStructureMotionInstanceNV
instance Show AccelerationStructureMotionInstanceNV


data AccelerationStructureSRTMotionInstanceNV

instance ToCStruct AccelerationStructureSRTMotionInstanceNV
instance Show AccelerationStructureSRTMotionInstanceNV

instance FromCStruct AccelerationStructureSRTMotionInstanceNV


data PhysicalDeviceRayTracingMotionBlurFeaturesNV

instance ToCStruct PhysicalDeviceRayTracingMotionBlurFeaturesNV
instance Show PhysicalDeviceRayTracingMotionBlurFeaturesNV

instance FromCStruct PhysicalDeviceRayTracingMotionBlurFeaturesNV


data SRTDataNV

instance ToCStruct SRTDataNV
instance Show SRTDataNV

instance FromCStruct SRTDataNV

