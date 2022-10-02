{-# language CPP #-}
-- | = Name
--
-- VK_EXT_opacity_micromap - device extension
--
-- == VK_EXT_opacity_micromap
--
-- [__Name String__]
--     @VK_EXT_opacity_micromap@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     397
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_acceleration_structure@ to be enabled for any
--         device-level functionality
--
--     -   Requires @VK_KHR_synchronization2@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Christoph Kubisch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_opacity_micromap] @pixeljetstream%0A*Here describe the issue or question you have about the VK_EXT_opacity_micromap extension* >
--
--     -   Eric Werness
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_opacity_micromap.adoc VK_EXT_opacity_micromap>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-08-24
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_ray_tracing_opacity_micromap.html SPV_EXT_ray_tracing_opacity_micromap>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/EXT/GLSL_EXT_ray_tracing_opacity_micromap.txt GLSL_EXT_ray_tracing_opacity_micromap>
--
-- [__Contributors__]
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Josh Barczak, Intel
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- When adding adding transparency to a ray traced scene, an application
-- can choose between further tessellating the geometry or using an any hit
-- shader to allow the ray through specific parts of the geometry. These
-- options have the downside of either significantly increasing memory
-- consumption or adding runtime overhead to run shader code in the middle
-- of traversal, respectively.
--
-- This extension adds the ability to add an /opacity micromap/ to geometry
-- when building an acceleration structure. The opacity micromap compactly
-- encodes opacity information which can be read by the implementation to
-- mark parts of triangles as opaque or transparent. The format is
-- externally visible to allow the application to compress its internal
-- geometry and surface representations into the compressed format ahead of
-- time. The compressed format subdivides each triangle into a set of
-- subtriangles, each of which can be assigned either two or four opacity
-- values. These opacity values can control if a ray hitting that
-- subtriangle is treated as an opaque hit, complete miss, or possible hit,
-- depending on the controls described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#ray-opacity-micromap Ray Opacity Micromap>.
--
-- This extension provides:
--
-- -   a 'Vulkan.Extensions.Handles.MicromapEXT' structure to store the
--     micromap,
--
-- -   functions similar to acceleration structure build functions to build
--     the opacity micromap array, and
--
-- -   a structure to extend
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'
--     to attach a micromap to the geometry of the acceleration structure.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.MicromapEXT'
--
-- == New Commands
--
-- -   'buildMicromapsEXT'
--
-- -   'cmdBuildMicromapsEXT'
--
-- -   'cmdCopyMemoryToMicromapEXT'
--
-- -   'cmdCopyMicromapEXT'
--
-- -   'cmdCopyMicromapToMemoryEXT'
--
-- -   'cmdWriteMicromapsPropertiesEXT'
--
-- -   'copyMemoryToMicromapEXT'
--
-- -   'copyMicromapEXT'
--
-- -   'copyMicromapToMemoryEXT'
--
-- -   'createMicromapEXT'
--
-- -   'destroyMicromapEXT'
--
-- -   'getDeviceMicromapCompatibilityEXT'
--
-- -   'getMicromapBuildSizesEXT'
--
-- -   'writeMicromapsPropertiesEXT'
--
-- == New Structures
--
-- -   'CopyMemoryToMicromapInfoEXT'
--
-- -   'CopyMicromapInfoEXT'
--
-- -   'CopyMicromapToMemoryInfoEXT'
--
-- -   'MicromapBuildInfoEXT'
--
-- -   'MicromapBuildSizesInfoEXT'
--
-- -   'MicromapCreateInfoEXT'
--
-- -   'MicromapTriangleEXT'
--
-- -   'MicromapUsageEXT'
--
-- -   'MicromapVersionInfoEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR':
--
--     -   'AccelerationStructureTrianglesOpacityMicromapEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceOpacityMicromapFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceOpacityMicromapPropertiesEXT'
--
-- == New Enums
--
-- -   'BuildMicromapFlagBitsEXT'
--
-- -   'BuildMicromapModeEXT'
--
-- -   'CopyMicromapModeEXT'
--
-- -   'MicromapCreateFlagBitsEXT'
--
-- -   'MicromapTypeEXT'
--
-- -   'OpacityMicromapFormatEXT'
--
-- -   'OpacityMicromapSpecialIndexEXT'
--
-- == New Bitmasks
--
-- -   'BuildMicromapFlagsEXT'
--
-- -   'MicromapCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_OPACITY_MICROMAP_EXTENSION_NAME'
--
-- -   'EXT_OPACITY_MICROMAP_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_MICROMAP_STORAGE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_DISABLE_OPACITY_MICROMAPS_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_OPACITY_MICROMAP_DATA_UPDATE_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_OPACITY_MICROMAP_UPDATE_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_INSTANCE_DISABLE_OPACITY_MICROMAPS_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_INSTANCE_FORCE_OPACITY_MICROMAP_2_STATE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_MICROMAP_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT'
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_MICROMAP_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MICROMAP_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MICROMAP_TO_MEMORY_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MICROMAP_BUILD_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MICROMAP_BUILD_SIZES_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MICROMAP_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MICROMAP_VERSION_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_EXT'
--
-- == Reference code
--
-- > uint32_t BarycentricsToSpaceFillingCurveIndex(float u, float v, uint32_t level)
-- > {
-- >     u = clamp(u, 0.0f, 1.0f);
-- >     v = clamp(v, 0.0f, 1.0f);
-- >
-- >     uint32_t iu, iv, iw;
-- >
-- >     // Quantize barycentric coordinates
-- >     float fu = u * (1u << level);
-- >     float fv = v * (1u << level);
-- >
-- >     iu = (uint32_t)fu;
-- >     iv = (uint32_t)fv;
-- >
-- >     float uf = fu - float(iu);
-- >     float vf = fv - float(iv);
-- >
-- >     if (iu >= (1u << level)) iu = (1u << level) - 1u;
-- >     if (iv >= (1u << level)) iv = (1u << level) - 1u;
-- >
-- >     uint32_t iuv = iu + iv;
-- >
-- >     if (iuv >= (1u << level))
-- >         iu -= iuv - (1u << level) + 1u;
-- >
-- >     iw = ~(iu + iv);
-- >
-- >     if (uf + vf >= 1.0f && iuv < (1u link:https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html# level) - 1u) --iw;
-- >
-- >     uint32_t b0 = ~(iu ^ iw);
-- >     b0 &= ((1u << level) - 1u);
-- >     uint32_t t = (iu ^ iv) & b0;
-- >
-- >     uint32_t f = t;
-- >     f ^= f [^] 1u;
-- >     f ^= f >> 2u;
-- >     f ^= f >> 4u;
-- >     f ^= f >> 8u;
-- >     uint32_t b1 = ((f ^ iu) & ~b0) | t;
-- >
-- >     // Interleave bits
-- >     b0 = (b0 | (b0 << 8u)) & 0x00ff00ffu;
-- >     b0 = (b0 | (b0 << 4u)) & 0x0f0f0f0fu;
-- >     b0 = (b0 | (b0 << 2u)) & 0x33333333u;
-- >     b0 = (b0 | (b0 << 1u)) & 0x55555555u;
-- >     b1 = (b1 | (b1 << 8u)) & 0x00ff00ffu;
-- >     b1 = (b1 | (b1 << 4u)) & 0x0f0f0f0fu;
-- >     b1 = (b1 | (b1 << 2u)) & 0x33333333u;
-- >     b1 = (b1 | (b1 << 1u)) & 0x55555555u;
-- >
-- >     return b0 | (b1 << 1u);
-- > }
--
-- == Issues
--
-- (1) Is the build actually similar to an acceleration structure build?
--
-- -   Resolved: The build should be much lighter-weight than an
--     acceleration structure build, but the infrastructure is similar
--     enough that it makes sense to keep the concepts compatible.
--
-- (2) Why does VkMicromapUsageEXT not have type\/pNext?
--
-- -   Resolved: There can be a very large number of these structures, so
--     doubling the size of these can be significant memory consumption.
--     Also, an application may be loading these directly from a file which
--     is more compatible with it being a flat structure. The including
--     structures are extensible and are probably a more suitable place to
--     add extensibility.
--
-- (3) Why is there a SPIR-V extension?
--
-- -   Resolved: There is a ray flag. To be consistent with how the
--     existing ray tracing extensions work that ray flag needs its own
--     extension.
--
-- (4) Should there be indirect micromap build?
--
-- -   Resolved: Not for now. There is more in-depth usage metadata
--     required and it seems less likely that something like a GPU culling
--     system would need to change the counts for a micromap.
--
-- (5) Should micromaps have a micromap device address?
--
-- -   Resolved: There is no need right now (can just use the handle) but
--     that is a bit different from acceleration structures, though the two
--     are not completely parallel in their usage.
--
-- (6) Why are the alignment requirements defined as a mix of hardcoded
-- values and caps?
--
-- -   Resolved: This is most parallel with the definition of
--     @VK_KHR_acceleration_structure@ and maintaining commonality makes it
--     easier for applications to share memory.
--
-- == Version History
--
-- -   Revision 2, 2022-06-22 (Eric Werness)
--
--     -   EXTify and clean up for discussion
--
-- -   Revision 1, 2022-01-01 (Eric Werness)
--
--     -   Initial revision
--
-- == See Also
--
-- 'AccelerationStructureTrianglesOpacityMicromapEXT',
-- 'BuildMicromapFlagBitsEXT', 'BuildMicromapFlagsEXT',
-- 'BuildMicromapModeEXT', 'CopyMemoryToMicromapInfoEXT',
-- 'CopyMicromapInfoEXT', 'CopyMicromapModeEXT',
-- 'CopyMicromapToMemoryInfoEXT', 'MicromapBuildInfoEXT',
-- 'MicromapBuildSizesInfoEXT', 'MicromapCreateFlagBitsEXT',
-- 'MicromapCreateFlagsEXT', 'MicromapCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.MicromapEXT', 'MicromapTriangleEXT',
-- 'MicromapTypeEXT', 'MicromapUsageEXT', 'MicromapVersionInfoEXT',
-- 'OpacityMicromapFormatEXT', 'OpacityMicromapSpecialIndexEXT',
-- 'PhysicalDeviceOpacityMicromapFeaturesEXT',
-- 'PhysicalDeviceOpacityMicromapPropertiesEXT', 'buildMicromapsEXT',
-- 'cmdBuildMicromapsEXT', 'cmdCopyMemoryToMicromapEXT',
-- 'cmdCopyMicromapEXT', 'cmdCopyMicromapToMemoryEXT',
-- 'cmdWriteMicromapsPropertiesEXT', 'copyMemoryToMicromapEXT',
-- 'copyMicromapEXT', 'copyMicromapToMemoryEXT', 'createMicromapEXT',
-- 'destroyMicromapEXT', 'getDeviceMicromapCompatibilityEXT',
-- 'getMicromapBuildSizesEXT', 'writeMicromapsPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_opacity_micromap Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_opacity_micromap  ( AccelerationStructureTrianglesOpacityMicromapEXT
                                                  , CopyMemoryToMicromapInfoEXT
                                                  , CopyMicromapInfoEXT
                                                  , CopyMicromapToMemoryInfoEXT
                                                  , MicromapBuildInfoEXT
                                                  , MicromapBuildSizesInfoEXT
                                                  , MicromapCreateInfoEXT
                                                  , MicromapTriangleEXT
                                                  , MicromapUsageEXT
                                                  , MicromapVersionInfoEXT
                                                  , PhysicalDeviceOpacityMicromapFeaturesEXT
                                                  , PhysicalDeviceOpacityMicromapPropertiesEXT
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AccelerationStructureTrianglesOpacityMicromapEXT

instance ToCStruct AccelerationStructureTrianglesOpacityMicromapEXT
instance Show AccelerationStructureTrianglesOpacityMicromapEXT


data CopyMemoryToMicromapInfoEXT

instance ToCStruct CopyMemoryToMicromapInfoEXT
instance Show CopyMemoryToMicromapInfoEXT


data CopyMicromapInfoEXT

instance ToCStruct CopyMicromapInfoEXT
instance Show CopyMicromapInfoEXT

instance FromCStruct CopyMicromapInfoEXT


data CopyMicromapToMemoryInfoEXT

instance ToCStruct CopyMicromapToMemoryInfoEXT
instance Show CopyMicromapToMemoryInfoEXT


data MicromapBuildInfoEXT

instance ToCStruct MicromapBuildInfoEXT
instance Show MicromapBuildInfoEXT


data MicromapBuildSizesInfoEXT

instance ToCStruct MicromapBuildSizesInfoEXT
instance Show MicromapBuildSizesInfoEXT

instance FromCStruct MicromapBuildSizesInfoEXT


data MicromapCreateInfoEXT

instance ToCStruct MicromapCreateInfoEXT
instance Show MicromapCreateInfoEXT

instance FromCStruct MicromapCreateInfoEXT


data MicromapTriangleEXT

instance ToCStruct MicromapTriangleEXT
instance Show MicromapTriangleEXT

instance FromCStruct MicromapTriangleEXT


data MicromapUsageEXT

instance ToCStruct MicromapUsageEXT
instance Show MicromapUsageEXT

instance FromCStruct MicromapUsageEXT


data MicromapVersionInfoEXT

instance ToCStruct MicromapVersionInfoEXT
instance Show MicromapVersionInfoEXT

instance FromCStruct MicromapVersionInfoEXT


data PhysicalDeviceOpacityMicromapFeaturesEXT

instance ToCStruct PhysicalDeviceOpacityMicromapFeaturesEXT
instance Show PhysicalDeviceOpacityMicromapFeaturesEXT

instance FromCStruct PhysicalDeviceOpacityMicromapFeaturesEXT


data PhysicalDeviceOpacityMicromapPropertiesEXT

instance ToCStruct PhysicalDeviceOpacityMicromapPropertiesEXT
instance Show PhysicalDeviceOpacityMicromapPropertiesEXT

instance FromCStruct PhysicalDeviceOpacityMicromapPropertiesEXT

