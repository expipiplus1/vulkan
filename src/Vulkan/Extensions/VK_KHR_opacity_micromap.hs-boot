{-# language CPP #-}
-- | = Name
--
-- VK_KHR_opacity_micromap - device extension
--
-- = VK_KHR_opacity_micromap
--
-- [__Name String__]
--     @VK_KHR_opacity_micromap@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     624
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_address_commands VK_KHR_device_address_commands>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_4
--
--     -   Interacts with VK_EXT_shader_object
--
--     -   Interacts with VK_KHR_maintenance5
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_opacity_micromap.html SPV_EXT_opacity_micromap>
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_opacity_micromap.html SPV_KHR_opacity_micromap>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_opacity_micromap] @mnetsch%0A*Here describe the issue or question you have about the VK_KHR_opacity_micromap extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_opacity_micromap.adoc VK_KHR_opacity_micromap>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-05-08
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_opacity_micromap.txt GLSL_EXT_opacity_micromap>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_opacity_micromap_ray_query_mode.txt GLSL_EXT_opacity_micromap_ray_query_mode>
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
--     -   Aleksandra Krstic, Qualcomm Technologies, Inc
--
--     -   Eric Werness, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Stu Smith, AMD
--
--     -   Sven Woop, Intel
--
--     -   Anton Berko, MediaTek
--
--     -   Radoslaw Drabinski, Intel
--
--     -   Simon Fenney, Imagination Technologies
--
--     -   Andrew Garrard, Imagination Technologies
--
--     -   Dae Kim, Imagination Technologies
--
--     -   Fred Saucedo, Qualcomm Technologies, Inc
--
--     -   Ramesh babu Admimula, Qualcomm Technologies, Inc
--
--     -   Zedian Zhang, Qualcomm Technologies, Inc
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Spencer Fricke, LunarG
--
--     -   Revanth Ponna, Qualcomm Technologies, Inc
--
--     -   Contributors to @VK_EXT_opacity_micromap@
--
-- == Description
--
-- When adding transparency to a ray traced scene, an application can
-- choose between further tessellating the geometry or using an any-hit
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#ray-opacity-micromap Ray Opacity Micromap>.
--
-- This extension provides:
--
-- -   the ability to create micromaps as
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' structures
--
-- -   the ability to build, copy, and query micromaps with the
--     acceleration structure functions, and
--
-- -   a structure to extend
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'
--     to attach a micromap to the geometry of the acceleration structure.
--
-- == New Structures
--
-- -   'MicromapTriangleKHR'
--
-- -   'MicromapUsageKHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryKHR':
--
--     -   'AccelerationStructureGeometryMicromapDataKHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR',
--     'Vulkan.Extensions.VK_AMDX_dense_geometry_format.AccelerationStructureDenseGeometryFormatTrianglesDataAMDX':
--
--     -   'AccelerationStructureTrianglesOpacityMicromapKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceOpacityMicromapFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceOpacityMicromapPropertiesKHR'
--
-- == New Enums
--
-- -   'AccelerationStructureSerializedBlockTypeKHR'
--
-- -   'OpacityMicromapFormatKHR'
--
-- -   'OpacityMicromapSpecialIndexKHR'
--
-- == New Enum Constants
--
-- -   'KHR_OPACITY_MICROMAP_EXTENSION_NAME'
--
-- -   'KHR_OPACITY_MICROMAP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureTypeKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_TYPE_OPACITY_MICROMAP_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_DISABLE_OPACITY_MICROMAPS_BIT_KHR'
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_OPACITY_MICROMAP_UPDATE_BIT_KHR'
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_MICROMAP_LOSSY_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_INSTANCE_DISABLE_OPACITY_MICROMAPS_BIT_KHR'
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_INSTANCE_FORCE_OPACITY_MICROMAP_2_STATE_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryTypeKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_MICROMAP_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MICROMAP_DATA_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagBitsEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_OPACITY_MICROMAP_DISALLOW_MIXED_SPECIAL_INDEX_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4 Vulkan Version 1.4>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_OPACITY_MICROMAP_DISALLOW_MIXED_SPECIAL_INDEX_BIT_KHR'
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_KHR'
--
-- == Reference Code
--
-- The following code illustrates an algorithm that converts the
-- barycentric coordinates inside a triangle into an OMM array index:
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
-- >     if (uf + vf >= 1.0f && iuv < (1u << level) - 1u) --iw;
-- >
-- >     uint32_t b0 = ~(iu ^ iw);
-- >     b0 &= ((1u << level) - 1u);
-- >     uint32_t t = (iu ^ iv) & b0;
-- >
-- >     uint32_t f = t;
-- >     f ^= f >> 1u;
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
--     acceleration structure build
--
-- (2) Why does VkMicromapUsageKHR not have type\/pNext?
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
-- -   Resolved: There is a ray flag and an execution mode. To be
--     consistent with how the existing ray tracing extensions work that
--     these needs its own extension.
--
-- (4) Should there be indirect micromap build?
--
-- -   Resolved: Not for now. There is more in-depth usage metadata
--     required and it seems less likely that something like a GPU culling
--     system would need to change the counts for a micromap.
--
-- (5) Should the feature struct be aliased with
-- VkPhysicalDeviceOpacityMicromapFeaturesEXT?
--
-- -   Resolved: No. This extension is not an exact promotion of
--     @VK_EXT_opacity_micromap@ and provides significantly different
--     functionality.
--
-- (6) Should micromaps API be similar to the @VK_EXT_opacity_micromap@?
--
-- @VK_EXT_opacity_micromap@ introduced almost an identical set of
-- functionality for micromaps as acceleration structures. Should this
-- promotion fold in micromaps as an acceleration structure type?
--
-- -   Resolved: Yes. While this is significant API breakage from the EXT,
--     it is a better design choice going forward and can eliminate
--     significant API surface area if promoted in the future.
--
-- (7) Should micromaps support host commands?
--
-- -   Resolved: No. Host commands are deprecated and not widely supported.
--
-- (8) Should
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.createAccelerationStructureKHR'
-- be used to create micromaps?
--
-- This interface that uses 'Vulkan.Core10.Handles.Buffer' to back
-- micromaps is deprecated, but is still available for acceleration
-- structure object creation.
--
-- -   Resolved: No. Make a new entry point that uses a device address to
--     back micromaps and other acceleration structures instead of a
--     buffer. Eliminate the ability to provide a separate capture\/replay
--     address as well. Top-level acceleration structures /must/ reference
--     the device address that backs the bottom-level acceleration
--     structures and implementations /must/ not expose an indirect handle.
--
-- (9) Should
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_OPACITY_MICROMAP_DATA_UPDATE_BIT_EXT'
-- be promoted from @VK_EXT_opacity_micromap@ to this extension?
--
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_OPACITY_MICROMAP_DATA_UPDATE_BIT_EXT'
-- distinguished between replacing a micromap with one of a different shape
-- ('Vulkan.Extensions.VK_EXT_opacity_micromap.BUILD_ACCELERATION_STRUCTURE_ALLOW_OPACITY_MICROMAP_UPDATE_BIT_EXT')
-- and replacing one where the shape is identical and only the opacity
-- values have changed (DATA_UPDATE). The DATA_UPDATE flag was correlated
-- with the discardable micromap feature, where an implementation might
-- embed micromap state directly in the acceleration structure and exploit
-- the tighter constraint to avoid a full rebuild. However, the spec never
-- required DATA_UPDATE to be restricted to discardable micromaps, and it
-- was already legal to treat DATA_UPDATE identically to UPDATE.
--
-- -   Resolved: No. The discardable micromap feature is not promoted to
--     this extension, removing the primary motivation for DATA_UPDATE. No
--     implementation identified a meaningful optimization from the
--     stricter constraint beyond what UPDATE already provides, and no CTS
--     coverage existed for either opacity micromap update flag in the EXT.
--     Removing it is a pure API simplification with no functional loss.
--     Applications must still perform a BLAS build update when opacity
--     micromap data changes, even if only values and not structure have
--     changed.
--
-- == Version History
--
-- -   Revision 1, 2026-05-08 (Matthew Netsch)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_opacity_micromap Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_opacity_micromap  ( AccelerationStructureGeometryMicromapDataKHR
                                                  , AccelerationStructureTrianglesOpacityMicromapKHR
                                                  , MicromapTriangleKHR
                                                  , MicromapUsageKHR
                                                  , PhysicalDeviceOpacityMicromapFeaturesKHR
                                                  , PhysicalDeviceOpacityMicromapPropertiesKHR
                                                  , OpacityMicromapFormatKHR
                                                  , OpacityMicromapSpecialIndexKHR
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AccelerationStructureGeometryMicromapDataKHR

instance ToCStruct AccelerationStructureGeometryMicromapDataKHR
instance Show AccelerationStructureGeometryMicromapDataKHR

instance FromCStruct AccelerationStructureGeometryMicromapDataKHR


data AccelerationStructureTrianglesOpacityMicromapKHR

instance ToCStruct AccelerationStructureTrianglesOpacityMicromapKHR
instance Show AccelerationStructureTrianglesOpacityMicromapKHR

instance FromCStruct AccelerationStructureTrianglesOpacityMicromapKHR


data MicromapTriangleKHR

instance ToCStruct MicromapTriangleKHR
instance Show MicromapTriangleKHR

instance FromCStruct MicromapTriangleKHR


data MicromapUsageKHR

instance ToCStruct MicromapUsageKHR
instance Show MicromapUsageKHR

instance FromCStruct MicromapUsageKHR


data PhysicalDeviceOpacityMicromapFeaturesKHR

instance ToCStruct PhysicalDeviceOpacityMicromapFeaturesKHR
instance Show PhysicalDeviceOpacityMicromapFeaturesKHR

instance FromCStruct PhysicalDeviceOpacityMicromapFeaturesKHR


data PhysicalDeviceOpacityMicromapPropertiesKHR

instance ToCStruct PhysicalDeviceOpacityMicromapPropertiesKHR
instance Show PhysicalDeviceOpacityMicromapPropertiesKHR

instance FromCStruct PhysicalDeviceOpacityMicromapPropertiesKHR


data OpacityMicromapFormatKHR


data OpacityMicromapSpecialIndexKHR

