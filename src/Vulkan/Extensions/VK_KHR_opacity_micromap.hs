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
module Vulkan.Extensions.VK_KHR_opacity_micromap  ( AccelerationStructureGeometryMicromapDataKHR(..)
                                                  , MicromapUsageKHR(..)
                                                  , MicromapTriangleKHR(..)
                                                  , PhysicalDeviceOpacityMicromapFeaturesKHR(..)
                                                  , PhysicalDeviceOpacityMicromapPropertiesKHR(..)
                                                  , AccelerationStructureTrianglesOpacityMicromapKHR(..)
                                                  , OpacityMicromapFormatKHR( OPACITY_MICROMAP_FORMAT_2_STATE_KHR
                                                                            , OPACITY_MICROMAP_FORMAT_4_STATE_KHR
                                                                            , ..
                                                                            )
                                                  , OpacityMicromapSpecialIndexKHR( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_KHR
                                                                                  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_KHR
                                                                                  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_KHR
                                                                                  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_KHR
                                                                                  , ..
                                                                                  )
                                                  , AccelerationStructureSerializedBlockTypeKHR( ACCELERATION_STRUCTURE_SERIALIZED_BLOCK_TYPE_OPACITY_MICROMAP_KHR
                                                                                               , ..
                                                                                               )
                                                  , KHR_OPACITY_MICROMAP_SPEC_VERSION
                                                  , pattern KHR_OPACITY_MICROMAP_SPEC_VERSION
                                                  , KHR_OPACITY_MICROMAP_EXTENSION_NAME
                                                  , pattern KHR_OPACITY_MICROMAP_EXTENSION_NAME
                                                  , AccelerationStructureKHR(..)
                                                  , GeometryInstanceFlagBitsKHR(..)
                                                  , GeometryInstanceFlagsKHR
                                                  , BuildAccelerationStructureFlagBitsKHR(..)
                                                  , BuildAccelerationStructureFlagsKHR
                                                  , AccelerationStructureTypeKHR(..)
                                                  , GeometryTypeKHR(..)
                                                  , ShaderCreateFlagBitsEXT(..)
                                                  , ShaderCreateFlagsEXT
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word16)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Extensions.Handles (AccelerationStructureKHR)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MICROMAP_DATA_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_KHR))
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureTypeKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR(..))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateFlagsEXT)
-- | VkAccelerationStructureGeometryMicromapDataKHR - Structure specifying
-- the data used to build a micromap
--
-- = Description
--
-- Only one of @pUsageCounts@ or @ppUsageCounts@ /can/ be a valid pointer,
-- the other /must/ be @NULL@. The elements of the non-@NULL@ array
-- describe the total counts used to build each micromap. Each element
-- contains a @count@ which is the number of micromap triangles of that
-- @format@ and @subdivisionLevel@ contained in the micromap. Multiple
-- elements with the same @format@ and @subdivisionLevel@ are allowed and
-- the total count for that @format@ and @subdivisionLevel@ is the sum of
-- the @count@ for each element.
--
-- Each micromap triangle refers to one element in @triangleArray@ which
-- contains the @format@ and @subdivisionLevel@ for that particular
-- triangle as well as a @dataOffset@ in bytes which is the location
-- relative to @data@ where that triangle’s micromap data begins. The data
-- at @triangleArray@ is laid out as a 4 byte unsigned integer for the
-- @dataOffset@ followed by a 2 byte unsigned integer for the subdivision
-- level then a 2 byte unsigned integer for the format. In practice,
-- compilers compile 'MicromapTriangleKHR' to match this pattern.
--
-- For opacity micromaps, the data at @data@ is packed as either one bit
-- per element for 'OPACITY_MICROMAP_FORMAT_2_STATE_KHR' or two bits per
-- element for 'OPACITY_MICROMAP_FORMAT_4_STATE_KHR' and is packed from LSB
-- to MSB in each byte. The data at each index in those bytes is
-- interpreted as discussed in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#ray-opacity-micromap Ray Opacity Micromap>.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureGeometryMicromapDataKHR-pUsageCounts-11642#
--     Only one of @pUsageCounts@ or @ppUsageCounts@ /can/ be a valid
--     pointer, the other /must/ be @NULL@
--
-- -   #VUID-VkAccelerationStructureGeometryMicromapDataKHR-count-11643#
--     The total sum of @count@ from all elements of @pUsageCounts@ and
--     @ppUsageCounts@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxMicromapTriangles maxMicromapTriangles>
--
-- -   #VUID-VkAccelerationStructureGeometryMicromapDataKHR-subdivisionLevel-11645#
--     For each member of @pUsageCounts@ or @ppUsageCounts@, if @format@ is
--     'OPACITY_MICROMAP_FORMAT_2_STATE_KHR', @subdivisionLevel@ /must/ be
--     less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxOpacity2StateSubdivisionLevel maxOpacity2StateSubdivisionLevel>
--
-- -   #VUID-VkAccelerationStructureGeometryMicromapDataKHR-triangleArrayStride-11646#
--     @triangleArrayStride@ /must/ be greater than or equal to @8@ bytes
--
-- -   #VUID-VkAccelerationStructureGeometryMicromapDataKHR-triangleArrayStride-11647#
--     @triangleArrayStride@ /must/ be a multiple of @4@ bytes
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureGeometryMicromapDataKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MICROMAP_DATA_KHR'
--
-- -   #VUID-VkAccelerationStructureGeometryMicromapDataKHR-pUsageCounts-parameter#
--     If @usageCountsCount@ is not @0@, and @pUsageCounts@ is not @NULL@,
--     @pUsageCounts@ /must/ be a valid pointer to an array of
--     @usageCountsCount@ valid 'MicromapUsageKHR' structures
--
-- -   #VUID-VkAccelerationStructureGeometryMicromapDataKHR-ppUsageCounts-parameter#
--     If @usageCountsCount@ is not @0@, and @ppUsageCounts@ is not @NULL@,
--     @ppUsageCounts@ /must/ be a valid pointer to an array of
--     @usageCountsCount@ valid pointers to valid 'MicromapUsageKHR'
--     structures
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryKHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_opacity_micromap VK_KHR_opacity_micromap>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'MicromapUsageKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureGeometryMicromapDataKHR = AccelerationStructureGeometryMicromapDataKHR
  { -- | @pUsageCounts@ is a pointer to an array of 'MicromapUsageKHR'
    -- structures.
    usageCounts :: Vector MicromapUsageKHR
  , -- | @data@ is the device address to memory which contains the data for the
    -- micromap.
    data' :: DeviceAddress
  , -- | @triangleArray@ is the device address to memory containing the
    -- 'MicromapTriangleKHR' data
    triangleArray :: DeviceAddress
  , -- | @triangleArrayStride@ is the stride in bytes between each element of
    -- @triangleArray@
    triangleArrayStride :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureGeometryMicromapDataKHR)
#endif
deriving instance Show AccelerationStructureGeometryMicromapDataKHR

instance ToCStruct AccelerationStructureGeometryMicromapDataKHR where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureGeometryMicromapDataKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MICROMAP_DATA_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (usageCounts)) :: Word32))
    pPUsageCounts' <- ContT $ allocaBytes @MicromapUsageKHR ((Data.Vector.length (usageCounts)) * 12)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPUsageCounts' `plusPtr` (12 * (i)) :: Ptr MicromapUsageKHR) (e)) (usageCounts)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr MicromapUsageKHR))) (pPUsageCounts')
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (Ptr MicromapUsageKHR)))) (nullPtr)
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceAddress)) (data')
    lift $ poke ((p `plusPtr` 48 :: Ptr DeviceAddress)) (triangleArray)
    lift $ poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (triangleArrayStride)
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MICROMAP_DATA_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 32 :: Ptr (Ptr (Ptr MicromapUsageKHR)))) (nullPtr)
    poke ((p `plusPtr` 40 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 56 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct AccelerationStructureGeometryMicromapDataKHR where
  peekCStruct p = do
    usageCountsCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pUsageCounts <- peek @(Ptr MicromapUsageKHR) ((p `plusPtr` 24 :: Ptr (Ptr MicromapUsageKHR)))
    pUsageCounts' <- generateM (fromIntegral usageCountsCount) (\i -> peekCStruct @MicromapUsageKHR ((pUsageCounts `advancePtrBytes` (12 * (i)) :: Ptr MicromapUsageKHR)))
    data' <- peek @DeviceAddress ((p `plusPtr` 40 :: Ptr DeviceAddress))
    triangleArray <- peek @DeviceAddress ((p `plusPtr` 48 :: Ptr DeviceAddress))
    triangleArrayStride <- peek @DeviceSize ((p `plusPtr` 56 :: Ptr DeviceSize))
    pure $ AccelerationStructureGeometryMicromapDataKHR
             pUsageCounts' data' triangleArray triangleArrayStride

instance Zero AccelerationStructureGeometryMicromapDataKHR where
  zero = AccelerationStructureGeometryMicromapDataKHR
           mempty
           zero
           zero
           zero


-- | VkMicromapUsageKHR - Structure specifying the usage information used to
-- build a micromap
--
-- = Description
--
-- The @format@ is interpreted based on the @type@ of the micromap using
-- it.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_opacity_micromap VK_KHR_opacity_micromap>,
-- 'AccelerationStructureGeometryMicromapDataKHR',
-- 'OpacityMicromapFormatKHR'
data MicromapUsageKHR = MicromapUsageKHR
  { -- | @count@ is the number of triangles in the usage format defined by the
    -- @subdivisionLevel@ and @format@ below in the micromap
    count :: Word32
  , -- | @subdivisionLevel@ is the subdivision level of this usage format
    subdivisionLevel :: Word32
  , -- | @format@ is the format of this usage format
    --
    -- #VUID-VkMicromapUsageKHR-format-parameter# @format@ /must/ be a valid
    -- 'OpacityMicromapFormatKHR' value
    format :: OpacityMicromapFormatKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MicromapUsageKHR)
#endif
deriving instance Show MicromapUsageKHR

instance ToCStruct MicromapUsageKHR where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MicromapUsageKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (count)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (subdivisionLevel)
    poke ((p `plusPtr` 8 :: Ptr OpacityMicromapFormatKHR)) (format)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr OpacityMicromapFormatKHR)) (zero)
    f

instance FromCStruct MicromapUsageKHR where
  peekCStruct p = do
    count <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    subdivisionLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    format <- peek @OpacityMicromapFormatKHR ((p `plusPtr` 8 :: Ptr OpacityMicromapFormatKHR))
    pure $ MicromapUsageKHR
             count subdivisionLevel format

instance Storable MicromapUsageKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MicromapUsageKHR where
  zero = MicromapUsageKHR
           zero
           zero
           zero


-- | VkMicromapTriangleKHR - Structure specifying the micromap format and
-- data for a triangle
--
-- = Description
--
-- The @format@ is interpreted based on the @type@ of the micromap using
-- it.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_opacity_micromap VK_KHR_opacity_micromap>
data MicromapTriangleKHR = MicromapTriangleKHR
  { -- | @dataOffset@ is the offset in bytes of the start of the data for this
    -- triangle. This is a byte aligned value.
    dataOffset :: Word32
  , -- | @subdivisionLevel@ is the subdivision level of this triangle
    subdivisionLevel :: Word16
  , -- | @format@ is the format of this triangle
    format :: Word16
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MicromapTriangleKHR)
#endif
deriving instance Show MicromapTriangleKHR

instance ToCStruct MicromapTriangleKHR where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MicromapTriangleKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (dataOffset)
    poke ((p `plusPtr` 4 :: Ptr Word16)) (subdivisionLevel)
    poke ((p `plusPtr` 6 :: Ptr Word16)) (format)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word16)) (zero)
    poke ((p `plusPtr` 6 :: Ptr Word16)) (zero)
    f

instance FromCStruct MicromapTriangleKHR where
  peekCStruct p = do
    dataOffset <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    subdivisionLevel <- peek @Word16 ((p `plusPtr` 4 :: Ptr Word16))
    format <- peek @Word16 ((p `plusPtr` 6 :: Ptr Word16))
    pure $ MicromapTriangleKHR
             dataOffset subdivisionLevel format

instance Storable MicromapTriangleKHR where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MicromapTriangleKHR where
  zero = MicromapTriangleKHR
           zero
           zero
           zero


-- | VkPhysicalDeviceOpacityMicromapFeaturesKHR - Structure describing the
-- ray tracing opacity micromap features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceOpacityMicromapFeaturesKHR' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceOpacityMicromapFeaturesKHR', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_opacity_micromap VK_KHR_opacity_micromap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceOpacityMicromapFeaturesKHR = PhysicalDeviceOpacityMicromapFeaturesKHR
  { -- | #features-micromap# @micromap@ indicates whether the implementation
    -- supports the micromap array feature.
    micromap :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceOpacityMicromapFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceOpacityMicromapFeaturesKHR

instance ToCStruct PhysicalDeviceOpacityMicromapFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceOpacityMicromapFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (micromap))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceOpacityMicromapFeaturesKHR where
  peekCStruct p = do
    micromap <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceOpacityMicromapFeaturesKHR
             (bool32ToBool micromap)

instance Storable PhysicalDeviceOpacityMicromapFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceOpacityMicromapFeaturesKHR where
  zero = PhysicalDeviceOpacityMicromapFeaturesKHR
           zero


-- | VkPhysicalDeviceOpacityMicromapPropertiesKHR - Structure describing the
-- opacity micromap properties of a physical device
--
-- = Description
--
-- If the 'PhysicalDeviceOpacityMicromapPropertiesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_opacity_micromap VK_KHR_opacity_micromap>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceOpacityMicromapPropertiesKHR = PhysicalDeviceOpacityMicromapPropertiesKHR
  { -- | #limits-maxOpacity2StateSubdivisionLevel#
    -- @maxOpacity2StateSubdivisionLevel@ is the maximum allowed
    -- @subdivisionLevel@ when @format@ is
    -- 'OPACITY_MICROMAP_FORMAT_2_STATE_KHR'.
    maxOpacity2StateSubdivisionLevel :: Word32
  , -- | #limits-maxOpacity4StateSubdivisionLevel#
    -- @maxOpacity4StateSubdivisionLevel@ is the maximum allowed
    -- @subdivisionLevel@ when @format@ is
    -- 'OPACITY_MICROMAP_FORMAT_4_STATE_KHR' and the micromap is not built with
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_MICROMAP_LOSSY_BIT_KHR'.
    maxOpacity4StateSubdivisionLevel :: Word32
  , -- | #limits-maxOpacityLossy4StateSubdivisionLevel#
    -- @maxOpacityLossy4StateSubdivisionLevel@ is the maximum allowed
    -- @subdivisionLevel@ when @format@ is
    -- 'OPACITY_MICROMAP_FORMAT_4_STATE_KHR' and the micromap is built with
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_MICROMAP_LOSSY_BIT_KHR'.
    maxOpacityLossy4StateSubdivisionLevel :: Word32
  , -- | #limits-maxMicromapTriangles# @maxMicromapTriangles@ limits the number
    -- of triangles allowed to be specified when building a micromap
    maxMicromapTriangles :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceOpacityMicromapPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceOpacityMicromapPropertiesKHR

instance ToCStruct PhysicalDeviceOpacityMicromapPropertiesKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceOpacityMicromapPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxOpacity2StateSubdivisionLevel)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxOpacity4StateSubdivisionLevel)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxOpacityLossy4StateSubdivisionLevel)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (maxMicromapTriangles)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    f

instance FromCStruct PhysicalDeviceOpacityMicromapPropertiesKHR where
  peekCStruct p = do
    maxOpacity2StateSubdivisionLevel <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxOpacity4StateSubdivisionLevel <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxOpacityLossy4StateSubdivisionLevel <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxMicromapTriangles <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    pure $ PhysicalDeviceOpacityMicromapPropertiesKHR
             maxOpacity2StateSubdivisionLevel
             maxOpacity4StateSubdivisionLevel
             maxOpacityLossy4StateSubdivisionLevel
             maxMicromapTriangles

instance Storable PhysicalDeviceOpacityMicromapPropertiesKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceOpacityMicromapPropertiesKHR where
  zero = PhysicalDeviceOpacityMicromapPropertiesKHR
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureTrianglesOpacityMicromapKHR - Structure
-- specifying an opacity micromap in a bottom-level acceleration structure
--
-- = Description
--
-- If 'AccelerationStructureTrianglesOpacityMicromapKHR' is included in the
-- @pNext@ chain of a
-- 'Vulkan.Extensions.VK_AMDX_dense_geometry_format.AccelerationStructureDenseGeometryFormatTrianglesDataAMDX'
-- or
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'
-- structure, that geometry will reference that micromap.
--
-- For each triangle in the geometry, the acceleration structure build
-- fetches an index from @indexBuffer@ using @indexType@ and @indexStride@
-- if present. If @indexType@ is
-- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', then the index used
-- is the index of the triangle in the geometry.
--
-- If that value is the unsigned cast of one of the values from
-- 'OpacityMicromapSpecialIndexKHR' then that triangle behaves as described
-- for that special value in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#ray-opacity-micromap Ray Opacity Micromap>.
--
-- Otherwise that triangle uses the opacity micromap information from
-- @micromap@ at that index plus @baseTriangle@.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-indexType-11570#
--     @indexType@ /must/ be
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8',
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16',
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32', or
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR'
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-indexBuffer-11571#
--     If @indexType@ is
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', @indexBuffer@
--     /must/ be 0
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-indexBuffer-11572#
--     If @indexType@ is not
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', @indexBuffer@
--     /must/ be a valid 'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-indexStride-11573#
--     If @indexType@ is not
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', @indexStride@
--     /must/ be a multiple of the size in bytes of @indexType@
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-indexStride-11574#
--     If @indexType@ is not
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', @indexStride@
--     /must/ be less than or equal to 232-1
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-geometry-11576#
--     If @indexType@ is
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', for each
--     triangle index in the geometry, index plus @baseTriangle@ /must/ be
--     less than or equal to /numTriangles/, where /numTriangles/ is given
--     by the sum of all @count@ parameters in the @pUsageCounts@ or
--     @ppUsageCounts@ provided to the micromap’s build command
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-micromap-11579#
--     If @indexType@ is
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR', @micromap@
--     /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_KHR'
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-indexType-parameter#
--     @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapKHR-micromap-parameter#
--     If @micromap@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @micromap@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Extensions.VK_AMDX_dense_geometry_format.AccelerationStructureDenseGeometryFormatTrianglesDataAMDX'
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_opacity_micromap VK_KHR_opacity_micromap>,
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureTrianglesOpacityMicromapKHR = AccelerationStructureTrianglesOpacityMicromapKHR
  { -- | @indexType@ is the type of triangle indices used when indexing this
    -- micromap
    indexType :: IndexType
  , -- | @indexBuffer@ is the address containing the triangle indices
    indexBuffer :: DeviceAddress
  , -- | @indexStride@ is the byte stride between triangle indices
    indexStride :: DeviceSize
  , -- | @baseTriangle@ is the base value added to the non-negative triangle
    -- indices
    baseTriangle :: Word32
  , -- | @micromap@ is the handle to the micromap object to include in this
    -- geometry
    micromap :: AccelerationStructureKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureTrianglesOpacityMicromapKHR)
#endif
deriving instance Show AccelerationStructureTrianglesOpacityMicromapKHR

instance ToCStruct AccelerationStructureTrianglesOpacityMicromapKHR where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureTrianglesOpacityMicromapKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr IndexType)) (indexType)
    poke ((p `plusPtr` 24 :: Ptr DeviceAddress)) (indexBuffer)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (indexStride)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (baseTriangle)
    poke ((p `plusPtr` 48 :: Ptr AccelerationStructureKHR)) (micromap)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr IndexType)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct AccelerationStructureTrianglesOpacityMicromapKHR where
  peekCStruct p = do
    indexType <- peek @IndexType ((p `plusPtr` 16 :: Ptr IndexType))
    indexBuffer <- peek @DeviceAddress ((p `plusPtr` 24 :: Ptr DeviceAddress))
    indexStride <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    baseTriangle <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    micromap <- peek @AccelerationStructureKHR ((p `plusPtr` 48 :: Ptr AccelerationStructureKHR))
    pure $ AccelerationStructureTrianglesOpacityMicromapKHR
             indexType indexBuffer indexStride baseTriangle micromap

instance Storable AccelerationStructureTrianglesOpacityMicromapKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureTrianglesOpacityMicromapKHR where
  zero = AccelerationStructureTrianglesOpacityMicromapKHR
           zero
           zero
           zero
           zero
           zero


-- | VkOpacityMicromapFormatKHR - Format enum for opacity micromaps
--
-- = Description
--
-- -   'OPACITY_MICROMAP_FORMAT_2_STATE_KHR' specifies that the given
--     micromap format has one bit per subtriangle encoding either fully
--     opaque or fully transparent.
--
-- -   'OPACITY_MICROMAP_FORMAT_4_STATE_KHR' specifies that the given
--     micromap format has two bits per subtriangle encoding four modes
--     which can be interpreted as described in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#ray-opacity-micromap Ray Opacity Micromap>.
--
-- For compactness, these values are stored as 16-bit in some structures.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_opacity_micromap VK_KHR_opacity_micromap>,
-- 'MicromapUsageKHR'
newtype OpacityMicromapFormatKHR = OpacityMicromapFormatKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkOpacityMicromapFormatKHR" "VK_OPACITY_MICROMAP_FORMAT_2_STATE_KHR"
pattern OPACITY_MICROMAP_FORMAT_2_STATE_KHR = OpacityMicromapFormatKHR 1

-- No documentation found for Nested "VkOpacityMicromapFormatKHR" "VK_OPACITY_MICROMAP_FORMAT_4_STATE_KHR"
pattern OPACITY_MICROMAP_FORMAT_4_STATE_KHR = OpacityMicromapFormatKHR 2

{-# COMPLETE
  OPACITY_MICROMAP_FORMAT_2_STATE_KHR
  , OPACITY_MICROMAP_FORMAT_4_STATE_KHR ::
    OpacityMicromapFormatKHR
  #-}

conNameOpacityMicromapFormatKHR :: String
conNameOpacityMicromapFormatKHR = "OpacityMicromapFormatKHR"

enumPrefixOpacityMicromapFormatKHR :: String
enumPrefixOpacityMicromapFormatKHR = "OPACITY_MICROMAP_FORMAT_"

showTableOpacityMicromapFormatKHR :: [(OpacityMicromapFormatKHR, String)]
showTableOpacityMicromapFormatKHR =
  [
    ( OPACITY_MICROMAP_FORMAT_2_STATE_KHR
    , "2_STATE_KHR"
    )
  ,
    ( OPACITY_MICROMAP_FORMAT_4_STATE_KHR
    , "4_STATE_KHR"
    )
  ]

instance Show OpacityMicromapFormatKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixOpacityMicromapFormatKHR
      showTableOpacityMicromapFormatKHR
      conNameOpacityMicromapFormatKHR
      (\(OpacityMicromapFormatKHR x) -> x)
      (showsPrec 11)

instance Read OpacityMicromapFormatKHR where
  readPrec =
    enumReadPrec
      enumPrefixOpacityMicromapFormatKHR
      showTableOpacityMicromapFormatKHR
      conNameOpacityMicromapFormatKHR
      OpacityMicromapFormatKHR

-- | VkOpacityMicromapSpecialIndexKHR - Enum for special indices in the
-- opacity micromap
--
-- = Description
--
-- -   'OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_KHR' specifies
--     that the entire triangle is fully transparent.
--
-- -   'OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_KHR' specifies that the
--     entire triangle is fully opaque.
--
-- -   'OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_KHR'
--     specifies that the entire triangle is unknown-transparent.
--
-- -   'OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_KHR' specifies
--     that the entire triangle is unknown-opaque.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkOpacityMicromapSpecialIndexEXT VK_OPACITY_MICROMAP_SPECIAL_INDEX_CLUSTER_GEOMETRY_DISABLE_OPACITY_MICROMAP_NV>
--     specifies that
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#ray-opacity-micromap Opacity Micromap>
--     will be disabled for this triangle and opacity value will be picked
--     from
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkClusterAccelerationStructureBuildTriangleClusterInfoNV VkClusterAccelerationStructureBuildTriangleClusterInfoNV>::@baseGeometryIndexAndGeometryFlags@
--     instead. Note that this special index is only valid for
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#cluster-geometry Cluster Geometry>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_opacity_micromap VK_KHR_opacity_micromap>
newtype OpacityMicromapSpecialIndexKHR = OpacityMicromapSpecialIndexKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkOpacityMicromapSpecialIndexKHR" "VK_OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_KHR"
pattern OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_KHR = OpacityMicromapSpecialIndexKHR (-1)

-- No documentation found for Nested "VkOpacityMicromapSpecialIndexKHR" "VK_OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_KHR"
pattern OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_KHR = OpacityMicromapSpecialIndexKHR (-2)

-- No documentation found for Nested "VkOpacityMicromapSpecialIndexKHR" "VK_OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_KHR"
pattern OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_KHR = OpacityMicromapSpecialIndexKHR (-3)

-- No documentation found for Nested "VkOpacityMicromapSpecialIndexKHR" "VK_OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_KHR"
pattern OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_KHR = OpacityMicromapSpecialIndexKHR (-4)

{-# COMPLETE
  OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_KHR
  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_KHR
  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_KHR
  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_KHR ::
    OpacityMicromapSpecialIndexKHR
  #-}

conNameOpacityMicromapSpecialIndexKHR :: String
conNameOpacityMicromapSpecialIndexKHR = "OpacityMicromapSpecialIndexKHR"

enumPrefixOpacityMicromapSpecialIndexKHR :: String
enumPrefixOpacityMicromapSpecialIndexKHR = "OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_"

showTableOpacityMicromapSpecialIndexKHR :: [(OpacityMicromapSpecialIndexKHR, String)]
showTableOpacityMicromapSpecialIndexKHR =
  [
    ( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_KHR
    , "TRANSPARENT_KHR"
    )
  ,
    ( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_KHR
    , "OPAQUE_KHR"
    )
  ,
    ( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_KHR
    , "UNKNOWN_TRANSPARENT_KHR"
    )
  ,
    ( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_KHR
    , "UNKNOWN_OPAQUE_KHR"
    )
  ]

instance Show OpacityMicromapSpecialIndexKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixOpacityMicromapSpecialIndexKHR
      showTableOpacityMicromapSpecialIndexKHR
      conNameOpacityMicromapSpecialIndexKHR
      (\(OpacityMicromapSpecialIndexKHR x) -> x)
      (showsPrec 11)

instance Read OpacityMicromapSpecialIndexKHR where
  readPrec =
    enumReadPrec
      enumPrefixOpacityMicromapSpecialIndexKHR
      showTableOpacityMicromapSpecialIndexKHR
      conNameOpacityMicromapSpecialIndexKHR
      OpacityMicromapSpecialIndexKHR

-- | VkAccelerationStructureSerializedBlockTypeKHR - Enum for block types in
-- a serialized acceleration structure
--
-- = Description
--
-- -   'ACCELERATION_STRUCTURE_SERIALIZED_BLOCK_TYPE_OPACITY_MICROMAP_KHR'
--     specifies that the block contains opacity micromaps.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_opacity_micromap VK_KHR_opacity_micromap>
newtype AccelerationStructureSerializedBlockTypeKHR = AccelerationStructureSerializedBlockTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkAccelerationStructureSerializedBlockTypeKHR" "VK_ACCELERATION_STRUCTURE_SERIALIZED_BLOCK_TYPE_OPACITY_MICROMAP_KHR"
pattern ACCELERATION_STRUCTURE_SERIALIZED_BLOCK_TYPE_OPACITY_MICROMAP_KHR = AccelerationStructureSerializedBlockTypeKHR 0

{-# COMPLETE ACCELERATION_STRUCTURE_SERIALIZED_BLOCK_TYPE_OPACITY_MICROMAP_KHR :: AccelerationStructureSerializedBlockTypeKHR #-}

conNameAccelerationStructureSerializedBlockTypeKHR :: String
conNameAccelerationStructureSerializedBlockTypeKHR = "AccelerationStructureSerializedBlockTypeKHR"

enumPrefixAccelerationStructureSerializedBlockTypeKHR :: String
enumPrefixAccelerationStructureSerializedBlockTypeKHR = "ACCELERATION_STRUCTURE_SERIALIZED_BLOCK_TYPE_OPACITY_MICROMAP_KHR"

showTableAccelerationStructureSerializedBlockTypeKHR :: [(AccelerationStructureSerializedBlockTypeKHR, String)]
showTableAccelerationStructureSerializedBlockTypeKHR =
  [
    ( ACCELERATION_STRUCTURE_SERIALIZED_BLOCK_TYPE_OPACITY_MICROMAP_KHR
    , ""
    )
  ]

instance Show AccelerationStructureSerializedBlockTypeKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixAccelerationStructureSerializedBlockTypeKHR
      showTableAccelerationStructureSerializedBlockTypeKHR
      conNameAccelerationStructureSerializedBlockTypeKHR
      (\(AccelerationStructureSerializedBlockTypeKHR x) -> x)
      (showsPrec 11)

instance Read AccelerationStructureSerializedBlockTypeKHR where
  readPrec =
    enumReadPrec
      enumPrefixAccelerationStructureSerializedBlockTypeKHR
      showTableAccelerationStructureSerializedBlockTypeKHR
      conNameAccelerationStructureSerializedBlockTypeKHR
      AccelerationStructureSerializedBlockTypeKHR

type KHR_OPACITY_MICROMAP_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_OPACITY_MICROMAP_SPEC_VERSION"
pattern KHR_OPACITY_MICROMAP_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_OPACITY_MICROMAP_SPEC_VERSION = 1


type KHR_OPACITY_MICROMAP_EXTENSION_NAME = "VK_KHR_opacity_micromap"

-- No documentation found for TopLevel "VK_KHR_OPACITY_MICROMAP_EXTENSION_NAME"
pattern KHR_OPACITY_MICROMAP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_OPACITY_MICROMAP_EXTENSION_NAME = "VK_KHR_opacity_micromap"

