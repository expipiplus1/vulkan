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
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_opacity_micromap.html SPV_EXT_opacity_micromap>
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
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_opacity_micromap.txt GLSL_EXT_opacity_micromap>
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
-- == Reference Code
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
module Vulkan.Extensions.VK_EXT_opacity_micromap  ( createMicromapEXT
                                                  , withMicromapEXT
                                                  , cmdBuildMicromapsEXT
                                                  , buildMicromapsEXT
                                                  , destroyMicromapEXT
                                                  , cmdCopyMicromapEXT
                                                  , copyMicromapEXT
                                                  , cmdCopyMicromapToMemoryEXT
                                                  , copyMicromapToMemoryEXT
                                                  , cmdCopyMemoryToMicromapEXT
                                                  , copyMemoryToMicromapEXT
                                                  , cmdWriteMicromapsPropertiesEXT
                                                  , writeMicromapsPropertiesEXT
                                                  , getDeviceMicromapCompatibilityEXT
                                                  , getMicromapBuildSizesEXT
                                                  , MicromapBuildInfoEXT(..)
                                                  , MicromapCreateInfoEXT(..)
                                                  , MicromapVersionInfoEXT(..)
                                                  , CopyMicromapInfoEXT(..)
                                                  , CopyMicromapToMemoryInfoEXT(..)
                                                  , CopyMemoryToMicromapInfoEXT(..)
                                                  , MicromapBuildSizesInfoEXT(..)
                                                  , MicromapUsageEXT(..)
                                                  , MicromapTriangleEXT(..)
                                                  , PhysicalDeviceOpacityMicromapFeaturesEXT(..)
                                                  , PhysicalDeviceOpacityMicromapPropertiesEXT(..)
                                                  , AccelerationStructureTrianglesOpacityMicromapEXT(..)
                                                  , MicromapTypeEXT( MICROMAP_TYPE_OPACITY_MICROMAP_EXT
                                                                   , MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV
                                                                   , ..
                                                                   )
                                                  , BuildMicromapFlagsEXT
                                                  , BuildMicromapFlagBitsEXT( BUILD_MICROMAP_PREFER_FAST_TRACE_BIT_EXT
                                                                            , BUILD_MICROMAP_PREFER_FAST_BUILD_BIT_EXT
                                                                            , BUILD_MICROMAP_ALLOW_COMPACTION_BIT_EXT
                                                                            , ..
                                                                            )
                                                  , MicromapCreateFlagsEXT
                                                  , MicromapCreateFlagBitsEXT( MICROMAP_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
                                                                             , ..
                                                                             )
                                                  , CopyMicromapModeEXT( COPY_MICROMAP_MODE_CLONE_EXT
                                                                       , COPY_MICROMAP_MODE_SERIALIZE_EXT
                                                                       , COPY_MICROMAP_MODE_DESERIALIZE_EXT
                                                                       , COPY_MICROMAP_MODE_COMPACT_EXT
                                                                       , ..
                                                                       )
                                                  , BuildMicromapModeEXT( BUILD_MICROMAP_MODE_BUILD_EXT
                                                                        , ..
                                                                        )
                                                  , OpacityMicromapFormatEXT( OPACITY_MICROMAP_FORMAT_2_STATE_EXT
                                                                            , OPACITY_MICROMAP_FORMAT_4_STATE_EXT
                                                                            , ..
                                                                            )
                                                  , OpacityMicromapSpecialIndexEXT( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_EXT
                                                                                  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_EXT
                                                                                  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_EXT
                                                                                  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_EXT
                                                                                  , ..
                                                                                  )
                                                  , EXT_OPACITY_MICROMAP_SPEC_VERSION
                                                  , pattern EXT_OPACITY_MICROMAP_SPEC_VERSION
                                                  , EXT_OPACITY_MICROMAP_EXTENSION_NAME
                                                  , pattern EXT_OPACITY_MICROMAP_EXTENSION_NAME
                                                  , DeferredOperationKHR(..)
                                                  , MicromapEXT(..)
                                                  , DeviceOrHostAddressKHR(..)
                                                  , DeviceOrHostAddressConstKHR(..)
                                                  , GeometryInstanceFlagBitsKHR(..)
                                                  , GeometryInstanceFlagsKHR
                                                  , BuildAccelerationStructureFlagBitsKHR(..)
                                                  , BuildAccelerationStructureFlagsKHR
                                                  , AccelerationStructureBuildTypeKHR(..)
                                                  , AccelerationStructureCompatibilityKHR(..)
                                                  ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import qualified Data.ByteString (length)
import Data.ByteString (packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word16)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildTypeKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildTypeKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCompatibilityKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCompatibilityKHR(..))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.Handles (DeferredOperationKHR)
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkBuildMicromapsEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildMicromapsEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMemoryToMicromapEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMicromapEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyMicromapToMemoryEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteMicromapsPropertiesEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCopyMemoryToMicromapEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCopyMicromapEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCopyMicromapToMemoryEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCreateMicromapEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyMicromapEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceMicromapCompatibilityEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetMicromapBuildSizesEXT))
import Vulkan.Dynamic (DeviceCmds(pVkWriteMicromapsPropertiesEXT))
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressKHR)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Extensions.Handles (MicromapEXT)
import Vulkan.Extensions.Handles (MicromapEXT(..))
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryType (QueryType)
import Vulkan.Core10.Enums.QueryType (QueryType(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MEMORY_TO_MICROMAP_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MICROMAP_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MICROMAP_TO_MEMORY_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MICROMAP_BUILD_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MICROMAP_BUILD_SIZES_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MICROMAP_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MICROMAP_VERSION_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.APIConstants (pattern UUID_SIZE)
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildTypeKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCompatibilityKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressConstKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (DeviceOrHostAddressKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.Handles (MicromapEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateMicromapEXT
  :: FunPtr (Ptr Device_T -> Ptr MicromapCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr MicromapEXT -> IO Result) -> Ptr Device_T -> Ptr MicromapCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr MicromapEXT -> IO Result

-- | vkCreateMicromapEXT - Create a new micromap object
--
-- = Description
--
-- Similar to other objects in Vulkan, the micromap creation merely creates
-- an object with a specific “shape”. The type and quantity of geometry
-- that can be built into a micromap is determined by the parameters of
-- 'MicromapCreateInfoEXT'.
--
-- The micromap data is stored in the object referred to by
-- 'MicromapCreateInfoEXT'::@buffer@. Once memory has been bound to that
-- buffer, it /must/ be populated by micromap build or micromap copy
-- commands such as 'cmdBuildMicromapsEXT', 'buildMicromapsEXT',
-- 'cmdCopyMicromapEXT', and 'copyMicromapEXT'.
--
-- Note
--
-- The expected usage for a trace capture\/replay tool is that it will
-- serialize and later deserialize the micromap data using micromap copy
-- commands. During capture the tool will use 'copyMicromapToMemoryEXT' or
-- 'cmdCopyMicromapToMemoryEXT' with a @mode@ of
-- 'COPY_MICROMAP_MODE_SERIALIZE_EXT', and 'copyMemoryToMicromapEXT' or
-- 'cmdCopyMemoryToMicromapEXT' with a @mode@ of
-- 'COPY_MICROMAP_MODE_DESERIALIZE_EXT' during replay.
--
-- The input buffers passed to micromap build commands will be referenced
-- by the implementation for the duration of the command. Micromaps /must/
-- be fully self-contained. The application /can/ reuse or free any memory
-- which was used by the command as an input or as scratch without
-- affecting the results of a subsequent acceleration structure build using
-- the micromap or traversal of that acceleration structure.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateMicromapEXT-micromap-07430# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromap micromap>
--     feature /must/ be enabled
--
-- -   #VUID-vkCreateMicromapEXT-deviceAddress-07431# If
--     'MicromapCreateInfoEXT'::@deviceAddress@ is not zero, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromapCaptureReplay micromapCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-vkCreateMicromapEXT-device-07432# If @device@ was created with
--     multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateMicromapEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateMicromapEXT-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'MicromapCreateInfoEXT'
--     structure
--
-- -   #VUID-vkCreateMicromapEXT-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateMicromapEXT-pMicromap-parameter# @pMicromap@ /must/ be
--     a valid pointer to a 'Vulkan.Extensions.Handles.MicromapEXT' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'MicromapCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.MicromapEXT'
createMicromapEXT :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the logical device that creates the acceleration structure
                     -- object.
                     Device
                  -> -- | @pCreateInfo@ is a pointer to a 'MicromapCreateInfoEXT' structure
                     -- containing parameters affecting creation of the micromap.
                     MicromapCreateInfoEXT
                  -> -- | @pAllocator@ controls host memory allocation as described in the
                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                     -- chapter.
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (MicromapEXT)
createMicromapEXT device createInfo allocator = liftIO . evalContT $ do
  let vkCreateMicromapEXTPtr = pVkCreateMicromapEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateMicromapEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateMicromapEXT is null" Nothing Nothing
  let vkCreateMicromapEXT' = mkVkCreateMicromapEXT vkCreateMicromapEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPMicromap <- ContT $ bracket (callocBytes @MicromapEXT 8) free
  r <- lift $ traceAroundEvent "vkCreateMicromapEXT" (vkCreateMicromapEXT'
                                                        (deviceHandle (device))
                                                        pCreateInfo
                                                        pAllocator
                                                        (pPMicromap))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMicromap <- lift $ peek @MicromapEXT pPMicromap
  pure $ (pMicromap)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createMicromapEXT' and 'destroyMicromapEXT'
--
-- To ensure that 'destroyMicromapEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withMicromapEXT :: forall io r . MonadIO io => Device -> MicromapCreateInfoEXT -> Maybe AllocationCallbacks -> (io MicromapEXT -> (MicromapEXT -> io ()) -> r) -> r
withMicromapEXT device pCreateInfo pAllocator b =
  b (createMicromapEXT device pCreateInfo pAllocator)
    (\(o0) -> destroyMicromapEXT device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildMicromapsEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr MicromapBuildInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr MicromapBuildInfoEXT -> IO ()

-- | vkCmdBuildMicromapsEXT - Build a micromap
--
-- = Description
--
-- The 'cmdBuildMicromapsEXT' command provides the ability to initiate
-- multiple micromaps builds, however there is no ordering or
-- synchronization implied between any of the individual micromap builds.
--
-- Note
--
-- This means that there /cannot/ be any memory aliasing between any
-- micromap memories or scratch memories being used by any of the builds.
--
-- Accesses to the micromap scratch buffers as identified by the
-- 'MicromapBuildInfoEXT'::@scratchData@ buffer device addresses /must/ be
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-types access type>
-- of ('Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT' |
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT').
-- Accesses to 'MicromapBuildInfoEXT'::@dstMicromap@ /must/ be
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-types access type>
-- of 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT'.
--
-- Accesses to other input buffers as identified by any used values of
-- 'MicromapBuildInfoEXT'::@data@ or
-- 'MicromapBuildInfoEXT'::@triangleArray@ /must/ be
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-types access type>
-- of 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_SHADER_READ_BIT'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-07461# For each @pInfos@[i],
--     @dstMicromap@ /must/ have been created with a value of
--     'MicromapCreateInfoEXT'::@size@ greater than or equal to the memory
--     size required by the build operation, as returned by
--     'getMicromapBuildSizesEXT' with @pBuildInfo@ = @pInfos@[i]
--
-- -   #VUID-vkCmdBuildMicromapsEXT-mode-07462# The @mode@ member of each
--     element of @pInfos@ /must/ be a valid 'BuildMicromapModeEXT' value
--
-- -   #VUID-vkCmdBuildMicromapsEXT-dstMicromap-07463# The @dstMicromap@
--     member of any element of @pInfos@ /must/ be a valid
--     'Vulkan.Extensions.Handles.MicromapEXT' handle
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-07464# For each element of
--     @pInfos@ its @type@ member /must/ match the value of
--     'MicromapCreateInfoEXT'::@type@ when its @dstMicromap@ was created
--
-- -   #VUID-vkCmdBuildMicromapsEXT-dstMicromap-07465# The range of memory
--     backing the @dstMicromap@ member of any element of @pInfos@ that is
--     accessed by this command /must/ not overlap the memory backing the
--     @dstMicromap@ member of any other element of @pInfos@, which is
--     accessed by this command
--
-- -   #VUID-vkCmdBuildMicromapsEXT-dstMicromap-07466# The range of memory
--     backing the @dstMicromap@ member of any element of @pInfos@ that is
--     accessed by this command /must/ not overlap the memory backing the
--     @scratchData@ member of any element of @pInfos@ (including the same
--     element), which is accessed by this command
--
-- -   #VUID-vkCmdBuildMicromapsEXT-scratchData-07467# The range of memory
--     backing the @scratchData@ member of any element of @pInfos@ that is
--     accessed by this command /must/ not overlap the memory backing the
--     @scratchData@ member of any other element of @pInfos@, which is
--     accessed by this command
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-07508# For each element of
--     @pInfos@, the @buffer@ used to create its @dstMicromap@ member
--     /must/ be bound to device memory
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-07509# If @pInfos@[i].@mode@ is
--     'BUILD_MICROMAP_MODE_BUILD_EXT', all addresses between
--     @pInfos@[i].@scratchData.deviceAddress@ and
--     @pInfos@[i].@scratchData.deviceAddress@ + N - 1 /must/ be in the
--     buffer device address range of the same buffer, where N is given by
--     the @buildScratchSize@ member of the 'MicromapBuildSizesInfoEXT'
--     structure returned from a call to 'getMicromapBuildSizesEXT' with an
--     identical 'MicromapBuildInfoEXT' structure and primitive count
--
-- -   #VUID-vkCmdBuildMicromapsEXT-data-07510# The buffers from which the
--     buffer device addresses for all of the @data@ and @triangleArray@
--     members of all @pInfos@[i] are queried /must/ have been created with
--     the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT'
--     usage flag
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-07511# For each element of
--     @pInfos@[i] the buffer from which the buffer device address
--     @pInfos@[i].@scratchData.deviceAddress@ is queried /must/ have been
--     created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     usage flag
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-07512# For each element of
--     @pInfos@, its @scratchData.deviceAddress@, @data.deviceAddress@, and
--     @triangleArray.deviceAddress@ members /must/ be valid device
--     addresses obtained from
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-07513# For each element of
--     @pInfos@, if @scratchData.deviceAddress@, @data.deviceAddress@, or
--     @triangleArray.deviceAddress@ is the address of a non-sparse buffer
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-07514# For each element of
--     @pInfos@, its @scratchData.deviceAddress@ member /must/ be a
--     multiple of
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.PhysicalDeviceAccelerationStructurePropertiesKHR'::@minAccelerationStructureScratchOffsetAlignment@
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-07515# For each element of
--     @pInfos@, its @triangleArray.deviceAddress@ and @data.deviceAddress@
--     members /must/ be a multiple of @256@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBuildMicromapsEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBuildMicromapsEXT-pInfos-parameter# @pInfos@ /must/ be a
--     valid pointer to an array of @infoCount@ valid
--     'MicromapBuildInfoEXT' structures
--
-- -   #VUID-vkCmdBuildMicromapsEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBuildMicromapsEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBuildMicromapsEXT-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdBuildMicromapsEXT-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdBuildMicromapsEXT-infoCount-arraylength# @infoCount@
--     /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Compute                                                                                                               | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'MicromapBuildInfoEXT'
cmdBuildMicromapsEXT :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @pInfos@ is a pointer to an array of @infoCount@ 'MicromapBuildInfoEXT'
                        -- structures defining the data used to build each micromap.
                        ("infos" ::: Vector MicromapBuildInfoEXT)
                     -> io ()
cmdBuildMicromapsEXT commandBuffer infos = liftIO . evalContT $ do
  let vkCmdBuildMicromapsEXTPtr = pVkCmdBuildMicromapsEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBuildMicromapsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBuildMicromapsEXT is null" Nothing Nothing
  let vkCmdBuildMicromapsEXT' = mkVkCmdBuildMicromapsEXT vkCmdBuildMicromapsEXTPtr
  pPInfos <- ContT $ allocaBytes @MicromapBuildInfoEXT ((Data.Vector.length (infos)) * 96)
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (96 * (i)) :: Ptr MicromapBuildInfoEXT) (e) . ($ ())) (infos)
  lift $ traceAroundEvent "vkCmdBuildMicromapsEXT" (vkCmdBuildMicromapsEXT'
                                                      (commandBufferHandle (commandBuffer))
                                                      ((fromIntegral (Data.Vector.length $ (infos)) :: Word32))
                                                      (pPInfos))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBuildMicromapsEXT
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Word32 -> Ptr MicromapBuildInfoEXT -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Word32 -> Ptr MicromapBuildInfoEXT -> IO Result

-- | vkBuildMicromapsEXT - Build a micromap on the host
--
-- = Description
--
-- This command fulfills the same task as 'cmdBuildMicromapsEXT' but is
-- executed by the host.
--
-- The 'buildMicromapsEXT' command provides the ability to initiate
-- multiple micromaps builds, however there is no ordering or
-- synchronization implied between any of the individual micromap builds.
--
-- Note
--
-- This means that there /cannot/ be any memory aliasing between any
-- micromap memories or scratch memories being used by any of the builds.
--
-- == Valid Usage
--
-- -   #VUID-vkBuildMicromapsEXT-pInfos-07461# For each @pInfos@[i],
--     @dstMicromap@ /must/ have been created with a value of
--     'MicromapCreateInfoEXT'::@size@ greater than or equal to the memory
--     size required by the build operation, as returned by
--     'getMicromapBuildSizesEXT' with @pBuildInfo@ = @pInfos@[i]
--
-- -   #VUID-vkBuildMicromapsEXT-mode-07462# The @mode@ member of each
--     element of @pInfos@ /must/ be a valid 'BuildMicromapModeEXT' value
--
-- -   #VUID-vkBuildMicromapsEXT-dstMicromap-07463# The @dstMicromap@
--     member of any element of @pInfos@ /must/ be a valid
--     'Vulkan.Extensions.Handles.MicromapEXT' handle
--
-- -   #VUID-vkBuildMicromapsEXT-pInfos-07464# For each element of @pInfos@
--     its @type@ member /must/ match the value of
--     'MicromapCreateInfoEXT'::@type@ when its @dstMicromap@ was created
--
-- -   #VUID-vkBuildMicromapsEXT-dstMicromap-07465# The range of memory
--     backing the @dstMicromap@ member of any element of @pInfos@ that is
--     accessed by this command /must/ not overlap the memory backing the
--     @dstMicromap@ member of any other element of @pInfos@, which is
--     accessed by this command
--
-- -   #VUID-vkBuildMicromapsEXT-dstMicromap-07466# The range of memory
--     backing the @dstMicromap@ member of any element of @pInfos@ that is
--     accessed by this command /must/ not overlap the memory backing the
--     @scratchData@ member of any element of @pInfos@ (including the same
--     element), which is accessed by this command
--
-- -   #VUID-vkBuildMicromapsEXT-scratchData-07467# The range of memory
--     backing the @scratchData@ member of any element of @pInfos@ that is
--     accessed by this command /must/ not overlap the memory backing the
--     @scratchData@ member of any other element of @pInfos@, which is
--     accessed by this command
--
-- -   #VUID-vkBuildMicromapsEXT-pInfos-07552# For each element of
--     @pInfos@, the @buffer@ used to create its @dstMicromap@ member
--     /must/ be bound to host-visible device memory
--
-- -   #VUID-vkBuildMicromapsEXT-pInfos-07553# For each element of
--     @pInfos@, all referenced addresses of @pInfos@[i].@data.hostAddress@
--     /must/ be valid host memory
--
-- -   #VUID-vkBuildMicromapsEXT-pInfos-07554# For each element of
--     @pInfos@, all referenced addresses of
--     @pInfos@[i].@triangleArray.hostAddress@ /must/ be valid host memory
--
-- -   #VUID-vkBuildMicromapsEXT-micromapHostCommands-07555# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromapHostCommands ::micromapHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkBuildMicromapsEXT-pInfos-07556# If @pInfos@[i].@mode@ is
--     'BUILD_MICROMAP_MODE_BUILD_EXT', all addresses between
--     @pInfos@[i].@scratchData.hostAddress@ and
--     @pInfos@[i].@scratchData.hostAddress@ + N - 1 /must/ be valid host
--     memory, where N is given by the @buildScratchSize@ member of the
--     'MicromapBuildSizesInfoEXT' structure returned from a call to
--     'getMicromapBuildSizesEXT' with an identical 'MicromapBuildInfoEXT'
--     structure and primitive count
--
-- -   #VUID-vkBuildMicromapsEXT-pInfos-07557# For each element of
--     @pInfos@, the @buffer@ used to create its @dstMicromap@ member
--     /must/ be bound to memory that was not allocated with multiple
--     instances
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkBuildMicromapsEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkBuildMicromapsEXT-deferredOperation-parameter# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @deferredOperation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkBuildMicromapsEXT-pInfos-parameter# @pInfos@ /must/ be a
--     valid pointer to an array of @infoCount@ valid
--     'MicromapBuildInfoEXT' structures
--
-- -   #VUID-vkBuildMicromapsEXT-infoCount-arraylength# @infoCount@ /must/
--     be greater than @0@
--
-- -   #VUID-vkBuildMicromapsEXT-deferredOperation-parent# If
--     @deferredOperation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device', 'MicromapBuildInfoEXT'
buildMicromapsEXT :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the 'Vulkan.Core10.Handles.Device' for which the micromaps
                     -- are being built.
                     Device
                  -> -- | @deferredOperation@ is an optional
                     -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' to
                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#deferred-host-operations-requesting request deferral>
                     -- for this command.
                     DeferredOperationKHR
                  -> -- | @pInfos@ is a pointer to an array of @infoCount@ 'MicromapBuildInfoEXT'
                     -- structures defining the geometry used to build each micromap.
                     ("infos" ::: Vector MicromapBuildInfoEXT)
                  -> io (Result)
buildMicromapsEXT device deferredOperation infos = liftIO . evalContT $ do
  let vkBuildMicromapsEXTPtr = pVkBuildMicromapsEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkBuildMicromapsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBuildMicromapsEXT is null" Nothing Nothing
  let vkBuildMicromapsEXT' = mkVkBuildMicromapsEXT vkBuildMicromapsEXTPtr
  pPInfos <- ContT $ allocaBytes @MicromapBuildInfoEXT ((Data.Vector.length (infos)) * 96)
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPInfos `plusPtr` (96 * (i)) :: Ptr MicromapBuildInfoEXT) (e) . ($ ())) (infos)
  r <- lift $ traceAroundEvent "vkBuildMicromapsEXT" (vkBuildMicromapsEXT'
                                                        (deviceHandle (device))
                                                        (deferredOperation)
                                                        ((fromIntegral (Data.Vector.length $ (infos)) :: Word32))
                                                        (pPInfos))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyMicromapEXT
  :: FunPtr (Ptr Device_T -> MicromapEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> MicromapEXT -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyMicromapEXT - Destroy a micromap object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyMicromapEXT-micromap-07441# All submitted commands
--     that refer to @micromap@ /must/ have completed execution
--
-- -   #VUID-vkDestroyMicromapEXT-micromap-07442# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @micromap@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyMicromapEXT-micromap-07443# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @micromap@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyMicromapEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyMicromapEXT-micromap-parameter# If @micromap@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @micromap@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.MicromapEXT' handle
--
-- -   #VUID-vkDestroyMicromapEXT-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyMicromapEXT-micromap-parent# If @micromap@ is a valid
--     handle, it /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @micromap@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.MicromapEXT'
destroyMicromapEXT :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the logical device that destroys the micromap.
                      Device
                   -> -- | @micromap@ is the micromap to destroy.
                      MicromapEXT
                   -> -- | @pAllocator@ controls host memory allocation as described in the
                      -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                      -- chapter.
                      ("allocator" ::: Maybe AllocationCallbacks)
                   -> io ()
destroyMicromapEXT device micromap allocator = liftIO . evalContT $ do
  let vkDestroyMicromapEXTPtr = pVkDestroyMicromapEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyMicromapEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyMicromapEXT is null" Nothing Nothing
  let vkDestroyMicromapEXT' = mkVkDestroyMicromapEXT vkDestroyMicromapEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyMicromapEXT" (vkDestroyMicromapEXT'
                                                    (deviceHandle (device))
                                                    (micromap)
                                                    pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMicromapEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyMicromapInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyMicromapInfoEXT -> IO ()

-- | vkCmdCopyMicromapEXT - Copy a micromap
--
-- = Description
--
-- This command copies the @pInfo->src@ micromap to the @pInfo->dst@
-- micromap in the manner specified by @pInfo->mode@.
--
-- Accesses to @pInfo->src@ and @pInfo->dst@ /must/ be
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-types access type>
-- of 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT' or
-- 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT' as
-- appropriate.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyMicromapEXT-buffer-07529# The @buffer@ used to create
--     @pInfo->src@ /must/ be bound to device memory
--
-- -   #VUID-vkCmdCopyMicromapEXT-buffer-07530# The @buffer@ used to create
--     @pInfo->dst@ /must/ be bound to device memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMicromapEXT-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyMicromapEXT-pInfo-parameter# @pInfo@ /must/ be a
--     valid pointer to a valid 'CopyMicromapInfoEXT' structure
--
-- -   #VUID-vkCmdCopyMicromapEXT-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyMicromapEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdCopyMicromapEXT-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyMicromapEXT-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Compute                                                                                                               | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyMicromapInfoEXT'
cmdCopyMicromapEXT :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command will be
                      -- recorded.
                      CommandBuffer
                   -> -- | @pInfo@ is a pointer to a 'CopyMicromapInfoEXT' structure defining the
                      -- copy operation.
                      CopyMicromapInfoEXT
                   -> io ()
cmdCopyMicromapEXT commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyMicromapEXTPtr = pVkCmdCopyMicromapEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyMicromapEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyMicromapEXT is null" Nothing Nothing
  let vkCmdCopyMicromapEXT' = mkVkCmdCopyMicromapEXT vkCmdCopyMicromapEXTPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ traceAroundEvent "vkCmdCopyMicromapEXT" (vkCmdCopyMicromapEXT'
                                                    (commandBufferHandle (commandBuffer))
                                                    pInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyMicromapEXT
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMicromapInfoEXT -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMicromapInfoEXT -> IO Result

-- | vkCopyMicromapEXT - Copy a micromap on the host
--
-- = Description
--
-- This command fulfills the same task as 'cmdCopyMicromapEXT' but is
-- executed by the host.
--
-- == Valid Usage
--
-- -   #VUID-vkCopyMicromapEXT-deferredOperation-03677# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     it /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' object
--
-- -   #VUID-vkCopyMicromapEXT-deferredOperation-03678# Any previous
--     deferred operation that was associated with @deferredOperation@
--     /must/ be complete
--
-- -   #VUID-vkCopyMicromapEXT-buffer-07558# The @buffer@ used to create
--     @pInfo->src@ /must/ be bound to host-visible device memory
--
-- -   #VUID-vkCopyMicromapEXT-buffer-07559# The @buffer@ used to create
--     @pInfo->dst@ /must/ be bound to host-visible device memory
--
-- -   #VUID-vkCopyMicromapEXT-micromapHostCommands-07560# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromapHostCommands ::micromapHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkCopyMicromapEXT-buffer-07561# The @buffer@ used to create
--     @pInfo->src@ /must/ be bound to memory that was not allocated with
--     multiple instances
--
-- -   #VUID-vkCopyMicromapEXT-buffer-07562# The @buffer@ used to create
--     @pInfo->dst@ /must/ be bound to memory that was not allocated with
--     multiple instances
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCopyMicromapEXT-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCopyMicromapEXT-deferredOperation-parameter# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @deferredOperation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkCopyMicromapEXT-pInfo-parameter# @pInfo@ /must/ be a valid
--     pointer to a valid 'CopyMicromapInfoEXT' structure
--
-- -   #VUID-vkCopyMicromapEXT-deferredOperation-parent# If
--     @deferredOperation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'CopyMicromapInfoEXT', 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
copyMicromapEXT :: forall io
                 . (MonadIO io)
                => -- | @device@ is the device which owns the micromaps.
                   Device
                -> -- | @deferredOperation@ is an optional
                   -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' to
                   -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#deferred-host-operations-requesting request deferral>
                   -- for this command.
                   DeferredOperationKHR
                -> -- | @pInfo@ is a pointer to a 'CopyMicromapInfoEXT' structure defining the
                   -- copy operation.
                   CopyMicromapInfoEXT
                -> io (Result)
copyMicromapEXT device deferredOperation info = liftIO . evalContT $ do
  let vkCopyMicromapEXTPtr = pVkCopyMicromapEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCopyMicromapEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyMicromapEXT is null" Nothing Nothing
  let vkCopyMicromapEXT' = mkVkCopyMicromapEXT vkCopyMicromapEXTPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkCopyMicromapEXT" (vkCopyMicromapEXT'
                                                      (deviceHandle (device))
                                                      (deferredOperation)
                                                      pInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMicromapToMemoryEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyMicromapToMemoryInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyMicromapToMemoryInfoEXT -> IO ()

-- | vkCmdCopyMicromapToMemoryEXT - Copy a micromap to device memory
--
-- = Description
--
-- Accesses to @pInfo->src@ /must/ be
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-types access type>
-- of 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT'.
-- Accesses to the buffer indicated by @pInfo->dst.deviceAddress@ /must/ be
-- synchronized with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
-- pipeline stage and an access type of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_WRITE_BIT'.
--
-- This command produces the same results as 'copyMicromapToMemoryEXT', but
-- writes its result to a device address, and is executed on the device
-- rather than the host. The output /may/ not necessarily be bit-for-bit
-- identical, but it can be equally used by either
-- 'cmdCopyMemoryToMicromapEXT' or 'copyMemoryToMicromapEXT'.
--
-- The defined header structure for the serialized data consists of:
--
-- -   'Vulkan.Core10.APIConstants.UUID_SIZE' bytes of data matching
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties'::@driverUUID@
--
-- -   'Vulkan.Core10.APIConstants.UUID_SIZE' bytes of data identifying the
--     compatibility for comparison using
--     'getDeviceMicromapCompatibilityEXT' The serialized data is written
--     to the buffer (or read from the buffer) according to the host
--     endianness.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-pInfo-07536#
--     @pInfo->dst.deviceAddress@ /must/ be a valid device address for a
--     buffer bound to device memory
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-pInfo-07537#
--     @pInfo->dst.deviceAddress@ /must/ be aligned to @256@ bytes
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-pInfo-07538# If the buffer
--     pointed to by @pInfo->dst.deviceAddress@ is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-buffer-07539# The @buffer@ used
--     to create @pInfo->src@ /must/ be bound to device memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-pInfo-parameter# @pInfo@ /must/
--     be a valid pointer to a valid 'CopyMicromapToMemoryInfoEXT'
--     structure
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-renderpass# This command /must/
--     only be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyMicromapToMemoryEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Compute                                                                                                               | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyMicromapToMemoryInfoEXT'
cmdCopyMicromapToMemoryEXT :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command will be
                              -- recorded.
                              CommandBuffer
                           -> -- | @pInfo@ is an a pointer to a 'CopyMicromapToMemoryInfoEXT' structure
                              -- defining the copy operation.
                              CopyMicromapToMemoryInfoEXT
                           -> io ()
cmdCopyMicromapToMemoryEXT commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyMicromapToMemoryEXTPtr = pVkCmdCopyMicromapToMemoryEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyMicromapToMemoryEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyMicromapToMemoryEXT is null" Nothing Nothing
  let vkCmdCopyMicromapToMemoryEXT' = mkVkCmdCopyMicromapToMemoryEXT vkCmdCopyMicromapToMemoryEXTPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ traceAroundEvent "vkCmdCopyMicromapToMemoryEXT" (vkCmdCopyMicromapToMemoryEXT'
                                                            (commandBufferHandle (commandBuffer))
                                                            pInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyMicromapToMemoryEXT
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMicromapToMemoryInfoEXT -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMicromapToMemoryInfoEXT -> IO Result

-- | vkCopyMicromapToMemoryEXT - Serialize a micromap on the host
--
-- = Description
--
-- This command fulfills the same task as 'cmdCopyMicromapToMemoryEXT' but
-- is executed by the host.
--
-- This command produces the same results as 'cmdCopyMicromapToMemoryEXT',
-- but writes its result directly to a host pointer, and is executed on the
-- host rather than the device. The output /may/ not necessarily be
-- bit-for-bit identical, but it can be equally used by either
-- 'cmdCopyMemoryToMicromapEXT' or 'copyMemoryToMicromapEXT'.
--
-- == Valid Usage
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-deferredOperation-03677# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     it /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' object
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-deferredOperation-03678# Any
--     previous deferred operation that was associated with
--     @deferredOperation@ /must/ be complete
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-buffer-07568# The @buffer@ used to
--     create @pInfo->src@ /must/ be bound to host-visible device memory
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-pInfo-07569#
--     @pInfo->dst.hostAddress@ /must/ be a valid host pointer
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-pInfo-07570#
--     @pInfo->dst.hostAddress@ /must/ be aligned to 16 bytes
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-micromapHostCommands-07571# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromapHostCommands ::micromapHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-buffer-07572# The @buffer@ used to
--     create @pInfo->src@ /must/ be bound to memory that was not allocated
--     with multiple instances
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-deferredOperation-parameter# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @deferredOperation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-pInfo-parameter# @pInfo@ /must/ be a
--     valid pointer to a valid 'CopyMicromapToMemoryInfoEXT' structure
--
-- -   #VUID-vkCopyMicromapToMemoryEXT-deferredOperation-parent# If
--     @deferredOperation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'CopyMicromapToMemoryInfoEXT',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
copyMicromapToMemoryEXT :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the device which owns @pInfo->src@.
                           Device
                        -> -- | @deferredOperation@ is an optional
                           -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' to
                           -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#deferred-host-operations-requesting request deferral>
                           -- for this command.
                           DeferredOperationKHR
                        -> -- | @pInfo@ is a pointer to a 'CopyMicromapToMemoryInfoEXT' structure
                           -- defining the copy operation.
                           CopyMicromapToMemoryInfoEXT
                        -> io (Result)
copyMicromapToMemoryEXT device deferredOperation info = liftIO . evalContT $ do
  let vkCopyMicromapToMemoryEXTPtr = pVkCopyMicromapToMemoryEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCopyMicromapToMemoryEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyMicromapToMemoryEXT is null" Nothing Nothing
  let vkCopyMicromapToMemoryEXT' = mkVkCopyMicromapToMemoryEXT vkCopyMicromapToMemoryEXTPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkCopyMicromapToMemoryEXT" (vkCopyMicromapToMemoryEXT'
                                                              (deviceHandle (device))
                                                              (deferredOperation)
                                                              pInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyMemoryToMicromapEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyMemoryToMicromapInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr CopyMemoryToMicromapInfoEXT -> IO ()

-- | vkCmdCopyMemoryToMicromapEXT - Copy device memory to a micromap
--
-- = Description
--
-- Accesses to @pInfo->dst@ /must/ be
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-types access type>
-- of 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT'.
-- Accesses to the buffer indicated by @pInfo->src.deviceAddress@ /must/ be
-- synchronized with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
-- pipeline stage and an access type of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_READ_BIT'.
--
-- This command can accept micromaps produced by either
-- 'cmdCopyMicromapToMemoryEXT' or 'copyMicromapToMemoryEXT'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-pInfo-07543#
--     @pInfo->src.deviceAddress@ /must/ be a valid device address for a
--     buffer bound to device memory
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-pInfo-07544#
--     @pInfo->src.deviceAddress@ /must/ be aligned to @256@ bytes
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-pInfo-07545# If the buffer
--     pointed to by @pInfo->src.deviceAddress@ is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-buffer-07546# The @buffer@ used
--     to create @pInfo->dst@ /must/ be bound to device memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-pInfo-parameter# @pInfo@ /must/
--     be a valid pointer to a valid 'CopyMemoryToMicromapInfoEXT'
--     structure
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-renderpass# This command /must/
--     only be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyMemoryToMicromapEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Compute                                                                                                               | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'CopyMemoryToMicromapInfoEXT'
cmdCopyMemoryToMicromapEXT :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command will be
                              -- recorded.
                              CommandBuffer
                           -> -- | @pInfo@ is a pointer to a 'CopyMicromapToMemoryInfoEXT' structure
                              -- defining the copy operation.
                              CopyMemoryToMicromapInfoEXT
                           -> io ()
cmdCopyMemoryToMicromapEXT commandBuffer info = liftIO . evalContT $ do
  let vkCmdCopyMemoryToMicromapEXTPtr = pVkCmdCopyMemoryToMicromapEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyMemoryToMicromapEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyMemoryToMicromapEXT is null" Nothing Nothing
  let vkCmdCopyMemoryToMicromapEXT' = mkVkCmdCopyMemoryToMicromapEXT vkCmdCopyMemoryToMicromapEXTPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ traceAroundEvent "vkCmdCopyMemoryToMicromapEXT" (vkCmdCopyMemoryToMicromapEXT'
                                                            (commandBufferHandle (commandBuffer))
                                                            pInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCopyMemoryToMicromapEXT
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMemoryToMicromapInfoEXT -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMemoryToMicromapInfoEXT -> IO Result

-- | vkCopyMemoryToMicromapEXT - Deserialize a micromap on the host
--
-- = Description
--
-- This command fulfills the same task as 'cmdCopyMemoryToMicromapEXT' but
-- is executed by the host.
--
-- This command can accept micromaps produced by either
-- 'cmdCopyMicromapToMemoryEXT' or 'copyMicromapToMemoryEXT'.
--
-- == Valid Usage
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-deferredOperation-03677# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     it /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' object
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-deferredOperation-03678# Any
--     previous deferred operation that was associated with
--     @deferredOperation@ /must/ be complete
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-pInfo-07563#
--     @pInfo->src.hostAddress@ /must/ be a valid host pointer
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-pInfo-07564#
--     @pInfo->src.hostAddress@ /must/ be aligned to 16 bytes
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-buffer-07565# The @buffer@ used to
--     create @pInfo->dst@ /must/ be bound to host-visible device memory
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-micromapHostCommands-07566# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromapHostCommands ::micromapHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-buffer-07567# The @buffer@ used to
--     create @pInfo->dst@ /must/ be bound to memory that was not allocated
--     with multiple instances
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-deferredOperation-parameter# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @deferredOperation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-pInfo-parameter# @pInfo@ /must/ be a
--     valid pointer to a valid 'CopyMemoryToMicromapInfoEXT' structure
--
-- -   #VUID-vkCopyMemoryToMicromapEXT-deferredOperation-parent# If
--     @deferredOperation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'CopyMemoryToMicromapInfoEXT',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
copyMemoryToMicromapEXT :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the device which owns @pInfo->dst@.
                           Device
                        -> -- | @deferredOperation@ is an optional
                           -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' to
                           -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#deferred-host-operations-requesting request deferral>
                           -- for this command.
                           DeferredOperationKHR
                        -> -- | @pInfo@ is a pointer to a 'CopyMemoryToMicromapInfoEXT' structure
                           -- defining the copy operation.
                           CopyMemoryToMicromapInfoEXT
                        -> io (Result)
copyMemoryToMicromapEXT device deferredOperation info = liftIO . evalContT $ do
  let vkCopyMemoryToMicromapEXTPtr = pVkCopyMemoryToMicromapEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCopyMemoryToMicromapEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCopyMemoryToMicromapEXT is null" Nothing Nothing
  let vkCopyMemoryToMicromapEXT' = mkVkCopyMemoryToMicromapEXT vkCopyMemoryToMicromapEXTPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkCopyMemoryToMicromapEXT" (vkCopyMemoryToMicromapEXT'
                                                              (deviceHandle (device))
                                                              (deferredOperation)
                                                              pInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteMicromapsPropertiesEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr MicromapEXT -> QueryType -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr MicromapEXT -> QueryType -> QueryPool -> Word32 -> IO ()

-- | vkCmdWriteMicromapsPropertiesEXT - Write micromap result parameters to
-- query results.
--
-- = Description
--
-- Accesses to any of the micromaps listed in @pMicromaps@ /must/ be
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-access-types access type>
-- of 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT'.
--
-- -   If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT',
--     then the value written out is the number of bytes required by a
--     serialized micromap.
--
-- -   If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT',
--     then the value written out is the number of bytes required by a
--     compacted micromap.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-queryPool-07525# @queryPool@
--     /must/ have been created with a @queryType@ matching @queryType@
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-queryPool-07526# The queries
--     identified by @queryPool@ and @firstQuery@ /must/ be /unavailable/
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-buffer-07527# The @buffer@
--     used to create each micromap in @pMicrmaps@ /must/ be bound to
--     device memory
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-query-07528# The sum of
--     @query@ plus @micromapCount@ /must/ be less than or equal to the
--     number of queries in @queryPool@
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-pMicromaps-07501# All
--     micromaps in @pMicromaps@ /must/ have been constructed prior to the
--     execution of this command
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-pMicromaps-07502# All
--     micromaps in @pMicromaps@ /must/ have been constructed with
--     'BUILD_MICROMAP_ALLOW_COMPACTION_BIT_EXT' if @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT'
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-queryType-07503# @queryType@
--     /must/ be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-pMicromaps-parameter#
--     @pMicromaps@ /must/ be a valid pointer to an array of
--     @micromapCount@ valid 'Vulkan.Extensions.Handles.MicromapEXT'
--     handles
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-queryType-parameter#
--     @queryType@ /must/ be a valid
--     'Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-queryPool-parameter#
--     @queryPool@ /must/ be a valid 'Vulkan.Core10.Handles.QueryPool'
--     handle
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-micromapCount-arraylength#
--     @micromapCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdWriteMicromapsPropertiesEXT-commonparent# Each of
--     @commandBuffer@, @queryPool@, and the elements of @pMicromaps@
--     /must/ have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Compute                                                                                                               | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.Handles.MicromapEXT',
-- 'Vulkan.Core10.Handles.QueryPool',
-- 'Vulkan.Core10.Enums.QueryType.QueryType'
cmdWriteMicromapsPropertiesEXT :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @pMicromaps@ is a pointer to an array of existing previously built
                                  -- micromaps.
                                  ("micromaps" ::: Vector MicromapEXT)
                               -> -- | @queryType@ is a 'Vulkan.Core10.Enums.QueryType.QueryType' value
                                  -- specifying the type of queries managed by the pool.
                                  QueryType
                               -> -- | @queryPool@ is the query pool that will manage the results of the query.
                                  QueryPool
                               -> -- | @firstQuery@ is the first query index within the query pool that will
                                  -- contain the @micromapCount@ number of results.
                                  ("firstQuery" ::: Word32)
                               -> io ()
cmdWriteMicromapsPropertiesEXT commandBuffer
                                 micromaps
                                 queryType
                                 queryPool
                                 firstQuery = liftIO . evalContT $ do
  let vkCmdWriteMicromapsPropertiesEXTPtr = pVkCmdWriteMicromapsPropertiesEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdWriteMicromapsPropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteMicromapsPropertiesEXT is null" Nothing Nothing
  let vkCmdWriteMicromapsPropertiesEXT' = mkVkCmdWriteMicromapsPropertiesEXT vkCmdWriteMicromapsPropertiesEXTPtr
  pPMicromaps <- ContT $ allocaBytes @MicromapEXT ((Data.Vector.length (micromaps)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMicromaps `plusPtr` (8 * (i)) :: Ptr MicromapEXT) (e)) (micromaps)
  lift $ traceAroundEvent "vkCmdWriteMicromapsPropertiesEXT" (vkCmdWriteMicromapsPropertiesEXT'
                                                                (commandBufferHandle (commandBuffer))
                                                                ((fromIntegral (Data.Vector.length $ (micromaps)) :: Word32))
                                                                (pPMicromaps)
                                                                (queryType)
                                                                (queryPool)
                                                                (firstQuery))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWriteMicromapsPropertiesEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr MicromapEXT -> QueryType -> CSize -> Ptr () -> CSize -> IO Result) -> Ptr Device_T -> Word32 -> Ptr MicromapEXT -> QueryType -> CSize -> Ptr () -> CSize -> IO Result

-- | vkWriteMicromapsPropertiesEXT - Query micromap meta-data on the host
--
-- = Description
--
-- This command fulfills the same task as 'cmdWriteMicromapsPropertiesEXT'
-- but is executed by the host.
--
-- == Valid Usage
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-pMicromaps-07501# All micromaps
--     in @pMicromaps@ /must/ have been constructed prior to the execution
--     of this command
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-pMicromaps-07502# All micromaps
--     in @pMicromaps@ /must/ have been constructed with
--     'BUILD_MICROMAP_ALLOW_COMPACTION_BIT_EXT' if @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT'
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-queryType-07503# @queryType@
--     /must/ be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT'
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-queryType-07573# If @queryType@
--     is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT',
--     then @stride@ /must/ be a multiple of the size of
--     'Vulkan.Core10.FundamentalTypes.DeviceSize'
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-queryType-07574# If @queryType@
--     is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT',
--     then @pData@ /must/ point to a
--     'Vulkan.Core10.FundamentalTypes.DeviceSize'
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-queryType-07575# If @queryType@
--     is
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-dataSize-07576# @dataSize@
--     /must/ be greater than or equal to @micromapCount@*@stride@
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-buffer-07577# The @buffer@ used
--     to create each micromap in @pMicromaps@ /must/ be bound to
--     host-visible device memory
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-micromapHostCommands-07578# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromapHostCommands ::micromapHostCommands>
--     feature /must/ be enabled
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-buffer-07579# The @buffer@ used
--     to create each micromap in @pMicromaps@ /must/ be bound to memory
--     that was not allocated with multiple instances
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-pMicromaps-parameter#
--     @pMicromaps@ /must/ be a valid pointer to an array of
--     @micromapCount@ valid 'Vulkan.Extensions.Handles.MicromapEXT'
--     handles
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-queryType-parameter# @queryType@
--     /must/ be a valid 'Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-pData-parameter# @pData@ /must/
--     be a valid pointer to an array of @dataSize@ bytes
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-micromapCount-arraylength#
--     @micromapCount@ /must/ be greater than @0@
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-dataSize-arraylength# @dataSize@
--     /must/ be greater than @0@
--
-- -   #VUID-vkWriteMicromapsPropertiesEXT-pMicromaps-parent# Each element
--     of @pMicromaps@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.MicromapEXT',
-- 'Vulkan.Core10.Enums.QueryType.QueryType'
writeMicromapsPropertiesEXT :: forall io
                             . (MonadIO io)
                            => -- | @device@ is the device which owns the micromaps in @pMicromaps@.
                               Device
                            -> -- | @pMicromaps@ is a pointer to an array of existing previously built
                               -- micromaps.
                               ("micromaps" ::: Vector MicromapEXT)
                            -> -- | @queryType@ is a 'Vulkan.Core10.Enums.QueryType.QueryType' value
                               -- specifying the property to be queried.
                               QueryType
                            -> -- | @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
                               ("dataSize" ::: Word64)
                            -> -- | @pData@ is a pointer to a user-allocated buffer where the results will
                               -- be written.
                               ("data" ::: Ptr ())
                            -> -- | @stride@ is the stride in bytes between results for individual queries
                               -- within @pData@.
                               ("stride" ::: Word64)
                            -> io ()
writeMicromapsPropertiesEXT device
                              micromaps
                              queryType
                              dataSize
                              data'
                              stride = liftIO . evalContT $ do
  let vkWriteMicromapsPropertiesEXTPtr = pVkWriteMicromapsPropertiesEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkWriteMicromapsPropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWriteMicromapsPropertiesEXT is null" Nothing Nothing
  let vkWriteMicromapsPropertiesEXT' = mkVkWriteMicromapsPropertiesEXT vkWriteMicromapsPropertiesEXTPtr
  pPMicromaps <- ContT $ allocaBytes @MicromapEXT ((Data.Vector.length (micromaps)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMicromaps `plusPtr` (8 * (i)) :: Ptr MicromapEXT) (e)) (micromaps)
  r <- lift $ traceAroundEvent "vkWriteMicromapsPropertiesEXT" (vkWriteMicromapsPropertiesEXT'
                                                                  (deviceHandle (device))
                                                                  ((fromIntegral (Data.Vector.length $ (micromaps)) :: Word32))
                                                                  (pPMicromaps)
                                                                  (queryType)
                                                                  (CSize (dataSize))
                                                                  (data')
                                                                  (CSize (stride)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceMicromapCompatibilityEXT
  :: FunPtr (Ptr Device_T -> Ptr MicromapVersionInfoEXT -> Ptr AccelerationStructureCompatibilityKHR -> IO ()) -> Ptr Device_T -> Ptr MicromapVersionInfoEXT -> Ptr AccelerationStructureCompatibilityKHR -> IO ()

-- | vkGetDeviceMicromapCompatibilityEXT - Check if a serialized micromap is
-- compatible with the current device
--
-- == Valid Usage
--
-- -   #VUID-vkGetDeviceMicromapCompatibilityEXT-micromap-07551# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromap micromap>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceMicromapCompatibilityEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceMicromapCompatibilityEXT-pVersionInfo-parameter#
--     @pVersionInfo@ /must/ be a valid pointer to a valid
--     'MicromapVersionInfoEXT' structure
--
-- -   #VUID-vkGetDeviceMicromapCompatibilityEXT-pCompatibility-parameter#
--     @pCompatibility@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCompatibilityKHR'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCompatibilityKHR',
-- 'Vulkan.Core10.Handles.Device', 'MicromapVersionInfoEXT'
getDeviceMicromapCompatibilityEXT :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the device to check the version against.
                                     Device
                                  -> -- | @pVersionInfo@ is a pointer to a 'MicromapVersionInfoEXT' structure
                                     -- specifying version information to check against the device.
                                     MicromapVersionInfoEXT
                                  -> io (AccelerationStructureCompatibilityKHR)
getDeviceMicromapCompatibilityEXT device versionInfo = liftIO . evalContT $ do
  let vkGetDeviceMicromapCompatibilityEXTPtr = pVkGetDeviceMicromapCompatibilityEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceMicromapCompatibilityEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceMicromapCompatibilityEXT is null" Nothing Nothing
  let vkGetDeviceMicromapCompatibilityEXT' = mkVkGetDeviceMicromapCompatibilityEXT vkGetDeviceMicromapCompatibilityEXTPtr
  pVersionInfo <- ContT $ withCStruct (versionInfo)
  pPCompatibility <- ContT $ bracket (callocBytes @AccelerationStructureCompatibilityKHR 4) free
  lift $ traceAroundEvent "vkGetDeviceMicromapCompatibilityEXT" (vkGetDeviceMicromapCompatibilityEXT'
                                                                   (deviceHandle (device))
                                                                   pVersionInfo
                                                                   (pPCompatibility))
  pCompatibility <- lift $ peek @AccelerationStructureCompatibilityKHR pPCompatibility
  pure $ (pCompatibility)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMicromapBuildSizesEXT
  :: FunPtr (Ptr Device_T -> AccelerationStructureBuildTypeKHR -> Ptr MicromapBuildInfoEXT -> Ptr MicromapBuildSizesInfoEXT -> IO ()) -> Ptr Device_T -> AccelerationStructureBuildTypeKHR -> Ptr MicromapBuildInfoEXT -> Ptr MicromapBuildSizesInfoEXT -> IO ()

-- | vkGetMicromapBuildSizesEXT - Retrieve the required size for a micromap
--
-- = Description
--
-- The @dstMicromap@ and @mode@ members of @pBuildInfo@ are ignored. Any
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR'
-- members of @pBuildInfo@ are ignored by this command.
--
-- A micromap created with the @micromapSize@ returned by this command
-- supports any build with a 'MicromapBuildInfoEXT' structure subject to
-- the following properties:
--
-- -   The build command is a host build command, and @buildType@ is
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR'
--
-- -   The build command is a device build command, and @buildType@ is
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_BUILD_TYPE_DEVICE_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_BUILD_TYPE_HOST_OR_DEVICE_KHR'
--
-- -   For 'MicromapBuildInfoEXT':
--
--     -   Its @type@, and @flags@ members are equal to @pBuildInfo->type@
--         and @pBuildInfo->flags@, respectively.
--
--     -   The sum of usage information in either @pUsageCounts@ or
--         @ppUsageCounts@ is equal to the sum of usage information in
--         either @pBuildInfo->pUsageCounts@ or
--         @pBuildInfo->ppUsageCounts@.
--
-- Similarly, the @buildScratchSize@ value will support any build command
-- specifying the 'BUILD_MICROMAP_MODE_BUILD_EXT' @mode@ under the above
-- conditions.
--
-- == Valid Usage
--
-- -   #VUID-vkGetMicromapBuildSizesEXT-dstMicromap-09180#
--     'MicromapBuildInfoEXT'::@dstMicromap@ /must/ have been created from
--     @device@
--
-- -   #VUID-vkGetMicromapBuildSizesEXT-micromap-07439# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromap micromap>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetMicromapBuildSizesEXT-device-07440# If @device@ was
--     created with multiple physical devices, then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetMicromapBuildSizesEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetMicromapBuildSizesEXT-buildType-parameter# @buildType@
--     /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureBuildTypeKHR'
--     value
--
-- -   #VUID-vkGetMicromapBuildSizesEXT-pBuildInfo-parameter# @pBuildInfo@
--     /must/ be a valid pointer to a valid 'MicromapBuildInfoEXT'
--     structure
--
-- -   #VUID-vkGetMicromapBuildSizesEXT-pSizeInfo-parameter# @pSizeInfo@
--     /must/ be a valid pointer to a 'MicromapBuildSizesInfoEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureBuildTypeKHR',
-- 'Vulkan.Core10.Handles.Device', 'MicromapBuildInfoEXT',
-- 'MicromapBuildSizesInfoEXT'
getMicromapBuildSizesEXT :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that will be used for creating the
                            -- micromap.
                            Device
                         -> -- | @buildType@ defines whether host or device operations (or both) are
                            -- being queried for.
                            AccelerationStructureBuildTypeKHR
                         -> -- | @pBuildInfo@ is a pointer to a 'MicromapBuildInfoEXT' structure
                            -- describing parameters of a build operation.
                            MicromapBuildInfoEXT
                         -> io (("sizeInfo" ::: MicromapBuildSizesInfoEXT))
getMicromapBuildSizesEXT device buildType buildInfo = liftIO . evalContT $ do
  let vkGetMicromapBuildSizesEXTPtr = pVkGetMicromapBuildSizesEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetMicromapBuildSizesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMicromapBuildSizesEXT is null" Nothing Nothing
  let vkGetMicromapBuildSizesEXT' = mkVkGetMicromapBuildSizesEXT vkGetMicromapBuildSizesEXTPtr
  pBuildInfo <- ContT $ withCStruct (buildInfo)
  pPSizeInfo <- ContT (withZeroCStruct @MicromapBuildSizesInfoEXT)
  lift $ traceAroundEvent "vkGetMicromapBuildSizesEXT" (vkGetMicromapBuildSizesEXT'
                                                          (deviceHandle (device))
                                                          (buildType)
                                                          pBuildInfo
                                                          (pPSizeInfo))
  pSizeInfo <- lift $ peekCStruct @MicromapBuildSizesInfoEXT pPSizeInfo
  pure $ (pSizeInfo)


-- | VkMicromapBuildInfoEXT - Structure specifying the data used to build a
-- micromap
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
-- compilers compile 'MicromapTriangleEXT' to match this pattern.
--
-- For opacity micromaps, the data at @data@ is packed as either one bit
-- per element for 'OPACITY_MICROMAP_FORMAT_2_STATE_EXT' or two bits per
-- element for 'OPACITY_MICROMAP_FORMAT_4_STATE_EXT' and is packed from LSB
-- to MSB in each byte. The data at each index in those bytes is
-- interpreted as discussed in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#ray-opacity-micromap Ray Opacity Micromap>.
--
-- For displacement micromaps, the data at @data@ is interpreted as
-- discussed in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#displacement-micromap-encoding Displacement Micromap Encoding>.
--
-- == Valid Usage
--
-- -   #VUID-VkMicromapBuildInfoEXT-pUsageCounts-07516# Only one of
--     @pUsageCounts@ or @ppUsageCounts@ /can/ be a valid pointer, the
--     other /must/ be @NULL@
--
-- -   #VUID-VkMicromapBuildInfoEXT-type-07517# If @type@ is
--     'MICROMAP_TYPE_OPACITY_MICROMAP_EXT' the @format@ member of
--     'MicromapUsageEXT' /must/ be a valid value from
--     'OpacityMicromapFormatEXT'
--
-- -   #VUID-VkMicromapBuildInfoEXT-type-07518# If @type@ is
--     'MICROMAP_TYPE_OPACITY_MICROMAP_EXT' the @format@ member of
--     'MicromapTriangleEXT' /must/ be a valid value from
--     'OpacityMicromapFormatEXT'
--
-- -   #VUID-VkMicromapBuildInfoEXT-type-08704# If @type@ is
--     'MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV' the @format@ member of
--     'MicromapUsageEXT' /must/ be a valid value from
--     'Vulkan.Extensions.VK_NV_displacement_micromap.DisplacementMicromapFormatNV'
--
-- -   #VUID-VkMicromapBuildInfoEXT-type-08705# If @type@ is
--     'MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV' the @format@ member of
--     'MicromapTriangleEXT' /must/ be a valid value from
--     'Vulkan.Extensions.VK_NV_displacement_micromap.DisplacementMicromapFormatNV'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMicromapBuildInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MICROMAP_BUILD_INFO_EXT'
--
-- -   #VUID-VkMicromapBuildInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkMicromapBuildInfoEXT-type-parameter# @type@ /must/ be a
--     valid 'MicromapTypeEXT' value
--
-- -   #VUID-VkMicromapBuildInfoEXT-flags-parameter# @flags@ /must/ be a
--     valid combination of 'BuildMicromapFlagBitsEXT' values
--
-- -   #VUID-VkMicromapBuildInfoEXT-pUsageCounts-parameter# If
--     @usageCountsCount@ is not @0@, and @pUsageCounts@ is not @NULL@,
--     @pUsageCounts@ /must/ be a valid pointer to an array of
--     @usageCountsCount@ 'MicromapUsageEXT' structures
--
-- -   #VUID-VkMicromapBuildInfoEXT-ppUsageCounts-parameter# If
--     @usageCountsCount@ is not @0@, and @ppUsageCounts@ is not @NULL@,
--     @ppUsageCounts@ /must/ be a valid pointer to an array of
--     @usageCountsCount@ valid pointers to 'MicromapUsageEXT' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'BuildMicromapFlagsEXT', 'BuildMicromapModeEXT',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Extensions.Handles.MicromapEXT', 'MicromapTypeEXT',
-- 'MicromapUsageEXT', 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'buildMicromapsEXT', 'cmdBuildMicromapsEXT', 'getMicromapBuildSizesEXT'
data MicromapBuildInfoEXT = MicromapBuildInfoEXT
  { -- | @type@ is a 'MicromapTypeEXT' value specifying the type of micromap
    -- being built.
    type' :: MicromapTypeEXT
  , -- | @flags@ is a bitmask of 'BuildMicromapFlagBitsEXT' specifying additional
    -- parameters of the micromap.
    flags :: BuildMicromapFlagsEXT
  , -- | @mode@ is a 'BuildMicromapModeEXT' value specifying the type of
    -- operation to perform.
    mode :: BuildMicromapModeEXT
  , -- | @dstMicromap@ is a pointer to the target micromap for the build.
    dstMicromap :: MicromapEXT
  , -- | @pUsageCounts@ is a pointer to an array of 'MicromapUsageEXT'
    -- structures.
    usageCounts :: Vector MicromapUsageEXT
  , -- | @data@ is the device or host address to memory which contains the data
    -- for the micromap.
    data' :: DeviceOrHostAddressConstKHR
  , -- | @scratchData@ is the device or host address to memory that will be used
    -- as scratch memory for the build.
    scratchData :: DeviceOrHostAddressKHR
  , -- | @triangleArray@ is the device or host address to memory containing the
    -- 'MicromapTriangleEXT' data
    triangleArray :: DeviceOrHostAddressConstKHR
  , -- | @triangleArrayStride@ is the stride in bytes between each element of
    -- @triangleArray@
    triangleArrayStride :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MicromapBuildInfoEXT)
#endif
deriving instance Show MicromapBuildInfoEXT

instance ToCStruct MicromapBuildInfoEXT where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MicromapBuildInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MICROMAP_BUILD_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr MicromapTypeEXT)) (type')
    lift $ poke ((p `plusPtr` 20 :: Ptr BuildMicromapFlagsEXT)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr BuildMicromapModeEXT)) (mode)
    lift $ poke ((p `plusPtr` 32 :: Ptr MicromapEXT)) (dstMicromap)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (usageCounts)) :: Word32))
    pPUsageCounts' <- ContT $ allocaBytes @MicromapUsageEXT ((Data.Vector.length (usageCounts)) * 12)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPUsageCounts' `plusPtr` (12 * (i)) :: Ptr MicromapUsageEXT) (e)) (usageCounts)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr MicromapUsageEXT))) (pPUsageCounts')
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr MicromapUsageEXT)))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 64 :: Ptr DeviceOrHostAddressConstKHR)) (data') . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressKHR)) (scratchData) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 80 :: Ptr DeviceOrHostAddressConstKHR)) (triangleArray) . ($ ())
    lift $ poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (triangleArrayStride)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MICROMAP_BUILD_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr MicromapTypeEXT)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr BuildMicromapModeEXT)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr MicromapUsageEXT)))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 64 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 72 :: Ptr DeviceOrHostAddressKHR)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 80 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (zero)
    lift $ f

instance Zero MicromapBuildInfoEXT where
  zero = MicromapBuildInfoEXT
           zero
           zero
           zero
           zero
           mempty
           zero
           zero
           zero
           zero


-- | VkMicromapCreateInfoEXT - Structure specifying the parameters of a newly
-- created micromap object
--
-- = Description
--
-- If @deviceAddress@ is zero, no specific address is requested.
--
-- If @deviceAddress@ is not zero, @deviceAddress@ /must/ be an address
-- retrieved from an identically created micromap on the same
-- implementation. The micromap /must/ also be placed on an identically
-- created @buffer@ and at the same @offset@.
--
-- Applications /should/ avoid creating micromaps with application-provided
-- addresses and implementation-provided addresses in the same process, to
-- reduce the likelihood of
-- 'Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
-- errors.
--
-- Note
--
-- The expected usage for this is that a trace capture\/replay tool will
-- add the
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
-- flag to all buffers that use
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT',
-- and will add
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT'
-- to all buffers used as storage for a micromap where @deviceAddress@ is
-- not zero. This also means that the tool will need to add
-- 'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT'
-- to memory allocations to allow the flag to be set where the application
-- may not have otherwise required it. During capture the tool will save
-- the queried opaque device addresses in the trace. During replay, the
-- buffers will be created specifying the original address so any address
-- values stored in the trace data will remain valid.
--
-- Implementations are expected to separate such buffers in the GPU address
-- space so normal allocations will avoid using these addresses.
-- Apps\/tools should avoid mixing app-provided and implementation-provided
-- addresses for buffers created with
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT',
-- to avoid address space allocation conflicts.
--
-- If the micromap will be the target of a build operation, the required
-- size for a micromap /can/ be queried with 'getMicromapBuildSizesEXT'.
--
-- == Valid Usage
--
-- -   #VUID-VkMicromapCreateInfoEXT-deviceAddress-07433# If
--     @deviceAddress@ is not zero, @createFlags@ /must/ include
--     'MICROMAP_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT'
--
-- -   #VUID-VkMicromapCreateInfoEXT-createFlags-07434# If @createFlags@
--     includes 'MICROMAP_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT',
--     'PhysicalDeviceOpacityMicromapFeaturesEXT'::@micromapCaptureReplay@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkMicromapCreateInfoEXT-buffer-07435# @buffer@ /must/ have
--     been created with a @usage@ value containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_MICROMAP_STORAGE_BIT_EXT'
--
-- -   #VUID-VkMicromapCreateInfoEXT-buffer-07436# @buffer@ /must/ not have
--     been created with
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkMicromapCreateInfoEXT-offset-07437# The sum of @offset@ and
--     @size@ /must/ be less than the size of @buffer@
--
-- -   #VUID-VkMicromapCreateInfoEXT-offset-07438# @offset@ /must/ be a
--     multiple of @256@ bytes
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMicromapCreateInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MICROMAP_CREATE_INFO_EXT'
--
-- -   #VUID-VkMicromapCreateInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkMicromapCreateInfoEXT-createFlags-parameter# @createFlags@
--     /must/ be a valid combination of 'MicromapCreateFlagBitsEXT' values
--
-- -   #VUID-VkMicromapCreateInfoEXT-buffer-parameter# @buffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkMicromapCreateInfoEXT-type-parameter# @type@ /must/ be a
--     valid 'MicromapTypeEXT' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'MicromapCreateFlagsEXT',
-- 'MicromapTypeEXT', 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createMicromapEXT'
data MicromapCreateInfoEXT = MicromapCreateInfoEXT
  { -- | @createFlags@ is a bitmask of 'MicromapCreateFlagBitsEXT' specifying
    -- additional creation parameters of the micromap.
    createFlags :: MicromapCreateFlagsEXT
  , -- | @buffer@ is the buffer on which the micromap will be stored.
    buffer :: Buffer
  , -- | @offset@ is an offset in bytes from the base address of the buffer at
    -- which the micromap will be stored, and /must/ be a multiple of @256@.
    offset :: DeviceSize
  , -- | @size@ is the size required for the micromap.
    size :: DeviceSize
  , -- | @type@ is a 'MicromapTypeEXT' value specifying the type of micromap that
    -- will be created.
    type' :: MicromapTypeEXT
  , -- | @deviceAddress@ is the device address requested for the micromap if the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-micromapCaptureReplay micromapCaptureReplay>
    -- feature is being used.
    deviceAddress :: DeviceAddress
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MicromapCreateInfoEXT)
#endif
deriving instance Show MicromapCreateInfoEXT

instance ToCStruct MicromapCreateInfoEXT where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MicromapCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MICROMAP_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MicromapCreateFlagsEXT)) (createFlags)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 48 :: Ptr MicromapTypeEXT)) (type')
    poke ((p `plusPtr` 56 :: Ptr DeviceAddress)) (deviceAddress)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MICROMAP_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr MicromapTypeEXT)) (zero)
    f

instance FromCStruct MicromapCreateInfoEXT where
  peekCStruct p = do
    createFlags <- peek @MicromapCreateFlagsEXT ((p `plusPtr` 16 :: Ptr MicromapCreateFlagsEXT))
    buffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    type' <- peek @MicromapTypeEXT ((p `plusPtr` 48 :: Ptr MicromapTypeEXT))
    deviceAddress <- peek @DeviceAddress ((p `plusPtr` 56 :: Ptr DeviceAddress))
    pure $ MicromapCreateInfoEXT
             createFlags buffer offset size type' deviceAddress

instance Storable MicromapCreateInfoEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MicromapCreateInfoEXT where
  zero = MicromapCreateInfoEXT
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkMicromapVersionInfoEXT - Micromap version information
--
-- = Description
--
-- Note
--
-- @pVersionData@ is a /pointer/ to an array of
-- 2×'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values instead of two
-- 'Vulkan.Core10.APIConstants.UUID_SIZE' arrays as the expected use case
-- for this member is to be pointed at the header of a previously
-- serialized micromap (via 'cmdCopyMicromapToMemoryEXT' or
-- 'copyMicromapToMemoryEXT') that is loaded in memory. Using arrays would
-- necessitate extra memory copies of the UUIDs.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceMicromapCompatibilityEXT'
data MicromapVersionInfoEXT = MicromapVersionInfoEXT
  { -- | @pVersionData@ is a pointer to the version header of a micromap as
    -- defined in 'cmdCopyMicromapToMemoryEXT'
    --
    -- #VUID-VkMicromapVersionInfoEXT-pVersionData-parameter# @pVersionData@
    -- /must/ be a valid pointer to an array of
    -- \(2 \times \mathtt{VK\_UUID\_SIZE}\) @uint8_t@ values
    versionData :: ByteString }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MicromapVersionInfoEXT)
#endif
deriving instance Show MicromapVersionInfoEXT

instance ToCStruct MicromapVersionInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MicromapVersionInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MICROMAP_VERSION_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ unless (Data.ByteString.length (versionData) == 2 * UUID_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "VkMicromapVersionInfoEXT::versionData must be 2*VK_UUID_SIZE bytes" Nothing Nothing
    versionData' <- fmap (castPtr @CChar @Word8) . ContT $ unsafeUseAsCString (versionData)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr Word8))) versionData'
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MICROMAP_VERSION_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MicromapVersionInfoEXT where
  peekCStruct p = do
    versionData <- peek @(Ptr Word8) ((p `plusPtr` 16 :: Ptr (Ptr Word8)))
    versionData' <- packCStringLen ( castPtr @Word8 @CChar versionData
                                   , 2 * UUID_SIZE )
    pure $ MicromapVersionInfoEXT
             versionData'

instance Zero MicromapVersionInfoEXT where
  zero = MicromapVersionInfoEXT
           mempty


-- | VkCopyMicromapInfoEXT - Parameters for copying a micromap
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMicromapInfoEXT-mode-07531# @mode@ /must/ be
--     'COPY_MICROMAP_MODE_COMPACT_EXT' or 'COPY_MICROMAP_MODE_CLONE_EXT'
--
-- -   #VUID-VkCopyMicromapInfoEXT-src-07532# The source acceleration
--     structure @src@ /must/ have been constructed prior to the execution
--     of this command
--
-- -   #VUID-VkCopyMicromapInfoEXT-mode-07533# If @mode@ is
--     'COPY_MICROMAP_MODE_COMPACT_EXT', @src@ /must/ have been constructed
--     with 'BUILD_MICROMAP_ALLOW_COMPACTION_BIT_EXT' in the build
--
-- -   #VUID-VkCopyMicromapInfoEXT-buffer-07534# The @buffer@ used to
--     create @src@ /must/ be bound to device memory
--
-- -   #VUID-VkCopyMicromapInfoEXT-buffer-07535# The @buffer@ used to
--     create @dst@ /must/ be bound to device memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMicromapInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MICROMAP_INFO_EXT'
--
-- -   #VUID-VkCopyMicromapInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCopyMicromapInfoEXT-src-parameter# @src@ /must/ be a valid
--     'Vulkan.Extensions.Handles.MicromapEXT' handle
--
-- -   #VUID-VkCopyMicromapInfoEXT-dst-parameter# @dst@ /must/ be a valid
--     'Vulkan.Extensions.Handles.MicromapEXT' handle
--
-- -   #VUID-VkCopyMicromapInfoEXT-mode-parameter# @mode@ /must/ be a valid
--     'CopyMicromapModeEXT' value
--
-- -   #VUID-VkCopyMicromapInfoEXT-commonparent# Both of @dst@, and @src@
--     /must/ have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'CopyMicromapModeEXT', 'Vulkan.Extensions.Handles.MicromapEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdCopyMicromapEXT',
-- 'copyMicromapEXT'
data CopyMicromapInfoEXT = CopyMicromapInfoEXT
  { -- | @src@ is the source micromap for the copy.
    src :: MicromapEXT
  , -- | @dst@ is the target micromap for the copy.
    dst :: MicromapEXT
  , -- | @mode@ is a 'CopyMicromapModeEXT' value specifying additional operations
    -- to perform during the copy.
    mode :: CopyMicromapModeEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMicromapInfoEXT)
#endif
deriving instance Show CopyMicromapInfoEXT

instance ToCStruct CopyMicromapInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMicromapInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MICROMAP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MicromapEXT)) (src)
    poke ((p `plusPtr` 24 :: Ptr MicromapEXT)) (dst)
    poke ((p `plusPtr` 32 :: Ptr CopyMicromapModeEXT)) (mode)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MICROMAP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MicromapEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr MicromapEXT)) (zero)
    poke ((p `plusPtr` 32 :: Ptr CopyMicromapModeEXT)) (zero)
    f

instance FromCStruct CopyMicromapInfoEXT where
  peekCStruct p = do
    src <- peek @MicromapEXT ((p `plusPtr` 16 :: Ptr MicromapEXT))
    dst <- peek @MicromapEXT ((p `plusPtr` 24 :: Ptr MicromapEXT))
    mode <- peek @CopyMicromapModeEXT ((p `plusPtr` 32 :: Ptr CopyMicromapModeEXT))
    pure $ CopyMicromapInfoEXT
             src dst mode

instance Storable CopyMicromapInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CopyMicromapInfoEXT where
  zero = CopyMicromapInfoEXT
           zero
           zero
           zero


-- | VkCopyMicromapToMemoryInfoEXT - Parameters for serializing a micromap
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMicromapToMemoryInfoEXT-src-07540# The source micromap
--     @src@ /must/ have been constructed prior to the execution of this
--     command
--
-- -   #VUID-VkCopyMicromapToMemoryInfoEXT-dst-07541# The memory pointed to
--     by @dst@ /must/ be at least as large as the serialization size of
--     @src@, as reported by 'writeMicromapsPropertiesEXT' or
--     'cmdWriteMicromapsPropertiesEXT' with a query type of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT'
--
-- -   #VUID-VkCopyMicromapToMemoryInfoEXT-mode-07542# @mode@ /must/ be
--     'COPY_MICROMAP_MODE_SERIALIZE_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMicromapToMemoryInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MICROMAP_TO_MEMORY_INFO_EXT'
--
-- -   #VUID-VkCopyMicromapToMemoryInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkCopyMicromapToMemoryInfoEXT-src-parameter# @src@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.MicromapEXT' handle
--
-- -   #VUID-VkCopyMicromapToMemoryInfoEXT-mode-parameter# @mode@ /must/ be
--     a valid 'CopyMicromapModeEXT' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'CopyMicromapModeEXT',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR',
-- 'Vulkan.Extensions.Handles.MicromapEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyMicromapToMemoryEXT', 'copyMicromapToMemoryEXT'
data CopyMicromapToMemoryInfoEXT = CopyMicromapToMemoryInfoEXT
  { -- | @src@ is the source micromap for the copy
    src :: MicromapEXT
  , -- | @dst@ is the device or host address to memory which is the target for
    -- the copy
    dst :: DeviceOrHostAddressKHR
  , -- | @mode@ is a 'CopyMicromapModeEXT' value specifying additional operations
    -- to perform during the copy.
    mode :: CopyMicromapModeEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMicromapToMemoryInfoEXT)
#endif
deriving instance Show CopyMicromapToMemoryInfoEXT

instance ToCStruct CopyMicromapToMemoryInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMicromapToMemoryInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MICROMAP_TO_MEMORY_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr MicromapEXT)) (src)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressKHR)) (dst) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyMicromapModeEXT)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MICROMAP_TO_MEMORY_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr MicromapEXT)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyMicromapModeEXT)) (zero)
    lift $ f

instance Zero CopyMicromapToMemoryInfoEXT where
  zero = CopyMicromapToMemoryInfoEXT
           zero
           zero
           zero


-- | VkCopyMemoryToMicromapInfoEXT - Parameters for deserializing a micromap
--
-- == Valid Usage
--
-- -   #VUID-VkCopyMemoryToMicromapInfoEXT-src-07547# The source memory
--     pointed to by @src@ /must/ contain data previously serialized using
--     'cmdCopyMicromapToMemoryEXT'
--
-- -   #VUID-VkCopyMemoryToMicromapInfoEXT-mode-07548# @mode@ /must/ be
--     'COPY_MICROMAP_MODE_DESERIALIZE_EXT'
--
-- -   #VUID-VkCopyMemoryToMicromapInfoEXT-src-07549# The data in @src@
--     /must/ have a format compatible with the destination physical device
--     as returned by 'getDeviceMicromapCompatibilityEXT'
--
-- -   #VUID-VkCopyMemoryToMicromapInfoEXT-dst-07550# @dst@ /must/ have
--     been created with a @size@ greater than or equal to that used to
--     serialize the data in @src@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCopyMemoryToMicromapInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_MICROMAP_INFO_EXT'
--
-- -   #VUID-VkCopyMemoryToMicromapInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkCopyMemoryToMicromapInfoEXT-dst-parameter# @dst@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.MicromapEXT' handle
--
-- -   #VUID-VkCopyMemoryToMicromapInfoEXT-mode-parameter# @mode@ /must/ be
--     a valid 'CopyMicromapModeEXT' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'CopyMicromapModeEXT',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Extensions.Handles.MicromapEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdCopyMemoryToMicromapEXT', 'copyMemoryToMicromapEXT'
data CopyMemoryToMicromapInfoEXT = CopyMemoryToMicromapInfoEXT
  { -- | @src@ is the device or host address to memory containing the source data
    -- for the copy.
    src :: DeviceOrHostAddressConstKHR
  , -- | @dst@ is the target micromap for the copy.
    dst :: MicromapEXT
  , -- | @mode@ is a 'CopyMicromapModeEXT' value specifying additional operations
    -- to perform during the copy.
    mode :: CopyMicromapModeEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CopyMemoryToMicromapInfoEXT)
#endif
deriving instance Show CopyMemoryToMicromapInfoEXT

instance ToCStruct CopyMemoryToMicromapInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CopyMemoryToMicromapInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_MICROMAP_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (src) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr MicromapEXT)) (dst)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyMicromapModeEXT)) (mode)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COPY_MEMORY_TO_MICROMAP_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 24 :: Ptr MicromapEXT)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr CopyMicromapModeEXT)) (zero)
    lift $ f

instance Zero CopyMemoryToMicromapInfoEXT where
  zero = CopyMemoryToMicromapInfoEXT
           zero
           zero
           zero


-- | VkMicromapBuildSizesInfoEXT - Structure specifying build sizes for a
-- micromap
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMicromapBuildSizesEXT'
data MicromapBuildSizesInfoEXT = MicromapBuildSizesInfoEXT
  { -- | @micromapSize@ is the size in bytes required in a
    -- 'Vulkan.Extensions.Handles.MicromapEXT' for a build or update operation.
    micromapSize :: DeviceSize
  , -- | @buildScratchSize@ is the size in bytes required in a scratch buffer for
    -- a build operation.
    buildScratchSize :: DeviceSize
  , -- | @discardable@ indicates whether or not the micromap object may be
    -- destroyed after an acceleration structure build or update. A false value
    -- means that acceleration structures built with this micromap /may/
    -- contain references to the data contained therein, and the application
    -- /must/ not destroy the micromap until ray traversal has concluded. A
    -- true value means that the information in the micromap will be copied by
    -- value into the acceleration structure, and the micromap /may/ be
    -- destroyed after the acceleration structure build concludes.
    discardable :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MicromapBuildSizesInfoEXT)
#endif
deriving instance Show MicromapBuildSizesInfoEXT

instance ToCStruct MicromapBuildSizesInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MicromapBuildSizesInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MICROMAP_BUILD_SIZES_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (micromapSize)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (buildScratchSize)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (discardable))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MICROMAP_BUILD_SIZES_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct MicromapBuildSizesInfoEXT where
  peekCStruct p = do
    micromapSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    buildScratchSize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    discardable <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ MicromapBuildSizesInfoEXT
             micromapSize buildScratchSize (bool32ToBool discardable)

instance Storable MicromapBuildSizesInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MicromapBuildSizesInfoEXT where
  zero = MicromapBuildSizesInfoEXT
           zero
           zero
           zero


-- | VkMicromapUsageEXT - Structure specifying the usage information used to
-- build a micromap
--
-- == Valid Usage
--
-- -   #VUID-VkMicromapUsageEXT-format-07519# If the 'MicromapTypeEXT' of
--     the micromap is 'MICROMAP_TYPE_OPACITY_MICROMAP_EXT' then @format@
--     /must/ be 'OPACITY_MICROMAP_FORMAT_2_STATE_EXT' or
--     'OPACITY_MICROMAP_FORMAT_4_STATE_EXT'
--
-- -   #VUID-VkMicromapUsageEXT-format-07520# If the 'MicromapTypeEXT' of
--     the micromap is 'MICROMAP_TYPE_OPACITY_MICROMAP_EXT' and @format@ is
--     'OPACITY_MICROMAP_FORMAT_2_STATE_EXT' then @subdivisionLevel@ /must/
--     be less than or equal to
--     'PhysicalDeviceOpacityMicromapPropertiesEXT'::@maxOpacity2StateSubdivisionLevel@
--
-- -   #VUID-VkMicromapUsageEXT-format-07521# If the 'MicromapTypeEXT' of
--     the micromap is 'MICROMAP_TYPE_OPACITY_MICROMAP_EXT' and @format@ is
--     'OPACITY_MICROMAP_FORMAT_4_STATE_EXT' then @subdivisionLevel@ /must/
--     be less than or equal to
--     'PhysicalDeviceOpacityMicromapPropertiesEXT'::@maxOpacity4StateSubdivisionLevel@
--
-- -   #VUID-VkMicromapUsageEXT-format-08706# If the 'MicromapTypeEXT' of
--     the micromap is 'MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV' then
--     @format@ /must/ be
--     'Vulkan.Extensions.VK_NV_displacement_micromap.DISPLACEMENT_MICROMAP_FORMAT_64_TRIANGLES_64_BYTES_NV',
--     'Vulkan.Extensions.VK_NV_displacement_micromap.DISPLACEMENT_MICROMAP_FORMAT_256_TRIANGLES_128_BYTES_NV'
--     or
--     'Vulkan.Extensions.VK_NV_displacement_micromap.DISPLACEMENT_MICROMAP_FORMAT_1024_TRIANGLES_128_BYTES_NV'
--
-- -   #VUID-VkMicromapUsageEXT-subdivisionLevel-08707# If the
--     'MicromapTypeEXT' of the micromap is
--     'MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV' then @subdivisionLevel@
--     /must/ be less than or equal to
--     'Vulkan.Extensions.VK_NV_displacement_micromap.PhysicalDeviceDisplacementMicromapPropertiesNV'::@maxDisplacementMicromapSubdivisionLevel@
--
-- The @format@ is interpreted based on the @type@ of the micromap using
-- it.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Extensions.VK_NV_displacement_micromap.AccelerationStructureTrianglesDisplacementMicromapNV',
-- 'AccelerationStructureTrianglesOpacityMicromapEXT',
-- 'MicromapBuildInfoEXT'
data MicromapUsageEXT = MicromapUsageEXT
  { -- | @count@ is the number of triangles in the usage format defined by the
    -- @subdivisionLevel@ and @format@ below in the micromap
    count :: Word32
  , -- | @subdivisionLevel@ is the subdivision level of this usage format
    subdivisionLevel :: Word32
  , -- | @format@ is the format of this usage format
    format :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MicromapUsageEXT)
#endif
deriving instance Show MicromapUsageEXT

instance ToCStruct MicromapUsageEXT where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MicromapUsageEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (count)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (subdivisionLevel)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (format)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct MicromapUsageEXT where
  peekCStruct p = do
    count <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    subdivisionLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    format <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ MicromapUsageEXT
             count subdivisionLevel format

instance Storable MicromapUsageEXT where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MicromapUsageEXT where
  zero = MicromapUsageEXT
           zero
           zero
           zero


-- | VkMicromapTriangleEXT - Structure specifying the micromap format and
-- data for a triangle
--
-- == Valid Usage
--
-- -   #VUID-VkMicromapTriangleEXT-format-07522# If the 'MicromapTypeEXT'
--     of the micromap is 'MICROMAP_TYPE_OPACITY_MICROMAP_EXT' then
--     @format@ /must/ be 'OPACITY_MICROMAP_FORMAT_2_STATE_EXT' or
--     'OPACITY_MICROMAP_FORMAT_4_STATE_EXT'
--
-- -   #VUID-VkMicromapTriangleEXT-format-07523# If the 'MicromapTypeEXT'
--     of the micromap is 'MICROMAP_TYPE_OPACITY_MICROMAP_EXT' and @format@
--     is 'OPACITY_MICROMAP_FORMAT_2_STATE_EXT' then @subdivisionLevel@
--     /must/ be less than or equal to
--     'PhysicalDeviceOpacityMicromapPropertiesEXT'::@maxOpacity2StateSubdivisionLevel@
--
-- -   #VUID-VkMicromapTriangleEXT-format-07524# If the 'MicromapTypeEXT'
--     of the micromap is 'MICROMAP_TYPE_OPACITY_MICROMAP_EXT' and @format@
--     is 'OPACITY_MICROMAP_FORMAT_4_STATE_EXT' then @subdivisionLevel@
--     /must/ be less than or equal to
--     'PhysicalDeviceOpacityMicromapPropertiesEXT'::@maxOpacity4StateSubdivisionLevel@
--
-- -   #VUID-VkMicromapTriangleEXT-format-08708# If the 'MicromapTypeEXT'
--     of the micromap is 'MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV' then
--     @format@ /must/ be
--     'Vulkan.Extensions.VK_NV_displacement_micromap.DISPLACEMENT_MICROMAP_FORMAT_64_TRIANGLES_64_BYTES_NV',
--     'Vulkan.Extensions.VK_NV_displacement_micromap.DISPLACEMENT_MICROMAP_FORMAT_256_TRIANGLES_128_BYTES_NV'
--     or
--     'Vulkan.Extensions.VK_NV_displacement_micromap.DISPLACEMENT_MICROMAP_FORMAT_1024_TRIANGLES_128_BYTES_NV'
--
-- -   #VUID-VkMicromapTriangleEXT-subdivisionLevel-08709# If the
--     'MicromapTypeEXT' of the micromap is
--     'MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV' then @subdivisionLevel@
--     /must/ be less than or equal to
--     'Vulkan.Extensions.VK_NV_displacement_micromap.PhysicalDeviceDisplacementMicromapPropertiesNV'::@maxDisplacementMicromapSubdivisionLevel@
--
-- The @format@ is interpreted based on the @type@ of the micromap using
-- it.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>
data MicromapTriangleEXT = MicromapTriangleEXT
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
deriving instance Generic (MicromapTriangleEXT)
#endif
deriving instance Show MicromapTriangleEXT

instance ToCStruct MicromapTriangleEXT where
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MicromapTriangleEXT{..} f = do
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

instance FromCStruct MicromapTriangleEXT where
  peekCStruct p = do
    dataOffset <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    subdivisionLevel <- peek @Word16 ((p `plusPtr` 4 :: Ptr Word16))
    format <- peek @Word16 ((p `plusPtr` 6 :: Ptr Word16))
    pure $ MicromapTriangleEXT
             dataOffset subdivisionLevel format

instance Storable MicromapTriangleEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MicromapTriangleEXT where
  zero = MicromapTriangleEXT
           zero
           zero
           zero


-- | VkPhysicalDeviceOpacityMicromapFeaturesEXT - Structure describing the
-- ray tracing opacity micromap features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceOpacityMicromapFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceOpacityMicromapFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceOpacityMicromapFeaturesEXT = PhysicalDeviceOpacityMicromapFeaturesEXT
  { -- | #features-micromap# @micromap@ indicates whether the implementation
    -- supports the micromap array feature.
    micromap :: Bool
  , -- | #features-micromapCaptureReplay# @micromapCaptureReplay@ indicates
    -- whether the implementation supports capture and replay of addresses for
    -- micromap arrays.
    micromapCaptureReplay :: Bool
  , -- | #features-micromapHostCommands# @micromapHostCommands@ indicates whether
    -- the implementation supports host side micromap array commands.
    micromapHostCommands :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceOpacityMicromapFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceOpacityMicromapFeaturesEXT

instance ToCStruct PhysicalDeviceOpacityMicromapFeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceOpacityMicromapFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (micromap))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (micromapCaptureReplay))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (micromapHostCommands))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceOpacityMicromapFeaturesEXT where
  peekCStruct p = do
    micromap <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    micromapCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    micromapHostCommands <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceOpacityMicromapFeaturesEXT
             (bool32ToBool micromap)
             (bool32ToBool micromapCaptureReplay)
             (bool32ToBool micromapHostCommands)

instance Storable PhysicalDeviceOpacityMicromapFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceOpacityMicromapFeaturesEXT where
  zero = PhysicalDeviceOpacityMicromapFeaturesEXT
           zero
           zero
           zero


-- | VkPhysicalDeviceOpacityMicromapPropertiesEXT - Structure describing the
-- opacity micromap properties of a physical device
--
-- = Description
--
-- If the 'PhysicalDeviceOpacityMicromapPropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceOpacityMicromapPropertiesEXT = PhysicalDeviceOpacityMicromapPropertiesEXT
  { -- | @maxOpacity2StateSubdivisionLevel@ is the maximum allowed
    -- @subdivisionLevel@ when @format@ is
    -- 'OPACITY_MICROMAP_FORMAT_2_STATE_EXT'
    maxOpacity2StateSubdivisionLevel :: Word32
  , -- | @maxOpacity4StateSubdivisionLevel@ is the maximum allowed
    -- @subdivisionLevel@ when @format@ is
    -- 'OPACITY_MICROMAP_FORMAT_4_STATE_EXT'
    maxOpacity4StateSubdivisionLevel :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceOpacityMicromapPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceOpacityMicromapPropertiesEXT

instance ToCStruct PhysicalDeviceOpacityMicromapPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceOpacityMicromapPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxOpacity2StateSubdivisionLevel)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxOpacity4StateSubdivisionLevel)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceOpacityMicromapPropertiesEXT where
  peekCStruct p = do
    maxOpacity2StateSubdivisionLevel <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxOpacity4StateSubdivisionLevel <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PhysicalDeviceOpacityMicromapPropertiesEXT
             maxOpacity2StateSubdivisionLevel maxOpacity4StateSubdivisionLevel

instance Storable PhysicalDeviceOpacityMicromapPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceOpacityMicromapPropertiesEXT where
  zero = PhysicalDeviceOpacityMicromapPropertiesEXT
           zero
           zero


-- | VkAccelerationStructureTrianglesOpacityMicromapEXT - Structure
-- specifying an opacity micromap in a bottom-level acceleration structure
--
-- = Description
--
-- If 'AccelerationStructureTrianglesOpacityMicromapEXT' is included in the
-- @pNext@ chain of a
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'
-- structure, that geometry will reference that micromap.
--
-- For each triangle in the geometry, the acceleration structure build
-- fetches an index from @indexBuffer@ using @indexType@ and @indexStride@.
-- If that value is the unsigned cast of one of the values from
-- 'OpacityMicromapSpecialIndexEXT' then that triangle behaves as described
-- for that special value in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#ray-opacity-micromap Ray Opacity Micromap>.
-- Otherwise that triangle uses the opacity micromap information from
-- @micromap@ at that index plus @baseTriangle@.
--
-- Only one of @pUsageCounts@ or @ppUsageCounts@ /can/ be a valid pointer,
-- the other /must/ be @NULL@. The elements of the non-@NULL@ array
-- describe the total count used to build this geometry. For a given
-- @format@ and @subdivisionLevel@ the number of triangles in this geometry
-- matching those values after indirection and special index handling
-- /must/ be equal to the sum of matching @count@ provided.
--
-- If @micromap@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', then every
-- value read from @indexBuffer@ /must/ be one of the values in
-- 'OpacityMicromapSpecialIndexEXT'.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapEXT-pUsageCounts-07335#
--     Only one of @pUsageCounts@ or @ppUsageCounts@ /can/ be a valid
--     pointer, the other /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_EXT'
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapEXT-indexType-parameter#
--     @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapEXT-pUsageCounts-parameter#
--     If @usageCountsCount@ is not @0@, and @pUsageCounts@ is not @NULL@,
--     @pUsageCounts@ /must/ be a valid pointer to an array of
--     @usageCountsCount@ 'MicromapUsageEXT' structures
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapEXT-ppUsageCounts-parameter#
--     If @usageCountsCount@ is not @0@, and @ppUsageCounts@ is not @NULL@,
--     @ppUsageCounts@ /must/ be a valid pointer to an array of
--     @usageCountsCount@ valid pointers to 'MicromapUsageEXT' structures
--
-- -   #VUID-VkAccelerationStructureTrianglesOpacityMicromapEXT-micromap-parameter#
--     If @micromap@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @micromap@ /must/ be a valid 'Vulkan.Extensions.Handles.MicromapEXT'
--     handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Vulkan.Extensions.Handles.MicromapEXT', 'MicromapUsageEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AccelerationStructureTrianglesOpacityMicromapEXT = AccelerationStructureTrianglesOpacityMicromapEXT
  { -- | @indexType@ is the type of triangle indices used when indexing this
    -- micromap
    indexType :: IndexType
  , -- | @indexBuffer@ is the address containing the triangle indices
    indexBuffer :: DeviceOrHostAddressConstKHR
  , -- | @indexStride@ is the byte stride between triangle indices
    indexStride :: DeviceSize
  , -- | @baseTriangle@ is the base value added to the non-negative triangle
    -- indices
    baseTriangle :: Word32
  , -- | @pUsageCounts@ is a pointer to an array of 'MicromapUsageEXT'
    -- structures.
    usageCounts :: Vector MicromapUsageEXT
  , -- | @micromap@ is the handle to the micromap object to include in this
    -- geometry
    micromap :: MicromapEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureTrianglesOpacityMicromapEXT)
#endif
deriving instance Show AccelerationStructureTrianglesOpacityMicromapEXT

instance ToCStruct AccelerationStructureTrianglesOpacityMicromapEXT where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureTrianglesOpacityMicromapEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndexType)) (indexType)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (indexBuffer) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (indexStride)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (baseTriangle)
    lift $ poke ((p `plusPtr` 44 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (usageCounts)) :: Word32))
    pPUsageCounts' <- ContT $ allocaBytes @MicromapUsageEXT ((Data.Vector.length (usageCounts)) * 12)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPUsageCounts' `plusPtr` (12 * (i)) :: Ptr MicromapUsageEXT) (e)) (usageCounts)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr MicromapUsageEXT))) (pPUsageCounts')
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr MicromapUsageEXT)))) (nullPtr)
    lift $ poke ((p `plusPtr` 64 :: Ptr MicromapEXT)) (micromap)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndexType)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr DeviceOrHostAddressConstKHR)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr MicromapUsageEXT)))) (nullPtr)
    lift $ f

instance Zero AccelerationStructureTrianglesOpacityMicromapEXT where
  zero = AccelerationStructureTrianglesOpacityMicromapEXT
           zero
           zero
           zero
           zero
           mempty
           zero


-- | VkMicromapTypeEXT - Type of micromap
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'MicromapBuildInfoEXT', 'MicromapCreateInfoEXT'
newtype MicromapTypeEXT = MicromapTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'MICROMAP_TYPE_OPACITY_MICROMAP_EXT' is a micromap containing data to
-- control the opacity of a triangle.
pattern MICROMAP_TYPE_OPACITY_MICROMAP_EXT = MicromapTypeEXT 0

-- | 'MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV' is a micromap containing data
-- to control the displacement of subtriangles within a triangle.
pattern MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV = MicromapTypeEXT 1000397000

{-# COMPLETE
  MICROMAP_TYPE_OPACITY_MICROMAP_EXT
  , MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV ::
    MicromapTypeEXT
  #-}

conNameMicromapTypeEXT :: String
conNameMicromapTypeEXT = "MicromapTypeEXT"

enumPrefixMicromapTypeEXT :: String
enumPrefixMicromapTypeEXT = "MICROMAP_TYPE_"

showTableMicromapTypeEXT :: [(MicromapTypeEXT, String)]
showTableMicromapTypeEXT =
  [
    ( MICROMAP_TYPE_OPACITY_MICROMAP_EXT
    , "OPACITY_MICROMAP_EXT"
    )
  ,
    ( MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV
    , "DISPLACEMENT_MICROMAP_NV"
    )
  ]

instance Show MicromapTypeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixMicromapTypeEXT
      showTableMicromapTypeEXT
      conNameMicromapTypeEXT
      (\(MicromapTypeEXT x) -> x)
      (showsPrec 11)

instance Read MicromapTypeEXT where
  readPrec =
    enumReadPrec
      enumPrefixMicromapTypeEXT
      showTableMicromapTypeEXT
      conNameMicromapTypeEXT
      MicromapTypeEXT

type BuildMicromapFlagsEXT = BuildMicromapFlagBitsEXT

-- | VkBuildMicromapFlagBitsEXT - Bitmask specifying additional parameters
-- for micromap builds
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'BuildMicromapFlagsEXT'
newtype BuildMicromapFlagBitsEXT = BuildMicromapFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'BUILD_MICROMAP_PREFER_FAST_TRACE_BIT_EXT' indicates that the given
-- micromap build /should/ prioritize trace performance over build time.
pattern BUILD_MICROMAP_PREFER_FAST_TRACE_BIT_EXT = BuildMicromapFlagBitsEXT 0x00000001

-- | 'BUILD_MICROMAP_PREFER_FAST_BUILD_BIT_EXT' indicates that the given
-- micromap build /should/ prioritize build time over trace performance.
pattern BUILD_MICROMAP_PREFER_FAST_BUILD_BIT_EXT = BuildMicromapFlagBitsEXT 0x00000002

-- No documentation found for Nested "VkBuildMicromapFlagBitsEXT" "VK_BUILD_MICROMAP_ALLOW_COMPACTION_BIT_EXT"
pattern BUILD_MICROMAP_ALLOW_COMPACTION_BIT_EXT = BuildMicromapFlagBitsEXT 0x00000004

conNameBuildMicromapFlagBitsEXT :: String
conNameBuildMicromapFlagBitsEXT = "BuildMicromapFlagBitsEXT"

enumPrefixBuildMicromapFlagBitsEXT :: String
enumPrefixBuildMicromapFlagBitsEXT = "BUILD_MICROMAP_"

showTableBuildMicromapFlagBitsEXT :: [(BuildMicromapFlagBitsEXT, String)]
showTableBuildMicromapFlagBitsEXT =
  [
    ( BUILD_MICROMAP_PREFER_FAST_TRACE_BIT_EXT
    , "PREFER_FAST_TRACE_BIT_EXT"
    )
  ,
    ( BUILD_MICROMAP_PREFER_FAST_BUILD_BIT_EXT
    , "PREFER_FAST_BUILD_BIT_EXT"
    )
  ,
    ( BUILD_MICROMAP_ALLOW_COMPACTION_BIT_EXT
    , "ALLOW_COMPACTION_BIT_EXT"
    )
  ]

instance Show BuildMicromapFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixBuildMicromapFlagBitsEXT
      showTableBuildMicromapFlagBitsEXT
      conNameBuildMicromapFlagBitsEXT
      (\(BuildMicromapFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read BuildMicromapFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixBuildMicromapFlagBitsEXT
      showTableBuildMicromapFlagBitsEXT
      conNameBuildMicromapFlagBitsEXT
      BuildMicromapFlagBitsEXT

type MicromapCreateFlagsEXT = MicromapCreateFlagBitsEXT

-- | VkMicromapCreateFlagBitsEXT - Bitmask specifying additional creation
-- parameters for micromap
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'MicromapCreateFlagsEXT'
newtype MicromapCreateFlagBitsEXT = MicromapCreateFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'MICROMAP_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT' specifies that
-- the micromap’s address /can/ be saved and reused on a subsequent run.
pattern MICROMAP_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT = MicromapCreateFlagBitsEXT 0x00000001

conNameMicromapCreateFlagBitsEXT :: String
conNameMicromapCreateFlagBitsEXT = "MicromapCreateFlagBitsEXT"

enumPrefixMicromapCreateFlagBitsEXT :: String
enumPrefixMicromapCreateFlagBitsEXT = "MICROMAP_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT"

showTableMicromapCreateFlagBitsEXT :: [(MicromapCreateFlagBitsEXT, String)]
showTableMicromapCreateFlagBitsEXT =
  [
    ( MICROMAP_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
    , ""
    )
  ]

instance Show MicromapCreateFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixMicromapCreateFlagBitsEXT
      showTableMicromapCreateFlagBitsEXT
      conNameMicromapCreateFlagBitsEXT
      (\(MicromapCreateFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read MicromapCreateFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixMicromapCreateFlagBitsEXT
      showTableMicromapCreateFlagBitsEXT
      conNameMicromapCreateFlagBitsEXT
      MicromapCreateFlagBitsEXT

-- | VkCopyMicromapModeEXT - Micromap copy mode
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'CopyMemoryToMicromapInfoEXT', 'CopyMicromapInfoEXT',
-- 'CopyMicromapToMemoryInfoEXT'
newtype CopyMicromapModeEXT = CopyMicromapModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COPY_MICROMAP_MODE_CLONE_EXT' creates a direct copy of the micromap
-- specified in @src@ into the one specified by @dst@. The @dst@ micromap
-- /must/ have been created with the same parameters as @src@.
pattern COPY_MICROMAP_MODE_CLONE_EXT = CopyMicromapModeEXT 0

-- | 'COPY_MICROMAP_MODE_SERIALIZE_EXT' serializes the micromap to a
-- semi-opaque format which can be reloaded on a compatible implementation.
pattern COPY_MICROMAP_MODE_SERIALIZE_EXT = CopyMicromapModeEXT 1

-- | 'COPY_MICROMAP_MODE_DESERIALIZE_EXT' deserializes the semi-opaque
-- serialization format in the buffer to the micromap.
pattern COPY_MICROMAP_MODE_DESERIALIZE_EXT = CopyMicromapModeEXT 2

-- | 'COPY_MICROMAP_MODE_COMPACT_EXT' creates a more compact version of a
-- micromap @src@ into @dst@. The micromap @dst@ /must/ have been created
-- with a size at least as large as that returned by
-- 'cmdWriteMicromapsPropertiesEXT' after the build of the micromap
-- specified by @src@.
pattern COPY_MICROMAP_MODE_COMPACT_EXT = CopyMicromapModeEXT 3

{-# COMPLETE
  COPY_MICROMAP_MODE_CLONE_EXT
  , COPY_MICROMAP_MODE_SERIALIZE_EXT
  , COPY_MICROMAP_MODE_DESERIALIZE_EXT
  , COPY_MICROMAP_MODE_COMPACT_EXT ::
    CopyMicromapModeEXT
  #-}

conNameCopyMicromapModeEXT :: String
conNameCopyMicromapModeEXT = "CopyMicromapModeEXT"

enumPrefixCopyMicromapModeEXT :: String
enumPrefixCopyMicromapModeEXT = "COPY_MICROMAP_MODE_"

showTableCopyMicromapModeEXT :: [(CopyMicromapModeEXT, String)]
showTableCopyMicromapModeEXT =
  [ (COPY_MICROMAP_MODE_CLONE_EXT, "CLONE_EXT")
  ,
    ( COPY_MICROMAP_MODE_SERIALIZE_EXT
    , "SERIALIZE_EXT"
    )
  ,
    ( COPY_MICROMAP_MODE_DESERIALIZE_EXT
    , "DESERIALIZE_EXT"
    )
  ,
    ( COPY_MICROMAP_MODE_COMPACT_EXT
    , "COMPACT_EXT"
    )
  ]

instance Show CopyMicromapModeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixCopyMicromapModeEXT
      showTableCopyMicromapModeEXT
      conNameCopyMicromapModeEXT
      (\(CopyMicromapModeEXT x) -> x)
      (showsPrec 11)

instance Read CopyMicromapModeEXT where
  readPrec =
    enumReadPrec
      enumPrefixCopyMicromapModeEXT
      showTableCopyMicromapModeEXT
      conNameCopyMicromapModeEXT
      CopyMicromapModeEXT

-- | VkBuildMicromapModeEXT - Enum specifying the type of build operation to
-- perform
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>,
-- 'MicromapBuildInfoEXT'
newtype BuildMicromapModeEXT = BuildMicromapModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'BUILD_MICROMAP_MODE_BUILD_EXT' specifies that the destination micromap
-- will be built using the specified data.
pattern BUILD_MICROMAP_MODE_BUILD_EXT = BuildMicromapModeEXT 0

{-# COMPLETE BUILD_MICROMAP_MODE_BUILD_EXT :: BuildMicromapModeEXT #-}

conNameBuildMicromapModeEXT :: String
conNameBuildMicromapModeEXT = "BuildMicromapModeEXT"

enumPrefixBuildMicromapModeEXT :: String
enumPrefixBuildMicromapModeEXT = "BUILD_MICROMAP_MODE_BUILD_EXT"

showTableBuildMicromapModeEXT :: [(BuildMicromapModeEXT, String)]
showTableBuildMicromapModeEXT = [(BUILD_MICROMAP_MODE_BUILD_EXT, "")]

instance Show BuildMicromapModeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixBuildMicromapModeEXT
      showTableBuildMicromapModeEXT
      conNameBuildMicromapModeEXT
      (\(BuildMicromapModeEXT x) -> x)
      (showsPrec 11)

instance Read BuildMicromapModeEXT where
  readPrec =
    enumReadPrec
      enumPrefixBuildMicromapModeEXT
      showTableBuildMicromapModeEXT
      conNameBuildMicromapModeEXT
      BuildMicromapModeEXT

-- | VkOpacityMicromapFormatEXT - Format enum for opacity micromaps
--
-- = Description
--
-- Note
--
-- For compactness, these values are stored as 16-bit in some structures.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>
newtype OpacityMicromapFormatEXT = OpacityMicromapFormatEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'OPACITY_MICROMAP_FORMAT_2_STATE_EXT' indicates that the given micromap
-- format has one bit per subtriangle encoding either fully opaque or fully
-- transparent.
pattern OPACITY_MICROMAP_FORMAT_2_STATE_EXT = OpacityMicromapFormatEXT 1

-- | 'OPACITY_MICROMAP_FORMAT_4_STATE_EXT' indicates that the given micromap
-- format has two bits per subtriangle encoding four modes which can be
-- interpreted as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#ray-opacity-micromap ray traversal>.
pattern OPACITY_MICROMAP_FORMAT_4_STATE_EXT = OpacityMicromapFormatEXT 2

{-# COMPLETE
  OPACITY_MICROMAP_FORMAT_2_STATE_EXT
  , OPACITY_MICROMAP_FORMAT_4_STATE_EXT ::
    OpacityMicromapFormatEXT
  #-}

conNameOpacityMicromapFormatEXT :: String
conNameOpacityMicromapFormatEXT = "OpacityMicromapFormatEXT"

enumPrefixOpacityMicromapFormatEXT :: String
enumPrefixOpacityMicromapFormatEXT = "OPACITY_MICROMAP_FORMAT_"

showTableOpacityMicromapFormatEXT :: [(OpacityMicromapFormatEXT, String)]
showTableOpacityMicromapFormatEXT =
  [
    ( OPACITY_MICROMAP_FORMAT_2_STATE_EXT
    , "2_STATE_EXT"
    )
  ,
    ( OPACITY_MICROMAP_FORMAT_4_STATE_EXT
    , "4_STATE_EXT"
    )
  ]

instance Show OpacityMicromapFormatEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixOpacityMicromapFormatEXT
      showTableOpacityMicromapFormatEXT
      conNameOpacityMicromapFormatEXT
      (\(OpacityMicromapFormatEXT x) -> x)
      (showsPrec 11)

instance Read OpacityMicromapFormatEXT where
  readPrec =
    enumReadPrec
      enumPrefixOpacityMicromapFormatEXT
      showTableOpacityMicromapFormatEXT
      conNameOpacityMicromapFormatEXT
      OpacityMicromapFormatEXT

-- | VkOpacityMicromapSpecialIndexEXT - Enum for special indices in the
-- opacity micromap
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>
newtype OpacityMicromapSpecialIndexEXT = OpacityMicromapSpecialIndexEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_EXT' specifies that
-- the entire triangle is fully transparent.
pattern OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_EXT = OpacityMicromapSpecialIndexEXT (-1)

-- | 'OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_EXT' specifies that the
-- entire triangle is fully opaque.
pattern OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_EXT = OpacityMicromapSpecialIndexEXT (-2)

-- | 'OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_EXT' specifies
-- that the entire triangle is unknown-transparent.
pattern OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_EXT = OpacityMicromapSpecialIndexEXT (-3)

-- | 'OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_EXT' specifies that
-- the entire triangle is unknown-opaque.
pattern OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_EXT = OpacityMicromapSpecialIndexEXT (-4)

{-# COMPLETE
  OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_EXT
  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_EXT
  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_EXT
  , OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_EXT ::
    OpacityMicromapSpecialIndexEXT
  #-}

conNameOpacityMicromapSpecialIndexEXT :: String
conNameOpacityMicromapSpecialIndexEXT = "OpacityMicromapSpecialIndexEXT"

enumPrefixOpacityMicromapSpecialIndexEXT :: String
enumPrefixOpacityMicromapSpecialIndexEXT = "OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_"

showTableOpacityMicromapSpecialIndexEXT :: [(OpacityMicromapSpecialIndexEXT, String)]
showTableOpacityMicromapSpecialIndexEXT =
  [
    ( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_TRANSPARENT_EXT
    , "TRANSPARENT_EXT"
    )
  ,
    ( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_OPAQUE_EXT
    , "OPAQUE_EXT"
    )
  ,
    ( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_TRANSPARENT_EXT
    , "UNKNOWN_TRANSPARENT_EXT"
    )
  ,
    ( OPACITY_MICROMAP_SPECIAL_INDEX_FULLY_UNKNOWN_OPAQUE_EXT
    , "UNKNOWN_OPAQUE_EXT"
    )
  ]

instance Show OpacityMicromapSpecialIndexEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixOpacityMicromapSpecialIndexEXT
      showTableOpacityMicromapSpecialIndexEXT
      conNameOpacityMicromapSpecialIndexEXT
      (\(OpacityMicromapSpecialIndexEXT x) -> x)
      (showsPrec 11)

instance Read OpacityMicromapSpecialIndexEXT where
  readPrec =
    enumReadPrec
      enumPrefixOpacityMicromapSpecialIndexEXT
      showTableOpacityMicromapSpecialIndexEXT
      conNameOpacityMicromapSpecialIndexEXT
      OpacityMicromapSpecialIndexEXT

type EXT_OPACITY_MICROMAP_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_OPACITY_MICROMAP_SPEC_VERSION"
pattern EXT_OPACITY_MICROMAP_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_OPACITY_MICROMAP_SPEC_VERSION = 2


type EXT_OPACITY_MICROMAP_EXTENSION_NAME = "VK_EXT_opacity_micromap"

-- No documentation found for TopLevel "VK_EXT_OPACITY_MICROMAP_EXTENSION_NAME"
pattern EXT_OPACITY_MICROMAP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_OPACITY_MICROMAP_EXTENSION_NAME = "VK_EXT_opacity_micromap"

