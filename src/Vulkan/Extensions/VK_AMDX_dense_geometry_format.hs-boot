{-# language CPP #-}
-- | = Name
--
-- VK_AMDX_dense_geometry_format - device extension
--
-- = VK_AMDX_dense_geometry_format
--
-- [__Name String__]
--     @VK_AMDX_dense_geometry_format@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     479
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4 Vulkan Version 1.4>
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__Contact__]
--
--     -   Stu Smith
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMDX_dense_geometry_format] @stu-s%0A*Here describe the issue or question you have about the VK_AMDX_dense_geometry_format extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_AMDX_dense_geometry_format.adoc VK_AMDX_dense_geometry_format>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-07-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Josh Barczak, AMD
--
--     -   Carsten Benthin, AMD
--
--     -   David McAllister, AMD
--
-- == Description
--
-- This extension adds the ability to build ray tracing acceleration
-- structures from pre-compressed @Dense Geometry Format@ geometry data.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryKHR':
--
--     -   'AccelerationStructureDenseGeometryFormatTrianglesDataAMDX'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDenseGeometryFormatFeaturesAMDX'
--
-- == New Enums
--
-- -   'CompressedTriangleFormatAMDX'
--
-- == New Enum Constants
--
-- -   'AMDX_DENSE_GEOMETRY_FORMAT_EXTENSION_NAME'
--
-- -   'AMDX_DENSE_GEOMETRY_FORMAT_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_ALIGNMENT_AMDX'
--
-- -   'Vulkan.Core10.APIConstants.COMPRESSED_TRIANGLE_FORMAT_DGF1_BYTE_STRIDE_AMDX'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_COMPRESSED_DATA_DGF1_BIT_AMDX'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryTypeKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_DENSE_GEOMETRY_FORMAT_TRIANGLES_AMDX'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DENSE_GEOMETRY_FORMAT_TRIANGLES_DATA_AMDX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DENSE_GEOMETRY_FORMAT_FEATURES_AMDX'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-07-10 (Stu Smith)
--
--     -   Initial revision.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_AMDX_dense_geometry_format Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMDX_dense_geometry_format  ( AccelerationStructureDenseGeometryFormatTrianglesDataAMDX
                                                        , PhysicalDeviceDenseGeometryFormatFeaturesAMDX
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role AccelerationStructureDenseGeometryFormatTrianglesDataAMDX nominal
data AccelerationStructureDenseGeometryFormatTrianglesDataAMDX (es :: [Type])

instance ( Extendss AccelerationStructureDenseGeometryFormatTrianglesDataAMDX es
         , PokeChain es ) => ToCStruct (AccelerationStructureDenseGeometryFormatTrianglesDataAMDX es)
instance Show (Chain es) => Show (AccelerationStructureDenseGeometryFormatTrianglesDataAMDX es)


data PhysicalDeviceDenseGeometryFormatFeaturesAMDX

instance ToCStruct PhysicalDeviceDenseGeometryFormatFeaturesAMDX
instance Show PhysicalDeviceDenseGeometryFormatFeaturesAMDX

instance FromCStruct PhysicalDeviceDenseGeometryFormatFeaturesAMDX

