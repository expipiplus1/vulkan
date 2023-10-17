{-# language CPP #-}
-- | = Name
--
-- VK_NV_displacement_micromap - device extension
--
-- == VK_NV_displacement_micromap
--
-- [__Name String__]
--     @VK_NV_displacement_micromap@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     398
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__Contact__]
--
--     -   Christoph Kubisch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_displacement_micromap] @pixeljetstream%0A*Here describe the issue or question you have about the VK_NV_displacement_micromap extension* >
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_displacement_micromap] @ewerness-nv%0A*Here describe the issue or question you have about the VK_NV_displacement_micromap extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-17
--
-- [__Interactions and External Dependencies__]
--     TBD
--
-- [__Contributors__]
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
-- == Description
--
-- Ray tracing can very efficiently render from geometry which has very
-- fine detail, but when using only a basic triangle representation, memory
-- consumption can be an issue. This extension adds the ability to add a
-- /displacement map/ to add more detail to triangles in an acceleration
-- structure with an efficient in-memory format. The format is externally
-- visible to allow the application to compress its internal geometry
-- representations into the compressed format ahead of time. This format
-- adds displacements along a defined vector to subtriangle vertices which
-- are subdivided from the main triangles.
--
-- This extension provides:
--
-- -   a new 'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapTypeEXT'
--     format for the displacement micromap,
--
-- -   a structure to extend
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'
--     to attach a displacement micromap to the geometry of the
--     acceleration structure,
--
-- -   enums extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR'
--     to allow for updates.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR':
--
--     -   'AccelerationStructureTrianglesDisplacementMicromapNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDisplacementMicromapFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDisplacementMicromapPropertiesNV'
--
-- == New Enums
--
-- -   'DisplacementMicromapFormatNV'
--
-- == New Enum Constants
--
-- -   'NV_DISPLACEMENT_MICROMAP_EXTENSION_NAME'
--
-- -   'NV_DISPLACEMENT_MICROMAP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_DISPLACEMENT_MICROMAP_UPDATE_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_opacity_micromap.MicromapTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_opacity_micromap.MICROMAP_TYPE_DISPLACEMENT_MICROMAP_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_PROPERTIES_NV'
--
-- == Issues
--
-- (1) What is the status of this extension?
--
-- -   Provisional and expected to change. The broad structure and encoding
--     format are stable, but there will likely be changes to the
--     structures, enumerant values, and shader interface.
--
-- == Version History
--
-- -   Revision 1, 2023-03-17 (Eric Werness)
--
--     -   Initial public revision
--
-- -   Revision 2, 2023-07-07 (Eric Werness)
--
--     -   Add shader support for decode intrinsics
--
-- == See Also
--
-- 'AccelerationStructureTrianglesDisplacementMicromapNV',
-- 'DisplacementMicromapFormatNV',
-- 'PhysicalDeviceDisplacementMicromapFeaturesNV',
-- 'PhysicalDeviceDisplacementMicromapPropertiesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_displacement_micromap Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_displacement_micromap  ( AccelerationStructureTrianglesDisplacementMicromapNV
                                                      , PhysicalDeviceDisplacementMicromapFeaturesNV
                                                      , PhysicalDeviceDisplacementMicromapPropertiesNV
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AccelerationStructureTrianglesDisplacementMicromapNV

instance ToCStruct AccelerationStructureTrianglesDisplacementMicromapNV
instance Show AccelerationStructureTrianglesDisplacementMicromapNV


data PhysicalDeviceDisplacementMicromapFeaturesNV

instance ToCStruct PhysicalDeviceDisplacementMicromapFeaturesNV
instance Show PhysicalDeviceDisplacementMicromapFeaturesNV

instance FromCStruct PhysicalDeviceDisplacementMicromapFeaturesNV


data PhysicalDeviceDisplacementMicromapPropertiesNV

instance ToCStruct PhysicalDeviceDisplacementMicromapPropertiesNV
instance Show PhysicalDeviceDisplacementMicromapPropertiesNV

instance FromCStruct PhysicalDeviceDisplacementMicromapPropertiesNV

