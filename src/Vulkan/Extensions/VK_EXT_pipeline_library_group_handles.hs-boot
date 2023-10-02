{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_library_group_handles - device extension
--
-- == VK_EXT_pipeline_library_group_handles
--
-- [__Name String__]
--     @VK_EXT_pipeline_library_group_handles@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     499
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_library VK_KHR_pipeline_library>
--
-- [__Contact__]
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_pipeline_library_group_handles] @HansKristian-Work%0A*Here describe the issue or question you have about the VK_EXT_pipeline_library_group_handles extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_pipeline_library_group_handles.adoc VK_EXT_pipeline_library_group_handles>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-01-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Stuart Smith, AMD
--
--     -   Ricardo Garcia, Igalia
--
--     -   Lionel Landwerlin, Intel
--
--     -   Eric Werness, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- When using pipeline libraries in ray tracing pipelines, a library might
-- get linked into different pipelines in an incremental way. An
-- application can have a strategy where a ray tracing pipeline is
-- comprised of N pipeline libraries and is later augumented by creating a
-- new pipeline with N + 1 libraries. Without this extension, all group
-- handles must be re-queried as the group handle is tied to the pipeline,
-- not the library. This is problematic for applications which aim to
-- decouple construction of record buffers and the linkage of ray tracing
-- pipelines.
--
-- To aid in this, this extension enables support for querying group
-- handles directly from pipeline libraries. Group handles obtained from a
-- library /must/ remain bitwise identical in any
-- 'Vulkan.Core10.Handles.Pipeline' that links to the library.
--
-- With this feature, the extension also improves compatibility with DXR
-- 1.1 AddToStateObject(), which guarantees that group handles returned
-- remain bitwise identical between parent and child pipelines. In
-- addition, querying group handles from COLLECTION objects is also
-- supported with that API.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PIPELINE_LIBRARY_GROUP_HANDLES_EXTENSION_NAME'
--
-- -   'EXT_PIPELINE_LIBRARY_GROUP_HANDLES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_LIBRARY_GROUP_HANDLES_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2023-01-25 (Hans-Kristian Arntzen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pipeline_library_group_handles Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_library_group_handles  (PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT

instance ToCStruct PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT
instance Show PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT

instance FromCStruct PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT

