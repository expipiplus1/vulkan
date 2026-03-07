{-# language CPP #-}
-- | = Name
--
-- VK_SEC_pipeline_cache_incremental_mode - device extension
--
-- = VK_SEC_pipeline_cache_incremental_mode
--
-- [__Name String__]
--     @VK_SEC_pipeline_cache_incremental_mode@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     638
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Chris Hambacher
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_SEC_pipeline_cache_incremental_mode] @chambacher%0A*Here describe the issue or question you have about the VK_SEC_pipeline_cache_incremental_mode extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-24
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Chris Hambacher, Samsung
--
--     -   Mohan Maiya, Samsung
--
--     -   Brandon Schade, Samsung
--
-- == Description
--
-- This extension allows layered implementations such as ANGLE to modify
-- the default behavior of VkPipelineCache to return only the incremental
-- data from the previous call to vkGetPipelineCacheData. Application
-- developers should avoid using this extension.
--
-- There is currently no specification language written for this extension.
-- The links to APIs defined by the extension are to stubs that only
-- include generated content such as API declarations and implicit valid
-- usage statements.
--
-- This extension is only intended for use in specific embedded
-- environments with known implementation details, and is therefore
-- undocumented.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC'
--
-- == New Enum Constants
--
-- -   'SEC_PIPELINE_CACHE_INCREMENTAL_MODE_EXTENSION_NAME'
--
-- -   'SEC_PIPELINE_CACHE_INCREMENTAL_MODE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CACHE_INCREMENTAL_MODE_FEATURES_SEC'
--
-- == Stub API References
--
-- There is currently no specification language written for this type. This
-- section acts only as placeholder and to avoid dead links in the
-- specification and reference pages.
--
-- > // Provided by VK_SEC_pipeline_cache_incremental_mode
-- > typedef struct VkPhysicalDevicePipelineCacheIncrementalModeFeaturesSEC {
-- >     VkStructureType    sType;
-- >     void*              pNext;
-- >     VkBool32           pipelineCacheIncrementalMode;
-- > } VkPhysicalDevicePipelineCacheIncrementalModeFeaturesSEC;
--
-- === Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDevicePipelineCacheIncrementalModeFeaturesSEC-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CACHE_INCREMENTAL_MODE_FEATURES_SEC'
--
-- == Version History
--
-- -   Revision 1, 2025-06-24 (Chris Hambacher)
--
--     -   Initial specification
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_SEC_pipeline_cache_incremental_mode Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_SEC_pipeline_cache_incremental_mode  (PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC

instance ToCStruct PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC
instance Show PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC

instance FromCStruct PhysicalDevicePipelineCacheIncrementalModeFeaturesSEC

