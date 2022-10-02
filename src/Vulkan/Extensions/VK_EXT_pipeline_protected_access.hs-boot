{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_protected_access - device extension
--
-- == VK_EXT_pipeline_protected_access
--
-- [__Name String__]
--     @VK_EXT_pipeline_protected_access@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     467
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_pipeline_protected_access] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_pipeline_protected_access extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_pipeline_protected_access.adoc VK_EXT_pipeline_protected_access>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-28
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Jörg Wagner, Arm
--
--     -   Ralph Potter, Samsung
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension allows protected memory access to be specified per
-- pipeline as opposed to per device. Through the usage of this extension,
-- any performance penalty paid due to access to protected memory will be
-- limited to the specific pipelines that make such accesses.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineProtectedAccessFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME'
--
-- -   'EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-07-28 (Shahbaz Youssefi)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDevicePipelineProtectedAccessFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pipeline_protected_access Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_protected_access  (PhysicalDevicePipelineProtectedAccessFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePipelineProtectedAccessFeaturesEXT

instance ToCStruct PhysicalDevicePipelineProtectedAccessFeaturesEXT
instance Show PhysicalDevicePipelineProtectedAccessFeaturesEXT

instance FromCStruct PhysicalDevicePipelineProtectedAccessFeaturesEXT

