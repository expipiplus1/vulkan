{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_protected_access - device extension
--
-- = VK_EXT_pipeline_protected_access
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
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
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
--     -   'PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT'
--
--     -   'PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- EXT suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2022-07-28 (Shahbaz Youssefi)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_pipeline_protected_access Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_protected_access  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT
                                                           , pattern PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT
                                                           , pattern PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT
                                                           , PhysicalDevicePipelineProtectedAccessFeaturesEXT
                                                           , EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION
                                                           , pattern EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION
                                                           , EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME
                                                           , pattern EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME
                                                           ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_EXT_pipeline_protected_accessAdditionalFunctionality' (PhysicalDevicePipelineProtectedAccessFeatures)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT"
pattern PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT = PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT"
pattern PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT = PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT


-- No documentation found for TopLevel "VkPhysicalDevicePipelineProtectedAccessFeaturesEXT"
type PhysicalDevicePipelineProtectedAccessFeaturesEXT = PhysicalDevicePipelineProtectedAccessFeatures


type EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION"
pattern EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_PROTECTED_ACCESS_SPEC_VERSION = 1


type EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME = "VK_EXT_pipeline_protected_access"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME"
pattern EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_PROTECTED_ACCESS_EXTENSION_NAME = "VK_EXT_pipeline_protected_access"

