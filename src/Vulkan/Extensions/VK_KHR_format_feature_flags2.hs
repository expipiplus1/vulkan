{-# language CPP #-}
-- | = Name
--
-- VK_KHR_format_feature_flags2 - device extension
--
-- == VK_KHR_format_feature_flags2
--
-- [__Name String__]
--     @VK_KHR_format_feature_flags2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     361
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Lionel Landwerlin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_format_feature_flags2] @llandwerlin%0A<<Here describe the issue or question you have about the VK_KHR_format_feature_flags2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-07-01
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Lionel Landwerlin, Intel
--
--     -   Jason Ekstrand, Intel
--
--     -   Tobias Hector, AMD
--
--     -   Spencer Fricke, Samsung Electronics
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, ARM
--
-- == Description
--
-- This extension adds a new 'FormatFeatureFlagBits2KHR' 64bits format
-- feature flag type to extend the existing
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits' which
-- is limited to 31 flags. At the time of this writing 29 bits of
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits' are
-- already used.
--
-- Because
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2'
-- is already defined to extend the Vulkan 1.0
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
-- entry point, this extension defines a new 'FormatProperties3KHR' to
-- extend the 'Vulkan.Core10.DeviceInitialization.FormatProperties'.
--
-- On top of replicating all the bits from
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits',
-- 'FormatFeatureFlagBits2KHR' adds the following bits :
--
-- -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT_KHR'
--     and
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT_KHR'
--     indicate that an implementation supports respectively reading and
--     writing a given 'Vulkan.Core10.Enums.Format.Format' through storage
--     operations without specifying the format in the shader.
--
-- -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT_KHR'
--     indicates that an implementation supports depth comparison performed
--     by @OpImage*Dref*@ instructions on a given
--     'Vulkan.Core10.Enums.Format.Format'. Previously the result of
--     executing a @OpImage*Dref*@ instruction on an image view, where the
--     @format@ was not one of the depth\/stencil formats with a depth
--     component, was undefined. This bit clarifies on which formats such
--     instructions can be used.
--
-- Prior to version 2 of this extension, implementations exposing the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderStorageImageReadWithoutFormat shaderStorageImageReadWithoutFormat>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderStorageImageWriteWithoutFormat shaderStorageImageWriteWithoutFormat>
-- implementations may not report
-- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT_KHR'
-- and
-- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT_KHR'
-- in 'FormatProperties3KHR'::@bufferFeatures@. Despite this, buffer
-- reads\/writes are supported as intended by the original features.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2':
--
--     -   'FormatProperties3KHR'
--
-- == New Enums
--
-- -   'FormatFeatureFlagBits2KHR'
--
-- == New Bitmasks
--
-- -   'FormatFeatureFlags2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME'
--
-- -   'KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_FORMAT_PROPERTIES_3_KHR'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- KHR suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 2, 2022-07-20 (Lionel Landwerlin)
--
--     -   Clarify that
--         VK_FORMAT_FEATURE_2_STORAGE_(READ|WRITE)_WITHOUT_FORMAT_BIT also
--         apply to buffer views.
--
-- -   Revision 1, 2020-07-21 (Lionel Landwerlin)
--
--     -   Initial draft
--
-- == See Also
--
-- 'FormatFeatureFlagBits2KHR', 'FormatFeatureFlags2KHR',
-- 'FormatProperties3KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_format_feature_flags2  ( pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_3_KHR
                                                       , FormatFeatureFlags2KHR
                                                       , FormatFeatureFlagBits2KHR
                                                       , FormatProperties3KHR
                                                       , KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION
                                                       , pattern KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION
                                                       , KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME
                                                       , pattern KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlagBits2)
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlags2)
import Vulkan.Core13.Promoted_From_VK_KHR_format_feature_flags2 (FormatProperties3)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FORMAT_PROPERTIES_3))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_3_KHR"
pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_3_KHR = STRUCTURE_TYPE_FORMAT_PROPERTIES_3


-- No documentation found for TopLevel "VkFormatFeatureFlags2KHR"
type FormatFeatureFlags2KHR = FormatFeatureFlags2


-- No documentation found for TopLevel "VkFormatFeatureFlagBits2KHR"
type FormatFeatureFlagBits2KHR = FormatFeatureFlagBits2


-- No documentation found for TopLevel "VkFormatProperties3KHR"
type FormatProperties3KHR = FormatProperties3


type KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION"
pattern KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_FORMAT_FEATURE_FLAGS_2_SPEC_VERSION = 2


type KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME = "VK_KHR_format_feature_flags2"

-- No documentation found for TopLevel "VK_KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME"
pattern KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_FORMAT_FEATURE_FLAGS_2_EXTENSION_NAME = "VK_KHR_format_feature_flags2"

