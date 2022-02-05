{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_subgroup_extended_types - device extension
--
-- == VK_KHR_shader_subgroup_extended_types
--
-- [__Name String__]
--     @VK_KHR_shader_subgroup_extended_types@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     176
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Neil Henning
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_subgroup_extended_types] @sheredom%0A<<Here describe the issue or question you have about the VK_KHR_shader_subgroup_extended_types extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_shader_subgroup_extended_types.txt GLSL_EXT_shader_subgroup_extended_types>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Neil Henning, AMD
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Leger, Qualcomm
--
--     -   Graeme Leese, Broadcom
--
--     -   David Neto, Google
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension enables the Non Uniform Group Operations in SPIR-V to
-- support 8-bit integer, 16-bit integer, 64-bit integer, 16-bit
-- floating-point, and vectors of these types.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME'
--
-- -   'KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2019-01-08 (Neil Henning)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_subgroup_extended_types Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_subgroup_extended_types  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES_KHR
                                                                , PhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR
                                                                , KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION
                                                                , pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION
                                                                , KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME
                                                                , pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME
                                                                ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types (PhysicalDeviceShaderSubgroupExtendedTypesFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR"
type PhysicalDeviceShaderSubgroupExtendedTypesFeaturesKHR = PhysicalDeviceShaderSubgroupExtendedTypesFeatures


type KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION"
pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_SPEC_VERSION = 1


type KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME = "VK_KHR_shader_subgroup_extended_types"

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME"
pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_SUBGROUP_EXTENDED_TYPES_EXTENSION_NAME = "VK_KHR_shader_subgroup_extended_types"

