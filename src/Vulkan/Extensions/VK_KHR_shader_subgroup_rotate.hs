{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_subgroup_rotate - device extension
--
-- = VK_KHR_shader_subgroup_rotate
--
-- [__Name String__]
--     @VK_KHR_shader_subgroup_rotate@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     417
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_subgroup_rotate.html SPV_KHR_subgroup_rotate>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_subgroup_rotate] @kpet%0A*Here describe the issue or question you have about the VK_KHR_shader_subgroup_rotate extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_subgroup_rotate.adoc VK_KHR_shader_subgroup_rotate>
--
-- [__Last Modified Date__]
--     2024-01-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Tobias Hector, AMD
--
--     -   Jon Leech, Khronos
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Graeme Leese, Broadcom
--
--     -   Tom Olson, Arm Ltd.
--
--     -   Spencer Fricke, LunarG Inc.
--
-- This extension adds support for the subgroup rotate instruction defined
-- in SPV_KHR_subgroup_rotate.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderSubgroupRotateFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME'
--
-- -   'KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlagBits':
--
--     -   'SUBGROUP_FEATURE_ROTATE_BIT_KHR'
--
--     -   'SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-GroupNonUniformRotateKHR GroupNonUniformRotateKHR>
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- KHR suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 2, 2024-01-29 (Kévin Petit)
--
--     -   Add 'SUBGROUP_FEATURE_ROTATE_BIT_KHR' and
--         'SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT_KHR'
--
-- -   Revision 1, 2023-06-20 (Kévin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_subgroup_rotate Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_subgroup_rotate  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES_KHR
                                                        , pattern SUBGROUP_FEATURE_ROTATE_BIT_KHR
                                                        , pattern SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT_KHR
                                                        , PhysicalDeviceShaderSubgroupRotateFeaturesKHR
                                                        , KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION
                                                        , pattern KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION
                                                        , KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME
                                                        , pattern KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_KHR_shader_subgroup_rotateRoadmap (PhysicalDeviceShaderSubgroupRotateFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_ROTATE_BIT))
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlags)
import Vulkan.Core11.Enums.SubgroupFeatureFlagBits (SubgroupFeatureFlagBits(SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES


-- No documentation found for TopLevel "VK_SUBGROUP_FEATURE_ROTATE_BIT_KHR"
pattern SUBGROUP_FEATURE_ROTATE_BIT_KHR = SUBGROUP_FEATURE_ROTATE_BIT


-- No documentation found for TopLevel "VK_SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT_KHR"
pattern SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT_KHR = SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT


-- No documentation found for TopLevel "VkPhysicalDeviceShaderSubgroupRotateFeaturesKHR"
type PhysicalDeviceShaderSubgroupRotateFeaturesKHR = PhysicalDeviceShaderSubgroupRotateFeatures


type KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION"
pattern KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_SUBGROUP_ROTATE_SPEC_VERSION = 2


type KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME = "VK_KHR_shader_subgroup_rotate"

-- No documentation found for TopLevel "VK_KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME"
pattern KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_SUBGROUP_ROTATE_EXTENSION_NAME = "VK_KHR_shader_subgroup_rotate"

