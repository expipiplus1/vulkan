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
--     None
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_subgroup_rotate.html SPV_KHR_subgroup_rotate>
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
--     -   John Leech, Khronos
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_ROTATE_FEATURES_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlagBits':
--
--     -   'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_ROTATE_BIT_KHR'
--
--     -   'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-GroupNonUniformRotateKHR GroupNonUniformRotateKHR>
--
-- == Version History
--
-- -   Revision 2, 2024-01-29 (Kévin Petit)
--
--     -   Add
--         'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_ROTATE_BIT_KHR'
--         and
--         'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_ROTATE_CLUSTERED_BIT_KHR'
--
-- -   Revision 1, 2023-06-20 (Kévin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceShaderSubgroupRotateFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_subgroup_rotate Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_subgroup_rotate  (PhysicalDeviceShaderSubgroupRotateFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderSubgroupRotateFeaturesKHR

instance ToCStruct PhysicalDeviceShaderSubgroupRotateFeaturesKHR
instance Show PhysicalDeviceShaderSubgroupRotateFeaturesKHR

instance FromCStruct PhysicalDeviceShaderSubgroupRotateFeaturesKHR

