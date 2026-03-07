{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_replicated_composites - device extension
--
-- = VK_EXT_shader_replicated_composites
--
-- [__Name String__]
--     @VK_EXT_shader_replicated_composites@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     565
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
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_replicated_composites.html SPV_EXT_replicated_composites>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_replicated_composites] @kpet%0A*Here describe the issue or question you have about the VK_EXT_shader_replicated_composites extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_replicated_composites.adoc VK_EXT_shader_replicated_composites>
--
-- [__Last Modified Date__]
--     2024-02-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
-- This extension adds support for creating composites from a single value
-- in SPIR-V modules, as defined by SPV_EXT_replicated_composites.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderReplicatedCompositesFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_REPLICATED_COMPOSITES_EXTENSION_NAME'
--
-- -   'EXT_SHADER_REPLICATED_COMPOSITES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_REPLICATED_COMPOSITES_FEATURES_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-ReplicatedCompositesEXT ReplicatedCompositesEXT>
--
-- == Version History
--
-- -   Revision 1, 2024-02-08 (Kévin Petit)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_replicated_composites Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_replicated_composites  (PhysicalDeviceShaderReplicatedCompositesFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderReplicatedCompositesFeaturesEXT

instance ToCStruct PhysicalDeviceShaderReplicatedCompositesFeaturesEXT
instance Show PhysicalDeviceShaderReplicatedCompositesFeaturesEXT

instance FromCStruct PhysicalDeviceShaderReplicatedCompositesFeaturesEXT

