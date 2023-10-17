{-# language CPP #-}
-- | = Name
--
-- VK_ARM_shader_core_properties - device extension
--
-- == VK_ARM_shader_core_properties
--
-- [__Name String__]
--     @VK_ARM_shader_core_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     416
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_shader_core_properties] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_ARM_shader_core_properties extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-02-07
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
-- == Description
--
-- This extension provides the ability to determine device-specific
-- performance properties of Arm GPUs.
--
-- It exposes properties for the number of texel, pixel, and fused
-- multiply-add operations per clock per shader core. This can be used in
-- combination with the @VK_ARM_shader_core_builtins@ extension that
-- provides the ability to query the number of shader cores on the physical
-- device.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderCorePropertiesARM'
--
-- == New Enum Constants
--
-- -   'ARM_SHADER_CORE_PROPERTIES_EXTENSION_NAME'
--
-- -   'ARM_SHADER_CORE_PROPERTIES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_ARM'
--
-- == Version History
--
-- -   Revision 1, 2023-02-07 (Jan-Harald Fredriksen)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceShaderCorePropertiesARM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ARM_shader_core_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_shader_core_properties  (PhysicalDeviceShaderCorePropertiesARM) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderCorePropertiesARM

instance ToCStruct PhysicalDeviceShaderCorePropertiesARM
instance Show PhysicalDeviceShaderCorePropertiesARM

instance FromCStruct PhysicalDeviceShaderCorePropertiesARM

