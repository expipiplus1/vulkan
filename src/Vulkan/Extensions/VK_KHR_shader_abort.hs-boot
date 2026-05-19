{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_abort - device extension
--
-- = VK_KHR_shader_abort
--
-- [__Name String__]
--     @VK_KHR_shader_abort@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     234
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_constant_data VK_KHR_shader_constant_data>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_abort.html SPV_KHR_abort>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_abort] @tobski%0A*Here describe the issue or question you have about the VK_KHR_shader_abort extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_abort.adoc VK_KHR_shader_abort>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Piers Daniell, Nvidia
--
--     -   Craig Graham, Samsung
--
--     -   Erik Hogeman, ARM
--
--     -   Ralph Potter, Samsung
--
--     -   Vikram Tarikere, IMG
--
-- == Description
--
-- This extension enables the use of the @OpAbortKHR@ instruction in
-- shaders.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_device_fault.DeviceFaultDebugInfoKHR':
--
--     -   'DeviceFaultShaderAbortMessageInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderAbortFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderAbortPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_ABORT_EXTENSION_NAME'
--
-- -   'KHR_SHADER_ABORT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_SHADER_ABORT_MESSAGE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ABORT_PROPERTIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2024-08-22 (Tobias Hector)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_abort Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_abort  ( DeviceFaultShaderAbortMessageInfoKHR
                                              , PhysicalDeviceShaderAbortFeaturesKHR
                                              , PhysicalDeviceShaderAbortPropertiesKHR
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceFaultShaderAbortMessageInfoKHR

instance ToCStruct DeviceFaultShaderAbortMessageInfoKHR
instance Show DeviceFaultShaderAbortMessageInfoKHR

instance FromCStruct DeviceFaultShaderAbortMessageInfoKHR


data PhysicalDeviceShaderAbortFeaturesKHR

instance ToCStruct PhysicalDeviceShaderAbortFeaturesKHR
instance Show PhysicalDeviceShaderAbortFeaturesKHR

instance FromCStruct PhysicalDeviceShaderAbortFeaturesKHR


data PhysicalDeviceShaderAbortPropertiesKHR

instance ToCStruct PhysicalDeviceShaderAbortPropertiesKHR
instance Show PhysicalDeviceShaderAbortPropertiesKHR

instance FromCStruct PhysicalDeviceShaderAbortPropertiesKHR

