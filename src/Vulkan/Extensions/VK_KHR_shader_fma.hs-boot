{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_fma - device extension
--
-- = VK_KHR_shader_fma
--
-- [__Name String__]
--     @VK_KHR_shader_fma@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     580
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
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_fma.html SPV_KHR_fma>
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_fma] @gnl21%0A*Here describe the issue or question you have about the VK_KHR_shader_fma extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_fma.adoc VK_KHR_shader_fma>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-10
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
-- == Description
--
-- This extension allows applications to use the SPV_KHR_fma extension to
-- obtain correctly-rounded results for fused-multiply add (fma)
-- operations.
--
-- Fused-multiply add is a building block of many high-precision numerical
-- functions. It provides better accuracy than separate operations, because
-- of the removal of the intermediate rounding step, and often costs less
-- than the pair of separate operations.
--
-- Vulkan currently exposes an fma primitive that can give the reduced
-- cost, but it is not guaranteed to be a fused operation, so the accuracy
-- cannot be relied on. For applications which require the high accuracy,
-- therefore, the operation must be emulated or the algorithm changed so as
-- not to require fma. This is often vastly more costly, even though fma is
-- supported in much of the underlying hardware.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderFmaFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_FMA_EXTENSION_NAME'
--
-- -   'KHR_SHADER_FMA_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FMA_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2025-06-10 (Graeme Leese)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_fma Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_fma  (PhysicalDeviceShaderFmaFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderFmaFeaturesKHR

instance ToCStruct PhysicalDeviceShaderFmaFeaturesKHR
instance Show PhysicalDeviceShaderFmaFeaturesKHR

instance FromCStruct PhysicalDeviceShaderFmaFeaturesKHR

