{-# language CPP #-}
-- | = Name
--
-- VK_NV_ray_tracing_validation - device extension
--
-- = VK_NV_ray_tracing_validation
--
-- [__Name String__]
--     @VK_NV_ray_tracing_validation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     569
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_ray_tracing_validation] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_NV_ray_tracing_validation extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_ray_tracing_validation.adoc VK_NV_ray_tracing_validation>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-03-04
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension adds support for performing ray tracing validation at an
-- implementation level.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingValidationFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_RAY_TRACING_VALIDATION_EXTENSION_NAME'
--
-- -   'NV_RAY_TRACING_VALIDATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_VALIDATION_FEATURES_NV'
--
-- == Version History
--
-- -   Revision 1, 2024-03-04 (Vikram Kushwaha)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_ray_tracing_validation Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_ray_tracing_validation  (PhysicalDeviceRayTracingValidationFeaturesNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRayTracingValidationFeaturesNV

instance ToCStruct PhysicalDeviceRayTracingValidationFeaturesNV
instance Show PhysicalDeviceRayTracingValidationFeaturesNV

instance FromCStruct PhysicalDeviceRayTracingValidationFeaturesNV

