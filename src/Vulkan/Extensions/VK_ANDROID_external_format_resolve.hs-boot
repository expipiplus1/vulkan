{-# language CPP #-}
-- | = Name
--
-- VK_ANDROID_external_format_resolve - device extension
--
-- == VK_ANDROID_external_format_resolve
--
-- [__Name String__]
--     @VK_ANDROID_external_format_resolve@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     469
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ANDROID_external_memory_android_hardware_buffer VK_ANDROID_external_memory_android_hardware_buffer>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Chris Forbes
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ANDROID_external_format_resolve] @chrisforbes%0A*Here describe the issue or question you have about the VK_ANDROID_external_format_resolve extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ANDROID_external_format_resolve.adoc VK_ANDROID_external_format_resolve>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Chris Forbes, Google
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Shahbaz Youssefi, Google
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Tony Zlatsinki, Nvidia
--
--     -   Daniel Koch, Nvidia
--
--     -   Jeff Leger, Qualcomm
--
--     -   Alex Walters, Imagination
--
--     -   Andrew Garrard, Imagination
--
--     -   Ralph Potter, Samsung
--
--     -   Ian Elliott, Google
--
-- == Description
--
-- This extension enables rendering to Android Hardware Buffers with
-- external formats which cannot be directly represented as renderable in
-- Vulkan, including Y′CBCR formats.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferPropertiesANDROID':
--
--     -   'AndroidHardwareBufferFormatResolvePropertiesANDROID'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExternalFormatResolveFeaturesANDROID'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceExternalFormatResolvePropertiesANDROID'
--
-- == New Enum Constants
--
-- -   'ANDROID_EXTERNAL_FORMAT_RESOLVE_EXTENSION_NAME'
--
-- -   'ANDROID_EXTERNAL_FORMAT_RESOLVE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_RESOLVE_PROPERTIES_ANDROID'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_FEATURES_ANDROID'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_PROPERTIES_ANDROID'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits':
--
--     -   'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'
--
-- == Version History
--
-- -   Revision 1, 2023-05-34 (Tobias Hector)
--
--     -   Initial version
--
-- == See Also
--
-- 'AndroidHardwareBufferFormatResolvePropertiesANDROID',
-- 'PhysicalDeviceExternalFormatResolveFeaturesANDROID',
-- 'PhysicalDeviceExternalFormatResolvePropertiesANDROID'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ANDROID_external_format_resolve Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ANDROID_external_format_resolve  ( AndroidHardwareBufferFormatResolvePropertiesANDROID
                                                             , PhysicalDeviceExternalFormatResolveFeaturesANDROID
                                                             , PhysicalDeviceExternalFormatResolvePropertiesANDROID
                                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AndroidHardwareBufferFormatResolvePropertiesANDROID

instance ToCStruct AndroidHardwareBufferFormatResolvePropertiesANDROID
instance Show AndroidHardwareBufferFormatResolvePropertiesANDROID

instance FromCStruct AndroidHardwareBufferFormatResolvePropertiesANDROID


data PhysicalDeviceExternalFormatResolveFeaturesANDROID

instance ToCStruct PhysicalDeviceExternalFormatResolveFeaturesANDROID
instance Show PhysicalDeviceExternalFormatResolveFeaturesANDROID

instance FromCStruct PhysicalDeviceExternalFormatResolveFeaturesANDROID


data PhysicalDeviceExternalFormatResolvePropertiesANDROID

instance ToCStruct PhysicalDeviceExternalFormatResolvePropertiesANDROID
instance Show PhysicalDeviceExternalFormatResolvePropertiesANDROID

instance FromCStruct PhysicalDeviceExternalFormatResolvePropertiesANDROID

