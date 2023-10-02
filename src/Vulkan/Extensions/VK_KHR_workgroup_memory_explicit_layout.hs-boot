{-# language CPP #-}
-- | = Name
--
-- VK_KHR_workgroup_memory_explicit_layout - device extension
--
-- == VK_KHR_workgroup_memory_explicit_layout
--
-- [__Name String__]
--     @VK_KHR_workgroup_memory_explicit_layout@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     337
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Caio Marcelo de Oliveira Filho
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_workgroup_memory_explicit_layout] @cmarcelo%0A*Here describe the issue or question you have about the VK_KHR_workgroup_memory_explicit_layout extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-06-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_workgroup_memory_explicit_layout.html SPV_KHR_workgroup_memory_explicit_layout>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_shared_memory_block.txt GL_EXT_shared_memory_block>
--
-- [__Contributors__]
--
--     -   Caio Marcelo de Oliveira Filho, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Graeme Leese, Broadcom
--
--     -   Faith Ekstrand, Intel
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_workgroup_memory_explicit_layout.html SPV_KHR_workgroup_memory_explicit_layout>
-- SPIR-V extension, which allows shaders to explicitly define the layout
-- of @Workgroup@ storage class memory and create aliases between variables
-- from that storage class in a compute shader.
--
-- The aliasing feature allows different “views” on the same data, so the
-- shader can bulk copy data from another storage class using one type
-- (e.g. an array of large vectors), and then use the data with a more
-- specific type. It also enables reducing the amount of workgroup memory
-- consumed by allowing the shader to alias data whose lifetimes do not
-- overlap.
--
-- The explicit layout support and some form of aliasing is also required
-- for layering OpenCL on top of Vulkan.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_EXTENSION_NAME'
--
-- -   'KHR_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-WorkgroupMemoryExplicitLayoutKHR WorkgroupMemoryExplicitLayoutKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-WorkgroupMemoryExplicitLayout8BitAccessKHR WorkgroupMemoryExplicitLayout8BitAccessKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-WorkgroupMemoryExplicitLayout16BitAccessKHR WorkgroupMemoryExplicitLayout16BitAccessKHR>
--
-- == Version History
--
-- -   Revision 1, 2020-06-01 (Caio Marcelo de Oliveira Filho)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_workgroup_memory_explicit_layout Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout  (PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR

instance ToCStruct PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR
instance Show PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR

instance FromCStruct PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR

