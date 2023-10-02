{-# language CPP #-}
-- | = Name
--
-- VK_ARM_shader_core_builtins - device extension
--
-- == VK_ARM_shader_core_builtins
--
-- [__Name String__]
--     @VK_ARM_shader_core_builtins@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     498
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_shader_core_builtins] @kevinpetit%0A*Here describe the issue or question you have about the VK_ARM_shader_core_builtins extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-10-05
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/ARM/SPV_ARM_core_builtins.html SPV_ARM_core_builtins>.
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/arm/GLSL_ARM_shader_core_builtins.txt GL_ARM_shader_core_builtins>
--
-- [__Contributors__]
--
--     -   Kevin Petit, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
-- == Description
--
-- This extension provides the ability to determine device-specific
-- properties on Arm GPUs. It exposes properties for the number of shader
-- cores, the maximum number of warps that can run on a shader core, and
-- shader builtins to enable invocations to identify which core and warp a
-- shader invocation is executing on.
--
-- This extension enables support for the SPIR-V @CoreBuiltinsARM@
-- capability.
--
-- These properties and built-ins can be used for debugging or performance
-- optimisation purposes. A typical optimisation example would be to use
-- @CoreIDARM@ to select a per-shader-core instance of a data structure in
-- algorithms that use atomics so as to reduce contention.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderCoreBuiltinsFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderCoreBuiltinsPropertiesARM'
--
-- == New Enum Constants
--
-- -   'ARM_SHADER_CORE_BUILTINS_EXTENSION_NAME'
--
-- -   'ARM_SHADER_CORE_BUILTINS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_PROPERTIES_ARM'
--
-- == New or Modified Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-corecountarm CoreCountARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-coremaxidarm CoreMaxIDARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-coreidarm CoreIDARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-warpmaxidarm WarpsMaxIDARM>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-warpidarm WarpIDARM>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CoreBuiltinsARM CoreBuiltinsARM>
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2022-10-05 (Kevin Petit)
--
--     -   Initial revision
--
-- -   Revision 2, 2022-10-26 (Kevin Petit)
--
--     -   Add @shaderCoreMask@ property
--
-- == See Also
--
-- 'PhysicalDeviceShaderCoreBuiltinsFeaturesARM',
-- 'PhysicalDeviceShaderCoreBuiltinsPropertiesARM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ARM_shader_core_builtins Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_shader_core_builtins  ( PhysicalDeviceShaderCoreBuiltinsFeaturesARM
                                                      , PhysicalDeviceShaderCoreBuiltinsPropertiesARM
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderCoreBuiltinsFeaturesARM

instance ToCStruct PhysicalDeviceShaderCoreBuiltinsFeaturesARM
instance Show PhysicalDeviceShaderCoreBuiltinsFeaturesARM

instance FromCStruct PhysicalDeviceShaderCoreBuiltinsFeaturesARM


data PhysicalDeviceShaderCoreBuiltinsPropertiesARM

instance ToCStruct PhysicalDeviceShaderCoreBuiltinsPropertiesARM
instance Show PhysicalDeviceShaderCoreBuiltinsPropertiesARM

instance FromCStruct PhysicalDeviceShaderCoreBuiltinsPropertiesARM

