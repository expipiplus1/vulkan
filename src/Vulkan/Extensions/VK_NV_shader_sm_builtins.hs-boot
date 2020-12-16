{-# language CPP #-}
-- | = Name
--
-- VK_NV_shader_sm_builtins - device extension
--
-- == VK_NV_shader_sm_builtins
--
-- [__Name String__]
--     @VK_NV_shader_sm_builtins@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     155
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_shader_sm_builtins:%20&body=@dgkoch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-28
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_sm_builtins.html SPV_NV_shader_sm_builtins>.
--
--     -   This extension enables
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_shader_sm_builtins.txt GL_NV_shader_sm_builtins>
--         for GLSL source languages.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
-- == Description
--
-- This extension provides the ability to determine device-specific
-- properties on NVIDIA GPUs. It provides the number of streaming
-- multiprocessors (SMs), the maximum number of warps (subgroups) that can
-- run on an SM, and shader builtins to enable invocations to identify
-- which SM and warp a shader invocation is executing on.
--
-- This extension enables support for the SPIR-V @ShaderSMBuiltinsNV@
-- capability.
--
-- These properties and built-ins /should/ typically only be used for
-- debugging purposes.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderSMBuiltinsFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderSMBuiltinsPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_SHADER_SM_BUILTINS_EXTENSION_NAME'
--
-- -   'NV_SHADER_SM_BUILTINS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV'
--
-- == New or Modified Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-warpspersmnv WarpsPerSMNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-smcountnv SMCountNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-warpidnv WarpIDNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-smidnv SMIDNV>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ShaderSMBuiltinsNV ShaderSMBuiltinsNV>
--
-- == Issues
--
-- 1.  What should we call this extension?
--
--     RESOLVED: Using NV_shader_sm_builtins. Other options considered
--     included:
--
--     -   NV_shader_smid - but SMID is really easy to typo\/confuse as
--         SIMD.
--
--     -   NV_shader_sm_info - but __Info__ is typically reserved for input
--         structures
--
-- == Version History
--
-- -   Revision 1, 2019-05-28 (Daniel Koch)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceShaderSMBuiltinsFeaturesNV',
-- 'PhysicalDeviceShaderSMBuiltinsPropertiesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shader_sm_builtins Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_shader_sm_builtins  ( PhysicalDeviceShaderSMBuiltinsFeaturesNV
                                                   , PhysicalDeviceShaderSMBuiltinsPropertiesNV
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderSMBuiltinsFeaturesNV

instance ToCStruct PhysicalDeviceShaderSMBuiltinsFeaturesNV
instance Show PhysicalDeviceShaderSMBuiltinsFeaturesNV

instance FromCStruct PhysicalDeviceShaderSMBuiltinsFeaturesNV


data PhysicalDeviceShaderSMBuiltinsPropertiesNV

instance ToCStruct PhysicalDeviceShaderSMBuiltinsPropertiesNV
instance Show PhysicalDeviceShaderSMBuiltinsPropertiesNV

instance FromCStruct PhysicalDeviceShaderSMBuiltinsPropertiesNV

