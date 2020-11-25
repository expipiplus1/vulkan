{-# language CPP #-}
-- | = Name
--
-- VK_INTEL_shader_integer_functions2 - device extension
--
-- = Registered Extension Number
--
-- 210
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-04-30
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ian Romanick, Intel
--
--     -   Ben Ashbaugh, Intel
--
-- == Description
--
-- This extension adds support for several new integer instructions in
-- SPIR-V for use in graphics shaders. Many of these instructions have
-- pre-existing counterparts in the Kernel environment.
--
-- The added integer functions are defined by the
-- {spirv}\/INTEL\/SPV_INTEL_shader_integer_functions2.html[@SPV_INTEL_shader_integer_functions@]
-- SPIR-V extension and can be used with the
-- GL_INTEL_shader_integer_functions2 GLSL extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL'
--
-- == New Enum Constants
--
-- -   'INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME'
--
-- -   'INTEL_SHADER_INTEGER_FUNCTIONS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-cooperativeMatrix IntegerFunctions2INTEL>
--
-- == Version History
--
-- -   Revision 1, 2019-04-30 (Ian Romanick)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_INTEL_shader_integer_functions2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_INTEL_shader_integer_functions2  (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL

instance ToCStruct PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
instance Show PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL

instance FromCStruct PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL

