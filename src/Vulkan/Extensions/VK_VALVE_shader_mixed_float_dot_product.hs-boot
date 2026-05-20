{-# language CPP #-}
-- | = Name
--
-- VK_VALVE_shader_mixed_float_dot_product - device extension
--
-- = VK_VALVE_shader_mixed_float_dot_product
--
-- [__Name String__]
--     @VK_VALVE_shader_mixed_float_dot_product@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     674
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_float16_int8 VK_KHR_shader_float16_int8>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/VALVE/SPV_VALVE_mixed_float_dot_product.html SPV_VALVE_mixed_float_dot_product>
--
-- [__Contact__]
--
--     -   Georg Lehmann
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_VALVE_shader_mixed_float_dot_product] @DadSchoorse%0A*Here describe the issue or question you have about the VK_VALVE_shader_mixed_float_dot_product extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-02-04
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Georg Lehmann, Valve
--
--     -   Mike Blumenkrantz, Valve
--
-- == Description
--
-- This extension enables support for mixed precision dot product
-- accumulate operations in shaders as defined in
-- @SPV_VALVE_mixed_float_dot_product@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE'
--
-- == New Enum Constants
--
-- -   'VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_EXTENSION_NAME'
--
-- -   'VALVE_SHADER_MIXED_FLOAT_DOT_PRODUCT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MIXED_FLOAT_DOT_PRODUCT_FEATURES_VALVE'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-DotProductFloat16AccFloat32VALVE DotProductFloat16AccFloat32VALVE>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-DotProductFloat16AccFloat16VALVE DotProductFloat16AccFloat16VALVE>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-DotProductBFloat16AccVALVE DotProductBFloat16AccVALVE>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-DotProductFloat8AccFloat32VALVE DotProductFloat8AccFloat32VALVE>
--
-- == Version History
--
-- -   Revision 1, 2026-02-04 (Georg Lehmann)
--
--     -   Initial specification
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_VALVE_shader_mixed_float_dot_product Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_VALVE_shader_mixed_float_dot_product  (PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE

instance ToCStruct PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE
instance Show PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE

instance FromCStruct PhysicalDeviceShaderMixedFloatDotProductFeaturesVALVE

