{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_integer_dot_product - device extension
--
-- == VK_KHR_shader_integer_dot_product
--
-- [__Name String__]
--     @VK_KHR_shader_integer_dot_product@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     281
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_integer_dot_product] @kevinpetit%0A<<Here describe the issue or question you have about the VK_KHR_shader_integer_dot_product extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_integer_dot_product.asciidoc VK_KHR_shader_integer_dot_product>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-16
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_integer_dot_product.html SPV_KHR_integer_dot_product>.
--
--     -   This extension interacts with @VK_KHR_shader_float16_int8@.
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Jeff Bolz, NVidia
--
--     -   Spencer Fricke, Samsung
--
--     -   Jesse Hall, Google
--
--     -   John Kessenich, Google
--
--     -   Graeme Leese, Broadcom
--
--     -   Einar Hov, Arm Ltd.
--
--     -   Stuart Brady, Arm Ltd.
--
--     -   Pablo Cascon, Arm Ltd.
--
--     -   Tobias Hector, AMD
--
--     -   Jeff Leger, Qualcomm
--
--     -   Ruihao Zhang, Qualcomm
--
--     -   Pierre Boudier, NVidia
--
--     -   Jon Leech, The Khronos Group
--
--     -   Tom Olson, Arm Ltd.
--
-- == Description
--
-- This extension adds support for the integer dot product SPIR-V
-- instructions defined in SPV_KHR_integer_dot_product. These instructions
-- are particularly useful for neural network inference and training but
-- find uses in other general purpose compute applications as well.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderIntegerDotProductFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderIntegerDotProductPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME'
--
-- -   'KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductInputAllKHR DotProductInputAllKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductInput4x8BitKHR DotProductInput4x8BitKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductInput4x8BitPackedKHR DotProductInput4x8BitPackedKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductKHR DotProductKHR>
--
-- == Version History
--
-- -   Revision 1, 2021-06-16 (Kévin Petit)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PhysicalDeviceShaderIntegerDotProductFeaturesKHR',
-- 'PhysicalDeviceShaderIntegerDotProductPropertiesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_integer_dot_product Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_integer_dot_product  ( PhysicalDeviceShaderIntegerDotProductFeaturesKHR
                                                            , PhysicalDeviceShaderIntegerDotProductPropertiesKHR
                                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderIntegerDotProductFeaturesKHR

instance ToCStruct PhysicalDeviceShaderIntegerDotProductFeaturesKHR
instance Show PhysicalDeviceShaderIntegerDotProductFeaturesKHR

instance FromCStruct PhysicalDeviceShaderIntegerDotProductFeaturesKHR


data PhysicalDeviceShaderIntegerDotProductPropertiesKHR

instance ToCStruct PhysicalDeviceShaderIntegerDotProductPropertiesKHR
instance Show PhysicalDeviceShaderIntegerDotProductPropertiesKHR

instance FromCStruct PhysicalDeviceShaderIntegerDotProductPropertiesKHR

