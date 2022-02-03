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
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
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
--     -   Promoted to Vulkan 1.3 Core
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
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES_KHR'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- KHR suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductInputAllKHR DotProductInputAllKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductInput4x8BitKHR DotProductInput4x8BitKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductInput4x8BitPackedKHR DotProductInput4x8BitPackedKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-DotProductKHR DotProductKHR>
--
-- == Version History
--
-- -   Revision 1, 2021-06-16 (Kévin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceShaderIntegerDotProductFeaturesKHR',
-- 'PhysicalDeviceShaderIntegerDotProductPropertiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_integer_dot_product Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_integer_dot_product  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES_KHR
                                                            , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES_KHR
                                                            , PhysicalDeviceShaderIntegerDotProductFeaturesKHR
                                                            , PhysicalDeviceShaderIntegerDotProductPropertiesKHR
                                                            , KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION
                                                            , pattern KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION
                                                            , KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME
                                                            , pattern KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME
                                                            ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product (PhysicalDeviceShaderIntegerDotProductFeatures)
import Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product (PhysicalDeviceShaderIntegerDotProductProperties)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES


-- No documentation found for TopLevel "VkPhysicalDeviceShaderIntegerDotProductFeaturesKHR"
type PhysicalDeviceShaderIntegerDotProductFeaturesKHR = PhysicalDeviceShaderIntegerDotProductFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceShaderIntegerDotProductPropertiesKHR"
type PhysicalDeviceShaderIntegerDotProductPropertiesKHR = PhysicalDeviceShaderIntegerDotProductProperties


type KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION"
pattern KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_INTEGER_DOT_PRODUCT_SPEC_VERSION = 1


type KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME = "VK_KHR_shader_integer_dot_product"

-- No documentation found for TopLevel "VK_KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME"
pattern KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_INTEGER_DOT_PRODUCT_EXTENSION_NAME = "VK_KHR_shader_integer_dot_product"

