{-# language CPP #-}
-- | = Name
--
-- VK_KHR_vertex_attribute_divisor - device extension
--
-- == VK_KHR_vertex_attribute_divisor
--
-- [__Name String__]
--     @VK_KHR_vertex_attribute_divisor@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     526
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_vertex_attribute_divisor] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_vertex_attribute_divisor extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_vertex_attribute_divisor.adoc VK_KHR_vertex_attribute_divisor>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-09-20
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Contributors to @VK_EXT_vertex_attribute_divisor@
--
-- == Description
--
-- This extension is based on the @VK_EXT_vertex_attribute_divisor@
-- extension. The only difference is the new property
-- @supportsNonZeroFirstInstance@, which indicates support for non-zero
-- values in @firstInstance@. This allows the extension to be supported on
-- implementations that have traditionally only supported OpenGL ES.
--
-- == New Structures
--
-- -   'VertexInputBindingDivisorDescriptionKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVertexAttributeDivisorFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceVertexAttributeDivisorPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo':
--
--     -   'PipelineVertexInputDivisorStateCreateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME'
--
-- -   'KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2023-09-20 (Shahbaz Youssefi)
--
--     -   First Version, based on @VK_EXT_vertex_attribute_divisor@
--
-- == See Also
--
-- 'PhysicalDeviceVertexAttributeDivisorFeaturesKHR',
-- 'PhysicalDeviceVertexAttributeDivisorPropertiesKHR',
-- 'PipelineVertexInputDivisorStateCreateInfoKHR',
-- 'VertexInputBindingDivisorDescriptionKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_vertex_attribute_divisor Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_vertex_attribute_divisor  ( PhysicalDeviceVertexAttributeDivisorFeaturesKHR
                                                          , PhysicalDeviceVertexAttributeDivisorPropertiesKHR
                                                          , PipelineVertexInputDivisorStateCreateInfoKHR
                                                          , VertexInputBindingDivisorDescriptionKHR
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceVertexAttributeDivisorFeaturesKHR

instance ToCStruct PhysicalDeviceVertexAttributeDivisorFeaturesKHR
instance Show PhysicalDeviceVertexAttributeDivisorFeaturesKHR

instance FromCStruct PhysicalDeviceVertexAttributeDivisorFeaturesKHR


data PhysicalDeviceVertexAttributeDivisorPropertiesKHR

instance ToCStruct PhysicalDeviceVertexAttributeDivisorPropertiesKHR
instance Show PhysicalDeviceVertexAttributeDivisorPropertiesKHR

instance FromCStruct PhysicalDeviceVertexAttributeDivisorPropertiesKHR


data PipelineVertexInputDivisorStateCreateInfoKHR

instance ToCStruct PipelineVertexInputDivisorStateCreateInfoKHR
instance Show PipelineVertexInputDivisorStateCreateInfoKHR

instance FromCStruct PipelineVertexInputDivisorStateCreateInfoKHR


data VertexInputBindingDivisorDescriptionKHR

instance ToCStruct VertexInputBindingDivisorDescriptionKHR
instance Show VertexInputBindingDivisorDescriptionKHR

instance FromCStruct VertexInputBindingDivisorDescriptionKHR

