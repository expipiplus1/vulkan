{-# language CPP #-}
-- | = Name
--
-- VK_KHR_vertex_attribute_divisor - device extension
--
-- = VK_KHR_vertex_attribute_divisor
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
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
--     'Vulkan.Core10.GraphicsPipeline.PipelineVertexInputStateCreateInfo':
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
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- KHR suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2023-09-20 (Shahbaz Youssefi)
--
--     -   First Version, based on @VK_EXT_vertex_attribute_divisor@
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_vertex_attribute_divisor Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_vertex_attribute_divisor  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_KHR
                                                          , pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR
                                                          , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR
                                                          , VertexInputBindingDivisorDescriptionKHR
                                                          , PipelineVertexInputDivisorStateCreateInfoKHR
                                                          , PhysicalDeviceVertexAttributeDivisorPropertiesKHR
                                                          , PhysicalDeviceVertexAttributeDivisorFeaturesKHR
                                                          , KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
                                                          , pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
                                                          , KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
                                                          , pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap (PhysicalDeviceVertexAttributeDivisorFeatures)
import Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap (PhysicalDeviceVertexAttributeDivisorProperties)
import Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap (PipelineVertexInputDivisorStateCreateInfo)
import Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap (VertexInputBindingDivisorDescription)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_KHR = STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES


-- No documentation found for TopLevel "VkVertexInputBindingDivisorDescriptionKHR"
type VertexInputBindingDivisorDescriptionKHR = VertexInputBindingDivisorDescription


-- No documentation found for TopLevel "VkPipelineVertexInputDivisorStateCreateInfoKHR"
type PipelineVertexInputDivisorStateCreateInfoKHR = PipelineVertexInputDivisorStateCreateInfo


-- No documentation found for TopLevel "VkPhysicalDeviceVertexAttributeDivisorPropertiesKHR"
type PhysicalDeviceVertexAttributeDivisorPropertiesKHR = PhysicalDeviceVertexAttributeDivisorProperties


-- No documentation found for TopLevel "VkPhysicalDeviceVertexAttributeDivisorFeaturesKHR"
type PhysicalDeviceVertexAttributeDivisorFeaturesKHR = PhysicalDeviceVertexAttributeDivisorFeatures


type KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION"
pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 1


type KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_KHR_vertex_attribute_divisor"

-- No documentation found for TopLevel "VK_KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME"
pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_KHR_vertex_attribute_divisor"

