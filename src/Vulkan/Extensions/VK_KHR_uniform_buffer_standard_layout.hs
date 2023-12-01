{-# language CPP #-}
-- | = Name
--
-- VK_KHR_uniform_buffer_standard_layout - device extension
--
-- == VK_KHR_uniform_buffer_standard_layout
--
-- [__Name String__]
--     @VK_KHR_uniform_buffer_standard_layout@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     254
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_uniform_buffer_standard_layout] @gnl21%0A*Here describe the issue or question you have about the VK_KHR_uniform_buffer_standard_layout extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-25
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Tobias Hector, AMD
--
--     -   Faith Ekstrand, Intel
--
--     -   Neil Henning, AMD
--
-- == Description
--
-- This extension enables tighter array and struct packing to be used with
-- uniform buffers.
--
-- It modifies the alignment rules for uniform buffers, allowing for
-- tighter packing of arrays and structures. This allows, for example, the
-- std430 layout, as defined in
-- <https://registry.khronos.org/OpenGL/specs/gl/GLSLangSpec.4.60.pdf GLSL>
-- to be supported in uniform buffers.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceUniformBufferStandardLayoutFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME'
--
-- -   'KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2019-01-25 (Graeme Leese)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceUniformBufferStandardLayoutFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_uniform_buffer_standard_layout Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_uniform_buffer_standard_layout  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR
                                                                , PhysicalDeviceUniformBufferStandardLayoutFeaturesKHR
                                                                , KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION
                                                                , pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION
                                                                , KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
                                                                , pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME
                                                                ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout (PhysicalDeviceUniformBufferStandardLayoutFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceUniformBufferStandardLayoutFeaturesKHR"
type PhysicalDeviceUniformBufferStandardLayoutFeaturesKHR = PhysicalDeviceUniformBufferStandardLayoutFeatures


type KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION"
pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_SPEC_VERSION = 1


type KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME = "VK_KHR_uniform_buffer_standard_layout"

-- No documentation found for TopLevel "VK_KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME"
pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_UNIFORM_BUFFER_STANDARD_LAYOUT_EXTENSION_NAME = "VK_KHR_uniform_buffer_standard_layout"

