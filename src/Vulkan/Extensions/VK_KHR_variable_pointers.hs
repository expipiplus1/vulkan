{-# language CPP #-}
-- | = Name
--
-- VK_KHR_variable_pointers - device extension
--
-- == VK_KHR_variable_pointers
--
-- [__Name String__]
--     @VK_KHR_variable_pointers@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     121
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
--     -   Requires @VK_KHR_storage_buffer_storage_class@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_variable_pointers:%20&body=@critsec%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_variable_pointers.html SPV_KHR_variable_pointers>
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   John Kessenich, Google
--
--     -   Neil Henning, Codeplay
--
--     -   David Neto, Google
--
--     -   Daniel Koch, Nvidia
--
--     -   Graeme Leese, Broadcom
--
--     -   Weifeng Zhang, Qualcomm
--
--     -   Stephen Clarke, Imagination Technologies
--
--     -   Jason Ekstrand, Intel
--
--     -   Jesse Hall, Google
--
-- == Description
--
-- The @VK_KHR_variable_pointers@ extension allows implementations to
-- indicate their level of support for the @SPV_KHR_variable_pointers@
-- SPIR-V extension. The SPIR-V extension allows shader modules to use
-- invocation-private pointers into uniform and\/or storage buffers, where
-- the pointer values can be dynamic and non-uniform.
--
-- The @SPV_KHR_variable_pointers@ extension introduces two capabilities.
-- The first, @VariablePointersStorageBuffer@, /must/ be supported by all
-- implementations of this extension. The second, @VariablePointers@, is
-- optional.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted, however support for the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-variablePointersStorageBuffer variablePointersStorageBuffer>
-- feature is made optional. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVariablePointerFeaturesKHR'
--
--     -   'PhysicalDeviceVariablePointersFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_VARIABLE_POINTERS_EXTENSION_NAME'
--
-- -   'KHR_VARIABLE_POINTERS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-VariablePointers VariablePointers>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-VariablePointersStorageBuffer VariablePointersStorageBuffer>
--
-- == Issues
--
-- 1) Do we need an optional property for the SPIR-V
-- @VariablePointersStorageBuffer@ capability or should it be mandatory
-- when this extension is advertised?
--
-- __RESOLVED__: Add it as a distinct feature, but make support mandatory.
-- Adding it as a feature makes the extension easier to include in a future
-- core API version. In the extension, the feature is mandatory, so that
-- presence of the extension guarantees some functionality. When included
-- in a core API version, the feature would be optional.
--
-- 2) Can support for these capabilities vary between shader stages?
--
-- __RESOLVED__: No, if the capability is supported in any stage it must be
-- supported in all stages.
--
-- 3) Should the capabilities be features or limits?
--
-- __RESOLVED__: Features, primarily for consistency with other similar
-- extensions.
--
-- == Version History
--
-- -   Revision 1, 2017-03-14 (Jesse Hall and John Kessenich)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceVariablePointerFeaturesKHR',
-- 'PhysicalDeviceVariablePointersFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_variable_pointers Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_variable_pointers  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR
                                                   , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR
                                                   , PhysicalDeviceVariablePointersFeaturesKHR
                                                   , PhysicalDeviceVariablePointerFeaturesKHR
                                                   , KHR_VARIABLE_POINTERS_SPEC_VERSION
                                                   , pattern KHR_VARIABLE_POINTERS_SPEC_VERSION
                                                   , KHR_VARIABLE_POINTERS_EXTENSION_NAME
                                                   , pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME
                                                   ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers (PhysicalDeviceVariablePointersFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES_KHR


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointersFeaturesKHR"
type PhysicalDeviceVariablePointersFeaturesKHR = PhysicalDeviceVariablePointersFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceVariablePointerFeaturesKHR"
type PhysicalDeviceVariablePointerFeaturesKHR = PhysicalDeviceVariablePointersFeatures


type KHR_VARIABLE_POINTERS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_VARIABLE_POINTERS_SPEC_VERSION"
pattern KHR_VARIABLE_POINTERS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_VARIABLE_POINTERS_SPEC_VERSION = 1


type KHR_VARIABLE_POINTERS_EXTENSION_NAME = "VK_KHR_variable_pointers"

-- No documentation found for TopLevel "VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME"
pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VARIABLE_POINTERS_EXTENSION_NAME = "VK_KHR_variable_pointers"

