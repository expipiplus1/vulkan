{-# language CPP #-}
-- | = Name
--
-- VK_KHR_16bit_storage - device extension
--
-- == VK_KHR_16bit_storage
--
-- [__Name String__]
--     @VK_KHR_16bit_storage@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     84
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
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_16bit_storage:%20&body=@janharaldfredriksen-arm%20 >
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
--     -   Promoted to Vulkan 1.1 Core
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_16bit_storage.html SPV_KHR_16bit_storage>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_shader_16bit_storage.txt GL_EXT_shader_16bit_storage>
--
-- [__Contributors__]
--
--     -   Alexander Galazin, ARM
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Joerg Wagner, ARM
--
--     -   Neil Henning, Codeplay
--
--     -   Jeff Bolz, Nvidia
--
--     -   Daniel Koch, Nvidia
--
--     -   David Neto, Google
--
--     -   John Kessenich, Google
--
-- == Description
--
-- The @VK_KHR_16bit_storage@ extension allows use of 16-bit types in
-- shader input and output interfaces, and push constant blocks. This
-- extension introduces several new optional features which map to SPIR-V
-- capabilities and allow access to 16-bit data in @Block@-decorated
-- objects in the @Uniform@ and the @StorageBuffer@ storage classes, and
-- objects in the @PushConstant@ storage class. This extension allows
-- 16-bit variables to be declared and used as user-defined shader inputs
-- and outputs but does not change location assignment and component
-- assignment rules.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. However, if Vulkan 1.1 is supported and this
-- extension is not, the @storageBuffer16BitAccess@ capability is optional.
-- The original type, enum and command names are still available as aliases
-- of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevice16BitStorageFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_16BIT_STORAGE_EXTENSION_NAME'
--
-- -   'KHR_16BIT_STORAGE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-StorageBuffer16BitAccess StorageBuffer16BitAccess>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-UniformAndStorageBuffer16BitAccess UniformAndStorageBuffer16BitAccess>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-StoragePushConstant16 StoragePushConstant16>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-StorageInputOutput16 StorageInputOutput16>
--
-- == Version History
--
-- -   Revision 1, 2017-03-23 (Alexander Galazin)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDevice16BitStorageFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_16bit_storage Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_16bit_storage  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR
                                               , PhysicalDevice16BitStorageFeaturesKHR
                                               , KHR_16BIT_STORAGE_SPEC_VERSION
                                               , pattern KHR_16BIT_STORAGE_SPEC_VERSION
                                               , KHR_16BIT_STORAGE_EXTENSION_NAME
                                               , pattern KHR_16BIT_STORAGE_EXTENSION_NAME
                                               ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage (PhysicalDevice16BitStorageFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES


-- No documentation found for TopLevel "VkPhysicalDevice16BitStorageFeaturesKHR"
type PhysicalDevice16BitStorageFeaturesKHR = PhysicalDevice16BitStorageFeatures


type KHR_16BIT_STORAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_16BIT_STORAGE_SPEC_VERSION"
pattern KHR_16BIT_STORAGE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_16BIT_STORAGE_SPEC_VERSION = 1


type KHR_16BIT_STORAGE_EXTENSION_NAME = "VK_KHR_16bit_storage"

-- No documentation found for TopLevel "VK_KHR_16BIT_STORAGE_EXTENSION_NAME"
pattern KHR_16BIT_STORAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_16BIT_STORAGE_EXTENSION_NAME = "VK_KHR_16bit_storage"

