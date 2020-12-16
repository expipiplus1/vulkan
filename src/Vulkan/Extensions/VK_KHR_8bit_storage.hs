{-# language CPP #-}
-- | = Name
--
-- VK_KHR_8bit_storage - device extension
--
-- == VK_KHR_8bit_storage
--
-- [__Name String__]
--     @VK_KHR_8bit_storage@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     178
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
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Alexander Galazin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_8bit_storage:%20&body=@alegal-arm%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-02-05
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_8bit_storage.html SPV_KHR_8bit_storage>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_shader_16bit_storage.txt GL_EXT_shader_16bit_storage>
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Alexander Galazin, Arm
--
-- == Description
--
-- The @VK_KHR_8bit_storage@ extension allows use of 8-bit types in uniform
-- and storage buffers, and push constant blocks. This extension introduces
-- several new optional features which map to SPIR-V capabilities and allow
-- access to 8-bit data in @Block@-decorated objects in the @Uniform@ and
-- the @StorageBuffer@ storage classes, and objects in the @PushConstant@
-- storage class.
--
-- The @StorageBuffer8BitAccess@ capability /must/ be supported by all
-- implementations of this extension. The other capabilities are optional.
--
-- == Promotion to Vulkan 1.2
--
-- Functionality in this extension is included in core Vulkan 1.2, with the
-- KHR suffix omitted. However, if Vulkan 1.2 is supported and this
-- extension is not, the @StorageBuffer8BitAccess@ capability is optional.
-- The original type, enum and command names are still available as aliases
-- of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevice8BitStorageFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_8BIT_STORAGE_EXTENSION_NAME'
--
-- -   'KHR_8BIT_STORAGE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-StorageBuffer8BitAccess StorageBuffer8BitAccess>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-UniformAndStorageBuffer8BitAccess UniformAndStorageBuffer8BitAccess>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-StoragePushConstant8 StoragePushConstant8>
--
-- == Version History
--
-- -   Revision 1, 2018-02-05 (Alexander Galazin)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDevice8BitStorageFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_8bit_storage Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_8bit_storage  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
                                              , PhysicalDevice8BitStorageFeaturesKHR
                                              , KHR_8BIT_STORAGE_SPEC_VERSION
                                              , pattern KHR_8BIT_STORAGE_SPEC_VERSION
                                              , KHR_8BIT_STORAGE_EXTENSION_NAME
                                              , pattern KHR_8BIT_STORAGE_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage (PhysicalDevice8BitStorageFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES


-- No documentation found for TopLevel "VkPhysicalDevice8BitStorageFeaturesKHR"
type PhysicalDevice8BitStorageFeaturesKHR = PhysicalDevice8BitStorageFeatures


type KHR_8BIT_STORAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_SPEC_VERSION"
pattern KHR_8BIT_STORAGE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_8BIT_STORAGE_SPEC_VERSION = 1


type KHR_8BIT_STORAGE_EXTENSION_NAME = "VK_KHR_8bit_storage"

-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_EXTENSION_NAME"
pattern KHR_8BIT_STORAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_8BIT_STORAGE_EXTENSION_NAME = "VK_KHR_8bit_storage"

