{-# language CPP #-}
-- | = Name
--
-- VK_KHR_index_type_uint8 - device extension
--
-- = VK_KHR_index_type_uint8
--
-- [__Name String__]
--     @VK_KHR_index_type_uint8@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     534
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
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_index_type_uint8] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_KHR_index_type_uint8 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-06-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows @uint8_t@ indices to be used with
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceIndexTypeUint8FeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_INDEX_TYPE_UINT8_EXTENSION_NAME'
--
-- -   'KHR_INDEX_TYPE_UINT8_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.IndexType.IndexType':
--
--     -   'INDEX_TYPE_UINT8_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_KHR'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- KHR suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2023-06-06 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_index_type_uint8 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_index_type_uint8  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_KHR
                                                  , pattern INDEX_TYPE_UINT8_KHR
                                                  , PhysicalDeviceIndexTypeUint8FeaturesKHR
                                                  , KHR_INDEX_TYPE_UINT8_SPEC_VERSION
                                                  , pattern KHR_INDEX_TYPE_UINT8_SPEC_VERSION
                                                  , KHR_INDEX_TYPE_UINT8_EXTENSION_NAME
                                                  , pattern KHR_INDEX_TYPE_UINT8_EXTENSION_NAME
                                                  ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_KHR_index_type_uint8Roadmap (PhysicalDeviceIndexTypeUint8Features)
import Vulkan.Core10.Enums.IndexType (IndexType(INDEX_TYPE_UINT8))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES


-- No documentation found for TopLevel "VK_INDEX_TYPE_UINT8_KHR"
pattern INDEX_TYPE_UINT8_KHR = INDEX_TYPE_UINT8


-- No documentation found for TopLevel "VkPhysicalDeviceIndexTypeUint8FeaturesKHR"
type PhysicalDeviceIndexTypeUint8FeaturesKHR = PhysicalDeviceIndexTypeUint8Features


type KHR_INDEX_TYPE_UINT8_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_INDEX_TYPE_UINT8_SPEC_VERSION"
pattern KHR_INDEX_TYPE_UINT8_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_INDEX_TYPE_UINT8_SPEC_VERSION = 1


type KHR_INDEX_TYPE_UINT8_EXTENSION_NAME = "VK_KHR_index_type_uint8"

-- No documentation found for TopLevel "VK_KHR_INDEX_TYPE_UINT8_EXTENSION_NAME"
pattern KHR_INDEX_TYPE_UINT8_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_INDEX_TYPE_UINT8_EXTENSION_NAME = "VK_KHR_index_type_uint8"

