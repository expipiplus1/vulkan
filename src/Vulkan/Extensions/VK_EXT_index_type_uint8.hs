{-# language CPP #-}
-- | = Name
--
-- VK_EXT_index_type_uint8 - device extension
--
-- = VK_EXT_index_type_uint8
--
-- [__Name String__]
--     @VK_EXT_index_type_uint8@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     266
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_index_type_uint8 VK_KHR_index_type_uint8>
--         extension
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_index_type_uint8] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_index_type_uint8 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-02
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
-- == Promotion to @VK_KHR_index_type_uint8@
--
-- All functionality in this extension is included in
-- @VK_KHR_index_type_uint8@, with the suffix changed to KHR. The original
-- enum names are still available as aliases of the KHR functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceIndexTypeUint8FeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_INDEX_TYPE_UINT8_EXTENSION_NAME'
--
-- -   'EXT_INDEX_TYPE_UINT8_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.IndexType.IndexType':
--
--     -   'INDEX_TYPE_UINT8_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-05-02 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceIndexTypeUint8FeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_index_type_uint8 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_index_type_uint8  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT
                                                  , pattern INDEX_TYPE_UINT8_EXT
                                                  , PhysicalDeviceIndexTypeUint8FeaturesEXT
                                                  , EXT_INDEX_TYPE_UINT8_SPEC_VERSION
                                                  , pattern EXT_INDEX_TYPE_UINT8_SPEC_VERSION
                                                  , EXT_INDEX_TYPE_UINT8_EXTENSION_NAME
                                                  , pattern EXT_INDEX_TYPE_UINT8_EXTENSION_NAME
                                                  , PhysicalDeviceIndexTypeUint8FeaturesKHR(..)
                                                  ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_index_type_uint8 (PhysicalDeviceIndexTypeUint8FeaturesKHR)
import Vulkan.Core10.Enums.IndexType (IndexType(INDEX_TYPE_UINT8_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_KHR))
import Vulkan.Extensions.VK_KHR_index_type_uint8 (PhysicalDeviceIndexTypeUint8FeaturesKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_KHR


-- No documentation found for TopLevel "VK_INDEX_TYPE_UINT8_EXT"
pattern INDEX_TYPE_UINT8_EXT = INDEX_TYPE_UINT8_KHR


-- No documentation found for TopLevel "VkPhysicalDeviceIndexTypeUint8FeaturesEXT"
type PhysicalDeviceIndexTypeUint8FeaturesEXT = PhysicalDeviceIndexTypeUint8FeaturesKHR


type EXT_INDEX_TYPE_UINT8_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_INDEX_TYPE_UINT8_SPEC_VERSION"
pattern EXT_INDEX_TYPE_UINT8_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_INDEX_TYPE_UINT8_SPEC_VERSION = 1


type EXT_INDEX_TYPE_UINT8_EXTENSION_NAME = "VK_EXT_index_type_uint8"

-- No documentation found for TopLevel "VK_EXT_INDEX_TYPE_UINT8_EXTENSION_NAME"
pattern EXT_INDEX_TYPE_UINT8_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_INDEX_TYPE_UINT8_EXTENSION_NAME = "VK_EXT_index_type_uint8"

