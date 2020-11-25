{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance3 - device extension
--
-- = Registered Extension Number
--
-- 169
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- = Deprecation state
--
-- -   /Promoted/ to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- @VK_KHR_maintenance3@ adds a collection of minor features that were
-- intentionally left out or overlooked from the original Vulkan 1.0
-- release.
--
-- The new features are as follows:
--
-- -   A limit on the maximum number of descriptors that are supported in a
--     single descriptor set layout. Some implementations have a limit on
--     the total size of descriptors in a set, which cannot be expressed in
--     terms of the limits in Vulkan 1.0.
--
-- -   A limit on the maximum size of a single memory allocation. Some
--     platforms have kernel interfaces that limit the maximum size of an
--     allocation.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'getDescriptorSetLayoutSupportKHR'
--
-- == New Structures
--
-- -   'DescriptorSetLayoutSupportKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance3PropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE3_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE3_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2017-08-22
--
-- = See Also
--
-- 'DescriptorSetLayoutSupportKHR',
-- 'PhysicalDeviceMaintenance3PropertiesKHR',
-- 'getDescriptorSetLayoutSupportKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance3 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance3  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR
                                              , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR
                                              , getDescriptorSetLayoutSupportKHR
                                              , PhysicalDeviceMaintenance3PropertiesKHR
                                              , DescriptorSetLayoutSupportKHR
                                              , KHR_MAINTENANCE3_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE3_SPEC_VERSION
                                              , KHR_MAINTENANCE3_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE3_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance3 (getDescriptorSetLayoutSupport)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance3 (DescriptorSetLayoutSupport)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance3 (PhysicalDeviceMaintenance3Properties)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR = STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT


-- No documentation found for TopLevel "vkGetDescriptorSetLayoutSupportKHR"
getDescriptorSetLayoutSupportKHR = getDescriptorSetLayoutSupport


-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance3PropertiesKHR"
type PhysicalDeviceMaintenance3PropertiesKHR = PhysicalDeviceMaintenance3Properties


-- No documentation found for TopLevel "VkDescriptorSetLayoutSupportKHR"
type DescriptorSetLayoutSupportKHR = DescriptorSetLayoutSupport


type KHR_MAINTENANCE3_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE3_SPEC_VERSION"
pattern KHR_MAINTENANCE3_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE3_SPEC_VERSION = 1


type KHR_MAINTENANCE3_EXTENSION_NAME = "VK_KHR_maintenance3"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE3_EXTENSION_NAME"
pattern KHR_MAINTENANCE3_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE3_EXTENSION_NAME = "VK_KHR_maintenance3"

