{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_maintenance3"
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

