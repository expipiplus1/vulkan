{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_maintenance3
  ( DescriptorSetLayoutSupportKHR
  , PhysicalDeviceMaintenance3PropertiesKHR
  , getDescriptorSetLayoutSupportKHR
  , pattern KHR_MAINTENANCE3_EXTENSION_NAME
  , pattern KHR_MAINTENANCE3_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_maintenance3
  ( pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME
  , pattern VK_KHR_MAINTENANCE3_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSetLayoutCreateInfo(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( DescriptorSetLayoutSupport(..)
  , PhysicalDeviceMaintenance3Properties(..)
  , getDescriptorSetLayoutSupport
  )


type DescriptorSetLayoutSupportKHR = DescriptorSetLayoutSupport
-- TODO: Pattern constructor alias)

type PhysicalDeviceMaintenance3PropertiesKHR = PhysicalDeviceMaintenance3Properties
-- TODO: Pattern constructor alias)

getDescriptorSetLayoutSupportKHR :: Device ->  DescriptorSetLayoutCreateInfo ->  IO (DescriptorSetLayoutSupport)
getDescriptorSetLayoutSupportKHR = getDescriptorSetLayoutSupport

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE3_EXTENSION_NAME"
pattern KHR_MAINTENANCE3_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_MAINTENANCE3_EXTENSION_NAME = VK_KHR_MAINTENANCE3_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE3_SPEC_VERSION"
pattern KHR_MAINTENANCE3_SPEC_VERSION :: Integral a => a
pattern KHR_MAINTENANCE3_SPEC_VERSION = VK_KHR_MAINTENANCE3_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR :: VkStructureType
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR = STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
