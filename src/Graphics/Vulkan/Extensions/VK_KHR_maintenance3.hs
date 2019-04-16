{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_maintenance3
  ( DescriptorSetLayoutSupportKHR
  , PhysicalDeviceMaintenance3PropertiesKHR
  , getDescriptorSetLayoutSupportKHR
  , pattern VK_KHR_MAINTENANCE3_SPEC_VERSION
  , pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  ) where




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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_maintenance3
  ( pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME
  , pattern VK_KHR_MAINTENANCE3_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR
  )


type DescriptorSetLayoutSupportKHR = DescriptorSetLayoutSupport
-- TODO: Pattern constructor alias)
type PhysicalDeviceMaintenance3PropertiesKHR = PhysicalDeviceMaintenance3Properties
-- TODO: Pattern constructor alias)
getDescriptorSetLayoutSupportKHR :: Device ->  DescriptorSetLayoutCreateInfo ->  IO (DescriptorSetLayoutSupport)
getDescriptorSetLayoutSupportKHR = getDescriptorSetLayoutSupport
