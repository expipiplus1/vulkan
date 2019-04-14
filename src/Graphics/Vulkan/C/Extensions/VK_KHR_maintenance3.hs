{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupportKHR
  , pattern VkDescriptorSetLayoutSupportKHR
  , VkPhysicalDeviceMaintenance3PropertiesKHR
  , pattern VkPhysicalDeviceMaintenance3PropertiesKHR
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetDescriptorSetLayoutSupportKHR
#endif
  , pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME
  , pattern VK_KHR_MAINTENANCE3_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorSetLayoutCreateInfo(..)
  )
#endif
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  , VkPhysicalDeviceMaintenance3Properties(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( vkGetDescriptorSetLayoutSupport
  )
#endif
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDescriptorSetLayoutSupportKHR"
type VkDescriptorSetLayoutSupportKHR = VkDescriptorSetLayoutSupport


-- No documentation found for TopLevel "VkDescriptorSetLayoutSupportKHR"
pattern VkDescriptorSetLayoutSupportKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("supported" ::: VkBool32) -> VkDescriptorSetLayoutSupportKHR
pattern VkDescriptorSetLayoutSupportKHR vkSType vkPNext vkSupported = VkDescriptorSetLayoutSupport vkSType vkPNext vkSupported
-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance3PropertiesKHR"
type VkPhysicalDeviceMaintenance3PropertiesKHR = VkPhysicalDeviceMaintenance3Properties


-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance3PropertiesKHR"
pattern VkPhysicalDeviceMaintenance3PropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("maxPerSetDescriptors" ::: Word32) -> ("maxMemoryAllocationSize" ::: VkDeviceSize) -> VkPhysicalDeviceMaintenance3PropertiesKHR
pattern VkPhysicalDeviceMaintenance3PropertiesKHR vkSType vkPNext vkMaxPerSetDescriptors vkMaxMemoryAllocationSize = VkPhysicalDeviceMaintenance3Properties vkSType vkPNext vkMaxPerSetDescriptors vkMaxMemoryAllocationSize

#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetDescriptorSetLayoutSupportKHR"
vkGetDescriptorSetLayoutSupportKHR :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()
vkGetDescriptorSetLayoutSupportKHR = vkGetDescriptorSetLayoutSupport
#endif
-- No documentation found for TopLevel "VK_KHR_MAINTENANCE3_EXTENSION_NAME"
pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME = "VK_KHR_maintenance3"
-- No documentation found for TopLevel "VK_KHR_MAINTENANCE3_SPEC_VERSION"
pattern VK_KHR_MAINTENANCE3_SPEC_VERSION :: Integral a => a
pattern VK_KHR_MAINTENANCE3_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
