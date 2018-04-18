{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_maintenance3
  ( pattern VK_KHR_MAINTENANCE3_SPEC_VERSION
  , pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME
  , vkGetDescriptorSetLayoutSupportKHR
  , VkPhysicalDeviceMaintenance3PropertiesKHR
  , pattern VkPhysicalDeviceMaintenance3PropertiesKHR
  , VkDescriptorSetLayoutSupportKHR
  , pattern VkDescriptorSetLayoutSupportKHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR
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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorSetLayoutCreateInfo(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkDevice
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  , VkPhysicalDeviceMaintenance3Properties(..)
  , VkDescriptorSetLayoutSupport(..)
  , vkGetDescriptorSetLayoutSupport
  )


pattern VK_KHR_MAINTENANCE3_SPEC_VERSION :: Integral a => a
pattern VK_KHR_MAINTENANCE3_SPEC_VERSION = 1
pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_MAINTENANCE3_EXTENSION_NAME = "VK_KHR_maintenance3"
vkGetDescriptorSetLayoutSupportKHR :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()
vkGetDescriptorSetLayoutSupportKHR = vkGetDescriptorSetLayoutSupport
type VkPhysicalDeviceMaintenance3PropertiesKHR = VkPhysicalDeviceMaintenance3Properties


pattern VkPhysicalDeviceMaintenance3PropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("maxPerSetDescriptors" ::: Word32) -> ("maxMemoryAllocationSize" ::: VkDeviceSize) -> VkPhysicalDeviceMaintenance3PropertiesKHR
pattern VkPhysicalDeviceMaintenance3PropertiesKHR vkSType vkPNext vkMaxPerSetDescriptors vkMaxMemoryAllocationSize = VkPhysicalDeviceMaintenance3Properties vkSType vkPNext vkMaxPerSetDescriptors vkMaxMemoryAllocationSize
type VkDescriptorSetLayoutSupportKHR = VkDescriptorSetLayoutSupport


pattern VkDescriptorSetLayoutSupportKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("supported" ::: VkBool32) -> VkDescriptorSetLayoutSupportKHR
pattern VkDescriptorSetLayoutSupportKHR vkSType vkPNext vkSupported = VkDescriptorSetLayoutSupport vkSType vkPNext vkSupported
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT_KHR = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
