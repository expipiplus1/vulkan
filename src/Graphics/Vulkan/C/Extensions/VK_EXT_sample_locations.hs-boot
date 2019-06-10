{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( VkAttachmentSampleLocationsEXT
  , VkMultisamplePropertiesEXT
  , VkPhysicalDeviceSampleLocationsPropertiesEXT
  , VkPipelineSampleLocationsStateCreateInfoEXT
  , VkRenderPassSampleLocationsBeginInfoEXT
  , VkSampleLocationEXT
  , VkSampleLocationsInfoEXT
  , VkSubpassSampleLocationsEXT
  , FN_vkCmdSetSampleLocationsEXT
  , PFN_vkCmdSetSampleLocationsEXT
  , FN_vkGetPhysicalDeviceMultisamplePropertiesEXT
  , PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT
  ) where

import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  , VkSampleCountFlagBits
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkAttachmentSampleLocationsEXT

data VkMultisamplePropertiesEXT

data VkPhysicalDeviceSampleLocationsPropertiesEXT

data VkPipelineSampleLocationsStateCreateInfoEXT

data VkRenderPassSampleLocationsBeginInfoEXT

data VkSampleLocationEXT

data VkSampleLocationsInfoEXT

data VkSubpassSampleLocationsEXT

type FN_vkCmdSetSampleLocationsEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pSampleLocationsInfo" ::: Ptr VkSampleLocationsInfoEXT) -> IO ()
type PFN_vkCmdSetSampleLocationsEXT = FunPtr FN_vkCmdSetSampleLocationsEXT

type FN_vkGetPhysicalDeviceMultisamplePropertiesEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ()
type PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT = FunPtr FN_vkGetPhysicalDeviceMultisamplePropertiesEXT
