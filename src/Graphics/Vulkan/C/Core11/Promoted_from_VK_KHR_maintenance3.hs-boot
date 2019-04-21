{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport
  , VkPhysicalDeviceMaintenance3Properties
  , FN_vkGetDescriptorSetLayoutSupport
  , PFN_vkGetDescriptorSetLayoutSupport
  ) where

import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorSetLayoutCreateInfo
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )


data VkDescriptorSetLayoutSupport

data VkPhysicalDeviceMaintenance3Properties

type FN_vkGetDescriptorSetLayoutSupport = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()
type PFN_vkGetDescriptorSetLayoutSupport = FunPtr FN_vkGetDescriptorSetLayoutSupport
