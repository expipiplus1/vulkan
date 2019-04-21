{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkDeviceGroupDeviceCreateInfo
  , VkPhysicalDeviceGroupProperties
  , FN_vkEnumeratePhysicalDeviceGroups
  , PFN_vkEnumeratePhysicalDeviceGroups
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkInstance
  )


data VkDeviceGroupDeviceCreateInfo

data VkPhysicalDeviceGroupProperties

type FN_vkEnumeratePhysicalDeviceGroups = ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
type PFN_vkEnumeratePhysicalDeviceGroups = FunPtr FN_vkEnumeratePhysicalDeviceGroups
