{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags
  , FN_vkTrimCommandPool
  , PFN_vkTrimCommandPool
  ) where

import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPool
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )


data VkCommandPoolTrimFlags

type FN_vkTrimCommandPool = ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ()
type PFN_vkTrimCommandPool = FunPtr FN_vkTrimCommandPool
