{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display
  ( FN_vkReleaseDisplayEXT
  , PFN_vkReleaseDisplayEXT
  ) where

import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )


type FN_vkReleaseDisplayEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult
type PFN_vkReleaseDisplayEXT = FunPtr FN_vkReleaseDisplayEXT
