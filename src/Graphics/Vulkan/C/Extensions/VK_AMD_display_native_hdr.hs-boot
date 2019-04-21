{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( VkDisplayNativeHdrSurfaceCapabilitiesAMD
  , VkSwapchainDisplayNativeHdrCreateInfoAMD
  , FN_vkSetLocalDimmingAMD
  , PFN_vkSetLocalDimmingAMD
  ) where

import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkBool32
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )


data VkDisplayNativeHdrSurfaceCapabilitiesAMD

data VkSwapchainDisplayNativeHdrCreateInfoAMD

type FN_vkSetLocalDimmingAMD = ("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ()
type PFN_vkSetLocalDimmingAMD = FunPtr FN_vkSetLocalDimmingAMD
