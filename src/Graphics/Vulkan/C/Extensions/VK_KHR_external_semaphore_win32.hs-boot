{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( VkD3D12FenceSubmitInfoKHR
  , VkExportSemaphoreWin32HandleInfoKHR
  , VkImportSemaphoreWin32HandleInfoKHR
  , VkSemaphoreGetWin32HandleInfoKHR
  , FN_vkGetSemaphoreWin32HandleKHR
  , PFN_vkGetSemaphoreWin32HandleKHR
  , FN_vkImportSemaphoreWin32HandleKHR
  , PFN_vkImportSemaphoreWin32HandleKHR
  ) where

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
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( HANDLE
  )


data VkD3D12FenceSubmitInfoKHR

data VkExportSemaphoreWin32HandleInfoKHR

data VkImportSemaphoreWin32HandleInfoKHR

data VkSemaphoreGetWin32HandleInfoKHR

type FN_vkGetSemaphoreWin32HandleKHR = ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetSemaphoreWin32HandleKHR = FunPtr FN_vkGetSemaphoreWin32HandleKHR

type FN_vkImportSemaphoreWin32HandleKHR = ("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult
type PFN_vkImportSemaphoreWin32HandleKHR = FunPtr FN_vkImportSemaphoreWin32HandleKHR
