{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkExportFenceWin32HandleInfoKHR
  , VkFenceGetWin32HandleInfoKHR
  , VkImportFenceWin32HandleInfoKHR
  , FN_vkGetFenceWin32HandleKHR
  , PFN_vkGetFenceWin32HandleKHR
  , FN_vkImportFenceWin32HandleKHR
  , PFN_vkImportFenceWin32HandleKHR
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


data VkExportFenceWin32HandleInfoKHR

data VkFenceGetWin32HandleInfoKHR

data VkImportFenceWin32HandleInfoKHR

type FN_vkGetFenceWin32HandleKHR = ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetFenceWin32HandleKHR = FunPtr FN_vkGetFenceWin32HandleKHR

type FN_vkImportFenceWin32HandleKHR = ("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult
type PFN_vkImportFenceWin32HandleKHR = FunPtr FN_vkImportFenceWin32HandleKHR
