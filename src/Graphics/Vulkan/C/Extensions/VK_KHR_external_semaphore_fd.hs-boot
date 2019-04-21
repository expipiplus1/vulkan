{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR
  , VkSemaphoreGetFdInfoKHR
  , FN_vkGetSemaphoreFdKHR
  , PFN_vkGetSemaphoreFdKHR
  , FN_vkImportSemaphoreFdKHR
  , PFN_vkImportSemaphoreFdKHR
  ) where

import Foreign.C.Types
  ( CInt(..)
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
  ( VkDevice
  )


data VkImportSemaphoreFdInfoKHR

data VkSemaphoreGetFdInfoKHR

type FN_vkGetSemaphoreFdKHR = ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
type PFN_vkGetSemaphoreFdKHR = FunPtr FN_vkGetSemaphoreFdKHR

type FN_vkImportSemaphoreFdKHR = ("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult
type PFN_vkImportSemaphoreFdKHR = FunPtr FN_vkImportSemaphoreFdKHR
