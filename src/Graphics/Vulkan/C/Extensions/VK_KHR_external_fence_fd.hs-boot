{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR
  , VkImportFenceFdInfoKHR
  , FN_vkGetFenceFdKHR
  , PFN_vkGetFenceFdKHR
  , FN_vkImportFenceFdKHR
  , PFN_vkImportFenceFdKHR
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


data VkFenceGetFdInfoKHR

data VkImportFenceFdInfoKHR

type FN_vkGetFenceFdKHR = ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkFenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
type PFN_vkGetFenceFdKHR = FunPtr FN_vkGetFenceFdKHR

type FN_vkImportFenceFdKHR = ("device" ::: VkDevice) -> ("pImportFenceFdInfo" ::: Ptr VkImportFenceFdInfoKHR) -> IO VkResult
type PFN_vkImportFenceFdKHR = FunPtr FN_vkImportFenceFdKHR
