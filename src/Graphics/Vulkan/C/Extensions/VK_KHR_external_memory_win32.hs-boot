{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  , VkExportMemoryWin32HandleInfoKHR
  , VkImportMemoryWin32HandleInfoKHR
  , VkMemoryGetWin32HandleInfoKHR
  , VkMemoryWin32HandlePropertiesKHR
  , FN_vkGetMemoryWin32HandleKHR
  , PFN_vkGetMemoryWin32HandleKHR
  , FN_vkGetMemoryWin32HandlePropertiesKHR
  , PFN_vkGetMemoryWin32HandlePropertiesKHR
  ) where

import Foreign.C.Types
  ( CWchar
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
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( HANDLE
  )


-- No documentation found for TopLevel "LPCWSTR"
type LPCWSTR = Ptr CWchar
  

data VkExportMemoryWin32HandleInfoKHR

data VkImportMemoryWin32HandleInfoKHR

data VkMemoryGetWin32HandleInfoKHR

data VkMemoryWin32HandlePropertiesKHR

type FN_vkGetMemoryWin32HandleKHR = ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetMemoryWin32HandleKHR = FunPtr FN_vkGetMemoryWin32HandleKHR

type FN_vkGetMemoryWin32HandlePropertiesKHR = ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult
type PFN_vkGetMemoryWin32HandlePropertiesKHR = FunPtr FN_vkGetMemoryWin32HandlePropertiesKHR
