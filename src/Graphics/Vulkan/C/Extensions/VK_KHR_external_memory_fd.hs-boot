{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( VkImportMemoryFdInfoKHR
  , VkMemoryFdPropertiesKHR
  , VkMemoryGetFdInfoKHR
  , FN_vkGetMemoryFdKHR
  , PFN_vkGetMemoryFdKHR
  , FN_vkGetMemoryFdPropertiesKHR
  , PFN_vkGetMemoryFdPropertiesKHR
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
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits
  )


data VkImportMemoryFdInfoKHR

data VkMemoryFdPropertiesKHR

data VkMemoryGetFdInfoKHR

type FN_vkGetMemoryFdKHR = ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
type PFN_vkGetMemoryFdKHR = FunPtr FN_vkGetMemoryFdKHR

type FN_vkGetMemoryFdPropertiesKHR = ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult
type PFN_vkGetMemoryFdPropertiesKHR = FunPtr FN_vkGetMemoryFdPropertiesKHR
