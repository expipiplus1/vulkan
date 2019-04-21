{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo
  , VkBindImageMemoryInfo
  , FN_vkBindBufferMemory2
  , PFN_vkBindBufferMemory2
  , FN_vkBindImageMemory2
  , PFN_vkBindImageMemory2
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
  ( VkDevice
  )


data VkBindBufferMemoryInfo

data VkBindImageMemoryInfo

type FN_vkBindBufferMemory2 = ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
type PFN_vkBindBufferMemory2 = FunPtr FN_vkBindBufferMemory2

type FN_vkBindImageMemory2 = ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
type PFN_vkBindImageMemory2 = FunPtr FN_vkBindImageMemory2
