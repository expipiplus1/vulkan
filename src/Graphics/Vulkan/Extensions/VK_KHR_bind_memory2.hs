{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_bind_memory2
  ( BindBufferMemoryInfoKHR
  , BindImageMemoryInfoKHR
  , bindBufferMemory2KHR
  , bindImageMemory2KHR
  , pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION
  , pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR
  , pattern VK_IMAGE_CREATE_ALIAS_BIT
  ) where

import Data.Vector
  ( Vector
  )


import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( BindBufferMemoryInfo(..)
  , BindImageMemoryInfo(..)
  , bindBufferMemory2
  , bindImageMemory2
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_ALIAS_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR
  , pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
  , pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR
  )


type BindBufferMemoryInfoKHR = BindBufferMemoryInfo
-- TODO: Pattern constructor alias)

type BindImageMemoryInfoKHR = BindImageMemoryInfo
-- TODO: Pattern constructor alias)

bindBufferMemory2KHR :: Device ->  Vector BindBufferMemoryInfo ->  IO ()
bindBufferMemory2KHR = bindBufferMemory2

bindImageMemory2KHR :: Device ->  Vector BindImageMemoryInfo ->  IO ()
bindImageMemory2KHR = bindImageMemory2
