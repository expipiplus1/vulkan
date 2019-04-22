{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_bind_memory2
  ( BindBufferMemoryInfoKHR
  , BindImageMemoryInfoKHR
  , bindBufferMemory2KHR
  , bindImageMemory2KHR
  , pattern IMAGE_CREATE_ALIAS_BIT_KHR
  , pattern KHR_BIND_MEMORY_2_EXTENSION_NAME
  , pattern KHR_BIND_MEMORY_2_SPEC_VERSION
  , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR
  , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern IMAGE_CREATE_ALIAS_BIT
  ) where

import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_bind_memory2
  ( pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
  , pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , pattern IMAGE_CREATE_ALIAS_BIT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( BindBufferMemoryInfo(..)
  , BindImageMemoryInfo(..)
  , bindBufferMemory2
  , bindImageMemory2
  )


type BindBufferMemoryInfoKHR = BindBufferMemoryInfo
-- TODO: Pattern constructor alias)

type BindImageMemoryInfoKHR = BindImageMemoryInfo
-- TODO: Pattern constructor alias)

bindBufferMemory2KHR :: Device ->  Vector BindBufferMemoryInfo ->  IO ()
bindBufferMemory2KHR = bindBufferMemory2

bindImageMemory2KHR :: Device ->  Vector BindImageMemoryInfo ->  IO ()
bindImageMemory2KHR = bindImageMemory2

-- No documentation found for TopLevel "IMAGE_CREATE_ALIAS_BIT_KHR"
pattern IMAGE_CREATE_ALIAS_BIT_KHR :: VkImageCreateFlagBits
pattern IMAGE_CREATE_ALIAS_BIT_KHR = IMAGE_CREATE_ALIAS_BIT

-- No documentation found for TopLevel "VK_KHR_BIND_MEMORY_2_EXTENSION_NAME"
pattern KHR_BIND_MEMORY_2_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_BIND_MEMORY_2_EXTENSION_NAME = VK_KHR_BIND_MEMORY_2_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_BIND_MEMORY_2_SPEC_VERSION"
pattern KHR_BIND_MEMORY_2_SPEC_VERSION :: Integral a => a
pattern KHR_BIND_MEMORY_2_SPEC_VERSION = VK_KHR_BIND_MEMORY_2_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR = STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR = STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
