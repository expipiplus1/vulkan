{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  BindBufferMemoryInfo(..)
  , 
  BindImageMemoryInfo(..)
#endif
  , bindBufferMemory2
  , bindImageMemory2
  , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern IMAGE_CREATE_ALIAS_BIT
  ) where

import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( vkBindBufferMemory2
  , vkBindImageMemory2
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
#endif
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_CREATE_ALIAS_BIT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBindBufferMemoryInfo"
data BindBufferMemoryInfo = BindBufferMemoryInfo
  { -- No documentation found for Nested "BindBufferMemoryInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindBufferMemoryInfo" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "BindBufferMemoryInfo" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "BindBufferMemoryInfo" "memoryOffset"
  memoryOffset :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero BindBufferMemoryInfo where
  zero = BindBufferMemoryInfo Nothing
                              zero
                              zero
                              zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBindImageMemoryInfo"
data BindImageMemoryInfo = BindImageMemoryInfo
  { -- No documentation found for Nested "BindImageMemoryInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImageMemoryInfo" "image"
  image :: Image
  , -- No documentation found for Nested "BindImageMemoryInfo" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "BindImageMemoryInfo" "memoryOffset"
  memoryOffset :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero BindImageMemoryInfo where
  zero = BindImageMemoryInfo Nothing
                             zero
                             zero
                             zero

#endif


-- No documentation found for TopLevel "vkBindBufferMemory2"
bindBufferMemory2 :: Device ->  Vector BindBufferMemoryInfo ->  IO ()
bindBufferMemory2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkBindImageMemory2"
bindImageMemory2 :: Device ->  Vector BindImageMemoryInfo ->  IO ()
bindImageMemory2 = undefined {- {wrapped (pretty cName) :: Doc ()} -}
