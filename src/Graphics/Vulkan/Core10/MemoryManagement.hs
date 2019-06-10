{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  , MemoryRequirements(..)
  , bindBufferMemory
  , bindImageMemory
#if defined(VK_USE_PLATFORM_GGP)
  , getBufferMemoryRequirements
  , getImageMemoryRequirements
#endif
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.Word
  ( Word32
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  , vkBindBufferMemory
  , vkBindImageMemory
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( vkGetBufferMemoryRequirements
  , vkGetImageMemoryRequirements
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif


-- No documentation found for TopLevel "Buffer"
type Buffer = VkBuffer

-- No documentation found for TopLevel "Image"
type Image = VkImage


-- No documentation found for TopLevel "VkMemoryRequirements"
data MemoryRequirements = MemoryRequirements
  { -- No documentation found for Nested "MemoryRequirements" "size"
  size :: DeviceSize
  , -- No documentation found for Nested "MemoryRequirements" "alignment"
  alignment :: DeviceSize
  , -- No documentation found for Nested "MemoryRequirements" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

instance Zero MemoryRequirements where
  zero = MemoryRequirements zero
                            zero
                            zero



-- No documentation found for TopLevel "vkBindBufferMemory"
bindBufferMemory :: Device ->  Buffer ->  DeviceMemory ->  DeviceSize ->  IO ()
bindBufferMemory = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkBindImageMemory"
bindImageMemory :: Device ->  Image ->  DeviceMemory ->  DeviceSize ->  IO ()
bindImageMemory = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetBufferMemoryRequirements"
getBufferMemoryRequirements :: Device ->  Buffer ->  IO (MemoryRequirements)
getBufferMemoryRequirements = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetImageMemoryRequirements"
getImageMemoryRequirements :: Device ->  Image ->  IO (MemoryRequirements)
getImageMemoryRequirements = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif
