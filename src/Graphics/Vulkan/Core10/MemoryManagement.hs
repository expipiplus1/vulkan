{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  , withCStructMemoryRequirements
  , fromCStructMemoryRequirements
  , MemoryRequirements(..)
  , bindBufferMemory
  , bindImageMemory
  , getBufferMemoryRequirements
  , getImageMemoryRequirements
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( bindBufferMemory
  , bindImageMemory
  , getBufferMemoryRequirements
  , getImageMemoryRequirements
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  , VkBuffer
  , VkImage
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )


-- No documentation found for TopLevel "Buffer"
type Buffer = VkBuffer
-- No documentation found for TopLevel "Image"
type Image = VkImage
-- No documentation found for TopLevel "MemoryRequirements"
data MemoryRequirements = MemoryRequirements
  { -- No documentation found for Nested "MemoryRequirements" "size"
  vkSize :: DeviceSize
  , -- No documentation found for Nested "MemoryRequirements" "alignment"
  vkAlignment :: DeviceSize
  , -- No documentation found for Nested "MemoryRequirements" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Show, Eq)
withCStructMemoryRequirements :: MemoryRequirements -> (VkMemoryRequirements -> IO a) -> IO a
withCStructMemoryRequirements from cont = cont (VkMemoryRequirements (vkSize (from :: MemoryRequirements)) (vkAlignment (from :: MemoryRequirements)) (vkMemoryTypeBits (from :: MemoryRequirements)))
fromCStructMemoryRequirements :: VkMemoryRequirements -> IO MemoryRequirements
fromCStructMemoryRequirements c = MemoryRequirements <$> pure (vkSize (c :: VkMemoryRequirements))
                                                     <*> pure (vkAlignment (c :: VkMemoryRequirements))
                                                     <*> pure (vkMemoryTypeBits (c :: VkMemoryRequirements))
instance Zero MemoryRequirements where
  zero = MemoryRequirements zero
                            zero
                            zero

-- | Wrapper for 'vkBindBufferMemory'
bindBufferMemory :: Device ->  Buffer ->  DeviceMemory ->  DeviceSize ->  IO ()
bindBufferMemory = \(Device device commandTable) -> \buffer -> \memory -> \memoryOffset -> Graphics.Vulkan.C.Dynamic.bindBufferMemory commandTable device buffer memory memoryOffset >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))

-- | Wrapper for 'vkBindImageMemory'
bindImageMemory :: Device ->  Image ->  DeviceMemory ->  DeviceSize ->  IO ()
bindImageMemory = \(Device device commandTable) -> \image -> \memory -> \memoryOffset -> Graphics.Vulkan.C.Dynamic.bindImageMemory commandTable device image memory memoryOffset >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))

-- | Wrapper for 'vkGetBufferMemoryRequirements'
getBufferMemoryRequirements :: Device ->  Buffer ->  IO (MemoryRequirements)
getBufferMemoryRequirements = \(Device device commandTable) -> \buffer -> alloca (\pMemoryRequirements -> Graphics.Vulkan.C.Dynamic.getBufferMemoryRequirements commandTable device buffer pMemoryRequirements *> ((fromCStructMemoryRequirements <=< peek) pMemoryRequirements))

-- | Wrapper for 'vkGetImageMemoryRequirements'
getImageMemoryRequirements :: Device ->  Image ->  IO (MemoryRequirements)
getImageMemoryRequirements = \(Device device commandTable) -> \image -> alloca (\pMemoryRequirements -> Graphics.Vulkan.C.Dynamic.getImageMemoryRequirements commandTable device image pMemoryRequirements *> ((fromCStructMemoryRequirements <=< peek) pMemoryRequirements))
