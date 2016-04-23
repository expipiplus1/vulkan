{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.MemoryManagement where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.Buffer( Buffer(..)
                             )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Memory( DeviceMemory(..)
                             )
import Graphics.Vulkan.Image( Image(..)
                            )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkDeviceSize(..)
                           )

-- ** vkGetImageMemoryRequirements
foreign import ccall "vkGetImageMemoryRequirements" vkGetImageMemoryRequirements ::
  Device -> Image -> Ptr MemoryRequirements -> IO ()


data MemoryRequirements =
  MemoryRequirements{ size :: VkDeviceSize 
                    , _alignment :: VkDeviceSize 
                    , memoryTypeBits :: Word32 
                    }
  deriving (Eq)

instance Storable MemoryRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = MemoryRequirements <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (size (poked :: MemoryRequirements))
                *> poke (ptr `plusPtr` 8) (_alignment (poked :: MemoryRequirements))
                *> poke (ptr `plusPtr` 16) (memoryTypeBits (poked :: MemoryRequirements))


-- ** vkGetBufferMemoryRequirements
foreign import ccall "vkGetBufferMemoryRequirements" vkGetBufferMemoryRequirements ::
  Device -> Buffer -> Ptr MemoryRequirements -> IO ()

-- ** vkBindBufferMemory
foreign import ccall "vkBindBufferMemory" vkBindBufferMemory ::
  Device -> Buffer -> DeviceMemory -> VkDeviceSize -> IO VkResult

-- ** vkBindImageMemory
foreign import ccall "vkBindImageMemory" vkBindImageMemory ::
  Device -> Image -> DeviceMemory -> VkDeviceSize -> IO VkResult

