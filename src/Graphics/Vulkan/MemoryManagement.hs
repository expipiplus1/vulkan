{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.MemoryManagement where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.Buffer( Buffer(..)
                             )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
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
  Device -> Image -> Ptr VkMemoryRequirements -> IO ()


data VkMemoryRequirements =
  VkMemoryRequirements{ size :: VkDeviceSize 
                      , _alignment :: VkDeviceSize 
                      , memoryTypeBits :: Word32 
                      }
  deriving (Eq)

instance Storable VkMemoryRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryRequirements <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (size (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 8) (_alignment (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 16) (memoryTypeBits (poked :: VkMemoryRequirements))


-- ** vkGetBufferMemoryRequirements
foreign import ccall "vkGetBufferMemoryRequirements" vkGetBufferMemoryRequirements ::
  Device -> Buffer -> Ptr VkMemoryRequirements -> IO ()

-- ** vkBindBufferMemory
foreign import ccall "vkBindBufferMemory" vkBindBufferMemory ::
  Device -> Buffer -> DeviceMemory -> VkDeviceSize -> IO VkResult

-- ** vkBindImageMemory
foreign import ccall "vkBindImageMemory" vkBindImageMemory ::
  Device -> Image -> DeviceMemory -> VkDeviceSize -> IO VkResult

