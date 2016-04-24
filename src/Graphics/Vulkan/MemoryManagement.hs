{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.MemoryManagement where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.Buffer( VkBuffer(..)
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
import Graphics.Vulkan.Memory( VkDeviceMemory(..)
                             )
import Graphics.Vulkan.Image( VkImage(..)
                            )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkDeviceSize(..)
                           )

-- ** vkGetImageMemoryRequirements
foreign import ccall "vkGetImageMemoryRequirements" vkGetImageMemoryRequirements ::
  VkDevice -> VkImage -> Ptr VkMemoryRequirements -> IO ()


data VkMemoryRequirements =
  VkMemoryRequirements{ vkSize :: VkDeviceSize 
                      , vkAlignment :: VkDeviceSize 
                      , vkMemoryTypeBits :: Word32 
                      }
  deriving (Eq, Ord)

instance Storable VkMemoryRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryRequirements <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSize (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 8) (vkAlignment (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryRequirements))


-- ** vkGetBufferMemoryRequirements
foreign import ccall "vkGetBufferMemoryRequirements" vkGetBufferMemoryRequirements ::
  VkDevice -> VkBuffer -> Ptr VkMemoryRequirements -> IO ()

-- ** vkBindBufferMemory
foreign import ccall "vkBindBufferMemory" vkBindBufferMemory ::
  VkDevice ->
  VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO VkResult

-- ** vkBindImageMemory
foreign import ccall "vkBindImageMemory" vkBindImageMemory ::
  VkDevice ->
  VkImage -> VkDeviceMemory -> VkDeviceSize -> IO VkResult

