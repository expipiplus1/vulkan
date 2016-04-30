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
import Graphics.Vulkan.Core( Result(..)
                           , DeviceSize(..)
                           )

-- ** getImageMemoryRequirements
foreign import ccall "vkGetImageMemoryRequirements" getImageMemoryRequirements ::
  Device -> Image -> Ptr MemoryRequirements -> IO ()


data MemoryRequirements =
  MemoryRequirements{ size :: DeviceSize 
                    , _alignment :: DeviceSize 
                    , memoryTypeBits :: Word32 
                    }
  deriving (Eq, Ord)

instance Storable MemoryRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = MemoryRequirements <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (size (poked :: MemoryRequirements))
                *> poke (ptr `plusPtr` 8) (_alignment (poked :: MemoryRequirements))
                *> poke (ptr `plusPtr` 16) (memoryTypeBits (poked :: MemoryRequirements))


-- ** getBufferMemoryRequirements
foreign import ccall "vkGetBufferMemoryRequirements" getBufferMemoryRequirements ::
  Device -> Buffer -> Ptr MemoryRequirements -> IO ()

-- ** bindBufferMemory
foreign import ccall "vkBindBufferMemory" bindBufferMemory ::
  Device -> Buffer -> DeviceMemory -> DeviceSize -> IO Result

-- ** bindImageMemory
foreign import ccall "vkBindImageMemory" bindImageMemory ::
  Device -> Image -> DeviceMemory -> DeviceSize -> IO Result

