{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.Queue where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )

data VkQueue_T
type Queue = Ptr VkQueue_T

-- ** vkDeviceWaitIdle
foreign import ccall "vkDeviceWaitIdle" vkDeviceWaitIdle ::
  Device -> IO VkResult

-- ** vkQueueSubmit
foreign import ccall "vkQueueSubmit" vkQueueSubmit ::
  Queue -> Word32 -> Ptr VkSubmitInfo -> Fence -> IO VkResult

-- ** vkQueueWaitIdle
foreign import ccall "vkQueueWaitIdle" vkQueueWaitIdle ::
  Queue -> IO VkResult

-- ** vkGetDeviceQueue
foreign import ccall "vkGetDeviceQueue" vkGetDeviceQueue ::
  Device -> Word32 -> Word32 -> Ptr Queue -> IO ()


data VkSubmitInfo =
  VkSubmitInfo{ sType :: VkStructureType 
              , pNext :: Ptr Void 
              , waitSemaphoreCount :: Word32 
              , pWaitSemaphores :: Ptr Semaphore 
              , pWaitDstStageMask :: Ptr VkPipelineStageFlags 
              , commandBufferCount :: Word32 
              , pCommandBuffers :: Ptr CommandBuffer 
              , signalSemaphoreCount :: Word32 
              , pSignalSemaphores :: Ptr Semaphore 
              }
  deriving (Eq)

instance Storable VkSubmitInfo where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkSubmitInfo <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
                          <*> peek (ptr `plusPtr` 24)
                          <*> peek (ptr `plusPtr` 32)
                          <*> peek (ptr `plusPtr` 40)
                          <*> peek (ptr `plusPtr` 48)
                          <*> peek (ptr `plusPtr` 56)
                          <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 16) (waitSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 24) (pWaitSemaphores (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 32) (pWaitDstStageMask (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 40) (commandBufferCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 48) (pCommandBuffers (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 56) (signalSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 64) (pSignalSemaphores (poked :: VkSubmitInfo))


