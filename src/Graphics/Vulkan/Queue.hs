{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.Queue where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.Pipeline( VkPipelineStageFlagBits(..)
                               , VkPipelineStageFlags(..)
                               )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
                                    )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( VkFence(..)
                            )
import Data.Void( Void
                )
import Graphics.Vulkan.QueueSemaphore( VkSemaphore(..)
                                     )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )

data VkQueue_T
type VkQueue = Ptr VkQueue_T

-- ** vkDeviceWaitIdle
foreign import ccall "vkDeviceWaitIdle" vkDeviceWaitIdle ::
  VkDevice -> IO VkResult

-- ** vkQueueSubmit
foreign import ccall "vkQueueSubmit" vkQueueSubmit ::
  VkQueue -> Word32 -> Ptr VkSubmitInfo -> VkFence -> IO VkResult

-- ** vkQueueWaitIdle
foreign import ccall "vkQueueWaitIdle" vkQueueWaitIdle ::
  VkQueue -> IO VkResult

-- ** vkGetDeviceQueue
foreign import ccall "vkGetDeviceQueue" vkGetDeviceQueue ::
  VkDevice -> Word32 -> Word32 -> Ptr VkQueue -> IO ()


data VkSubmitInfo =
  VkSubmitInfo{ vkSType :: VkStructureType 
              , vkPNext :: Ptr Void 
              , vkWaitSemaphoreCount :: Word32 
              , vkPWaitSemaphores :: Ptr VkSemaphore 
              , vkPWaitDstStageMask :: Ptr VkPipelineStageFlags 
              , vkCommandBufferCount :: Word32 
              , vkPCommandBuffers :: Ptr VkCommandBuffer 
              , vkSignalSemaphoreCount :: Word32 
              , vkPSignalSemaphores :: Ptr VkSemaphore 
              }
  deriving (Eq, Ord)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 32) (vkPWaitDstStageMask (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 40) (vkCommandBufferCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 48) (vkPCommandBuffers (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 56) (vkSignalSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 64) (vkPSignalSemaphores (poked :: VkSubmitInfo))


