{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.Queue where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.Pipeline( PipelineStageFlags(..)
                               )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.CommandBuffer( CommandBuffer(..)
                                    )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Fence( Fence(..)
                            )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.QueueSemaphore( Semaphore(..)
                                     )
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           )

data VkQueue_T
type Queue = Ptr VkQueue_T

-- ** deviceWaitIdle
foreign import ccall "vkDeviceWaitIdle" deviceWaitIdle ::
  Device -> IO Result

-- ** queueSubmit
foreign import ccall "vkQueueSubmit" queueSubmit ::
  Queue -> Word32 -> Ptr SubmitInfo -> Fence -> IO Result

-- ** queueWaitIdle
foreign import ccall "vkQueueWaitIdle" queueWaitIdle ::
  Queue -> IO Result

-- ** getDeviceQueue
foreign import ccall "vkGetDeviceQueue" getDeviceQueue ::
  Device -> Word32 -> Word32 -> Ptr Queue -> IO ()


data SubmitInfo =
  SubmitInfo{ sType :: StructureType 
            , pNext :: Ptr Void 
            , waitSemaphoreCount :: Word32 
            , pWaitSemaphores :: Ptr Semaphore 
            , pWaitDstStageMask :: Ptr PipelineStageFlags 
            , commandBufferCount :: Word32 
            , pCommandBuffers :: Ptr CommandBuffer 
            , signalSemaphoreCount :: Word32 
            , pSignalSemaphores :: Ptr Semaphore 
            }
  deriving (Eq, Ord)

instance Storable SubmitInfo where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = SubmitInfo <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 8)
                        <*> peek (ptr `plusPtr` 16)
                        <*> peek (ptr `plusPtr` 24)
                        <*> peek (ptr `plusPtr` 32)
                        <*> peek (ptr `plusPtr` 40)
                        <*> peek (ptr `plusPtr` 48)
                        <*> peek (ptr `plusPtr` 56)
                        <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: SubmitInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: SubmitInfo))
                *> poke (ptr `plusPtr` 16) (waitSemaphoreCount (poked :: SubmitInfo))
                *> poke (ptr `plusPtr` 24) (pWaitSemaphores (poked :: SubmitInfo))
                *> poke (ptr `plusPtr` 32) (pWaitDstStageMask (poked :: SubmitInfo))
                *> poke (ptr `plusPtr` 40) (commandBufferCount (poked :: SubmitInfo))
                *> poke (ptr `plusPtr` 48) (pCommandBuffers (poked :: SubmitInfo))
                *> poke (ptr `plusPtr` 56) (signalSemaphoreCount (poked :: SubmitInfo))
                *> poke (ptr `plusPtr` 64) (pSignalSemaphores (poked :: SubmitInfo))


