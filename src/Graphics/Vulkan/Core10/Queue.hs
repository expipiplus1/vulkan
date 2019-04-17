{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , Fence
  , PipelineStageFlagBits
  , PipelineStageFlags
  , Queue(..)
  , Semaphore
  , withCStructSubmitInfo
  , fromCStructSubmitInfo
  , SubmitInfo(..)
  , deviceWaitIdle
  , getDeviceQueue
  , queueSubmit
  , queueWaitIdle
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Function
  ( (&)
  , on
  )
import Data.List
  ( minimum
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( deviceWaitIdle
  , getDeviceQueue
  , queueSubmit
  , queueWaitIdle
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_SUBMIT_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkSubmitInfo(..)
  , VkCommandBuffer
  , VkFence
  , VkQueue
  , VkSemaphore
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


data CommandBuffer = CommandBuffer
  { commandBufferHandle :: VkCommandBuffer
  , commandBufferCmds    :: DeviceCmds
  }
  deriving Show

instance Eq CommandBuffer where
  (==) = (==) `on` commandBufferHandle

instance Ord CommandBuffer where
  compare = compare `on` commandBufferHandle

-- No documentation found for TopLevel "Fence"
type Fence = VkFence
-- No documentation found for TopLevel "PipelineStageFlagBits"
type PipelineStageFlagBits = VkPipelineStageFlagBits
-- No documentation found for TopLevel "PipelineStageFlags"
type PipelineStageFlags = PipelineStageFlagBits
data Queue = Queue
  { queueHandle :: VkQueue
  , queueCmds    :: DeviceCmds
  }
  deriving Show

instance Eq Queue where
  (==) = (==) `on` queueHandle

instance Ord Queue where
  compare = compare `on` queueHandle

-- No documentation found for TopLevel "Semaphore"
type Semaphore = VkSemaphore
-- No documentation found for TopLevel "SubmitInfo"
data SubmitInfo = SubmitInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "SubmitInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "SubmitInfo" "pWaitSemaphores"
  vkPWaitSemaphores :: Vector Semaphore
  , -- No documentation found for Nested "SubmitInfo" "pWaitDstStageMask"
  vkPWaitDstStageMask :: Vector PipelineStageFlags
  -- Length valued member elided
  , -- No documentation found for Nested "SubmitInfo" "pCommandBuffers"
  vkPCommandBuffers :: Vector CommandBuffer
  -- Length valued member elided
  , -- No documentation found for Nested "SubmitInfo" "pSignalSemaphores"
  vkPSignalSemaphores :: Vector Semaphore
  }
  deriving (Show, Eq)
withCStructSubmitInfo :: SubmitInfo -> (VkSubmitInfo -> IO a) -> IO a
withCStructSubmitInfo from cont = withVec (&) (vkPSignalSemaphores (from :: SubmitInfo)) (\pSignalSemaphores -> withVec ((&) . commandBufferHandle) (vkPCommandBuffers (from :: SubmitInfo)) (\pCommandBuffers -> withVec (&) (vkPWaitDstStageMask (from :: SubmitInfo)) (\pWaitDstStageMask -> withVec (&) (vkPWaitSemaphores (from :: SubmitInfo)) (\pWaitSemaphores -> maybeWith withSomeVkStruct (vkPNext (from :: SubmitInfo)) (\pPNext -> cont (VkSubmitInfo VK_STRUCTURE_TYPE_SUBMIT_INFO pPNext (fromIntegral (minimum ([Data.Vector.length (vkPWaitSemaphores (from :: SubmitInfo)), Data.Vector.length (vkPWaitDstStageMask (from :: SubmitInfo))]))) pWaitSemaphores pWaitDstStageMask (fromIntegral (Data.Vector.length (vkPCommandBuffers (from :: SubmitInfo)))) pCommandBuffers (fromIntegral (Data.Vector.length (vkPSignalSemaphores (from :: SubmitInfo)))) pSignalSemaphores))))))
fromCStructSubmitInfo :: DeviceCmds -> VkSubmitInfo -> IO SubmitInfo
fromCStructSubmitInfo commandTable c = SubmitInfo <$> -- Univalued Member elided
                                                  maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSubmitInfo)))
                                                  -- Length valued member elided
                                                  <*> (Data.Vector.generateM (fromIntegral (vkWaitSemaphoreCount (c :: VkSubmitInfo))) (peekElemOff (vkPWaitSemaphores (c :: VkSubmitInfo))))
                                                  <*> (Data.Vector.generateM (fromIntegral (vkWaitSemaphoreCount (c :: VkSubmitInfo))) (peekElemOff (vkPWaitDstStageMask (c :: VkSubmitInfo))))
                                                  -- Length valued member elided
                                                  <*> (Data.Vector.generateM (fromIntegral (vkCommandBufferCount (c :: VkSubmitInfo))) ((\p i -> flip CommandBuffer commandTable <$> peekElemOff p i) (vkPCommandBuffers (c :: VkSubmitInfo))))
                                                  -- Length valued member elided
                                                  <*> (Data.Vector.generateM (fromIntegral (vkSignalSemaphoreCount (c :: VkSubmitInfo))) (peekElemOff (vkPSignalSemaphores (c :: VkSubmitInfo))))
instance Zero SubmitInfo where
  zero = SubmitInfo Nothing
                    Data.Vector.empty
                    Data.Vector.empty
                    Data.Vector.empty
                    Data.Vector.empty

-- | Wrapper for 'vkDeviceWaitIdle'
deviceWaitIdle :: Device ->  IO ()
deviceWaitIdle = \(Device device commandTable) -> Graphics.Vulkan.C.Dynamic.deviceWaitIdle commandTable device >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))

-- | Wrapper for 'vkGetDeviceQueue'
getDeviceQueue :: Device ->  Word32 ->  Word32 ->  IO (Queue)
getDeviceQueue = \(Device device commandTable) -> \queueFamilyIndex -> \queueIndex -> alloca (\pQueue -> Graphics.Vulkan.C.Dynamic.getDeviceQueue commandTable device queueFamilyIndex queueIndex pQueue *> (flip Queue commandTable <$> peek pQueue))

-- | Wrapper for 'vkQueueSubmit'
queueSubmit :: Queue ->  Vector SubmitInfo ->  Fence ->  IO ()
queueSubmit = \(Queue queue commandTable) -> \submits -> \fence -> withVec withCStructSubmitInfo submits (\pSubmits -> Graphics.Vulkan.C.Dynamic.queueSubmit commandTable queue (fromIntegral $ Data.Vector.length submits) pSubmits fence >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for 'vkQueueWaitIdle'
queueWaitIdle :: Queue ->  IO ()
queueWaitIdle = \(Queue queue commandTable) -> Graphics.Vulkan.C.Dynamic.queueWaitIdle commandTable queue >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))
