{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.QueueSemaphore
  ( SemaphoreCreateFlags
  , withCStructSemaphoreCreateInfo
  , fromCStructSemaphoreCreateInfo
  , SemaphoreCreateInfo(..)
  , createSemaphore
  , destroySemaphore
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createSemaphore
  , destroySemaphore
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateFlags(..)
  , VkSemaphoreCreateInfo(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Queue
  ( Semaphore
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "SemaphoreCreateFlags"
type SemaphoreCreateFlags = VkSemaphoreCreateFlags
-- No documentation found for TopLevel "SemaphoreCreateInfo"
data SemaphoreCreateInfo = SemaphoreCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "SemaphoreCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SemaphoreCreateInfo" "flags"
  vkFlags :: SemaphoreCreateFlags
  }
  deriving (Show, Eq)
withCStructSemaphoreCreateInfo :: SemaphoreCreateInfo -> (VkSemaphoreCreateInfo -> IO a) -> IO a
withCStructSemaphoreCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: SemaphoreCreateInfo)) (\pPNext -> cont (VkSemaphoreCreateInfo VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO pPNext (vkFlags (from :: SemaphoreCreateInfo))))
fromCStructSemaphoreCreateInfo :: VkSemaphoreCreateInfo -> IO SemaphoreCreateInfo
fromCStructSemaphoreCreateInfo c = SemaphoreCreateInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSemaphoreCreateInfo)))
                                                       <*> pure (vkFlags (c :: VkSemaphoreCreateInfo))

-- | Wrapper for vkCreateSemaphore
createSemaphore :: Device ->  SemaphoreCreateInfo ->  Maybe AllocationCallbacks ->  IO (Semaphore)
createSemaphore = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pSemaphore -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructSemaphoreCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createSemaphore commandTable device pCreateInfo pAllocator pSemaphore >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSemaphore)))))

-- | Wrapper for vkDestroySemaphore
destroySemaphore :: Device ->  Semaphore ->  Maybe AllocationCallbacks ->  IO ()
destroySemaphore = \(Device device commandTable) -> \semaphore -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroySemaphore commandTable device semaphore pAllocator *> (pure ()))
