{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Fence
  ( FenceCreateFlagBits
  , FenceCreateFlags
  , withCStructFenceCreateInfo
  , fromCStructFenceCreateInfo
  , FenceCreateInfo(..)
  , createFence
  , destroyFence
  , getFenceStatus
  , resetFences
  , waitForFences
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Data.Word
  ( Word64
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
  ( createFence
  , destroyFence
  , getFenceStatus
  , resetFences
  , waitForFences
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateFlagBits(..)
  , VkFenceCreateInfo(..)
  )
import Graphics.Vulkan.Core10.Core
  ( boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Queue
  ( Fence
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


-- No documentation found for TopLevel "FenceCreateFlagBits"
type FenceCreateFlagBits = VkFenceCreateFlagBits
-- No documentation found for TopLevel "FenceCreateFlags"
type FenceCreateFlags = FenceCreateFlagBits
-- No documentation found for TopLevel "FenceCreateInfo"
data FenceCreateInfo = FenceCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "FenceCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FenceCreateInfo" "flags"
  vkFlags :: FenceCreateFlags
  }
  deriving (Show, Eq)
withCStructFenceCreateInfo :: FenceCreateInfo -> (VkFenceCreateInfo -> IO a) -> IO a
withCStructFenceCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: FenceCreateInfo)) (\pPNext -> cont (VkFenceCreateInfo VK_STRUCTURE_TYPE_FENCE_CREATE_INFO pPNext (vkFlags (from :: FenceCreateInfo))))
fromCStructFenceCreateInfo :: VkFenceCreateInfo -> IO FenceCreateInfo
fromCStructFenceCreateInfo c = FenceCreateInfo <$> -- Univalued Member elided
                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFenceCreateInfo)))
                                               <*> pure (vkFlags (c :: VkFenceCreateInfo))

-- | Wrapper for vkCreateFence
createFence :: Device ->  FenceCreateInfo ->  Maybe AllocationCallbacks ->  IO (Fence)
createFence = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pFence -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructFenceCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createFence commandTable device pCreateInfo pAllocator pFence >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pFence)))))

-- | Wrapper for vkDestroyFence
destroyFence :: Device ->  Fence ->  Maybe AllocationCallbacks ->  IO ()
destroyFence = \(Device device commandTable) -> \fence -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyFence commandTable device fence pAllocator *> (pure ()))

-- | Wrapper for vkGetFenceStatus
getFenceStatus :: Device ->  Fence ->  IO (VkResult)
getFenceStatus = \(Device device commandTable) -> \fence -> Graphics.Vulkan.C.Dynamic.getFenceStatus commandTable device fence >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure r))

-- | Wrapper for vkResetFences
resetFences :: Device ->  Vector Fence ->  IO ()
resetFences = \(Device device commandTable) -> \fences -> withVec (&) fences (\pFences -> Graphics.Vulkan.C.Dynamic.resetFences commandTable device (fromIntegral $ Data.Vector.length fences) pFences >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for vkWaitForFences
waitForFences :: Device ->  Vector Fence ->  Bool ->  Word64 ->  IO (VkResult)
waitForFences = \(Device device commandTable) -> \fences -> \waitAll -> \timeout -> withVec (&) fences (\pFences -> Graphics.Vulkan.C.Dynamic.waitForFences commandTable device (fromIntegral $ Data.Vector.length fences) pFences (boolToBool32 waitAll) timeout >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure r)))
