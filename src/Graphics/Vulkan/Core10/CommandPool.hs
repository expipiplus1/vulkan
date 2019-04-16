{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  , CommandPoolCreateFlagBits
  , CommandPoolCreateFlags
  , withCStructCommandPoolCreateInfo
  , fromCStructCommandPoolCreateInfo
  , CommandPoolCreateInfo(..)
  , CommandPoolResetFlagBits
  , CommandPoolResetFlags
  , createCommandPool
  , destroyCommandPool
  , resetCommandPool
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
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
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createCommandPool
  , destroyCommandPool
  , resetCommandPool
  )


import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateFlagBits(..)
  , VkCommandPoolCreateInfo(..)
  , VkCommandPoolResetFlagBits(..)
  , VkCommandPool
  )
import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "CommandPool"
type CommandPool = VkCommandPool
-- No documentation found for TopLevel "CommandPoolCreateFlagBits"
type CommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits
-- No documentation found for TopLevel "CommandPoolCreateFlags"
type CommandPoolCreateFlags = CommandPoolCreateFlagBits
-- No documentation found for TopLevel "CommandPoolCreateInfo"
data CommandPoolCreateInfo = CommandPoolCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "CommandPoolCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandPoolCreateInfo" "flags"
  vkFlags :: CommandPoolCreateFlags
  , -- No documentation found for Nested "CommandPoolCreateInfo" "queueFamilyIndex"
  vkQueueFamilyIndex :: Word32
  }
  deriving (Show, Eq)
withCStructCommandPoolCreateInfo :: CommandPoolCreateInfo -> (VkCommandPoolCreateInfo -> IO a) -> IO a
withCStructCommandPoolCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: CommandPoolCreateInfo)) (\pPNext -> cont (VkCommandPoolCreateInfo VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO pPNext (vkFlags (from :: CommandPoolCreateInfo)) (vkQueueFamilyIndex (from :: CommandPoolCreateInfo))))
fromCStructCommandPoolCreateInfo :: VkCommandPoolCreateInfo -> IO CommandPoolCreateInfo
fromCStructCommandPoolCreateInfo c = CommandPoolCreateInfo <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandPoolCreateInfo)))
                                                           <*> pure (vkFlags (c :: VkCommandPoolCreateInfo))
                                                           <*> pure (vkQueueFamilyIndex (c :: VkCommandPoolCreateInfo))
-- No documentation found for TopLevel "CommandPoolResetFlagBits"
type CommandPoolResetFlagBits = VkCommandPoolResetFlagBits
-- No documentation found for TopLevel "CommandPoolResetFlags"
type CommandPoolResetFlags = CommandPoolResetFlagBits

-- | Wrapper for vkCreateCommandPool
createCommandPool :: Device ->  CommandPoolCreateInfo ->  Maybe AllocationCallbacks ->  IO (CommandPool)
createCommandPool = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pCommandPool -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructCommandPoolCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createCommandPool commandTable device pCreateInfo pAllocator pCommandPool >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pCommandPool)))))

-- | Wrapper for vkDestroyCommandPool
destroyCommandPool :: Device ->  CommandPool ->  Maybe AllocationCallbacks ->  IO ()
destroyCommandPool = \(Device device commandTable) -> \commandPool -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyCommandPool commandTable device commandPool pAllocator *> (pure ()))

-- | Wrapper for vkResetCommandPool
resetCommandPool :: Device ->  CommandPool ->  CommandPoolResetFlags ->  IO ()
resetCommandPool = \(Device device commandTable) -> \commandPool -> \flags -> Graphics.Vulkan.C.Dynamic.resetCommandPool commandTable device commandPool flags >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))
