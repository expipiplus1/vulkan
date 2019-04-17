{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Buffer
  ( BufferCreateFlagBits
  , BufferCreateFlags
  , withCStructBufferCreateInfo
  , fromCStructBufferCreateInfo
  , BufferCreateInfo(..)
  , BufferUsageFlagBits
  , BufferUsageFlags
  , SharingMode
  , createBuffer
  , destroyBuffer
  , withBuffer
  ) where

import Control.Exception
  ( bracket
  , throwIO
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
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createBuffer
  , destroyBuffer
  )


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits(..)
  , VkBufferCreateInfo(..)
  , VkBufferUsageFlagBits(..)
  , VkSharingMode(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
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


-- No documentation found for TopLevel "BufferCreateFlagBits"
type BufferCreateFlagBits = VkBufferCreateFlagBits
-- No documentation found for TopLevel "BufferCreateFlags"
type BufferCreateFlags = BufferCreateFlagBits
-- No documentation found for TopLevel "BufferCreateInfo"
data BufferCreateInfo = BufferCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "BufferCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferCreateInfo" "flags"
  vkFlags :: BufferCreateFlags
  , -- No documentation found for Nested "BufferCreateInfo" "size"
  vkSize :: DeviceSize
  , -- No documentation found for Nested "BufferCreateInfo" "usage"
  vkUsage :: BufferUsageFlags
  , -- No documentation found for Nested "BufferCreateInfo" "sharingMode"
  vkSharingMode :: SharingMode
  -- Length valued member elided
  , -- No documentation found for Nested "BufferCreateInfo" "pQueueFamilyIndices"
  vkPQueueFamilyIndices :: Vector Word32
  }
  deriving (Show, Eq)
withCStructBufferCreateInfo :: BufferCreateInfo -> (VkBufferCreateInfo -> IO a) -> IO a
withCStructBufferCreateInfo from cont = withVec (&) (vkPQueueFamilyIndices (from :: BufferCreateInfo)) (\pQueueFamilyIndices -> maybeWith withSomeVkStruct (vkPNext (from :: BufferCreateInfo)) (\pPNext -> cont (VkBufferCreateInfo VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO pPNext (vkFlags (from :: BufferCreateInfo)) (vkSize (from :: BufferCreateInfo)) (vkUsage (from :: BufferCreateInfo)) (vkSharingMode (from :: BufferCreateInfo)) (fromIntegral (Data.Vector.length (vkPQueueFamilyIndices (from :: BufferCreateInfo)))) pQueueFamilyIndices)))
fromCStructBufferCreateInfo :: VkBufferCreateInfo -> IO BufferCreateInfo
fromCStructBufferCreateInfo c = BufferCreateInfo <$> -- Univalued Member elided
                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferCreateInfo)))
                                                 <*> pure (vkFlags (c :: VkBufferCreateInfo))
                                                 <*> pure (vkSize (c :: VkBufferCreateInfo))
                                                 <*> pure (vkUsage (c :: VkBufferCreateInfo))
                                                 <*> pure (vkSharingMode (c :: VkBufferCreateInfo))
                                                 -- Length valued member elided
                                                 <*> (Data.Vector.generateM (fromIntegral (vkQueueFamilyIndexCount (c :: VkBufferCreateInfo))) (peekElemOff (vkPQueueFamilyIndices (c :: VkBufferCreateInfo))))
instance Zero BufferCreateInfo where
  zero = BufferCreateInfo Nothing
                          zero
                          zero
                          zero
                          zero
                          Data.Vector.empty
-- No documentation found for TopLevel "BufferUsageFlagBits"
type BufferUsageFlagBits = VkBufferUsageFlagBits
-- No documentation found for TopLevel "BufferUsageFlags"
type BufferUsageFlags = BufferUsageFlagBits
-- No documentation found for TopLevel "SharingMode"
type SharingMode = VkSharingMode

-- | Wrapper for 'vkCreateBuffer'
createBuffer :: Device ->  BufferCreateInfo ->  Maybe AllocationCallbacks ->  IO (Buffer)
createBuffer = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pBuffer -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructBufferCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createBuffer commandTable device pCreateInfo pAllocator pBuffer >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pBuffer)))))

-- | Wrapper for 'vkDestroyBuffer'
destroyBuffer :: Device ->  Buffer ->  Maybe AllocationCallbacks ->  IO ()
destroyBuffer = \(Device device commandTable) -> \buffer -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyBuffer commandTable device buffer pAllocator *> (pure ()))
-- | Wrapper for 'createBuffer' and 'destroyBuffer' using 'bracket'
withBuffer
  :: Device -> BufferCreateInfo -> Maybe (AllocationCallbacks) -> (Buffer -> IO a) -> IO a
withBuffer device bufferCreateInfo allocationCallbacks = bracket
  (createBuffer device bufferCreateInfo allocationCallbacks)
  (\o -> destroyBuffer device o allocationCallbacks)
