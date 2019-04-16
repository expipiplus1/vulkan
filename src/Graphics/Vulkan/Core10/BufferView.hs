{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.BufferView
  ( BufferView
  , BufferViewCreateFlags
  , withCStructBufferViewCreateInfo
  , fromCStructBufferViewCreateInfo
  , BufferViewCreateInfo(..)
  , createBufferView
  , destroyBufferView
  , withBufferView
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
  ( createBufferView
  , destroyBufferView
  )


import Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferViewCreateFlags(..)
  , VkBufferViewCreateInfo(..)
  , VkBufferView
  )
import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.Core10.Core
  ( Format
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
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "BufferView"
type BufferView = VkBufferView
-- No documentation found for TopLevel "BufferViewCreateFlags"
type BufferViewCreateFlags = VkBufferViewCreateFlags
-- No documentation found for TopLevel "BufferViewCreateInfo"
data BufferViewCreateInfo = BufferViewCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "BufferViewCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferViewCreateInfo" "flags"
  vkFlags :: BufferViewCreateFlags
  , -- No documentation found for Nested "BufferViewCreateInfo" "buffer"
  vkBuffer :: Buffer
  , -- No documentation found for Nested "BufferViewCreateInfo" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "BufferViewCreateInfo" "offset"
  vkOffset :: DeviceSize
  , -- No documentation found for Nested "BufferViewCreateInfo" "range"
  vkRange :: DeviceSize
  }
  deriving (Show, Eq)
withCStructBufferViewCreateInfo :: BufferViewCreateInfo -> (VkBufferViewCreateInfo -> IO a) -> IO a
withCStructBufferViewCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: BufferViewCreateInfo)) (\pPNext -> cont (VkBufferViewCreateInfo VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO pPNext (vkFlags (from :: BufferViewCreateInfo)) (vkBuffer (from :: BufferViewCreateInfo)) (vkFormat (from :: BufferViewCreateInfo)) (vkOffset (from :: BufferViewCreateInfo)) (vkRange (from :: BufferViewCreateInfo))))
fromCStructBufferViewCreateInfo :: VkBufferViewCreateInfo -> IO BufferViewCreateInfo
fromCStructBufferViewCreateInfo c = BufferViewCreateInfo <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferViewCreateInfo)))
                                                         <*> pure (vkFlags (c :: VkBufferViewCreateInfo))
                                                         <*> pure (vkBuffer (c :: VkBufferViewCreateInfo))
                                                         <*> pure (vkFormat (c :: VkBufferViewCreateInfo))
                                                         <*> pure (vkOffset (c :: VkBufferViewCreateInfo))
                                                         <*> pure (vkRange (c :: VkBufferViewCreateInfo))

-- | Wrapper for 'vkCreateBufferView'
createBufferView :: Device ->  BufferViewCreateInfo ->  Maybe AllocationCallbacks ->  IO ( BufferView )
createBufferView = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pView -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructBufferViewCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createBufferView commandTable device pCreateInfo pAllocator pView >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pView)))))

-- | Wrapper for 'vkDestroyBufferView'
destroyBufferView :: Device ->  BufferView ->  Maybe AllocationCallbacks ->  IO ()
destroyBufferView = \(Device device commandTable) -> \bufferView -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyBufferView commandTable device bufferView pAllocator *> (pure ()))
withBufferView :: CreateInfo -> Maybe AllocationCallbacks -> (t -> IO a) -> IO a
withBufferView createInfo allocationCallbacks =
  bracket
    (vkCreateBufferView createInfo allocationCallbacks)
    (`vkDestroyBufferView` allocationCallbacks)
