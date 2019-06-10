{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.BufferView
  ( BufferView
  , BufferViewCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , BufferViewCreateInfo(..)
#endif
  , createBufferView
  , destroyBufferView
  , withBufferView
  ) where

import Control.Exception
  ( bracket
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferViewCreateFlags(..)
  , VkBufferView
  , vkCreateBufferView
  , vkDestroyBufferView
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "BufferView"
type BufferView = VkBufferView

-- No documentation found for TopLevel "BufferViewCreateFlags"
type BufferViewCreateFlags = VkBufferViewCreateFlags


-- No complete pragma for BufferViewCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBufferViewCreateInfo"
data BufferViewCreateInfo = BufferViewCreateInfo
  { -- No documentation found for Nested "BufferViewCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferViewCreateInfo" "flags"
  flags :: BufferViewCreateFlags
  , -- No documentation found for Nested "BufferViewCreateInfo" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "BufferViewCreateInfo" "format"
  format :: Format
  , -- No documentation found for Nested "BufferViewCreateInfo" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "BufferViewCreateInfo" "range"
  range :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero BufferViewCreateInfo where
  zero = BufferViewCreateInfo Nothing
                              zero
                              zero
                              zero
                              zero
                              zero

#endif


-- No documentation found for TopLevel "vkCreateBufferView"
createBufferView :: Device ->  BufferViewCreateInfo ->  Maybe AllocationCallbacks ->  IO (BufferView)
createBufferView = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyBufferView"
destroyBufferView :: Device ->  BufferView ->  Maybe AllocationCallbacks ->  IO ()
destroyBufferView = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createBufferView' and 'destroyBufferView' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withBufferView
  :: Device -> BufferViewCreateInfo -> Maybe AllocationCallbacks -> (BufferView -> IO a) -> IO a
withBufferView device bufferViewCreateInfo allocationCallbacks = bracket
  (createBufferView device bufferViewCreateInfo allocationCallbacks)
  (\o -> destroyBufferView device o allocationCallbacks)
