{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.QueueSemaphore
  ( SemaphoreCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , SemaphoreCreateInfo(..)
#endif
  , createSemaphore
  , destroySemaphore
  , withSemaphore
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



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateFlags(..)
  , vkCreateSemaphore
  , vkDestroySemaphore
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( Semaphore
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "SemaphoreCreateFlags"
type SemaphoreCreateFlags = VkSemaphoreCreateFlags


-- No complete pragma for SemaphoreCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSemaphoreCreateInfo"
data SemaphoreCreateInfo = SemaphoreCreateInfo
  { -- No documentation found for Nested "SemaphoreCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SemaphoreCreateInfo" "flags"
  flags :: SemaphoreCreateFlags
  }
  deriving (Show, Eq)

instance Zero SemaphoreCreateInfo where
  zero = SemaphoreCreateInfo Nothing
                             zero

#endif


-- No documentation found for TopLevel "vkCreateSemaphore"
createSemaphore :: Device ->  SemaphoreCreateInfo ->  Maybe AllocationCallbacks ->  IO (Semaphore)
createSemaphore = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroySemaphore"
destroySemaphore :: Device ->  Semaphore ->  Maybe AllocationCallbacks ->  IO ()
destroySemaphore = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createSemaphore' and 'destroySemaphore' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withSemaphore
  :: Device -> SemaphoreCreateInfo -> Maybe AllocationCallbacks -> (Semaphore -> IO a) -> IO a
withSemaphore device semaphoreCreateInfo allocationCallbacks = bracket
  (createSemaphore device semaphoreCreateInfo allocationCallbacks)
  (\o -> destroySemaphore device o allocationCallbacks)
