{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Fence
  ( FenceCreateFlagBits
  , pattern FENCE_CREATE_SIGNALED_BIT
  , FenceCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , FenceCreateInfo(..)
#endif
  , createFence
  , destroyFence
  , getFenceStatus
  , resetFences
  , waitForFences
  , withFence
  ) where

import Control.Exception
  ( bracket
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
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateFlagBits(..)
  , vkCreateFence
  , vkDestroyFence
  , vkGetFenceStatus
  , vkResetFences
  , vkWaitForFences
  , pattern VK_FENCE_CREATE_SIGNALED_BIT
  )
import Graphics.Vulkan.Core10.Core
  ( boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( Fence
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "FenceCreateFlagBits"
type FenceCreateFlagBits = VkFenceCreateFlagBits


{-# complete FENCE_CREATE_SIGNALED_BIT :: FenceCreateFlagBits #-}


-- No documentation found for Nested "FenceCreateFlagBits" "FENCE_CREATE_SIGNALED_BIT"
pattern FENCE_CREATE_SIGNALED_BIT :: (a ~ FenceCreateFlagBits) => a
pattern FENCE_CREATE_SIGNALED_BIT = VK_FENCE_CREATE_SIGNALED_BIT

-- No documentation found for TopLevel "FenceCreateFlags"
type FenceCreateFlags = FenceCreateFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkFenceCreateInfo"
data FenceCreateInfo = FenceCreateInfo
  { -- No documentation found for Nested "FenceCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FenceCreateInfo" "flags"
  flags :: FenceCreateFlags
  }
  deriving (Show, Eq)

instance Zero FenceCreateInfo where
  zero = FenceCreateInfo Nothing
                         zero

#endif


-- No documentation found for TopLevel "vkCreateFence"
createFence :: Device ->  FenceCreateInfo ->  Maybe AllocationCallbacks ->  IO (Fence)
createFence = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyFence"
destroyFence :: Device ->  Fence ->  Maybe AllocationCallbacks ->  IO ()
destroyFence = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetFenceStatus"
getFenceStatus :: Device ->  Fence ->  IO (VkResult)
getFenceStatus = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkResetFences"
resetFences :: Device ->  Vector Fence ->  IO ()
resetFences = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkWaitForFences"
waitForFences :: Device ->  Vector Fence ->  Bool ->  Word64 ->  IO (VkResult)
waitForFences = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createFence' and 'destroyFence' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withFence
  :: Device -> FenceCreateInfo -> Maybe AllocationCallbacks -> (Fence -> IO a) -> IO a
withFence device fenceCreateInfo allocationCallbacks = bracket
  (createFence device fenceCreateInfo allocationCallbacks)
  (\o -> destroyFence device o allocationCallbacks)
