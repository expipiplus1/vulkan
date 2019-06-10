{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  , CommandPoolCreateFlagBits
  , pattern COMMAND_POOL_CREATE_TRANSIENT_BIT
  , pattern COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
  , pattern COMMAND_POOL_CREATE_PROTECTED_BIT
  , CommandPoolCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , CommandPoolCreateInfo(..)
#endif
  , CommandPoolResetFlagBits
  , pattern COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
  , CommandPoolResetFlags
  , createCommandPool
  , destroyCommandPool
  , resetCommandPool
  , withCommandPool
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
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


import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateFlagBits(..)
  , VkCommandPoolResetFlagBits(..)
  , VkCommandPool
  , vkCreateCommandPool
  , vkDestroyCommandPool
  , vkResetCommandPool
  , pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
  , pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
  , pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "CommandPool"
type CommandPool = VkCommandPool

-- No documentation found for TopLevel "CommandPoolCreateFlagBits"
type CommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits


{-# complete COMMAND_POOL_CREATE_TRANSIENT_BIT, COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, COMMAND_POOL_CREATE_PROTECTED_BIT :: CommandPoolCreateFlagBits #-}


-- No documentation found for Nested "CommandPoolCreateFlagBits" "COMMAND_POOL_CREATE_TRANSIENT_BIT"
pattern COMMAND_POOL_CREATE_TRANSIENT_BIT :: (a ~ CommandPoolCreateFlagBits) => a
pattern COMMAND_POOL_CREATE_TRANSIENT_BIT = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT


-- No documentation found for Nested "CommandPoolCreateFlagBits" "COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
pattern COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT :: (a ~ CommandPoolCreateFlagBits) => a
pattern COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT


-- No documentation found for Nested "CommandPoolCreateFlagBits" "COMMAND_POOL_CREATE_PROTECTED_BIT"
pattern COMMAND_POOL_CREATE_PROTECTED_BIT :: (a ~ CommandPoolCreateFlagBits) => a
pattern COMMAND_POOL_CREATE_PROTECTED_BIT = VK_COMMAND_POOL_CREATE_PROTECTED_BIT

-- No documentation found for TopLevel "CommandPoolCreateFlags"
type CommandPoolCreateFlags = CommandPoolCreateFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCommandPoolCreateInfo"
data CommandPoolCreateInfo = CommandPoolCreateInfo
  { -- No documentation found for Nested "CommandPoolCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandPoolCreateInfo" "flags"
  flags :: CommandPoolCreateFlags
  , -- No documentation found for Nested "CommandPoolCreateInfo" "queueFamilyIndex"
  queueFamilyIndex :: Word32
  }
  deriving (Show, Eq)

instance Zero CommandPoolCreateInfo where
  zero = CommandPoolCreateInfo Nothing
                               zero
                               zero

#endif

-- No documentation found for TopLevel "CommandPoolResetFlagBits"
type CommandPoolResetFlagBits = VkCommandPoolResetFlagBits


{-# complete COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT :: CommandPoolResetFlagBits #-}


-- No documentation found for Nested "CommandPoolResetFlagBits" "COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
pattern COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT :: (a ~ CommandPoolResetFlagBits) => a
pattern COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT

-- No documentation found for TopLevel "CommandPoolResetFlags"
type CommandPoolResetFlags = CommandPoolResetFlagBits


-- No documentation found for TopLevel "vkCreateCommandPool"
createCommandPool :: Device ->  CommandPoolCreateInfo ->  Maybe AllocationCallbacks ->  IO (CommandPool)
createCommandPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyCommandPool"
destroyCommandPool :: Device ->  CommandPool ->  Maybe AllocationCallbacks ->  IO ()
destroyCommandPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkResetCommandPool"
resetCommandPool :: Device ->  CommandPool ->  CommandPoolResetFlags ->  IO ()
resetCommandPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createCommandPool' and 'destroyCommandPool' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withCommandPool
  :: Device -> CommandPoolCreateInfo -> Maybe AllocationCallbacks -> (CommandPool -> IO a) -> IO a
withCommandPool device commandPoolCreateInfo allocationCallbacks = bracket
  (createCommandPool device commandPoolCreateInfo allocationCallbacks)
  (\o -> destroyCommandPool device o allocationCallbacks)
