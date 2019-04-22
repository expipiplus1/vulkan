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
  , withCStructCommandPoolCreateInfo
  , fromCStructCommandPoolCreateInfo
  , CommandPoolCreateInfo(..)
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
  , throwIO
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


import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateFlagBits(..)
  , VkCommandPoolCreateInfo(..)
  , VkCommandPoolResetFlagBits(..)
  , VkCommandPool
  , vkCreateCommandPool
  , vkDestroyCommandPool
  , vkResetCommandPool
  , pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
  , pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
  , pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT
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


-- | VkCommandPool - Opaque handle to a command pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkCreateCommandPool',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkDestroyCommandPool',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkResetCommandPool',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.vkTrimCommandPool',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_maintenance1.vkTrimCommandPoolKHR'
type CommandPool = VkCommandPool

-- | VkCommandPoolCreateFlagBits - Bitmask specifying usage behavior for a
-- command pool
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateFlags'
type CommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits


{-# complete COMMAND_POOL_CREATE_TRANSIENT_BIT, COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT, COMMAND_POOL_CREATE_PROTECTED_BIT :: CommandPoolCreateFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.CommandPool.VK_COMMAND_POOL_CREATE_TRANSIENT_BIT'
-- specifies that command buffers allocated from the pool will be
-- short-lived, meaning that they will be reset or freed in a relatively
-- short timeframe. This flag /may/ be used by the implementation to
-- control memory allocation behavior within the pool.
pattern COMMAND_POOL_CREATE_TRANSIENT_BIT :: (a ~ CommandPoolCreateFlagBits) => a
pattern COMMAND_POOL_CREATE_TRANSIENT_BIT = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT


-- | 'Graphics.Vulkan.C.Core10.CommandPool.VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT'
-- allows any command buffer allocated from a pool to be individually reset
-- to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>;
-- either by calling
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkResetCommandBuffer', or via
-- the implicit reset when calling
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkBeginCommandBuffer'. If this
-- flag is not set on a pool, then
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkResetCommandBuffer' /must/ not
-- be called for any command buffer allocated from that pool.
pattern COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT :: (a ~ CommandPoolCreateFlagBits) => a
pattern COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT


-- No documentation found for Nested "CommandPoolCreateFlagBits" "COMMAND_POOL_CREATE_PROTECTED_BIT"
pattern COMMAND_POOL_CREATE_PROTECTED_BIT :: (a ~ CommandPoolCreateFlagBits) => a
pattern COMMAND_POOL_CREATE_PROTECTED_BIT = VK_COMMAND_POOL_CREATE_PROTECTED_BIT

-- | VkCommandPoolCreateFlags - Bitmask of VkCommandPoolCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo'
type CommandPoolCreateFlags = CommandPoolCreateFlagBits


-- | VkCommandPoolCreateInfo - Structure specifying parameters of a newly
-- created command pool
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkCreateCommandPool'
data CommandPoolCreateInfo = CommandPoolCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "CommandPoolCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CommandPoolCreateInfo" "flags"
  flags :: CommandPoolCreateFlags
  , -- No documentation found for Nested "CommandPoolCreateInfo" "queueFamilyIndex"
  queueFamilyIndex :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCommandPoolCreateInfo' and
-- marshal a 'CommandPoolCreateInfo' into it. The 'VkCommandPoolCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCommandPoolCreateInfo :: CommandPoolCreateInfo -> (VkCommandPoolCreateInfo -> IO a) -> IO a
withCStructCommandPoolCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: CommandPoolCreateInfo)) (\pPNext -> cont (VkCommandPoolCreateInfo VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO pPNext (flags (marshalled :: CommandPoolCreateInfo)) (queueFamilyIndex (marshalled :: CommandPoolCreateInfo))))

-- | A function to read a 'VkCommandPoolCreateInfo' and all additional
-- structures in the pointer chain into a 'CommandPoolCreateInfo'.
fromCStructCommandPoolCreateInfo :: VkCommandPoolCreateInfo -> IO CommandPoolCreateInfo
fromCStructCommandPoolCreateInfo c = CommandPoolCreateInfo <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCommandPoolCreateInfo)))
                                                           <*> pure (vkFlags (c :: VkCommandPoolCreateInfo))
                                                           <*> pure (vkQueueFamilyIndex (c :: VkCommandPoolCreateInfo))

instance Zero CommandPoolCreateInfo where
  zero = CommandPoolCreateInfo Nothing
                               zero
                               zero


-- | VkCommandPoolResetFlagBits - Bitmask controlling behavior of a command
-- pool reset
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlags'
type CommandPoolResetFlagBits = VkCommandPoolResetFlagBits


{-# complete COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT :: CommandPoolResetFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.CommandPool.VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT'
-- specifies that resetting a command pool recycles all of the resources
-- from the command pool back to the system.
pattern COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT :: (a ~ CommandPoolResetFlagBits) => a
pattern COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT

-- | VkCommandPoolResetFlags - Bitmask of VkCommandPoolResetFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkResetCommandPool'
type CommandPoolResetFlags = CommandPoolResetFlagBits


-- | vkCreateCommandPool - Create a new command pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the command pool.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo'
--     structure specifying the state of the command pool object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pCommandPool@ points to a
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' handle in which
--     the created pool is returned.
--
-- == Valid Usage
--
-- -   @pCreateInfo@::@queueFamilyIndex@ /must/ be the index of a queue
--     family available in the logical device @device@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo'
--     structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pCommandPool@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
createCommandPool :: Device ->  CommandPoolCreateInfo ->  Maybe AllocationCallbacks ->  IO (CommandPool)
createCommandPool = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pCommandPool' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructCommandPoolCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateCommandPool commandTable device' pCreateInfo' pAllocator pCommandPool' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pCommandPool')))))


-- | vkDestroyCommandPool - Destroy a command pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the command pool.
--
-- -   @commandPool@ is the handle of the command pool to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- = Description
--
-- When a pool is destroyed, all command buffers allocated from the pool
-- are
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkFreeCommandBuffers freed>.
--
-- Any primary command buffer allocated from another
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has a secondary command buffer allocated from @commandPool@ recorded
-- into it, becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   All 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' objects
--     allocated from @commandPool@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @commandPool@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @commandPool@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @commandPool@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @commandPool@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @commandPool@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @commandPool@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
destroyCommandPool :: Device ->  CommandPool ->  Maybe AllocationCallbacks ->  IO ()
destroyCommandPool = \(Device device' commandTable) -> \commandPool' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyCommandPool commandTable device' commandPool' pAllocator *> (pure ()))


-- | vkResetCommandPool - Reset a command pool
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the command pool.
--
-- -   @commandPool@ is the command pool to reset.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlagBits'
--     controlling the reset operation.
--
-- = Description
--
-- Resetting a command pool recycles all of the resources from all of the
-- command buffers allocated from the command pool back to the command
-- pool. All command buffers that have been allocated from the command pool
-- are put in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- Any primary command buffer allocated from another
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has a secondary command buffer allocated from @commandPool@ recorded
-- into it, becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   All 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' objects
--     allocated from @commandPool@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @commandPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlagBits'
--     values
--
-- -   @commandPool@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @commandPool@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
resetCommandPool :: Device ->  CommandPool ->  CommandPoolResetFlags ->  IO ()
resetCommandPool = \(Device device' commandTable) -> \commandPool' -> \flags' -> vkResetCommandPool commandTable device' commandPool' flags' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))

-- | A safe wrapper for 'createCommandPool' and 'destroyCommandPool' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withCommandPool
  :: Device -> CommandPoolCreateInfo -> Maybe (AllocationCallbacks) -> (CommandPool -> IO a) -> IO a
withCommandPool device commandPoolCreateInfo allocationCallbacks = bracket
  (createCommandPool device commandPoolCreateInfo allocationCallbacks)
  (\o -> destroyCommandPool device o allocationCallbacks)
