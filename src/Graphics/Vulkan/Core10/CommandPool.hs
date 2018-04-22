{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.CommandPool
  ( VkCommandPoolCreateFlagBits(..)
  , pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT
  , pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
  , VkCommandPoolResetFlagBits(..)
  , pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
  , VkCommandPool
  , vkCreateCommandPool
  , vkDestroyCommandPool
  , vkResetCommandPool
  , VkCommandPoolCreateInfo(..)
  , VkCommandPoolCreateFlags
  , VkCommandPoolResetFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )


-- ** VkCommandPoolCreateFlagBits

-- | VkCommandPoolCreateFlagBits - Bitmask specifying usage behavior for a
-- command pool
--
-- = See Also
--
-- 'VkCommandPoolCreateFlags'
newtype VkCommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkCommandPoolCreateFlagBits where
  showsPrec _ VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = showString "VK_COMMAND_POOL_CREATE_TRANSIENT_BIT"
  showsPrec _ VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = showString "VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkCommandPoolCreateFlagBits 0x00000004) = showString "VK_COMMAND_POOL_CREATE_PROTECTED_BIT"
  showsPrec p (VkCommandPoolCreateFlagBits x) = showParen (p >= 11) (showString "VkCommandPoolCreateFlagBits " . showsPrec 11 x)

instance Read VkCommandPoolCreateFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_CREATE_TRANSIENT_BIT",            pure VK_COMMAND_POOL_CREATE_TRANSIENT_BIT)
                             , ("VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT", pure VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_COMMAND_POOL_CREATE_PROTECTED_BIT", pure (VkCommandPoolCreateFlagBits 0x00000004))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandPoolCreateFlagBits")
                        v <- step readPrec
                        pure (VkCommandPoolCreateFlagBits v)
                        )
                    )

-- | @VK_COMMAND_POOL_CREATE_TRANSIENT_BIT@ specifies that command buffers
-- allocated from the pool will be short-lived, meaning that they will be
-- reset or freed in a relatively short timeframe. This flag /may/ be used
-- by the implementation to control memory allocation behavior within the
-- pool.
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT :: VkCommandPoolCreateFlagBits
pattern VK_COMMAND_POOL_CREATE_TRANSIENT_BIT = VkCommandPoolCreateFlagBits 0x00000001

-- | @VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT@ allows any command
-- buffer allocated from a pool to be individually reset to the [initial
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle);
-- either by calling
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkResetCommandBuffer', or via the
-- implicit reset when calling
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkBeginCommandBuffer'. If this
-- flag is not set on a pool, then @vkResetCommandBuffer@ /must/ not be
-- called for any command buffer allocated from that pool.
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT :: VkCommandPoolCreateFlagBits
pattern VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT = VkCommandPoolCreateFlagBits 0x00000002
-- ** VkCommandPoolResetFlagBits

-- | VkCommandPoolResetFlagBits - Bitmask controlling behavior of a command
-- pool reset
--
-- = See Also
--
-- 'VkCommandPoolResetFlags'
newtype VkCommandPoolResetFlagBits = VkCommandPoolResetFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkCommandPoolResetFlagBits where
  showsPrec _ VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = showString "VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT"
  showsPrec p (VkCommandPoolResetFlagBits x) = showParen (p >= 11) (showString "VkCommandPoolResetFlagBits " . showsPrec 11 x)

instance Read VkCommandPoolResetFlagBits where
  readPrec = parens ( choose [ ("VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT", pure VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCommandPoolResetFlagBits")
                        v <- step readPrec
                        pure (VkCommandPoolResetFlagBits v)
                        )
                    )

-- | @VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT@ specifies that resetting a
-- command pool recycles all of the resources from the command pool back to
-- the system.
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT :: VkCommandPoolResetFlagBits
pattern VK_COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT = VkCommandPoolResetFlagBits 0x00000001
-- | Dummy data to tag the 'Ptr' with
data VkCommandPool_T
-- | VkCommandPool - Opaque handle to a command pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkCommandBufferAllocateInfo',
-- 'vkCreateCommandPool', 'vkDestroyCommandPool',
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'vkResetCommandPool',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1.vkTrimCommandPool',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance1.vkTrimCommandPoolKHR'
type VkCommandPool = Ptr VkCommandPool_T
-- | vkCreateCommandPool - Create a new command pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the command pool.
--
-- -   @pCreateInfo@ contains information used to create the command pool.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pCommandPool@ points to a @VkCommandPool@ handle in which the
--     created pool is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkCommandPoolCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pCommandPool@ /must/ be a valid pointer to a @VkCommandPool@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkCommandPool', 'VkCommandPoolCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateCommandPool" vkCreateCommandPool :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkCommandPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCommandPool" ::: Ptr VkCommandPool) -> IO VkResult
-- | vkDestroyCommandPool - Destroy a command pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the command pool.
--
-- -   @commandPool@ is the handle of the command pool to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- = Description
--
-- When a pool is destroyed, all command buffers allocated from the pool
-- are
-- [freed](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkFreeCommandBuffers).
--
-- Any primary command buffer allocated from another 'VkCommandPool' that
-- is in the [recording or executable
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
-- and has a secondary command buffer allocated from @commandPool@ recorded
-- into it, becomes
-- [invalid](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- == Valid Usage
--
-- -   All @VkCommandBuffer@ objects allocated from @commandPool@ /must/
--     not be in the [pending
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- -   If @VkAllocationCallbacks@ were provided when @commandPool@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @commandPool@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @commandPool@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @commandPool@
--     /must/ be a valid @VkCommandPool@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkCommandPool', 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyCommandPool" vkDestroyCommandPool :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkResetCommandPool - Reset a command pool
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the command pool.
--
-- -   @commandPool@ is the command pool to reset.
--
-- -   @flags@ is a bitmask of 'VkCommandPoolResetFlagBits' controlling the
--     reset operation.
--
-- = Description
--
-- Resetting a command pool recycles all of the resources from all of the
-- command buffers allocated from the command pool back to the command
-- pool. All command buffers that have been allocated from the command pool
-- are put in the [initial
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- Any primary command buffer allocated from another 'VkCommandPool' that
-- is in the [recording or executable
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
-- and has a secondary command buffer allocated from @commandPool@ recorded
-- into it, becomes
-- [invalid](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- == Valid Usage
--
-- -   All @VkCommandBuffer@ objects allocated from @commandPool@ /must/
--     not be in the [pending
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @commandPool@ /must/ be a valid @VkCommandPool@ handle
--
-- -   @flags@ /must/ be a valid combination of
--     'VkCommandPoolResetFlagBits' values
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
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'VkCommandPool', 'VkCommandPoolResetFlags',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetCommandPool" vkResetCommandPool :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolResetFlags) -> IO VkResult
-- | VkCommandPoolCreateInfo - Structure specifying parameters of a newly
-- created command pool
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be the index of a queue family available
--     in the calling command’s @device@ parameter
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be a valid combination of
--     'VkCommandPoolCreateFlagBits' values
--
-- = See Also
--
-- 'VkCommandPoolCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateCommandPool'
data VkCommandPoolCreateInfo = VkCommandPoolCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkCommandPoolCreateFlagBits' indicating usage
  -- behavior for the pool and command buffers allocated from it.
  vkFlags :: VkCommandPoolCreateFlags
  , -- | @queueFamilyIndex@ designates a queue family as described in section
  -- [Queue Family
  -- Properties](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-queueprops).
  -- All command buffers allocated from this command pool /must/ be submitted
  -- on queues from the same queue family.
  vkQueueFamilyIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkCommandPoolCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCommandPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkCommandPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkCommandPoolCreateInfo))
-- | VkCommandPoolCreateFlags - Bitmask of VkCommandPoolCreateFlagBits
--
-- = Description
--
-- @VkCommandPoolCreateFlags@ is a bitmask type for setting a mask of zero
-- or more 'VkCommandPoolCreateFlagBits'.
--
-- = See Also
--
-- 'VkCommandPoolCreateFlagBits', 'VkCommandPoolCreateInfo'
type VkCommandPoolCreateFlags = VkCommandPoolCreateFlagBits
-- | VkCommandPoolResetFlags - Bitmask of VkCommandPoolResetFlagBits
--
-- = Description
--
-- @VkCommandPoolResetFlags@ is a bitmask type for setting a mask of zero
-- or more 'VkCommandPoolResetFlagBits'.
--
-- = See Also
--
-- 'VkCommandPoolResetFlagBits', 'vkResetCommandPool'
type VkCommandPoolResetFlags = VkCommandPoolResetFlagBits
