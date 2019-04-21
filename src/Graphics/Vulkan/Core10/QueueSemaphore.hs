{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.QueueSemaphore
  ( SemaphoreCreateFlags
  , withCStructSemaphoreCreateInfo
  , fromCStructSemaphoreCreateInfo
  , SemaphoreCreateInfo(..)
  , createSemaphore
  , destroySemaphore
  , withSemaphore
  ) where

import Control.Exception
  ( bracket
  , throwIO
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateFlags(..)
  , VkSemaphoreCreateInfo(..)
  , vkCreateSemaphore
  , vkDestroySemaphore
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Queue
  ( Semaphore
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- | VkSemaphoreCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.VkSemaphoreCreateFlags' is a
-- bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.VkSemaphoreCreateInfo'
type SemaphoreCreateFlags = VkSemaphoreCreateFlags


-- | VkSemaphoreCreateInfo - Structure specifying parameters of a newly
-- created semaphore
--
-- = Description
--
-- Unresolved directive in VkSemaphoreCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkSemaphoreCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.VkSemaphoreCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkCreateSemaphore'
data SemaphoreCreateInfo = SemaphoreCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "SemaphoreCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SemaphoreCreateInfo" "flags"
  flags :: SemaphoreCreateFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSemaphoreCreateInfo' and
-- marshal a 'SemaphoreCreateInfo' into it. The 'VkSemaphoreCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSemaphoreCreateInfo :: SemaphoreCreateInfo -> (VkSemaphoreCreateInfo -> IO a) -> IO a
withCStructSemaphoreCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SemaphoreCreateInfo)) (\pPNext -> cont (VkSemaphoreCreateInfo VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO pPNext (flags (marshalled :: SemaphoreCreateInfo))))

-- | A function to read a 'VkSemaphoreCreateInfo' and all additional
-- structures in the pointer chain into a 'SemaphoreCreateInfo'.
fromCStructSemaphoreCreateInfo :: VkSemaphoreCreateInfo -> IO SemaphoreCreateInfo
fromCStructSemaphoreCreateInfo c = SemaphoreCreateInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSemaphoreCreateInfo)))
                                                       <*> pure (vkFlags (c :: VkSemaphoreCreateInfo))

instance Zero SemaphoreCreateInfo where
  zero = SemaphoreCreateInfo Nothing
                             zero



-- | vkCreateSemaphore - Create a new queue semaphore object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the semaphore.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.QueueSemaphore.VkSemaphoreCreateInfo'
--     structure which contains information about how the semaphore is to
--     be created.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pSemaphore@ points to a handle in which the resulting semaphore
--     object is returned.
--
-- = Description
--
-- When created, the semaphore is in the unsignaled state.
--
-- Unresolved directive in vkCreateSemaphore.txt -
-- include::{generated}\/validity\/protos\/vkCreateSemaphore.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSemaphore',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.VkSemaphoreCreateInfo'
createSemaphore :: Device ->  SemaphoreCreateInfo ->  Maybe AllocationCallbacks ->  IO (Semaphore)
createSemaphore = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pSemaphore' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructSemaphoreCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateSemaphore commandTable device' pCreateInfo' pAllocator pSemaphore' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSemaphore')))))


-- | vkDestroySemaphore - Destroy a semaphore object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the semaphore.
--
-- -   @semaphore@ is the handle of the semaphore to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted batches that refer to @semaphore@ /must/ have
--     completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @semaphore@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @semaphore@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroySemaphore.txt -
-- include::{generated}\/validity\/protos\/vkDestroySemaphore.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSemaphore'
destroySemaphore :: Device ->  Semaphore ->  Maybe AllocationCallbacks ->  IO ()
destroySemaphore = \(Device device' commandTable) -> \semaphore' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroySemaphore commandTable device' semaphore' pAllocator *> (pure ()))

-- | A safe wrapper for 'createSemaphore' and 'destroySemaphore' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withSemaphore
  :: Device -> SemaphoreCreateInfo -> Maybe (AllocationCallbacks) -> (Semaphore -> IO a) -> IO a
withSemaphore device semaphoreCreateInfo allocationCallbacks = bracket
  (createSemaphore device semaphoreCreateInfo allocationCallbacks)
  (\o -> destroySemaphore device o allocationCallbacks)
