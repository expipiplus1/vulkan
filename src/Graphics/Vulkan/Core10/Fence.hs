{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Fence
  ( FenceCreateFlagBits
  , pattern FENCE_CREATE_SIGNALED_BIT
  , FenceCreateFlags
  , withCStructFenceCreateInfo
  , fromCStructFenceCreateInfo
  , FenceCreateInfo(..)
  , createFence
  , destroyFence
  , getFenceStatus
  , resetFences
  , waitForFences
  , withFence
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
  ( length
  )
import Data.Word
  ( Word64
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
  ( VkResult(..)
  , Zero(..)
  , pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateFlagBits(..)
  , VkFenceCreateInfo(..)
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
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Queue
  ( Fence
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


-- | VkFenceCreateFlagBits - Bitmask specifying initial state and behavior of
-- a fence
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlags'
type FenceCreateFlagBits = VkFenceCreateFlagBits


{-# complete FENCE_CREATE_SIGNALED_BIT :: FenceCreateFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.Fence.VK_FENCE_CREATE_SIGNALED_BIT' specifies
-- that the fence object is created in the signaled state. Otherwise, it is
-- created in the unsignaled state.
pattern FENCE_CREATE_SIGNALED_BIT :: (a ~ FenceCreateFlagBits) => a
pattern FENCE_CREATE_SIGNALED_BIT = VK_FENCE_CREATE_SIGNALED_BIT

-- | VkFenceCreateFlags - Bitmask of VkFenceCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateInfo'
type FenceCreateFlags = FenceCreateFlagBits


-- | VkFenceCreateInfo - Structure specifying parameters of a newly created
-- fence
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_FENCE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkExportFenceWin32HandleInfoKHR'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlagBits' values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Fence.vkCreateFence'
data FenceCreateInfo = FenceCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "FenceCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FenceCreateInfo" "flags"
  flags :: FenceCreateFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkFenceCreateInfo' and
-- marshal a 'FenceCreateInfo' into it. The 'VkFenceCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructFenceCreateInfo :: FenceCreateInfo -> (VkFenceCreateInfo -> IO a) -> IO a
withCStructFenceCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: FenceCreateInfo)) (\pPNext -> cont (VkFenceCreateInfo VK_STRUCTURE_TYPE_FENCE_CREATE_INFO pPNext (flags (marshalled :: FenceCreateInfo))))

-- | A function to read a 'VkFenceCreateInfo' and all additional
-- structures in the pointer chain into a 'FenceCreateInfo'.
fromCStructFenceCreateInfo :: VkFenceCreateInfo -> IO FenceCreateInfo
fromCStructFenceCreateInfo c = FenceCreateInfo <$> -- Univalued Member elided
                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFenceCreateInfo)))
                                               <*> pure (vkFlags (c :: VkFenceCreateInfo))

instance Zero FenceCreateInfo where
  zero = FenceCreateInfo Nothing
                         zero



-- | vkCreateFence - Create a new fence object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the fence.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateInfo' structure which
--     contains information about how the fence is to be created.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pFence@ points to a handle in which the resulting fence object is
--     returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pFence@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.Queue.VkFence' handle
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence',
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateInfo'
createFence :: Device ->  FenceCreateInfo ->  Maybe AllocationCallbacks ->  IO (Fence)
createFence = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pFence' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructFenceCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateFence commandTable device' pCreateInfo' pAllocator pFence' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pFence')))))


-- | vkDestroyFence - Destroy a fence object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the fence.
--
-- -   @fence@ is the handle of the fence to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-submission queue submission>
--     commands that refer to @fence@ /must/ have completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @fence@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @fence@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @fence@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.Queue.VkFence' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @fence@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence'
destroyFence :: Device ->  Fence ->  Maybe AllocationCallbacks ->  IO ()
destroyFence = \(Device device' commandTable) -> \fence' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyFence commandTable device' fence' pAllocator *> (pure ()))


-- | vkGetFenceStatus - Return the status of a fence
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the fence.
--
-- -   @fence@ is the handle of the fence to query.
--
-- = Description
--
-- Upon success, 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus' returns
-- the status of the fence object, with the following return codes:
--
-- > +-----------------------------------+-----------------------------------+
-- > | Status                            | Meaning                           |
-- > +===================================+===================================+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK | The fence specified by @fence@ is |
-- > | _SUCCESS'                         | signaled.                         |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK | The fence specified by @fence@ is |
-- > | _NOT_READY'                       | unsignaled.                       |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK | The device has been lost. See     |
-- > | _ERROR_DEVICE_LOST'               | <https://www.khronos.org/registry |
-- > |                                   | /vulkan/specs/1.1-extensions/html |
-- > |                                   | /vkspec.html#devsandqueues-lost-d |
-- > |                                   | evice Lost Device>.               |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Fence Object Status Codes
--
-- If a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-submission queue submission>
-- command is pending execution, then the value returned by this command
-- /may/ immediately be out of date.
--
-- If the device has been lost (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>),
-- 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus' /may/ return any of
-- the above status codes. If the device has been lost and
-- 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus' is called repeatedly,
-- it will eventually return either
-- 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' or
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_NOT_READY'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence'
getFenceStatus :: Device ->  Fence ->  IO (VkResult)
getFenceStatus = \(Device device' commandTable) -> \fence' -> vkGetFenceStatus commandTable device' fence' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ret))


-- | vkResetFences - Resets one or more fence objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the fences.
--
-- -   @fenceCount@ is the number of fences to reset.
--
-- -   @pFences@ is a pointer to an array of fence handles to reset.
--
-- = Description
--
-- When 'Graphics.Vulkan.C.Core10.Fence.vkResetFences' is executed on the
-- host, it defines a /fence unsignal operation/ for each fence, which
-- resets the fence to the unsignaled state.
--
-- If any member of @pFences@ is already in the unsignaled state when
-- 'Graphics.Vulkan.C.Core10.Fence.vkResetFences' is executed, then
-- 'Graphics.Vulkan.C.Core10.Fence.vkResetFences' has no effect on that
-- fence.
--
-- == Valid Usage
--
-- -   Each element of @pFences@ /must/ not be currently associated with
--     any queue command that has not yet completed execution on that queue
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pFences@ /must/ be a valid pointer to an array of @fenceCount@
--     valid 'Graphics.Vulkan.C.Core10.Queue.VkFence' handles
--
-- -   @fenceCount@ /must/ be greater than @0@
--
-- -   Each element of @pFences@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to each member of @pFences@ /must/ be externally
--     synchronized
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence'
resetFences :: Device ->  Vector Fence ->  IO ()
resetFences = \(Device device' commandTable) -> \fences' -> withVec (&) fences' (\pFences' -> vkResetFences commandTable device' (fromIntegral $ Data.Vector.length fences') pFences' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkWaitForFences - Wait for one or more fences to become signaled
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the fences.
--
-- -   @fenceCount@ is the number of fences to wait on.
--
-- -   @pFences@ is a pointer to an array of @fenceCount@ fence handles.
--
-- -   @waitAll@ is the condition that /must/ be satisfied to successfully
--     unblock the wait. If @waitAll@ is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', then the condition is that
--     all fences in @pFences@ are signaled. Otherwise, the condition is
--     that at least one fence in @pFences@ is signaled.
--
-- -   @timeout@ is the timeout period in units of nanoseconds. @timeout@
--     is adjusted to the closest value allowed by the
--     implementation-dependent timeout accuracy, which /may/ be
--     substantially longer than one nanosecond, and /may/ be longer than
--     the requested period.
--
-- = Description
--
-- If the condition is satisfied when
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences' is called, then
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences' returns immediately. If
-- the condition is not satisfied at the time
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences' is called, then
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences' will block and wait up
-- to @timeout@ nanoseconds for the condition to become satisfied.
--
-- If @timeout@ is zero, then
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences' does not wait, but
-- simply returns the current state of the fences.
-- 'Graphics.Vulkan.C.Core10.Core.VK_TIMEOUT' will be returned in this case
-- if the condition is not satisfied, even though no actual wait was
-- performed.
--
-- If the specified timeout period expires before the condition is
-- satisfied, 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences' returns
-- 'Graphics.Vulkan.C.Core10.Core.VK_TIMEOUT'. If the condition is
-- satisfied before @timeout@ nanoseconds has expired,
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences' returns
-- 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'.
--
-- If device loss occurs (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>)
-- before the timeout has expired,
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences' /must/ return in finite
-- time with either 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' or
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'.
--
-- __Note__
--
-- While we guarantee that 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences'
-- /must/ return in finite time, no guarantees are made that it returns
-- immediately upon device loss. However, the client can reasonably expect
-- that the delay will be on the order of seconds and that calling
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences' will not result in a
-- permanently (or seemingly permanently) dead process.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pFences@ /must/ be a valid pointer to an array of @fenceCount@
--     valid 'Graphics.Vulkan.C.Core10.Queue.VkFence' handles
--
-- -   @fenceCount@ /must/ be greater than @0@
--
-- -   Each element of @pFences@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_TIMEOUT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence'
waitForFences :: Device ->  Vector Fence ->  Bool ->  Word64 ->  IO (VkResult)
waitForFences = \(Device device' commandTable) -> \fences' -> \waitAll' -> \timeout' -> withVec (&) fences' (\pFences' -> vkWaitForFences commandTable device' (fromIntegral $ Data.Vector.length fences') pFences' (boolToBool32 waitAll') timeout' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ret)))

-- | A safe wrapper for 'createFence' and 'destroyFence' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withFence
  :: Device -> FenceCreateInfo -> Maybe (AllocationCallbacks) -> (Fence -> IO a) -> IO a
withFence device fenceCreateInfo allocationCallbacks = bracket
  (createFence device fenceCreateInfo allocationCallbacks)
  (\o -> destroyFence device o allocationCallbacks)
