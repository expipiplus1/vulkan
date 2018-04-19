{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Fence
  ( VkFenceCreateFlagBits(..)
  , pattern VK_FENCE_CREATE_SIGNALED_BIT
  , vkCreateFence
  , vkDestroyFence
  , vkResetFences
  , vkGetFenceStatus
  , vkWaitForFences
  , VkFenceCreateInfo(..)
  , VkFenceCreateFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word64
  , Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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
  ( VkStructureType(..)
  , VkBool32(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Queue
  ( VkFence
  )


-- ** VkFenceCreateFlagBits

-- | VkFenceCreateFlagBits - Bitmask specifying initial state and behavior of
-- a fence
--
-- = See Also
-- #_see_also#
--
-- 'VkFenceCreateFlags'
newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkFenceCreateFlagBits where
  showsPrec _ VK_FENCE_CREATE_SIGNALED_BIT = showString "VK_FENCE_CREATE_SIGNALED_BIT"
  showsPrec p (VkFenceCreateFlagBits x) = showParen (p >= 11) (showString "VkFenceCreateFlagBits " . showsPrec 11 x)

instance Read VkFenceCreateFlagBits where
  readPrec = parens ( choose [ ("VK_FENCE_CREATE_SIGNALED_BIT", pure VK_FENCE_CREATE_SIGNALED_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFenceCreateFlagBits")
                        v <- step readPrec
                        pure (VkFenceCreateFlagBits v)
                        )
                    )

-- | @VK_FENCE_CREATE_SIGNALED_BIT@ specifies that the fence object is
-- created in the signaled state. Otherwise, it is created in the
-- unsignaled state.
pattern VK_FENCE_CREATE_SIGNALED_BIT :: VkFenceCreateFlagBits
pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 0x00000001
-- | vkCreateFence - Create a new fence object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that creates the fence.
--
-- -   @pCreateInfo@ is a pointer to an instance of the @VkFenceCreateInfo@
--     structure which contains information about how the fence is to be
--     created.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pFence@ points to a handle in which the resulting fence object is
--     returned.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkFenceCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pFence@ /must/ be a valid pointer to a @VkFence@ handle
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Queue.VkFence', 'VkFenceCreateInfo'
foreign import ccall "vkCreateFence" vkCreateFence :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
-- | vkDestroyFence - Destroy a fence object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that destroys the fence.
--
-- -   @fence@ is the handle of the fence to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   All <{html_spec_relative}#devsandqueues-submission queue submission>
--     commands that refer to @fence@ /must/ have completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @fence@ was created, a
--     compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @fence@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @fence@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @fence@ /must/ be a valid @VkFence@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @fence@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Queue.VkFence'
foreign import ccall "vkDestroyFence" vkDestroyFence :: ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkResetFences - Resets one or more fence objects
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that owns the fences.
--
-- -   @fenceCount@ is the number of fences to reset.
--
-- -   @pFences@ is a pointer to an array of fence handles to reset.
--
-- = Description
-- #_description#
--
-- When 'vkResetFences' is executed on the host, it defines a /fence
-- unsignal operation/ for each fence, which resets the fence to the
-- unsignaled state.
--
-- If any member of @pFences@ is already in the unsignaled state when
-- 'vkResetFences' is executed, then 'vkResetFences' has no effect on that
-- fence.
--
-- == Valid Usage
--
-- -   Each element of @pFences@ /must/ not be currently associated with
--     any queue command that has not yet completed execution on that queue
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pFences@ /must/ be a valid pointer to an array of @fenceCount@
--     valid @VkFence@ handles
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
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Queue.VkFence'
foreign import ccall "vkResetFences" vkResetFences :: ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult
-- | vkGetFenceStatus - Return the status of a fence
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that owns the fence.
--
-- -   @fence@ is the handle of the fence to query.
--
-- = Description
-- #_description#
--
-- Upon success, @vkGetFenceStatus@ returns the status of the fence object,
-- with the following return codes:
--
-- > +-----------------------------------+-----------------------------------+
-- > | Status                            | Meaning                           |
-- > +===================================+===================================+
-- > | @VK_SUCCESS@                      | The fence specified by @fence@ is |
-- > |                                   | signaled.                         |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_NOT_READY@                    | The fence specified by @fence@ is |
-- > |                                   | unsignaled.                       |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ERROR_DEVICE_LOST@            | The device has been lost. See     |
-- > |                                   | <{html_spec_relative}#devsandqueu |
-- > |                                   | es-lost-device Lost Device>.      |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Fence Object Status Codes
--
-- If a <{html_spec_relative}#devsandqueues-submission queue submission>
-- command is pending execution, then the value returned by this command
-- /may/ immediately be out of date.
--
-- If the device has been lost (see
-- <{html_spec_relative}#devsandqueues-lost-device Lost Device>),
-- @vkGetFenceStatus@ /may/ return any of the above status codes. If the
-- device has been lost and @vkGetFenceStatus@ is called repeatedly, it
-- will eventually return either @VK_SUCCESS@ or @VK_ERROR_DEVICE_LOST@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @fence@ /must/ be a valid @VkFence@ handle
--
-- -   @fence@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_NOT_READY@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Queue.VkFence'
foreign import ccall "vkGetFenceStatus" vkGetFenceStatus :: ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult
-- | vkWaitForFences - Wait for one or more fences to become signaled
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that owns the fences.
--
-- -   @fenceCount@ is the number of fences to wait on.
--
-- -   @pFences@ is a pointer to an array of @fenceCount@ fence handles.
--
-- -   @waitAll@ is the condition that /must/ be satisfied to successfully
--     unblock the wait. If @waitAll@ is @VK_TRUE@, then the condition is
--     that all fences in @pFences@ are signaled. Otherwise, the condition
--     is that at least one fence in @pFences@ is signaled.
--
-- -   @timeout@ is the timeout period in units of nanoseconds. @timeout@
--     is adjusted to the closest value allowed by the
--     implementation-dependent timeout accuracy, which /may/ be
--     substantially longer than one nanosecond, and /may/ be longer than
--     the requested period.
--
-- = Description
-- #_description#
--
-- If the condition is satisfied when @vkWaitForFences@ is called, then
-- @vkWaitForFences@ returns immediately. If the condition is not satisfied
-- at the time @vkWaitForFences@ is called, then @vkWaitForFences@ will
-- block and wait up to @timeout@ nanoseconds for the condition to become
-- satisfied.
--
-- If @timeout@ is zero, then @vkWaitForFences@ does not wait, but simply
-- returns the current state of the fences. @VK_TIMEOUT@ will be returned
-- in this case if the condition is not satisfied, even though no actual
-- wait was performed.
--
-- If the specified timeout period expires before the condition is
-- satisfied, @vkWaitForFences@ returns @VK_TIMEOUT@. If the condition is
-- satisfied before @timeout@ nanoseconds has expired, @vkWaitForFences@
-- returns @VK_SUCCESS@.
--
-- If device loss occurs (see
-- <{html_spec_relative}#devsandqueues-lost-device Lost Device>) before the
-- timeout has expired, @vkWaitForFences@ /must/ return in finite time with
-- either @VK_SUCCESS@ or @VK_ERROR_DEVICE_LOST@.
--
-- __Note__
--
-- While we guarantee that @vkWaitForFences@ /must/ return in finite time,
-- no guarantees are made that it returns immediately upon device loss.
-- However, the client can reasonably expect that the delay will be on the
-- order of seconds and that calling @vkWaitForFences@ will not result in a
-- permanently (or seemingly permanently) dead process.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pFences@ /must/ be a valid pointer to an array of @fenceCount@
--     valid @VkFence@ handles
--
-- -   @fenceCount@ /must/ be greater than @0@
--
-- -   Each element of @pFences@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_TIMEOUT@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Queue.VkFence'
foreign import ccall "vkWaitForFences" vkWaitForFences :: ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult
-- | VkFenceCreateInfo - Structure specifying parameters of a newly created
-- fence
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_FENCE_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo'
--     or
--     'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.VkExportFenceWin32HandleInfoKHR'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be a valid combination of 'VkFenceCreateFlagBits'
--     values
--
-- = See Also
-- #_see_also#
--
-- 'VkFenceCreateFlags', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateFence'
data VkFenceCreateInfo = VkFenceCreateInfo
  { -- No documentation found for Nested "VkFenceCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFenceCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFenceCreateInfo" "vkFlags"
  vkFlags :: VkFenceCreateFlags
  }
  deriving (Eq, Show)

instance Storable VkFenceCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkFenceCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkFenceCreateInfo))
-- | VkFenceCreateFlags - Bitmask of VkFenceCreateFlagBits
--
-- = Description
-- #_description#
--
-- @VkFenceCreateFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkFenceCreateFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkFenceCreateFlagBits', 'VkFenceCreateInfo'
type VkFenceCreateFlags = VkFenceCreateFlagBits
