{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateFlagBits(..)
  , pattern VK_FENCE_CREATE_SIGNALED_BIT
  , VkFenceCreateFlags
  , VkFenceCreateInfo(..)
  , FN_vkCreateFence
  , PFN_vkCreateFence
  , vkCreateFence
  , FN_vkDestroyFence
  , PFN_vkDestroyFence
  , vkDestroyFence
  , FN_vkGetFenceStatus
  , PFN_vkGetFenceStatus
  , vkGetFenceStatus
  , FN_vkResetFences
  , PFN_vkResetFences
  , vkResetFences
  , FN_vkWaitForFences
  , PFN_vkWaitForFences
  , vkWaitForFences
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkFenceCreateFlagBits

-- | VkFenceCreateFlagBits - Bitmask specifying initial state and behavior of
-- a fence
--
-- = See Also
--
-- 'VkFenceCreateFlags'
newtype VkFenceCreateFlagBits = VkFenceCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- | 'VK_FENCE_CREATE_SIGNALED_BIT' specifies that the fence object is
-- created in the signaled state. Otherwise, it is created in the
-- unsignaled state.
pattern VK_FENCE_CREATE_SIGNALED_BIT :: VkFenceCreateFlagBits
pattern VK_FENCE_CREATE_SIGNALED_BIT = VkFenceCreateFlagBits 0x00000001

-- | VkFenceCreateFlags - Bitmask of VkFenceCreateFlagBits
--
-- = Description
--
-- 'VkFenceCreateFlags' is a bitmask type for setting a mask of zero or
-- more 'VkFenceCreateFlagBits'.
--
-- = See Also
--
-- 'VkFenceCreateFlagBits', 'VkFenceCreateInfo'
type VkFenceCreateFlags = VkFenceCreateFlagBits

-- | VkFenceCreateInfo - Structure specifying parameters of a newly created
-- fence
--
-- = Description
--
-- Unresolved directive in VkFenceCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkFenceCreateInfo.txt[]
--
-- = See Also
--
-- 'VkFenceCreateFlags', 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateFence'
data VkFenceCreateInfo = VkFenceCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkFenceCreateFlagBits' specifying the initial
  -- state and behavior of the fence.
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

instance Zero VkFenceCreateInfo where
  zero = VkFenceCreateInfo VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
                           zero
                           zero

-- | vkCreateFence - Create a new fence object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the fence.
--
-- -   @pCreateInfo@ is a pointer to an instance of the 'VkFenceCreateInfo'
--     structure which contains information about how the fence is to be
--     created.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pFence@ points to a handle in which the resulting fence object is
--     returned.
--
-- = Description
--
-- Unresolved directive in vkCreateFence.txt -
-- include::{generated}\/validity\/protos\/vkCreateFence.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence', 'VkFenceCreateInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateFence" vkCreateFence :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
#else
vkCreateFence :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
vkCreateFence deviceCmds = mkVkCreateFence (pVkCreateFence deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateFence
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
#endif

type FN_vkCreateFence = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
type PFN_vkCreateFence = FunPtr FN_vkCreateFence

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
-- Unresolved directive in vkDestroyFence.txt -
-- include::{generated}\/validity\/protos\/vkDestroyFence.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyFence" vkDestroyFence :: ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyFence :: DeviceCmds -> ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyFence deviceCmds = mkVkDestroyFence (pVkDestroyFence deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyFence
  :: FunPtr (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyFence = ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyFence = FunPtr FN_vkDestroyFence

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
-- Upon success, 'vkGetFenceStatus' returns the status of the fence object,
-- with the following return codes:
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
-- 'vkGetFenceStatus' /may/ return any of the above status codes. If the
-- device has been lost and 'vkGetFenceStatus' is called repeatedly, it
-- will eventually return either 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
-- or 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'.
--
-- Unresolved directive in vkGetFenceStatus.txt -
-- include::{generated}\/validity\/protos\/vkGetFenceStatus.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetFenceStatus" vkGetFenceStatus :: ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult
#else
vkGetFenceStatus :: DeviceCmds -> ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult
vkGetFenceStatus deviceCmds = mkVkGetFenceStatus (pVkGetFenceStatus deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceStatus
  :: FunPtr (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult)
#endif

type FN_vkGetFenceStatus = ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult
type PFN_vkGetFenceStatus = FunPtr FN_vkGetFenceStatus

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
-- If any member of @pFences@ currently has its
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-importing payload imported>
-- with temporary permanence, that fence’s prior permanent payload is first
-- restored. The remaining operations described therefore operate on the
-- restored payload.
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
-- Unresolved directive in vkResetFences.txt -
-- include::{generated}\/validity\/protos\/vkResetFences.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetFences" vkResetFences :: ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult
#else
vkResetFences :: DeviceCmds -> ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult
vkResetFences deviceCmds = mkVkResetFences (pVkResetFences deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetFences
  :: FunPtr (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult)
#endif

type FN_vkResetFences = ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult
type PFN_vkResetFences = FunPtr FN_vkResetFences

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
-- If the condition is satisfied when 'vkWaitForFences' is called, then
-- 'vkWaitForFences' returns immediately. If the condition is not satisfied
-- at the time 'vkWaitForFences' is called, then 'vkWaitForFences' will
-- block and wait up to @timeout@ nanoseconds for the condition to become
-- satisfied.
--
-- If @timeout@ is zero, then 'vkWaitForFences' does not wait, but simply
-- returns the current state of the fences.
-- 'Graphics.Vulkan.C.Core10.Core.VK_TIMEOUT' will be returned in this case
-- if the condition is not satisfied, even though no actual wait was
-- performed.
--
-- If the specified timeout period expires before the condition is
-- satisfied, 'vkWaitForFences' returns
-- 'Graphics.Vulkan.C.Core10.Core.VK_TIMEOUT'. If the condition is
-- satisfied before @timeout@ nanoseconds has expired, 'vkWaitForFences'
-- returns 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'.
--
-- If device loss occurs (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>)
-- before the timeout has expired, 'vkWaitForFences' /must/ return in
-- finite time with either 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' or
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'.
--
-- __Note__
--
-- While we guarantee that 'vkWaitForFences' /must/ return in finite time,
-- no guarantees are made that it returns immediately upon device loss.
-- However, the client can reasonably expect that the delay will be on the
-- order of seconds and that calling 'vkWaitForFences' will not result in a
-- permanently (or seemingly permanently) dead process.
--
-- Unresolved directive in vkWaitForFences.txt -
-- include::{generated}\/validity\/protos\/vkWaitForFences.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkWaitForFences" vkWaitForFences :: ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult
#else
vkWaitForFences :: DeviceCmds -> ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult
vkWaitForFences deviceCmds = mkVkWaitForFences (pVkWaitForFences deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWaitForFences
  :: FunPtr (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult) -> (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult)
#endif

type FN_vkWaitForFences = ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult
type PFN_vkWaitForFences = FunPtr FN_vkWaitForFences
