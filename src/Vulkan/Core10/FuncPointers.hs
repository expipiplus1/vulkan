{-# language CPP #-}
module Vulkan.Core10.FuncPointers  ( PFN_vkInternalAllocationNotification
                                   , FN_vkInternalAllocationNotification
                                   , PFN_vkInternalFreeNotification
                                   , FN_vkInternalFreeNotification
                                   , PFN_vkReallocationFunction
                                   , FN_vkReallocationFunction
                                   , PFN_vkAllocationFunction
                                   , FN_vkAllocationFunction
                                   , PFN_vkFreeFunction
                                   , FN_vkFreeFunction
                                   , PFN_vkVoidFunction
                                   , FN_vkVoidFunction
                                   ) where

import Foreign.C.Types (CSize)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Enums.InternalAllocationType (InternalAllocationType)
import Vulkan.Core10.Enums.SystemAllocationScope (SystemAllocationScope)
type FN_vkInternalAllocationNotification = ("pUserData" ::: Ptr ()) -> CSize -> InternalAllocationType -> SystemAllocationScope -> IO ()
-- | PFN_vkInternalAllocationNotification - Application-defined memory
-- allocation notification function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'::@pUserData@
--     in the allocator specified by the application.
--
-- -   @size@ is the requested size of an allocation.
--
-- -   @allocationType@ is a
--     'Vulkan.Core10.Enums.InternalAllocationType.InternalAllocationType'
--     value specifying the requested type of an allocation.
--
-- -   @allocationScope@ is a
--     'Vulkan.Core10.Enums.SystemAllocationScope.SystemAllocationScope'
--     value specifying the allocation scope of the lifetime of the
--     allocation, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-host-allocation-scope here>.
--
-- = Description
--
-- This is a purely informational callback.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
type PFN_vkInternalAllocationNotification = FunPtr FN_vkInternalAllocationNotification


type FN_vkInternalFreeNotification = ("pUserData" ::: Ptr ()) -> CSize -> InternalAllocationType -> SystemAllocationScope -> IO ()
-- | PFN_vkInternalFreeNotification - Application-defined memory free
-- notification function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'::@pUserData@
--     in the allocator specified by the application.
--
-- -   @size@ is the requested size of an allocation.
--
-- -   @allocationType@ is a
--     'Vulkan.Core10.Enums.InternalAllocationType.InternalAllocationType'
--     value specifying the requested type of an allocation.
--
-- -   @allocationScope@ is a
--     'Vulkan.Core10.Enums.SystemAllocationScope.SystemAllocationScope'
--     value specifying the allocation scope of the lifetime of the
--     allocation, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-host-allocation-scope here>.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
type PFN_vkInternalFreeNotification = FunPtr FN_vkInternalFreeNotification


type FN_vkReallocationFunction = ("pUserData" ::: Ptr ()) -> ("pOriginal" ::: Ptr ()) -> CSize -> ("alignment" ::: CSize) -> SystemAllocationScope -> IO (Ptr ())
-- | PFN_vkReallocationFunction - Application-defined memory reallocation
-- function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'::@pUserData@
--     in the allocator specified by the application.
--
-- -   @pOriginal@ /must/ be either @NULL@ or a pointer previously returned
--     by @pfnReallocation@ or @pfnAllocation@ of a compatible allocator.
--
-- -   @size@ is the size in bytes of the requested allocation.
--
-- -   @alignment@ is the requested alignment of the allocation in bytes
--     and /must/ be a power of two.
--
-- -   @allocationScope@ is a
--     'Vulkan.Core10.Enums.SystemAllocationScope.SystemAllocationScope'
--     value specifying the allocation scope of the lifetime of the
--     allocation, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-host-allocation-scope here>.
--
-- = Description
--
-- @pfnReallocation@ /must/ return an allocation with enough space for
-- @size@ bytes, and the contents of the original allocation from bytes
-- zero to min(original size, new size) - 1 /must/ be preserved in the
-- returned allocation. If @size@ is larger than the old size, the contents
-- of the additional space are undefined. If satisfying these requirements
-- involves creating a new allocation, then the old allocation /should/ be
-- freed.
--
-- If @pOriginal@ is @NULL@, then @pfnReallocation@ /must/ behave
-- equivalently to a call to 'PFN_vkAllocationFunction' with the same
-- parameter values (without @pOriginal@).
--
-- If @size@ is zero, then @pfnReallocation@ /must/ behave equivalently to
-- a call to 'PFN_vkFreeFunction' with the same @pUserData@ parameter
-- value, and @pMemory@ equal to @pOriginal@.
--
-- If @pOriginal@ is non-@NULL@, the implementation /must/ ensure that
-- @alignment@ is equal to the @alignment@ used to originally allocate
-- @pOriginal@.
--
-- If this function fails and @pOriginal@ is non-@NULL@ the application
-- /must/ not free the old allocation.
--
-- @pfnReallocation@ /must/ follow the same
-- <vkAllocationFunction_return_rules.html rules for return values as >.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
type PFN_vkReallocationFunction = FunPtr FN_vkReallocationFunction


type FN_vkAllocationFunction = ("pUserData" ::: Ptr ()) -> CSize -> ("alignment" ::: CSize) -> SystemAllocationScope -> IO (Ptr ())
-- | PFN_vkAllocationFunction - Application-defined memory allocation
-- function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'::@pUserData@
--     in the allocator specified by the application.
--
-- -   @size@ is the size in bytes of the requested allocation.
--
-- -   @alignment@ is the requested alignment of the allocation in bytes
--     and /must/ be a power of two.
--
-- -   @allocationScope@ is a
--     'Vulkan.Core10.Enums.SystemAllocationScope.SystemAllocationScope'
--     value specifying the allocation scope of the lifetime of the
--     allocation, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-host-allocation-scope here>.
--
-- = Description
--
-- If @pfnAllocation@ is unable to allocate the requested memory, it /must/
-- return @NULL@. If the allocation was successful, it /must/ return a
-- valid pointer to memory allocation containing at least @size@ bytes, and
-- with the pointer value being a multiple of @alignment@.
--
-- Note
--
-- Correct Vulkan operation /cannot/ be assumed if the application does not
-- follow these rules.
--
-- For example, @pfnAllocation@ (or @pfnReallocation@) could cause
-- termination of running Vulkan instance(s) on a failed allocation for
-- debugging purposes, either directly or indirectly. In these
-- circumstances, it /cannot/ be assumed that any part of any affected
-- 'Vulkan.Core10.Handles.Instance' objects are going to operate correctly
-- (even 'Vulkan.Core10.DeviceInitialization.destroyInstance'), and the
-- application /must/ ensure it cleans up properly via other means (e.g.
-- process termination).
--
-- If @pfnAllocation@ returns @NULL@, and if the implementation is unable
-- to continue correct processing of the current command without the
-- requested allocation, it /must/ treat this as a run-time error, and
-- generate 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' at the
-- appropriate time for the command in which the condition was detected, as
-- described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Return Codes>.
--
-- If the implementation is able to continue correct processing of the
-- current command without the requested allocation, then it /may/ do so,
-- and /must/ not generate
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' as a result of
-- this failed allocation.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
type PFN_vkAllocationFunction = FunPtr FN_vkAllocationFunction


type FN_vkFreeFunction = ("pUserData" ::: Ptr ()) -> ("pMemory" ::: Ptr ()) -> IO ()
-- | PFN_vkFreeFunction - Application-defined memory free function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'::@pUserData@
--     in the allocator specified by the application.
--
-- -   @pMemory@ is the allocation to be freed.
--
-- = Description
--
-- @pMemory@ /may/ be @NULL@, which the callback /must/ handle safely. If
-- @pMemory@ is non-@NULL@, it /must/ be a pointer previously allocated by
-- @pfnAllocation@ or @pfnReallocation@. The application /should/ free this
-- memory.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
type PFN_vkFreeFunction = FunPtr FN_vkFreeFunction


type FN_vkVoidFunction = () -> IO ()
-- | PFN_vkVoidFunction - Dummy function pointer type returned by queries
--
-- = See Also
--
-- 'Vulkan.Core10.DeviceInitialization.getDeviceProcAddr',
-- 'Vulkan.Core10.DeviceInitialization.getInstanceProcAddr'
type PFN_vkVoidFunction = FunPtr FN_vkVoidFunction

