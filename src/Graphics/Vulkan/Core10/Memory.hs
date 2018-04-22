{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Memory
  ( VkMemoryMapFlags(..)
  , VkDeviceMemory
  , vkAllocateMemory
  , vkFreeMemory
  , vkMapMemory
  , vkUnmapMemory
  , vkFlushMappedMemoryRanges
  , vkInvalidateMappedMemoryRanges
  , vkGetDeviceMemoryCommitment
  , VkMemoryAllocateInfo(..)
  , VkMappedMemoryRange(..)
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
  , VkDeviceSize
  )


-- ** VkMemoryMapFlags

-- | VkMemoryMapFlags - Reserved for future use
--
-- = Description
--
-- @VkMemoryMapFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'vkMapMemory'
newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkMemoryMapFlags where
  
  showsPrec p (VkMemoryMapFlags x) = showParen (p >= 11) (showString "VkMemoryMapFlags " . showsPrec 11 x)

instance Read VkMemoryMapFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryMapFlags")
                        v <- step readPrec
                        pure (VkMemoryMapFlags v)
                        )
                    )


-- | Dummy data to tag the 'Ptr' with
data VkDeviceMemory_T
-- | VkDeviceMemory - Opaque handle to a device memory object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'VkMappedMemoryRange',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkMemoryGetAndroidHardwareBufferInfoANDROID',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.VkMemoryGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkMemoryGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex.VkWin32KeyedMutexAcquireReleaseInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex.VkWin32KeyedMutexAcquireReleaseInfoNV',
-- 'vkAllocateMemory',
-- 'Graphics.Vulkan.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.Core10.MemoryManagement.vkBindImageMemory',
-- 'vkFreeMemory', 'vkGetDeviceMemoryCommitment',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV',
-- 'vkMapMemory', 'vkUnmapMemory'
type VkDeviceMemory = Ptr VkDeviceMemory_T
-- | vkAllocateMemory - Allocate device memory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @pAllocateInfo@ is a pointer to an instance of the
--     'VkMemoryAllocateInfo' structure describing parameters of the
--     allocation. A successful returned allocation /must/ use the
--     requested parameters — no substitution is permitted by the
--     implementation.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pMemory@ is a pointer to a @VkDeviceMemory@ handle in which
--     information about the allocated memory is returned.
--
-- = Description
--
-- Allocations returned by @vkAllocateMemory@ are guaranteed to meet any
-- alignment requirement of the implementation. For example, if an
-- implementation requires 128 byte alignment for images and 64 byte
-- alignment for buffers, the device memory returned through this mechanism
-- would be 128-byte aligned. This ensures that applications /can/
-- correctly suballocate objects of different types (with potentially
-- different alignment requirements) in the same memory object.
--
-- When memory is allocated, its contents are undefined.
--
-- The maximum number of valid memory allocations that /can/ exist
-- simultaneously within a
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice' /may/ be
-- restricted by implementation- or platform-dependent limits. If a call to
-- 'vkAllocateMemory' would cause the total number of allocations to exceed
-- these limits, such a call will fail and /must/ return
-- @VK_ERROR_TOO_MANY_OBJECTS@. The
-- [@maxMemoryAllocationCount@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-maxMemoryAllocationCount)
-- feature describes the number of allocations that /can/ exist
-- simultaneously before encountering these internal limits.
--
-- Some platforms /may/ have a limit on the maximum size of a single
-- allocation. For example, certain systems /may/ fail to create
-- allocations with a size greater than or equal to 4GB. Such a limit is
-- implementation-dependent, and if such a failure occurs then the error
-- @VK_ERROR_OUT_OF_DEVICE_MEMORY@ /must/ be returned. This limit is
-- advertised in
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties'::@maxMemoryAllocationSize@.
--
-- == Valid Usage
--
-- -   @pAllocateInfo@->@allocationSize@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'::@memoryHeaps@[@pAllocateInfo@->@memoryTypeIndex@].@size@
--     as returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceMemoryProperties'
--     for the
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice' that
--     @device@ was created from.
--
-- -   @pAllocateInfo@->@memoryTypeIndex@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'::@memoryTypeCount@
--     as returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceMemoryProperties'
--     for the
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice' that
--     @device@ was created from.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pAllocateInfo@ /must/ be a valid pointer to a valid
--     @VkMemoryAllocateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pMemory@ /must/ be a valid pointer to a @VkDeviceMemory@ handle
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
--     -   @VK_ERROR_TOO_MANY_OBJECTS@
--
--     -   @VK_ERROR_INVALID_EXTERNAL_HANDLE@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkDeviceMemory', 'VkMemoryAllocateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAllocateMemory" vkAllocateMemory :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
-- | vkFreeMemory - Free device memory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the @VkDeviceMemory@ object to be freed.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- = Description
--
-- Before freeing a memory object, an application /must/ ensure the memory
-- object is no longer in use by the device—​for example by command buffers
-- in the /pending state/. The memory /can/ remain bound to images or
-- buffers at the time the memory object is freed, but any further use of
-- them (on host or device) for anything other than destroying those
-- objects will result in undefined behavior. If there are still any bound
-- images or buffers, the memory /may/ not be immediately released by the
-- implementation, but /must/ be released by the time all bound images and
-- buffers have been destroyed. Once memory is released, it is returned to
-- the heap from which it was allocated.
--
-- How memory objects are bound to Images and Buffers is described in
-- detail in the [Resource Memory
-- Association](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-association)
-- section.
--
-- If a memory object is mapped at the time it is freed, it is implicitly
-- unmapped.
--
-- __Note__
--
-- As described
-- [below](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-device-unmap-does-not-flush),
-- host writes are not implicitly flushed when the memory object is
-- unmapped, but the implementation /must/ guarantee that writes that have
-- not been flushed do not affect any other memory.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @memory@ (via images or
--     buffers) /must/ have completed execution
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @memory@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @memory@ /must/
--     be a valid @VkDeviceMemory@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @memory@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @memory@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkDeviceMemory'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFreeMemory" vkFreeMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkMapMemory - Map a memory object into application address space
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the @VkDeviceMemory@ object to be mapped.
--
-- -   @offset@ is a zero-based byte offset from the beginning of the
--     memory object.
--
-- -   @size@ is the size of the memory range to map, or @VK_WHOLE_SIZE@ to
--     map from @offset@ to the end of the allocation.
--
-- -   @flags@ is reserved for future use.
--
-- -   @ppData@ points to a pointer in which is returned a host-accessible
--     pointer to the beginning of the mapped range. This pointer minus
--     @offset@ /must/ be aligned to at least
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@minMemoryMapAlignment@.
--
-- = Description
--
-- It is an application error to call @vkMapMemory@ on a memory object that
-- is already mapped.
--
-- __Note__
--
-- @vkMapMemory@ will fail if the implementation is unable to allocate an
-- appropriately sized contiguous virtual address range, e.g. due to
-- virtual address space fragmentation or platform limits. In such cases,
-- @vkMapMemory@ /must/ return @VK_ERROR_MEMORY_MAP_FAILED@. The
-- application /can/ improve the likelihood of success by reducing the size
-- of the mapped range and\/or removing unneeded mappings using
-- @VkUnmapMemory@.
--
-- @vkMapMemory@ does not check whether the device memory is currently in
-- use before returning the host-accessible pointer. The application /must/
-- guarantee that any previously submitted command that writes to this
-- range has completed before the host reads from or writes to that range,
-- and that any previously submitted command that reads from that range has
-- completed before the host writes to that region (see
-- [here](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-host-writes)
-- for details on fulfilling such a guarantee). If the device memory was
-- allocated without the @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@ set, these
-- guarantees /must/ be made for an extended range: the application /must/
-- round down the start of the range to the nearest multiple of
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@,
-- and round the end of the range up to the nearest multiple of
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@.
--
-- While a range of device memory is mapped for host access, the
-- application is responsible for synchronizing both device and host access
-- to that memory range.
--
-- __Note__
--
-- It is important for the application developer to become meticulously
-- familiar with all of the mechanisms described in the chapter on
-- [Synchronization and Cache
-- Control](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization)
-- as they are crucial to maintaining memory access ordering.
--
-- == Valid Usage
--
-- -   @memory@ /must/ not be currently mapped
--
-- -   @offset@ /must/ be less than the size of @memory@
--
-- -   If @size@ is not equal to @VK_WHOLE_SIZE@, @size@ /must/ be greater
--     than @0@
--
-- -   If @size@ is not equal to @VK_WHOLE_SIZE@, @size@ /must/ be less
--     than or equal to the size of the @memory@ minus @offset@
--
-- -   @memory@ /must/ have been created with a memory type that reports
--     @VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT@
--
-- -   @memory@ /must/ not have been allocated with multiple instances.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- -   @flags@ /must/ be @0@
--
-- -   @ppData@ /must/ be a valid pointer to a pointer value
--
-- -   @memory@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @memory@ /must/ be externally synchronized
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
--     -   @VK_ERROR_MEMORY_MAP_FAILED@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkDeviceMemory', @VkDeviceSize@, 'VkMemoryMapFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkMapMemory" vkMapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
-- | vkUnmapMemory - Unmap a previously mapped memory object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the memory object to be unmapped.
--
-- == Valid Usage
--
-- -   @memory@ /must/ be currently mapped
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- -   @memory@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @memory@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkDeviceMemory'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUnmapMemory" vkUnmapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
-- | vkFlushMappedMemoryRanges - Flush mapped memory ranges
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory ranges.
--
-- -   @memoryRangeCount@ is the length of the @pMemoryRanges@ array.
--
-- -   @pMemoryRanges@ is a pointer to an array of 'VkMappedMemoryRange'
--     structures describing the memory ranges to flush.
--
-- = Description
--
-- @vkFlushMappedMemoryRanges@ guarantees that host writes to the memory
-- ranges described by @pMemoryRanges@ /can/ be made available to device
-- access, via [availability
-- operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-available-and-visible)
-- from the @VK_ACCESS_HOST_WRITE_BIT@ [access
-- type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-types).
--
-- Within each range described by @pMemoryRanges@, each set of
-- @nonCoherentAtomSize@ bytes in that range is flushed if any byte in that
-- set has been written by the host since it was first mapped, or the last
-- time it was flushed. If @pMemoryRanges@ includes sets of
-- @nonCoherentAtomSize@ bytes where no bytes have been written by the
-- host, those bytes /must/ not be flushed.
--
-- Unmapping non-coherent memory does not implicitly flush the mapped
-- memory, and host writes that have not been flushed /may/ not ever be
-- visible to the device. However, implementations /must/ ensure that
-- writes that have not been flushed do not become visible to any other
-- memory.
--
-- __Note__
--
-- The above guarantee avoids a potential memory corruption in scenarios
-- where host writes to a mapped memory object have not been flushed before
-- the memory is unmapped (or freed), and the virtual address range is
-- subsequently reused for a different mapping (or memory allocation).
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pMemoryRanges@ /must/ be a valid pointer to an array of
--     @memoryRangeCount@ valid @VkMappedMemoryRange@ structures
--
-- -   @memoryRangeCount@ /must/ be greater than @0@
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkMappedMemoryRange'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFlushMappedMemoryRanges" vkFlushMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
-- | vkInvalidateMappedMemoryRanges - Invalidate ranges of mapped memory
-- objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory ranges.
--
-- -   @memoryRangeCount@ is the length of the @pMemoryRanges@ array.
--
-- -   @pMemoryRanges@ is a pointer to an array of 'VkMappedMemoryRange'
--     structures describing the memory ranges to invalidate.
--
-- = Description
--
-- @vkInvalidateMappedMemoryRanges@ guarantees that device writes to the
-- memory ranges described by @pMemoryRanges@, which have been made visible
-- to the @VK_ACCESS_HOST_WRITE_BIT@ and @VK_ACCESS_HOST_READ_BIT@ [access
-- types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-types),
-- are made visible to the host. If a range of non-coherent memory is
-- written by the host and then invalidated without first being flushed,
-- its contents are undefined.
--
-- Within each range described by @pMemoryRanges@, each set of
-- @nonCoherentAtomSize@ bytes in that range is invalidated if any byte in
-- that set has been written by the device since it was first mapped, or
-- the last time it was invalidated.
--
-- __Note__
--
-- Mapping non-coherent memory does not implicitly invalidate the mapped
-- memory, and device writes that have not been invalidated /must/ be made
-- visible before the host reads or overwrites them.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pMemoryRanges@ /must/ be a valid pointer to an array of
--     @memoryRangeCount@ valid @VkMappedMemoryRange@ structures
--
-- -   @memoryRangeCount@ /must/ be greater than @0@
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkMappedMemoryRange'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkInvalidateMappedMemoryRanges" vkInvalidateMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
-- | vkGetDeviceMemoryCommitment - Query the current commitment for a
-- VkDeviceMemory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the memory object being queried.
--
-- -   @pCommittedMemoryInBytes@ is a pointer to a @VkDeviceSize@ value in
--     which the number of bytes currently committed is returned, on
--     success.
--
-- = Description
--
-- The implementation /may/ update the commitment at any time, and the
-- value returned by this query /may/ be out of date.
--
-- The implementation guarantees to allocate any committed memory from the
-- heapIndex indicated by the memory type that the memory object was
-- created with.
--
-- == Valid Usage
--
-- -   @memory@ /must/ have been created with a memory type that reports
--     @VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- -   @pCommittedMemoryInBytes@ /must/ be a valid pointer to a
--     @VkDeviceSize@ value
--
-- -   @memory@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkDeviceMemory', @VkDeviceSize@
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceMemoryCommitment" vkGetDeviceMemoryCommitment :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
-- | VkMemoryAllocateInfo - Structure containing parameters of a memory
-- allocation
--
-- = Description
--
-- An instance of the 'VkMemoryAllocateInfo' structure defines a memory
-- import operation if the @pNext@ chain contains an instance of one of the
-- following structures: *
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR'
-- with non-zero @handleType@ value *
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.VkImportMemoryFdInfoKHR'
-- with a non-zero @handleType@ value *
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.VkImportMemoryHostPointerInfoEXT'
-- with a non-zero @handleType@ value *
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkImportAndroidHardwareBufferInfoANDROID'
-- with a non-@NULL@ @buffer@ value
--
-- Importing memory /must/ not modify the content of the memory.
-- Implementations /must/ ensure that importing memory does not enable the
-- importing Vulkan instance to access any memory or resources in other
-- Vulkan instances other than that corresponding to the memory object
-- imported. Implementations /must/ also ensure accessing imported memory
-- which has not been initialized does not allow the importing Vulkan
-- instance to obtain data from the exporting Vulkan instance or
-- vice-versa.
--
-- __Note__
--
-- How exported and imported memory is isolated is left to the
-- implementation, but applications should be aware that such isolation
-- /may/ prevent implementations from placing multiple exportable memory
-- objects in the same physical or virtual page. Hence, applications
-- /should/ avoid creating many small external memory objects whenever
-- possible.
--
-- When performing a memory import operation, it is the responsibility of
-- the application to ensure the external handles meet all valid usage
-- requirements. However, implementations /must/ perform sufficient
-- validation of external handles to ensure that the operation results in a
-- valid memory object which will not cause program termination, device
-- loss, queue stalls, or corruption of other resources when used as
-- allowed according to its allocation parameters. If the external handle
-- provided does not meet these requirements, the implementation /must/
-- fail the memory import operation with the error code
-- @VK_ERROR_INVALID_EXTERNAL_HANDLE@.
--
-- == Valid Usage
--
-- -   If the @pNext@ chain contains an instance of
--     @VkExportMemoryAllocateInfo@, and any of the handle types specified
--     in @VkExportMemoryAllocateInfo@::@handleTypes@ require a dedicated
--     allocation, as reported by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--     in
--     @VkExternalImageFormatProperties@::@externalMemoryProperties@::@externalMemoryFeatures@
--     or
--     @VkExternalBufferProperties@::@externalMemoryProperties@::@externalMemoryFeatures@,
--     the @pNext@ chain must contain an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     or
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'
--     with either its @image@ or @buffer@ field set to a value other than
--     @VK_NULL_HANDLE@.
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
--     it /must/ not contain an instance of
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExportMemoryAllocateInfoNV'
--     or
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.VkExportMemoryWin32HandleInfoNV'.
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR',
--     it /must/ not contain an instance of
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.VkImportMemoryWin32HandleInfoNV'.
--
-- -   If the parameters define an import operation, the external handle
--     specified was created by the Vulkan API, and the external handle
--     type is @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR@, then the
--     values of @allocationSize@ and @memoryTypeIndex@ /must/ match those
--     specified when the memory object being imported was created.
--
-- -   If the parameters define an import operation and the external handle
--     specified was created by the Vulkan API, the device mask specified
--     by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo'
--     /must/ match that specified when the memory object being imported
--     was allocated.
--
-- -   If the parameters define an import operation and the external handle
--     specified was created by the Vulkan API, the list of physical
--     devices that comprise the logical device passed to
--     'vkAllocateMemory' /must/ match the list of physical devices that
--     comprise the logical device on which the memory was originally
--     allocated.
--
-- -   If the parameters define an import operation and the external handle
--     is an NT handle or a global share handle created outside of the
--     Vulkan API, the value of @memoryTypeIndex@ /must/ be one of those
--     returned by
--     'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandlePropertiesKHR'.
--
-- -   If the parameters define an import operation, the external handle
--     was created by the Vulkan API, and the external handle type is
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR@ or
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR@, then the
--     values of @allocationSize@ and @memoryTypeIndex@ /must/ match those
--     specified when the memory object being imported was created.
--
-- -   If the parameters define an import operation and the external handle
--     type is @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT@, or
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT@,
--     @allocationSize@ /must/ match the size reported in the memory
--     requirements of the @image@ or @buffer@ member of the instance of
--     @VkDedicatedAllocationMemoryAllocateInfoNV@ included in the @pNext@
--     chain.
--
-- -   If the parameters define an import operation and the external handle
--     type is @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT@,
--     @allocationSize@ /must/ match the size specified when creating the
--     Direct3D 12 heap from which the external handle was extracted.
--
-- -   If the parameters define an import operation and the external handle
--     is a POSIX file descriptor created outside of the Vulkan API, the
--     value of @memoryTypeIndex@ /must/ be one of those returned by
--     'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdPropertiesKHR'.
--
-- -   If the parameters define an import operation and the external handle
--     is a host pointer, the value of @memoryTypeIndex@ /must/ be one of
--     those returned by
--     'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.vkGetMemoryHostPointerPropertiesEXT'
--
-- -   If the parameters define an import operation and the external handle
--     is a host pointer, @allocationSize@ /must/ be an integer multiple of
--     @VkPhysicalDeviceExternalMemoryHostPropertiesEXT@::@minImportedHostPointerAlignment@
--
-- -   If the parameters define an import operation and the external handle
--     type is
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BIT_ANDROID@:
--
--     -   @allocationSize@ /must/ be the size returned by
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID'
--         for the Android hardware buffer
--
--     -   If the @pNext@ chain doesn’t contain an instance of
--         'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--         or @VkMemoryDedicatedAllocateInfo@::@image@ is
--         'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', the Android
--         hardware buffer /must/ have a format of
--         @AHARDWAREBUFFER_FORMAT_BLOB@ and a usage that includes
--         @AHARDWAREBUFFER_USAGE_GPU_DATA_BUFFER@
--
--     -   @memoryTypeIndex@ /must/ be one of those returned by
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID'
--         for the Android hardware buffer
--
-- -   If the parameters do not define an import operation, and the @pNext@
--     chain contains an instance of @VkExportMemoryAllocateInfo@ with
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID@
--     included in its @handleTypes@ member, and the @pNext@ contains an
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with @image@ not equal to
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', then
--     @allocationSize@ /must/ be @0@, otherwise @allocationSize@ /must/ be
--     greater than @0@.
--
-- -   If the parameters define an import operation, the external handle is
--     an Android hardware buffer, and the @pNext@ chain includes an
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with @image@ that is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE':
--
--     -   The Android hardware buffer’s usage /must/ include at least one
--         of @AHARDWAREBUFFER_USAGE_GPU_COLOR_OUTPUT@ or
--         @AHARDWAREBUFFER_USAGE_GPU_SAMPLED_IMAGE@
--
--     -   The @image@’s format /must/ be @VK_FORMAT_UNDEFINED@ or the
--         format returned by
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID'
--         in
--         'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID'::@format@
--         for the Android hardware buffer.
--
--     -   The image’s and Android hardware buffer’s width, height, and
--         array layer dimensions /must/ be the same
--
--     -   If the Android hardware buffer’s usage includes
--         @AHARDWAREBUFFER_USAGE_GPU_MIPMAP_COMPLETE@, the image must have
--         ⌊log2(max(@width@, @height@))⌋ + 1 mip levels, otherwise it must
--         have exactly @1@ mip level.
--
--     -   Each bit set in the image’s usage /must/ be listed in
--         [AHardwareBuffer Usage
--         Equivalence](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-external-android-hardware-buffer-usage),
--         and if there is a corresponding @AHARDWAREBUFFER_USAGE@ bit
--         listed that bit /must/ be included in the Android hardware
--         buffer’s usage
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV',
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExportMemoryAllocateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkExportMemoryWin32HandleInfoKHR',
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.VkExportMemoryWin32HandleInfoNV',
--     'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkImportAndroidHardwareBufferInfoANDROID',
--     'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.VkImportMemoryFdInfoKHR',
--     'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.VkImportMemoryHostPointerInfoEXT',
--     'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR',
--     'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.VkImportMemoryWin32HandleInfoNV',
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo',
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- @VkDeviceSize@, 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkAllocateMemory'
data VkMemoryAllocateInfo = VkMemoryAllocateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @allocationSize@ is the size of the allocation in bytes
  vkAllocationSize :: VkDeviceSize
  , -- | @memoryTypeIndex@ is an index identifying a memory type from the
  -- @memoryTypes@ array of the
  -- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'
  -- structure
  vkMemoryTypeIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryAllocateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkAllocationSize (poked :: VkMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkMemoryTypeIndex (poked :: VkMemoryAllocateInfo))
-- | VkMappedMemoryRange - Structure specifying a mapped memory range
--
-- == Valid Usage
--
-- -   @memory@ /must/ be currently mapped
--
-- -   If @size@ is not equal to @VK_WHOLE_SIZE@, @offset@ and @size@
--     /must/ specify a range contained within the currently mapped range
--     of @memory@
--
-- -   If @size@ is equal to @VK_WHOLE_SIZE@, @offset@ /must/ be within the
--     currently mapped range of @memory@
--
-- -   If @size@ is equal to @VK_WHOLE_SIZE@, the end of the current
--     mapping of @memory@ /must/ be a multiple of
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@
--     bytes from the beginning of the memory object.
--
-- -   @offset@ /must/ be a multiple of
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@
--
-- -   If @size@ is not equal to @VK_WHOLE_SIZE@, @size@ /must/ either be a
--     multiple of
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@,
--     or @offset@ plus @size@ /must/ equal the size of @memory@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- = See Also
--
-- 'VkDeviceMemory', @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkFlushMappedMemoryRanges', 'vkInvalidateMappedMemoryRanges'
data VkMappedMemoryRange = VkMappedMemoryRange
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @memory@ is the memory object to which this range belongs.
  vkMemory :: VkDeviceMemory
  , -- | @offset@ is the zero-based byte offset from the beginning of the memory
  -- object.
  vkOffset :: VkDeviceSize
  , -- | @size@ is either the size of range, or @VK_WHOLE_SIZE@ to affect the
  -- range from @offset@ to the end of the current mapping of the allocation.
  vkSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkMappedMemoryRange where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkMappedMemoryRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 24) (vkOffset (poked :: VkMappedMemoryRange))
                *> poke (ptr `plusPtr` 32) (vkSize (poked :: VkMappedMemoryRange))
