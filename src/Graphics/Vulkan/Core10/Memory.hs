{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  , withCStructMappedMemoryRange
  , fromCStructMappedMemoryRange
  , MappedMemoryRange(..)
  , withCStructMemoryAllocateInfo
  , fromCStructMemoryAllocateInfo
  , MemoryAllocateInfo(..)
  , MemoryMapFlags
  , allocateMemory
  , flushMappedMemoryRanges
  , freeMemory
  , getDeviceMemoryCommitment
  , invalidateMappedMemoryRanges
  , mapMemory
  , unmapMemory
  , withMappedMemory
  , withMemory
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
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
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  , VkMemoryMapFlags(..)
  , VkDeviceMemory
  , vkAllocateMemory
  , vkFlushMappedMemoryRanges
  , vkFreeMemory
  , vkGetDeviceMemoryCommitment
  , vkInvalidateMappedMemoryRanges
  , vkMapMemory
  , vkUnmapMemory
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  , withCStructAllocationCallbacks
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


-- | VkDeviceMemory - Opaque handle to a device memory object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBindAccelerationStructureMemoryInfoNV',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkMemoryGetAndroidHardwareBufferInfoANDROID',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.VkMemoryGetFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkMemoryGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex.VkWin32KeyedMutexAcquireReleaseInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex.VkWin32KeyedMutexAcquireReleaseInfoNV',
-- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.C.Core10.Memory.vkFreeMemory',
-- 'Graphics.Vulkan.C.Core10.Memory.vkGetDeviceMemoryCommitment',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV',
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory',
-- 'Graphics.Vulkan.C.Core10.Memory.vkUnmapMemory'
type DeviceMemory = VkDeviceMemory


-- | VkMappedMemoryRange - Structure specifying a mapped memory range
--
-- == Valid Usage
--
-- -   @memory@ /must/ be currently host mapped
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @offset@ and
--     @size@ /must/ specify a range contained within the currently mapped
--     range of @memory@
--
-- -   If @size@ is equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @offset@ /must/
--     be within the currently mapped range of @memory@
--
-- -   If @size@ is equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', the end of the
--     current mapping of @memory@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@
--     bytes from the beginning of the memory object.
--
-- -   @offset@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @size@ /must/
--     either be a multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@,
--     or @offset@ plus @size@ /must/ equal the size of @memory@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Memory.vkFlushMappedMemoryRanges',
-- 'Graphics.Vulkan.C.Core10.Memory.vkInvalidateMappedMemoryRanges'
data MappedMemoryRange = MappedMemoryRange
  { -- Univalued member elided
  -- No documentation found for Nested "MappedMemoryRange" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MappedMemoryRange" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "MappedMemoryRange" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "MappedMemoryRange" "size"
  size :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMappedMemoryRange' and
-- marshal a 'MappedMemoryRange' into it. The 'VkMappedMemoryRange' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMappedMemoryRange :: MappedMemoryRange -> (VkMappedMemoryRange -> IO a) -> IO a
withCStructMappedMemoryRange marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MappedMemoryRange)) (\pPNext -> cont (VkMappedMemoryRange VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE pPNext (memory (marshalled :: MappedMemoryRange)) (offset (marshalled :: MappedMemoryRange)) (size (marshalled :: MappedMemoryRange))))

-- | A function to read a 'VkMappedMemoryRange' and all additional
-- structures in the pointer chain into a 'MappedMemoryRange'.
fromCStructMappedMemoryRange :: VkMappedMemoryRange -> IO MappedMemoryRange
fromCStructMappedMemoryRange c = MappedMemoryRange <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMappedMemoryRange)))
                                                   <*> pure (vkMemory (c :: VkMappedMemoryRange))
                                                   <*> pure (vkOffset (c :: VkMappedMemoryRange))
                                                   <*> pure (vkSize (c :: VkMappedMemoryRange))

instance Zero MappedMemoryRange where
  zero = MappedMemoryRange Nothing
                           zero
                           zero
                           zero



-- | VkMemoryAllocateInfo - Structure containing parameters of a memory
-- allocation
--
-- == Valid Usage
--
-- -   @allocationSize@ /must/ be greater than @0@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExportMemoryAllocateInfoNV',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkExportMemoryWin32HandleInfoKHR',
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.VkExportMemoryWin32HandleInfoNV',
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkImportAndroidHardwareBufferInfoANDROID',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.VkImportMemoryFdInfoKHR',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkImportMemoryHostPointerInfoEXT',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR',
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.VkImportMemoryWin32HandleInfoNV',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo',
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority.VkMemoryPriorityAllocateInfoEXT'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory'
data MemoryAllocateInfo = MemoryAllocateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryAllocateInfo" "allocationSize"
  allocationSize :: DeviceSize
  , -- No documentation found for Nested "MemoryAllocateInfo" "memoryTypeIndex"
  memoryTypeIndex :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryAllocateInfo' and
-- marshal a 'MemoryAllocateInfo' into it. The 'VkMemoryAllocateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryAllocateInfo :: MemoryAllocateInfo -> (VkMemoryAllocateInfo -> IO a) -> IO a
withCStructMemoryAllocateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryAllocateInfo)) (\pPNext -> cont (VkMemoryAllocateInfo VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO pPNext (allocationSize (marshalled :: MemoryAllocateInfo)) (memoryTypeIndex (marshalled :: MemoryAllocateInfo))))

-- | A function to read a 'VkMemoryAllocateInfo' and all additional
-- structures in the pointer chain into a 'MemoryAllocateInfo'.
fromCStructMemoryAllocateInfo :: VkMemoryAllocateInfo -> IO MemoryAllocateInfo
fromCStructMemoryAllocateInfo c = MemoryAllocateInfo <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryAllocateInfo)))
                                                     <*> pure (vkAllocationSize (c :: VkMemoryAllocateInfo))
                                                     <*> pure (vkMemoryTypeIndex (c :: VkMemoryAllocateInfo))

instance Zero MemoryAllocateInfo where
  zero = MemoryAllocateInfo Nothing
                            zero
                            zero


-- | VkMemoryMapFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryMapFlags' is a bitmask type for
-- setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory'
type MemoryMapFlags = VkMemoryMapFlags


-- No complete pragma for MemoryMapFlags as it has no patterns


-- | vkAllocateMemory - Allocate device memory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @pAllocateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' structure
--     describing parameters of the allocation. A successful returned
--     allocation /must/ use the requested parameters — no substitution is
--     permitted by the implementation.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pMemory@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle in which
--     information about the allocated memory is returned.
--
-- = Description
--
-- Allocations returned by
-- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory' are guaranteed to
-- meet any alignment requirement of the implementation. For example, if an
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' /may/ be
-- restricted by implementation- or platform-dependent limits. If a call to
-- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory' would cause the total
-- number of allocations to exceed these limits, such a call will fail and
-- /must/ return 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_TOO_MANY_OBJECTS'.
-- The
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-maxMemoryAllocationCount maxMemoryAllocationCount>
-- feature describes the number of allocations that /can/ exist
-- simultaneously before encountering these internal limits.
--
-- Some platforms /may/ have a limit on the maximum size of a single
-- allocation. For example, certain systems /may/ fail to create
-- allocations with a size greater than or equal to 4GB. Such a limit is
-- implementation-dependent, and if such a failure occurs then the error
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY' /must/ be
-- returned.
--
-- == Valid Usage
--
-- -   @pAllocateInfo@->@allocationSize@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'::@memoryHeaps@[@pAllocateInfo@->@memoryTypeIndex@].@size@
--     as returned by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceMemoryProperties'
--     for the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     that @device@ was created from.
--
-- -   @pAllocateInfo@->@memoryTypeIndex@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'::@memoryTypeCount@
--     as returned by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceMemoryProperties'
--     for the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     that @device@ was created from.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pAllocateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pMemory@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
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
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_TOO_MANY_OBJECTS'
--
--     -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VK_ERROR_INVALID_EXTERNAL_HANDLE'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'
allocateMemory :: Device ->  MemoryAllocateInfo ->  Maybe AllocationCallbacks ->  IO (DeviceMemory)
allocateMemory = \(Device device' commandTable) -> \allocateInfo' -> \allocator -> alloca (\pMemory' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructMemoryAllocateInfo marshalled . flip with) allocateInfo' (\pAllocateInfo' -> vkAllocateMemory commandTable device' pAllocateInfo' pAllocator pMemory' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pMemory')))))


-- | vkFlushMappedMemoryRanges - Flush mapped memory ranges
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory ranges.
--
-- -   @memoryRangeCount@ is the length of the @pMemoryRanges@ array.
--
-- -   @pMemoryRanges@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange' structures
--     describing the memory ranges to flush.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Memory.vkFlushMappedMemoryRanges' guarantees
-- that host writes to the memory ranges described by @pMemoryRanges@ are
-- made available to the host memory domain, such that they /can/ be made
-- available to the device memory domain via
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-available-and-visible memory domain operations>
-- using the 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_WRITE_BIT'
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types access type>.
--
-- Within each range described by @pMemoryRanges@, each set of
-- @nonCoherentAtomSize@ bytes in that range is flushed if any byte in that
-- set has been written by the host since it was first host mapped, or the
-- last time it was flushed. If @pMemoryRanges@ includes sets of
-- @nonCoherentAtomSize@ bytes where no bytes have been written by the
-- host, those bytes /must/ not be flushed.
--
-- Unmapping non-coherent memory does not implicitly flush the host mapped
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
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange'
flushMappedMemoryRanges :: Device ->  Vector MappedMemoryRange ->  IO ()
flushMappedMemoryRanges = \(Device device' commandTable) -> \memoryRanges' -> withVec withCStructMappedMemoryRange memoryRanges' (\pMemoryRanges' -> vkFlushMappedMemoryRanges commandTable device' (fromIntegral $ Data.Vector.length memoryRanges') pMemoryRanges' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkFreeMemory - Free device memory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory'
--     object to be freed.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- = Description
--
-- Before freeing a memory object, an application /must/ ensure the memory
-- object is no longer in use by the device—​for example by command buffers
-- in the /pending state/. Memory /can/ be freed whilst still bound to
-- resources, but those resources /must/ not be used afterwards. If there
-- are still any bound images or buffers, the memory /may/ not be
-- immediately released by the implementation, but /must/ be released by
-- the time all bound images and buffers have been destroyed. Once memory
-- is released, it is returned to the heap from which it was allocated.
--
-- How memory objects are bound to Images and Buffers is described in
-- detail in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-association Resource Memory Association>
-- section.
--
-- If a memory object is mapped at the time it is freed, it is implicitly
-- unmapped.
--
-- __Note__
--
-- As described
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-device-unmap-does-not-flush below>,
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
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @memory@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @memory@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory'
freeMemory :: Device ->  DeviceMemory ->  Maybe AllocationCallbacks ->  IO ()
freeMemory = \(Device device' commandTable) -> \memory' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkFreeMemory commandTable device' memory' pAllocator *> (pure ()))


-- | vkGetDeviceMemoryCommitment - Query the current commitment for a
-- VkDeviceMemory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the memory object being queried.
--
-- -   @pCommittedMemoryInBytes@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize' value
--     in which the number of bytes currently committed is returned, on
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
getDeviceMemoryCommitment :: Device ->  DeviceMemory ->  IO (DeviceSize)
getDeviceMemoryCommitment = \(Device device' commandTable) -> \memory' -> alloca (\pCommittedMemoryInBytes' -> vkGetDeviceMemoryCommitment commandTable device' memory' pCommittedMemoryInBytes' *> (peek pCommittedMemoryInBytes'))


-- | vkInvalidateMappedMemoryRanges - Invalidate ranges of mapped memory
-- objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory ranges.
--
-- -   @memoryRangeCount@ is the length of the @pMemoryRanges@ array.
--
-- -   @pMemoryRanges@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange' structures
--     describing the memory ranges to invalidate.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Memory.vkInvalidateMappedMemoryRanges'
-- guarantees that device writes to the memory ranges described by
-- @pMemoryRanges@, which have been made available to the host memory
-- domain using the
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_WRITE_BIT' and
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_READ_BIT'
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types access types>,
-- are made visible to the host. If a range of non-coherent memory is
-- written by the host and then invalidated without first being flushed,
-- its contents are undefined.
--
-- Within each range described by @pMemoryRanges@, each set of
-- @nonCoherentAtomSize@ bytes in that range is invalidated if any byte in
-- that set has been written by the device since it was first host mapped,
-- or the last time it was invalidated.
--
-- __Note__
--
-- Mapping non-coherent memory does not implicitly invalidate the mapped
-- memory, and device writes that have not been invalidated /must/ be made
-- visible before the host reads or overwrites them.
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
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange'
invalidateMappedMemoryRanges :: Device ->  Vector MappedMemoryRange ->  IO ()
invalidateMappedMemoryRanges = \(Device device' commandTable) -> \memoryRanges' -> withVec withCStructMappedMemoryRange memoryRanges' (\pMemoryRanges' -> vkInvalidateMappedMemoryRanges commandTable device' (fromIntegral $ Data.Vector.length memoryRanges') pMemoryRanges' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkMapMemory - Map a memory object into application address space
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory'
--     object to be mapped.
--
-- -   @offset@ is a zero-based byte offset from the beginning of the
--     memory object.
--
-- -   @size@ is the size of the memory range to map, or
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE' to map from
--     @offset@ to the end of the allocation.
--
-- -   @flags@ is reserved for future use.
--
-- -   @ppData@ points to a pointer in which is returned a host-accessible
--     pointer to the beginning of the mapped range. This pointer minus
--     @offset@ /must/ be aligned to at least
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@minMemoryMapAlignment@.
--
-- = Description
--
-- After a successful call to 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory'
-- the memory object @memory@ is considered to be currently /host mapped/.
-- It is an application error to call
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory' on a memory object that is
-- already host mapped.
--
-- __Note__
--
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory' will fail if the
-- implementation is unable to allocate an appropriately sized contiguous
-- virtual address range, e.g. due to virtual address space fragmentation
-- or platform limits. In such cases,
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory' /must/ return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_MEMORY_MAP_FAILED'. The
-- application /can/ improve the likelihood of success by reducing the size
-- of the mapped range and\/or removing unneeded mappings using
-- 'Graphics.Vulkan.C.Core10.Memory.vkUnmapMemory'.
--
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory' does not check whether the
-- device memory is currently in use before returning the host-accessible
-- pointer. The application /must/ guarantee that any previously submitted
-- command that writes to this range has completed before the host reads
-- from or writes to that range, and that any previously submitted command
-- that reads from that range has completed before the host writes to that
-- region (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-host-writes here>
-- for details on fulfilling such a guarantee). If the device memory was
-- allocated without the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
-- set, these guarantees /must/ be made for an extended range: the
-- application /must/ round down the start of the range to the nearest
-- multiple of
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@,
-- and round the end of the range up to the nearest multiple of
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@nonCoherentAtomSize@.
--
-- While a range of device memory is host mapped, the application is
-- responsible for synchronizing both device and host access to that memory
-- range.
--
-- __Note__
--
-- It is important for the application developer to become meticulously
-- familiar with all of the mechanisms described in the chapter on
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization Synchronization and Cache Control>
-- as they are crucial to maintaining memory access ordering.
--
-- == Valid Usage
--
-- -   @memory@ /must/ not be currently host mapped
--
-- -   @offset@ /must/ be less than the size of @memory@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @size@ /must/ be
--     greater than @0@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @size@ /must/ be
--     less than or equal to the size of the @memory@ minus @offset@
--
-- -   @memory@ /must/ have been created with a memory type that reports
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_MEMORY_MAP_FAILED'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryMapFlags'
mapMemory :: Device ->  DeviceMemory ->  DeviceSize ->  DeviceSize ->  MemoryMapFlags ->  IO (Ptr ())
mapMemory = \(Device device' commandTable) -> \memory' -> \offset' -> \size' -> \flags' -> alloca (\pData' -> vkMapMemory commandTable device' memory' offset' size' flags' pData' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pData')))


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
-- -   @memory@ /must/ be currently host mapped
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory'
unmapMemory :: Device ->  DeviceMemory ->  IO ()
unmapMemory = \(Device device' commandTable) -> \memory' -> vkUnmapMemory commandTable device' memory' *> (pure ())

-- | A safe wrapper for 'mapMemory' and 'unmapMemory' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withMappedMemory
  :: Device -> DeviceMemory -> DeviceSize -> DeviceSize -> MemoryMapFlags -> (Ptr () -> IO a) -> IO a
withMappedMemory device deviceMemory offset' size' flags' = bracket
  (mapMemory device deviceMemory offset' size' flags')
  (\_ -> unmapMemory device deviceMemory)

-- | A safe wrapper for 'allocateMemory' and 'freeMemory' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withMemory
  :: Device -> MemoryAllocateInfo -> Maybe (AllocationCallbacks) -> (DeviceMemory -> IO a) -> IO a
withMemory device memoryAllocateInfo allocationCallbacks = bracket
  (allocateMemory device memoryAllocateInfo allocationCallbacks)
  (\o -> freeMemory device o allocationCallbacks)
