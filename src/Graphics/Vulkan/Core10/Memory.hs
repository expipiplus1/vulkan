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
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.C.Core10.Memory.vkFreeMemory',
-- 'Graphics.Vulkan.C.Core10.Memory.vkGetDeviceMemoryCommitment',
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
-- Unresolved directive in VkMappedMemoryRange.txt -
-- include::{generated}\/validity\/structs\/VkMappedMemoryRange.txt[]
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
-- = Description
--
-- An instance of the
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' structure defines
-- a memory import operation if the @pNext@ chain contains an instance of
-- one of the following structures:
--
-- -   'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR'
--     with non-zero @handleType@ value
--
-- -   'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.VkImportMemoryFdInfoKHR'
--     with a non-zero @handleType@ value
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkImportMemoryHostPointerInfoEXT'
--     with a non-zero @handleType@ value
--
-- -   'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkImportAndroidHardwareBufferInfoANDROID'
--     with a non-@NULL@ @buffer@ value
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
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VK_ERROR_INVALID_EXTERNAL_HANDLE'.
--
-- == Valid Usage
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
--     and any of the handle types specified in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo'::@handleTypes@
--     require a dedicated allocation, as reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--     in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'::@externalMemoryProperties@::@externalMemoryFeatures@
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'::@externalMemoryProperties@::@externalMemoryFeatures@,
--     the @pNext@ chain must contain an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'
--     with either its @image@ or @buffer@ field set to a value other than
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'.
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
--     it /must/ not contain an instance of
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExportMemoryAllocateInfoNV'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.VkExportMemoryWin32HandleInfoNV'.
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR',
--     it /must/ not contain an instance of
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.VkImportMemoryWin32HandleInfoNV'.
--
-- -   If the parameters define an import operation, the external handle
--     specified was created by the Vulkan API, and the external handle
--     type is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR',
--     then the values of @allocationSize@ and @memoryTypeIndex@ /must/
--     match those specified when the memory object being imported was
--     created.
--
-- -   If the parameters define an import operation and the external handle
--     specified was created by the Vulkan API, the device mask specified
--     by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo'
--     /must/ match that specified when the memory object being imported
--     was allocated.
--
-- -   If the parameters define an import operation and the external handle
--     specified was created by the Vulkan API, the list of physical
--     devices that comprise the logical device passed to
--     'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory' /must/ match the
--     list of physical devices that comprise the logical device on which
--     the memory was originally allocated.
--
-- -   If the parameters define an import operation and the external handle
--     is an NT handle or a global share handle created outside of the
--     Vulkan API, the value of @memoryTypeIndex@ /must/ be one of those
--     returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandlePropertiesKHR'.
--
-- -   If the parameters define an import operation, the external handle
--     was created by the Vulkan API, and the external handle type is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR',
--     then the values of @allocationSize@ and @memoryTypeIndex@ /must/
--     match those specified when the memory object being imported was
--     created.
--
-- -   If the parameters define an import operation and the external handle
--     type is
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     @allocationSize@ /must/ match the size reported in the memory
--     requirements of the @image@ or @buffer@ member of the instance of
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'
--     included in the @pNext@ chain.
--
-- -   If the parameters define an import operation and the external handle
--     type is
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     @allocationSize@ /must/ match the size specified when creating the
--     Direct3D 12 heap from which the external handle was extracted.
--
-- -   If the parameters define an import operation and the external handle
--     is a POSIX file descriptor created outside of the Vulkan API, the
--     value of @memoryTypeIndex@ /must/ be one of those returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdPropertiesKHR'.
--
-- -   If the parameters define an import operation and the external handle
--     is a host pointer, the value of @memoryTypeIndex@ /must/ be one of
--     those returned by
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.vkGetMemoryHostPointerPropertiesEXT'
--
-- -   If the parameters define an import operation and the external handle
--     is a host pointer, @allocationSize@ /must/ be an integer multiple of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkPhysicalDeviceExternalMemoryHostPropertiesEXT'::@minImportedHostPointerAlignment@
--
-- -   If the parameters define an import operation and the external handle
--     type is
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID',
--     @allocationSize@ /must/ be the size returned by
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID'
--     for the Android hardware buffer.
--
-- -   If the parameters define an import operation and the external handle
--     type is
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID',
--     and the @pNext@ chain does not contain an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     is 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the Android
--     hardware buffer /must/ have a @AHardwareBuffer_Desc@::@format@ of
--     @AHARDWAREBUFFER_FORMAT_BLOB@ and a @AHardwareBuffer_Desc@::@usage@
--     that includes @AHARDWAREBUFFER_USAGE_GPU_DATA_BUFFER@.
--
-- -   If the parameters define an import operation and the external handle
--     type is
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID',
--     @memoryTypeIndex@ /must/ be one of those returned by
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID'
--     for the Android hardware buffer.
--
-- -   If the parameters do not define an import operation, and the @pNext@
--     chain contains an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo'
--     with
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VK_EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     included in its @handleTypes@ member, and the @pNext@ contains an
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with @image@ not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', then
--     @allocationSize@ /must/ be @0@, otherwise @allocationSize@ /must/ be
--     greater than @0@.
--
-- -   If the parameters define an import operation, the external handle is
--     an Android hardware buffer, and the @pNext@ chain includes an
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with @image@ that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the Android
--     hardware buffer’s
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AHardwareBuffer'::@usage@
--     /must/ include at least one of
--     @AHARDWAREBUFFER_USAGE_GPU_COLOR_OUTPUT@ or
--     @AHARDWAREBUFFER_USAGE_GPU_SAMPLED_IMAGE@.
--
-- -   If the parameters define an import operation, the external handle is
--     an Android hardware buffer, and the @pNext@ chain includes an
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with @image@ that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the format of
--     @image@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED' or the format
--     returned by
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID'
--     in
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID'::@format@
--     for the Android hardware buffer.
--
-- -   If the parameters define an import operation, the external handle is
--     an Android hardware buffer, and the @pNext@ chain includes an
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with @image@ that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the width,
--     height, and array layer dimensions of @image@ and the Android
--     hardware buffer’s @AHardwareBuffer_Desc@ /must/ be identical.
--
-- -   If the parameters define an import operation, the external handle is
--     an Android hardware buffer, and the @pNext@ chain includes an
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with @image@ that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', and the Android
--     hardware buffer’s
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AHardwareBuffer'::@usage@
--     includes @AHARDWAREBUFFER_USAGE_GPU_MIPMAP_COMPLETE@, the @image@
--     /must/ have a complete mipmap chain.
--
-- -   If the parameters define an import operation, the external handle is
--     an Android hardware buffer, and the @pNext@ chain includes an
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with @image@ that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', and the Android
--     hardware buffer’s
--     'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AHardwareBuffer'::@usage@
--     does not include @AHARDWAREBUFFER_USAGE_GPU_MIPMAP_COMPLETE@, the
--     @image@ /must/ have exactly one mipmap level.
--
-- -   If the parameters define an import operation, the external handle is
--     an Android hardware buffer, and the @pNext@ chain includes an
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     with @image@ that is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', each bit set in
--     the usage of @image@ /must/ be listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-external-android-hardware-buffer-usage AHardwareBuffer Usage Equivalence>,
--     and if there is a corresponding @AHARDWAREBUFFER_USAGE@ bit listed
--     that bit /must/ be included in the Android hardware buffer’s
--     @AHardwareBuffer_Desc@::@usage@.
--
-- Unresolved directive in VkMemoryAllocateInfo.txt -
-- include::{generated}\/validity\/structs\/VkMemoryAllocateInfo.txt[]
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
-- returned. This limit is advertised in
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties'::@maxMemoryAllocationSize@.
--
-- The cumulative memory size allocated to a heap /can/ be limited by the
-- size of the specified heap. In such cases, allocated memory is tracked
-- on a per-device and per-heap basis. Some platforms allow overallocation
-- into other heaps. The overallocation behavior /can/ be specified through
-- the
-- @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_AMD_memory_overallocation_behavior@
-- extension.
--
-- == Valid Usage
--
-- Unresolved directive in vkAllocateMemory.txt -
-- include::{generated}\/validity\/protos\/vkAllocateMemory.txt[]
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
-- Unresolved directive in vkFlushMappedMemoryRanges.txt -
-- include::{generated}\/validity\/protos\/vkFlushMappedMemoryRanges.txt[]
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
-- Unresolved directive in vkFreeMemory.txt -
-- include::{generated}\/validity\/protos\/vkFreeMemory.txt[]
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
-- == Valid Usage
--
-- Unresolved directive in vkGetDeviceMemoryCommitment.txt -
-- include::{generated}\/validity\/protos\/vkGetDeviceMemoryCommitment.txt[]
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
-- Unresolved directive in vkInvalidateMappedMemoryRanges.txt -
-- include::{generated}\/validity\/protos\/vkInvalidateMappedMemoryRanges.txt[]
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
-- -   @memory@ /must/ not have been allocated with multiple instances.
--
-- Unresolved directive in vkMapMemory.txt -
-- include::{generated}\/validity\/protos\/vkMapMemory.txt[]
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
-- Unresolved directive in vkUnmapMemory.txt -
-- include::{generated}\/validity\/protos\/vkUnmapMemory.txt[]
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
