{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  , VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  , VkMemoryMapFlags(..)
  , FN_vkAllocateMemory
  , PFN_vkAllocateMemory
  , vkAllocateMemory
  , FN_vkFlushMappedMemoryRanges
  , PFN_vkFlushMappedMemoryRanges
  , vkFlushMappedMemoryRanges
  , FN_vkFreeMemory
  , PFN_vkFreeMemory
  , vkFreeMemory
  , FN_vkGetDeviceMemoryCommitment
  , PFN_vkGetDeviceMemoryCommitment
  , vkGetDeviceMemoryCommitment
  , FN_vkInvalidateMappedMemoryRanges
  , PFN_vkInvalidateMappedMemoryRanges
  , vkInvalidateMappedMemoryRanges
  , FN_vkMapMemory
  , PFN_vkMapMemory
  , vkMapMemory
  , FN_vkUnmapMemory
  , PFN_vkUnmapMemory
  , vkUnmapMemory
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
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
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkDeviceMemory_T
-- | VkDeviceMemory - Opaque handle to a device memory object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'vkAllocateMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'vkFreeMemory', 'vkGetDeviceMemoryCommitment', 'vkMapMemory',
-- 'vkUnmapMemory'
type VkDeviceMemory = Ptr VkDeviceMemory_T

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
-- 'VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
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
  , -- | @size@ is either the size of range, or
  -- 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE' to affect the range
  -- from @offset@ to the end of the current mapping of the allocation.
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

instance Zero VkMappedMemoryRange where
  zero = VkMappedMemoryRange VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
                             zero
                             zero
                             zero
                             zero

-- | VkMemoryAllocateInfo - Structure containing parameters of a memory
-- allocation
--
-- = Description
--
-- An instance of the 'VkMemoryAllocateInfo' structure defines a memory
-- import operation if the @pNext@ chain contains an instance of one of the
-- following structures:
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
--     'vkAllocateMemory' /must/ match the list of physical devices that
--     comprise the logical device on which the memory was originally
--     allocated.
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
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkAllocateMemory'
data VkMemoryAllocateInfo = VkMemoryAllocateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @allocationSize@ is the size of the allocation in bytes
  vkAllocationSize :: VkDeviceSize
  , -- | @memoryTypeIndex@ is an index identifying a memory type from the
  -- @memoryTypes@ array of the
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'
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

instance Zero VkMemoryAllocateInfo where
  zero = VkMemoryAllocateInfo VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
                              zero
                              zero
                              zero

-- ** VkMemoryMapFlags

-- | VkMemoryMapFlags - Reserved for future use
--
-- = Description
--
-- 'VkMemoryMapFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'vkMapMemory'
newtype VkMemoryMapFlags = VkMemoryMapFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pMemory@ is a pointer to a 'VkDeviceMemory' handle in which
--     information about the allocated memory is returned.
--
-- = Description
--
-- Allocations returned by 'vkAllocateMemory' are guaranteed to meet any
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' /may/ be
-- restricted by implementation- or platform-dependent limits. If a call to
-- 'vkAllocateMemory' would cause the total number of allocations to exceed
-- these limits, such a call will fail and /must/ return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_TOO_MANY_OBJECTS'. The
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
-- 'VkDeviceMemory', 'VkMemoryAllocateInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAllocateMemory" vkAllocateMemory :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
#else
vkAllocateMemory :: DeviceCmds -> ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
vkAllocateMemory deviceCmds = mkVkAllocateMemory (pVkAllocateMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateMemory
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult)
#endif

type FN_vkAllocateMemory = ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
type PFN_vkAllocateMemory = FunPtr FN_vkAllocateMemory

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
-- 'vkFlushMappedMemoryRanges' guarantees that host writes to the memory
-- ranges described by @pMemoryRanges@ are made available to the host
-- memory domain, such that they /can/ be made available to the device
-- memory domain via
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
-- 'VkMappedMemoryRange'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFlushMappedMemoryRanges" vkFlushMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
#else
vkFlushMappedMemoryRanges :: DeviceCmds -> ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
vkFlushMappedMemoryRanges deviceCmds = mkVkFlushMappedMemoryRanges (pVkFlushMappedMemoryRanges deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFlushMappedMemoryRanges
  :: FunPtr (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
#endif

type FN_vkFlushMappedMemoryRanges = ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
type PFN_vkFlushMappedMemoryRanges = FunPtr FN_vkFlushMappedMemoryRanges

-- | vkFreeMemory - Free device memory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the 'VkDeviceMemory' object to be freed.
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
-- 'VkDeviceMemory'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFreeMemory" vkFreeMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkFreeMemory :: DeviceCmds -> ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkFreeMemory deviceCmds = mkVkFreeMemory (pVkFreeMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeMemory
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkFreeMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkFreeMemory = FunPtr FN_vkFreeMemory

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
-- 'VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceMemoryCommitment" vkGetDeviceMemoryCommitment :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
#else
vkGetDeviceMemoryCommitment :: DeviceCmds -> ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
vkGetDeviceMemoryCommitment deviceCmds = mkVkGetDeviceMemoryCommitment (pVkGetDeviceMemoryCommitment deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceMemoryCommitment
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ())
#endif

type FN_vkGetDeviceMemoryCommitment = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkGetDeviceMemoryCommitment = FunPtr FN_vkGetDeviceMemoryCommitment

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
-- 'vkInvalidateMappedMemoryRanges' guarantees that device writes to the
-- memory ranges described by @pMemoryRanges@, which have been made
-- available to the host memory domain using the
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
-- 'VkMappedMemoryRange'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkInvalidateMappedMemoryRanges" vkInvalidateMappedMemoryRanges :: ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
#else
vkInvalidateMappedMemoryRanges :: DeviceCmds -> ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
vkInvalidateMappedMemoryRanges deviceCmds = mkVkInvalidateMappedMemoryRanges (pVkInvalidateMappedMemoryRanges deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkInvalidateMappedMemoryRanges
  :: FunPtr (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
#endif

type FN_vkInvalidateMappedMemoryRanges = ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
type PFN_vkInvalidateMappedMemoryRanges = FunPtr FN_vkInvalidateMappedMemoryRanges

-- | vkMapMemory - Map a memory object into application address space
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @memory@ is the 'VkDeviceMemory' object to be mapped.
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
-- After a successful call to 'vkMapMemory' the memory object @memory@ is
-- considered to be currently /host mapped/. It is an application error to
-- call 'vkMapMemory' on a memory object that is already host mapped.
--
-- __Note__
--
-- 'vkMapMemory' will fail if the implementation is unable to allocate an
-- appropriately sized contiguous virtual address range, e.g. due to
-- virtual address space fragmentation or platform limits. In such cases,
-- 'vkMapMemory' /must/ return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_MEMORY_MAP_FAILED'. The
-- application /can/ improve the likelihood of success by reducing the size
-- of the mapped range and\/or removing unneeded mappings using
-- 'vkUnmapMemory'.
--
-- 'vkMapMemory' does not check whether the device memory is currently in
-- use before returning the host-accessible pointer. The application /must/
-- guarantee that any previously submitted command that writes to this
-- range has completed before the host reads from or writes to that range,
-- and that any previously submitted command that reads from that range has
-- completed before the host writes to that region (see
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
-- 'VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'VkMemoryMapFlags'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkMapMemory" vkMapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
#else
vkMapMemory :: DeviceCmds -> ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
vkMapMemory deviceCmds = mkVkMapMemory (pVkMapMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMapMemory
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult)
#endif

type FN_vkMapMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
type PFN_vkMapMemory = FunPtr FN_vkMapMemory

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
-- 'VkDeviceMemory'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUnmapMemory" vkUnmapMemory :: ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
#else
vkUnmapMemory :: DeviceCmds -> ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
vkUnmapMemory deviceCmds = mkVkUnmapMemory (pVkUnmapMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUnmapMemory
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ())
#endif

type FN_vkUnmapMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
type PFN_vkUnmapMemory = FunPtr FN_vkUnmapMemory
