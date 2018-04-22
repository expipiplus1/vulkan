{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  , vkGetMemoryFdKHR
  , vkGetMemoryFdPropertiesKHR
  , VkImportMemoryFdInfoKHR(..)
  , VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CInt(..)
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR = VkStructureType 1000074000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR = VkStructureType 1000074001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR = VkStructureType 1000074002
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME = "VK_KHR_external_memory_fd"
-- | vkGetMemoryFdKHR - Get a POSIX file descriptor for a memory object
--
-- = Parameters
--
-- -   @device@ is the logical device that created the device memory being
--     exported.
--
-- -   @pGetFdInfo@ is a pointer to an instance of the
--     'VkMemoryGetFdInfoKHR' structure containing parameters of the export
--     operation.
--
-- -   @pFd@ will return a file descriptor representing the underlying
--     resources of the device memory object.
--
-- = Description
--
-- Each call to @vkGetMemoryFdKHR@ /must/ create a new file descriptor and
-- transfer ownership of it to the application. To avoid leaking resources,
-- the application /must/ release ownership of the file descriptor using
-- the @close@ system call when it is no longer needed, or by importing a
-- Vulkan memory object from it. Where supported by the operating system,
-- the implementation /must/ set the file descriptor to be closed
-- automatically when an @execve@ system call is made.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pGetFdInfo@ /must/ be a valid pointer to a valid
--     @VkMemoryGetFdInfoKHR@ structure
--
-- -   @pFd@ /must/ be a valid pointer to a @int@ value
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_TOO_MANY_OBJECTS@
--
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkMemoryGetFdInfoKHR'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryFdKHR" vkGetMemoryFdKHR :: ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
-- | vkGetMemoryFdPropertiesKHR - Get Properties of External Memory File
-- Descriptors
--
-- = Parameters
--
-- -   @device@ is the logical device that will be importing @fd@.
--
-- -   @handleType@ is the type of the handle @fd@.
--
-- -   @fd@ is the handle which will be imported.
--
-- -   @pMemoryFdProperties@ will return properties of the handle @fd@.
--
-- == Valid Usage
--
-- -   @fd@ /must/ be an external memory handle created outside of the
--     Vulkan API.
--
-- -   @handleType@ /must/ not be
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- -   @pMemoryFdProperties@ /must/ be a valid pointer to a
--     @VkMemoryFdPropertiesKHR@ structure
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_INVALID_EXTERNAL_HANDLE@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'VkMemoryFdPropertiesKHR'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetMemoryFdPropertiesKHR" vkGetMemoryFdPropertiesKHR :: ("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult
-- | VkImportMemoryFdInfoKHR - import memory created on the same physical
-- device from a file descriptor
--
-- = Description
--
-- Importing memory from a file descriptor transfers ownership of the file
-- descriptor from the application to the Vulkan implementation. The
-- application /must/ not perform any operations on the file descriptor
-- after a successful import.
--
-- Applications /can/ import the same underlying memory into multiple
-- instances of Vulkan, into the same instance from which it was exported,
-- and multiple times into a given Vulkan instance. In all cases, each
-- import operation /must/ create a distinct @VkDeviceMemory@ object.
--
-- == Valid Usage
--
-- -   If @handleType@ is not @0@, it /must/ be supported for import, as
--     reported by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'.
--
-- -   The memory from which @fd@ was exported /must/ have been created on
--     the same underlying physical device as @device@.
--
-- -   If @handleType@ is not @0@, it /must/ be defined as a POSIX file
--     descriptor handle.
--
-- -   If @handleType@ is not @0@, @fd@ /must/ be a valid handle of the
--     type specified by @handleType@.
--
-- -   The memory represented by @fd@ /must/ have been created from a
--     physical device and driver that is compatible with @device@ and
--     @handleType@, as described in
--     [{html_spec_relative}#external-memory-handle-types-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#external-memory-handle-types-compatibility).
--
-- -   @fd@ /must/ obey any requirements listed for @handleType@ in
--     [external memory handle types
--     compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#external-memory-handle-types-compatibility).
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR@
--
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkImportMemoryFdInfoKHR = VkImportMemoryFdInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @handleType@ specifies the handle type of @fd@.
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  , -- | @fd@ is the external handle to import.
  vkFd :: CInt
  }
  deriving (Eq, Show)

instance Storable VkImportMemoryFdInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImportMemoryFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportMemoryFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportMemoryFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleType (poked :: VkImportMemoryFdInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkFd (poked :: VkImportMemoryFdInfoKHR))
-- | VkMemoryFdPropertiesKHR - Properties of External Memory File Descriptors
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetMemoryFdPropertiesKHR'
data VkMemoryFdPropertiesKHR = VkMemoryFdPropertiesKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @memoryTypeBits@ is a bitmask containing one bit set for every memory
  -- type which the specified file descriptor /can/ be imported as.
  vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryFdPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryFdPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryFdPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryFdPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryFdPropertiesKHR))
-- | VkMemoryGetFdInfoKHR - Structure describing a POSIX FD semaphore export
-- operation
--
-- = Description
--
-- The properties of the file descriptor exported depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
-- for a description of the properties of the defined external memory
-- handle types.
--
-- __Note__
--
-- The size of the exported file /may/ be larger than the size requested by
-- 'Graphics.Vulkan.Core10.Memory.VkMemoryAllocateInfo'::allocationSize. If
-- @handleType@ is @VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT@, then
-- the application /can/ query the file’s actual size with
-- [lseek(2)](man:lseek(2)).
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo'::@handleTypes@
--     when @memory@ was created.
--
-- -   @handleType@ /must/ be defined as a POSIX file descriptor handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkGetMemoryFdKHR'
data VkMemoryGetFdInfoKHR = VkMemoryGetFdInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @memory@ is the memory object from which the handle will be exported.
  vkMemory :: VkDeviceMemory
  , -- | @handleType@ is the type of handle requested.
  vkHandleType :: VkExternalMemoryHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkMemoryGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryGetFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryGetFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryGetFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkMemory (poked :: VkMemoryGetFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkMemoryGetFdInfoKHR))
