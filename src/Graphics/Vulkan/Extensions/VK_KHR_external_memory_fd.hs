{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( withCStructImportMemoryFdInfoKHR
  , fromCStructImportMemoryFdInfoKHR
  , ImportMemoryFdInfoKHR(..)
  , withCStructMemoryFdPropertiesKHR
  , fromCStructMemoryFdPropertiesKHR
  , MemoryFdPropertiesKHR(..)
  , withCStructMemoryGetFdInfoKHR
  , fromCStructMemoryGetFdInfoKHR
  , MemoryGetFdInfoKHR(..)
  , getMemoryFdKHR
  , getMemoryFdPropertiesKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CInt(..)
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
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( VkImportMemoryFdInfoKHR(..)
  , VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  , vkGetMemoryFdKHR
  , vkGetMemoryFdPropertiesKHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalMemoryHandleTypeFlagBits
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( pattern VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
  )



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
-- import operation /must/ create a distinct
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object.
--
-- == Valid Usage
--
-- -   If @handleType@ is not @0@, it /must/ be supported for import, as
--     reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'.
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
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-memory-handle-types-compatibility>.
--
-- -   @fd@ /must/ obey any requirements listed for @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>.
--
-- Unresolved directive in VkImportMemoryFdInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkImportMemoryFdInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data ImportMemoryFdInfoKHR = ImportMemoryFdInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ImportMemoryFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportMemoryFdInfoKHR" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "ImportMemoryFdInfoKHR" "fd"
  fd :: CInt
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImportMemoryFdInfoKHR' and
-- marshal a 'ImportMemoryFdInfoKHR' into it. The 'VkImportMemoryFdInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImportMemoryFdInfoKHR :: ImportMemoryFdInfoKHR -> (VkImportMemoryFdInfoKHR -> IO a) -> IO a
withCStructImportMemoryFdInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImportMemoryFdInfoKHR)) (\pPNext -> cont (VkImportMemoryFdInfoKHR VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR pPNext (handleType (marshalled :: ImportMemoryFdInfoKHR)) (fd (marshalled :: ImportMemoryFdInfoKHR))))

-- | A function to read a 'VkImportMemoryFdInfoKHR' and all additional
-- structures in the pointer chain into a 'ImportMemoryFdInfoKHR'.
fromCStructImportMemoryFdInfoKHR :: VkImportMemoryFdInfoKHR -> IO ImportMemoryFdInfoKHR
fromCStructImportMemoryFdInfoKHR c = ImportMemoryFdInfoKHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportMemoryFdInfoKHR)))
                                                           <*> pure (vkHandleType (c :: VkImportMemoryFdInfoKHR))
                                                           <*> pure (vkFd (c :: VkImportMemoryFdInfoKHR))

instance Zero ImportMemoryFdInfoKHR where
  zero = ImportMemoryFdInfoKHR Nothing
                               zero
                               zero



-- | VkMemoryFdPropertiesKHR - Properties of External Memory File Descriptors
--
-- = Description
--
-- Unresolved directive in VkMemoryFdPropertiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkMemoryFdPropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data MemoryFdPropertiesKHR = MemoryFdPropertiesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryFdPropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryFdPropertiesKHR" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryFdPropertiesKHR' and
-- marshal a 'MemoryFdPropertiesKHR' into it. The 'VkMemoryFdPropertiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryFdPropertiesKHR :: MemoryFdPropertiesKHR -> (VkMemoryFdPropertiesKHR -> IO a) -> IO a
withCStructMemoryFdPropertiesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryFdPropertiesKHR)) (\pPNext -> cont (VkMemoryFdPropertiesKHR VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR pPNext (memoryTypeBits (marshalled :: MemoryFdPropertiesKHR))))

-- | A function to read a 'VkMemoryFdPropertiesKHR' and all additional
-- structures in the pointer chain into a 'MemoryFdPropertiesKHR'.
fromCStructMemoryFdPropertiesKHR :: VkMemoryFdPropertiesKHR -> IO MemoryFdPropertiesKHR
fromCStructMemoryFdPropertiesKHR c = MemoryFdPropertiesKHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryFdPropertiesKHR)))
                                                           <*> pure (vkMemoryTypeBits (c :: VkMemoryFdPropertiesKHR))

instance Zero MemoryFdPropertiesKHR where
  zero = MemoryFdPropertiesKHR Nothing
                               zero



-- | VkMemoryGetFdInfoKHR - Structure describing a POSIX FD semaphore export
-- operation
--
-- = Description
--
-- The properties of the file descriptor exported depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBits'
-- for a description of the properties of the defined external memory
-- handle types.
--
-- __Note__
--
-- The size of the exported file /may/ be larger than the size requested by
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'::allocationSize.
-- If @handleType@ is
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_dma_buf.VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT',
-- then the application /can/ query the file’s actual size with
-- <man:lseek(2) lseek(2)>.
--
-- == Valid Usage
--
-- Unresolved directive in VkMemoryGetFdInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkMemoryGetFdInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data MemoryGetFdInfoKHR = MemoryGetFdInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryGetFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryGetFdInfoKHR" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "MemoryGetFdInfoKHR" "handleType"
  handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryGetFdInfoKHR' and
-- marshal a 'MemoryGetFdInfoKHR' into it. The 'VkMemoryGetFdInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryGetFdInfoKHR :: MemoryGetFdInfoKHR -> (VkMemoryGetFdInfoKHR -> IO a) -> IO a
withCStructMemoryGetFdInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryGetFdInfoKHR)) (\pPNext -> cont (VkMemoryGetFdInfoKHR VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR pPNext (memory (marshalled :: MemoryGetFdInfoKHR)) (handleType (marshalled :: MemoryGetFdInfoKHR))))

-- | A function to read a 'VkMemoryGetFdInfoKHR' and all additional
-- structures in the pointer chain into a 'MemoryGetFdInfoKHR'.
fromCStructMemoryGetFdInfoKHR :: VkMemoryGetFdInfoKHR -> IO MemoryGetFdInfoKHR
fromCStructMemoryGetFdInfoKHR c = MemoryGetFdInfoKHR <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryGetFdInfoKHR)))
                                                     <*> pure (vkMemory (c :: VkMemoryGetFdInfoKHR))
                                                     <*> pure (vkHandleType (c :: VkMemoryGetFdInfoKHR))

instance Zero MemoryGetFdInfoKHR where
  zero = MemoryGetFdInfoKHR Nothing
                            zero
                            zero



-- | vkGetMemoryFdKHR - Get a POSIX file descriptor for a memory object
--
-- = Parameters
--
-- -   @device@ is the logical device that created the device memory being
--     exported.
--
-- -   @pGetFdInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.VkMemoryGetFdInfoKHR'
--     structure containing parameters of the export operation.
--
-- -   @pFd@ will return a file descriptor representing the underlying
--     resources of the device memory object.
--
-- = Description
--
-- Each call to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdKHR'
-- /must/ create a new file descriptor and transfer ownership of it to the
-- application. To avoid leaking resources, the application /must/ release
-- ownership of the file descriptor using the @close@ system call when it
-- is no longer needed, or by importing a Vulkan memory object from it.
-- Where supported by the operating system, the implementation /must/ set
-- the file descriptor to be closed automatically when an @execve@ system
-- call is made.
--
-- Unresolved directive in vkGetMemoryFdKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetMemoryFdKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getMemoryFdKHR :: Device ->  MemoryGetFdInfoKHR ->  IO (CInt)
getMemoryFdKHR = \(Device device' commandTable) -> \getFdInfo' -> alloca (\pFd' -> (\marshalled -> withCStructMemoryGetFdInfoKHR marshalled . flip with) getFdInfo' (\pGetFdInfo' -> vkGetMemoryFdKHR commandTable device' pGetFdInfo' pFd' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pFd'))))


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
-- -   @pMemoryFdProperties@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.VkMemoryFdPropertiesKHR'
--     structure in which the properties of the handle @fd@ are returned.
--
-- == Valid Usage
--
-- Unresolved directive in vkGetMemoryFdPropertiesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetMemoryFdPropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getMemoryFdPropertiesKHR :: Device ->  ExternalMemoryHandleTypeFlagBits ->  CInt ->  IO (MemoryFdPropertiesKHR)
getMemoryFdPropertiesKHR = \(Device device' commandTable) -> \handleType' -> \fd' -> alloca (\pMemoryFdProperties' -> vkGetMemoryFdPropertiesKHR commandTable device' handleType' fd' pMemoryFdProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructMemoryFdPropertiesKHR <=< peek) pMemoryFdProperties')))
