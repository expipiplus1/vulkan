{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  ( withCStructImportSemaphoreFdInfoKHR
  , fromCStructImportSemaphoreFdInfoKHR
  , ImportSemaphoreFdInfoKHR(..)
  , withCStructSemaphoreGetFdInfoKHR
  , fromCStructSemaphoreGetFdInfoKHR
  , SemaphoreGetFdInfoKHR(..)
  , getSemaphoreFdKHR
  , importSemaphoreFdKHR
  , pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  , pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.String
  ( IsString
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
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  , vkGetSemaphoreFdKHR
  , vkImportSemaphoreFdKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( Semaphore
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( SemaphoreImportFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreHandleTypeFlagBits
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  )



-- | VkImportSemaphoreFdInfoKHR - Structure specifying POSIX file descriptor
-- to import to a semaphore
--
-- = Description
--
-- The handle types supported by @handleType@ are:
--
-- > +-----------------------+-----------------------+-----------------------+
-- > | Handle Type           | Transference          | Permanence Supported  |
-- > +=======================+=======================+=======================+
-- > | 'Graphics.Vulkan.C.Co | Reference             | Temporary,Permanent   |
-- > | re11.Promoted_from_VK |                       |                       |
-- > | _KHR_external_semapho |                       |                       |
-- > | re_capabilities.VK_EX |                       |                       |
-- > | TERNAL_SEMAPHORE_HAND |                       |                       |
-- > | LE_TYPE_OPAQUE_FD_BIT |                       |                       |
-- > | '                     |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.Co | Copy                  | Temporary             |
-- > | re11.Promoted_from_VK |                       |                       |
-- > | _KHR_external_semapho |                       |                       |
-- > | re_capabilities.VK_EX |                       |                       |
-- > | TERNAL_SEMAPHORE_HAND |                       |                       |
-- > | LE_TYPE_SYNC_FD_BIT'  |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > Handle Types Supported by
-- > 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR'
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphore-handletypes-fd Handle Types Supported by VkImportSemaphoreFdInfoKHR>
--     table.
--
-- -   @fd@ /must/ obey any requirements listed for @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-semaphore-handle-types-compatibility external semaphore handle types compatibility>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @semaphore@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkSemaphore' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlagBits'
--     values
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- == Host Synchronization
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSemaphore',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.vkImportSemaphoreFdKHR'
data ImportSemaphoreFdInfoKHR = ImportSemaphoreFdInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "semaphore"
  semaphore :: Semaphore
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "flags"
  flags :: SemaphoreImportFlags
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "ImportSemaphoreFdInfoKHR" "fd"
  fd :: CInt
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImportSemaphoreFdInfoKHR' and
-- marshal a 'ImportSemaphoreFdInfoKHR' into it. The 'VkImportSemaphoreFdInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImportSemaphoreFdInfoKHR :: ImportSemaphoreFdInfoKHR -> (VkImportSemaphoreFdInfoKHR -> IO a) -> IO a
withCStructImportSemaphoreFdInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImportSemaphoreFdInfoKHR)) (\pPNext -> cont (VkImportSemaphoreFdInfoKHR VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR pPNext (semaphore (marshalled :: ImportSemaphoreFdInfoKHR)) (flags (marshalled :: ImportSemaphoreFdInfoKHR)) (handleType (marshalled :: ImportSemaphoreFdInfoKHR)) (fd (marshalled :: ImportSemaphoreFdInfoKHR))))

-- | A function to read a 'VkImportSemaphoreFdInfoKHR' and all additional
-- structures in the pointer chain into a 'ImportSemaphoreFdInfoKHR'.
fromCStructImportSemaphoreFdInfoKHR :: VkImportSemaphoreFdInfoKHR -> IO ImportSemaphoreFdInfoKHR
fromCStructImportSemaphoreFdInfoKHR c = ImportSemaphoreFdInfoKHR <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportSemaphoreFdInfoKHR)))
                                                                 <*> pure (vkSemaphore (c :: VkImportSemaphoreFdInfoKHR))
                                                                 <*> pure (vkFlags (c :: VkImportSemaphoreFdInfoKHR))
                                                                 <*> pure (vkHandleType (c :: VkImportSemaphoreFdInfoKHR))
                                                                 <*> pure (vkFd (c :: VkImportSemaphoreFdInfoKHR))

instance Zero ImportSemaphoreFdInfoKHR where
  zero = ImportSemaphoreFdInfoKHR Nothing
                                  zero
                                  zero
                                  zero
                                  zero



-- | VkSemaphoreGetFdInfoKHR - Structure describing a POSIX FD semaphore
-- export operation
--
-- = Description
--
-- The properties of the file descriptor returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
-- for a description of the properties of the defined external semaphore
-- handle types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo'::@handleTypes@
--     when @semaphore@’s current payload was created.
--
-- -   @semaphore@ /must/ not currently have its payload replaced by an
--     imported payload as described below in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>
--     unless that imported payload’s handle type was included in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties'::@exportFromImportedHandleTypes@
--     for @handleType@.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, as defined below in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>,
--     there /must/ be no queue waiting on @semaphore@.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, @semaphore@ /must/ be signaled, or have an
--     associated
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
--     pending execution.
--
-- -   @handleType@ /must/ be defined as a POSIX file descriptor handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @semaphore@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkSemaphore' handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSemaphore',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.vkGetSemaphoreFdKHR'
data SemaphoreGetFdInfoKHR = SemaphoreGetFdInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "SemaphoreGetFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SemaphoreGetFdInfoKHR" "semaphore"
  semaphore :: Semaphore
  , -- No documentation found for Nested "SemaphoreGetFdInfoKHR" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSemaphoreGetFdInfoKHR' and
-- marshal a 'SemaphoreGetFdInfoKHR' into it. The 'VkSemaphoreGetFdInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSemaphoreGetFdInfoKHR :: SemaphoreGetFdInfoKHR -> (VkSemaphoreGetFdInfoKHR -> IO a) -> IO a
withCStructSemaphoreGetFdInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SemaphoreGetFdInfoKHR)) (\pPNext -> cont (VkSemaphoreGetFdInfoKHR VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR pPNext (semaphore (marshalled :: SemaphoreGetFdInfoKHR)) (handleType (marshalled :: SemaphoreGetFdInfoKHR))))

-- | A function to read a 'VkSemaphoreGetFdInfoKHR' and all additional
-- structures in the pointer chain into a 'SemaphoreGetFdInfoKHR'.
fromCStructSemaphoreGetFdInfoKHR :: VkSemaphoreGetFdInfoKHR -> IO SemaphoreGetFdInfoKHR
fromCStructSemaphoreGetFdInfoKHR c = SemaphoreGetFdInfoKHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSemaphoreGetFdInfoKHR)))
                                                           <*> pure (vkSemaphore (c :: VkSemaphoreGetFdInfoKHR))
                                                           <*> pure (vkHandleType (c :: VkSemaphoreGetFdInfoKHR))

instance Zero SemaphoreGetFdInfoKHR where
  zero = SemaphoreGetFdInfoKHR Nothing
                               zero
                               zero



-- | vkGetSemaphoreFdKHR - Get a POSIX file descriptor handle for a semaphore
--
-- = Parameters
--
-- -   @device@ is the logical device that created the semaphore being
--     exported.
--
-- -   @pGetFdInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkSemaphoreGetFdInfoKHR'
--     structure containing parameters of the export operation.
--
-- -   @pFd@ will return the file descriptor representing the semaphore
--     payload.
--
-- = Description
--
-- Each call to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.vkGetSemaphoreFdKHR'
-- /must/ create a new file descriptor and transfer ownership of it to the
-- application. To avoid leaking resources, the application /must/ release
-- ownership of the file descriptor when it is no longer needed.
--
-- __Note__
--
-- Ownership can be released in many ways. For example, the application can
-- call @close@() on the file descriptor, or transfer ownership back to
-- Vulkan by using the file descriptor to import a semaphore payload.
--
-- Where supported by the operating system, the implementation /must/ set
-- the file descriptor to be closed automatically when an @execve@ system
-- call is made.
--
-- Exporting a file descriptor from a semaphore /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore State>.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_TOO_MANY_OBJECTS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkSemaphoreGetFdInfoKHR'
getSemaphoreFdKHR :: Device ->  SemaphoreGetFdInfoKHR ->  IO (CInt)
getSemaphoreFdKHR = \(Device device' commandTable) -> \getFdInfo' -> alloca (\pFd' -> (\marshalled -> withCStructSemaphoreGetFdInfoKHR marshalled . flip with) getFdInfo' (\pGetFdInfo' -> vkGetSemaphoreFdKHR commandTable device' pGetFdInfo' pFd' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pFd'))))


-- | vkImportSemaphoreFdKHR - Import a semaphore from a POSIX file descriptor
--
-- = Parameters
--
-- -   @device@ is the logical device that created the semaphore.
--
-- -   @pImportSemaphoreFdInfo@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR'
--     structure specifying the semaphore and import parameters.
--
-- = Description
--
-- Importing a semaphore payload from a file descriptor transfers ownership
-- of the file descriptor from the application to the Vulkan
-- implementation. The application /must/ not perform any operations on the
-- file descriptor after a successful import.
--
-- Applications /can/ import the same semaphore payload into multiple
-- instances of Vulkan, into the same instance from which it was exported,
-- and multiple times into a given Vulkan instance.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VK_ERROR_INVALID_EXTERNAL_HANDLE'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR'
importSemaphoreFdKHR :: Device ->  ImportSemaphoreFdInfoKHR ->  IO ()
importSemaphoreFdKHR = \(Device device' commandTable) -> \importSemaphoreFdInfo' -> (\marshalled -> withCStructImportSemaphoreFdInfoKHR marshalled . flip with) importSemaphoreFdInfo' (\pImportSemaphoreFdInfo' -> vkImportSemaphoreFdKHR commandTable device' pImportSemaphoreFdInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
