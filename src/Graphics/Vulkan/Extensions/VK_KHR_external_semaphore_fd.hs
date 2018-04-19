{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  ( pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  , vkGetSemaphoreFdKHR
  , vkImportSemaphoreFdKHR
  , VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Foreign.C.Types
  ( CInt(..)
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
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
import Graphics.Vulkan.Core10.Queue
  ( VkSemaphore
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkSemaphoreImportFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlagBits(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR = VkStructureType 1000079000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR = VkStructureType 1000079001
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = "VK_KHR_external_semaphore_fd"
-- | vkGetSemaphoreFdKHR - Get a POSIX file descriptor handle for a semaphore
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that created the semaphore being
--     exported.
--
-- -   @pGetFdInfo@ is a pointer to an instance of the
--     'VkSemaphoreGetFdInfoKHR' structure containing parameters of the
--     export operation.
--
-- -   @pFd@ will return the file descriptor representing the semaphore
--     payload.
--
-- = Description
-- #_description#
--
-- Each call to @vkGetSemaphoreFdKHR@ /must/ create a new file descriptor
-- and transfer ownership of it to the application. To avoid leaking
-- resources, the application /must/ release ownership of the file
-- descriptor when it is no longer needed.
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
-- <{html_spec_relative}#synchronization-semaphores-importing Importing Semaphore State>.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pGetFdInfo@ /must/ be a valid pointer to a valid
--     @VkSemaphoreGetFdInfoKHR@ structure
--
-- -   @pFd@ /must/ be a valid pointer to a @int@ value
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_TOO_MANY_OBJECTS@
--
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkSemaphoreGetFdInfoKHR'
foreign import ccall "vkGetSemaphoreFdKHR" vkGetSemaphoreFdKHR :: ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
-- | vkImportSemaphoreFdKHR - Import a semaphore from a POSIX file descriptor
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that created the semaphore.
--
-- -   @pImportSemaphoreFdInfo@ points to a 'VkImportSemaphoreFdInfoKHR'
--     structure specifying the semaphore and import parameters.
--
-- = Description
-- #_description#
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
-- == Valid Usage
--
-- -   @semaphore@ /must/ not be associated with any queue command that has
--     not yet completed execution on that queue
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pImportSemaphoreFdInfo@ /must/ be a valid pointer to a valid
--     @VkImportSemaphoreFdInfoKHR@ structure
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_INVALID_EXTERNAL_HANDLE@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkImportSemaphoreFdInfoKHR'
foreign import ccall "vkImportSemaphoreFdKHR" vkImportSemaphoreFdKHR :: ("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult
-- | VkImportSemaphoreFdInfoKHR - Structure specifying POSIX file descriptor
-- to import to a semaphore
--
-- = Description
-- #_description#
--
-- The handle types supported by @handleType@ are:
--
-- > +-----------------------+-----------------------+-----------------------+
-- > | Handle Type           | Transference          | Permanence Supported  |
-- > +=======================+=======================+=======================+
-- > | @VK_EXTERNAL_SEMAPHOR | Reference             | Temporary,Permanent   |
-- > | E_HANDLE_TYPE_OPAQUE_ |                       |                       |
-- > | FD_BIT@               |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | @VK_EXTERNAL_SEMAPHOR | Copy                  | Temporary             |
-- > | E_HANDLE_TYPE_SYNC_FD |                       |                       |
-- > | _BIT@                 |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > Handle Types Supported by VkImportSemaphoreFdInfoKHR
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <{html_spec_relative}#synchronization-semaphore-handletypes-fd Handle Types Supported by VkImportSemaphoreFdInfoKHR>
--     table.
--
-- -   @fd@ /must/ obey any requirements listed for @handleType@ in
--     <{html_spec_relative}#external-semaphore-handle-types-compatibility external semaphore handle types compatibility>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @semaphore@ /must/ be a valid @VkSemaphore@ handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlagBits'
--     values
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- == Host Synchronization
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Queue.VkSemaphore',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkImportSemaphoreFdKHR'
data VkImportSemaphoreFdInfoKHR = VkImportSemaphoreFdInfoKHR
  { -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "vkSemaphore"
  vkSemaphore :: VkSemaphore
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "vkFlags"
  vkFlags :: VkSemaphoreImportFlags
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "vkHandleType"
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "vkFd"
  vkFd :: CInt
  }
  deriving (Eq, Show)

instance Storable VkImportSemaphoreFdInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImportSemaphoreFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 28)
                                        <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportSemaphoreFdInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkFd (poked :: VkImportSemaphoreFdInfoKHR))
-- | VkSemaphoreGetFdInfoKHR - Structure describing a POSIX FD semaphore
-- export operation
--
-- = Description
-- #_description#
--
-- The properties of the file descriptor returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
-- for a description of the properties of the defined external semaphore
-- handle types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo'::@handleTypes@
--     when @semaphore@’s current payload was created.
--
-- -   @semaphore@ /must/ not currently have its payload replaced by an
--     imported payload as described below in
--     <{html_spec_relative}#synchronization-semaphores-importing Importing Semaphore Payloads>
--     unless that imported payload’s handle type was included in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties'::@exportFromImportedHandleTypes@
--     for @handleType@.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, as defined below in
--     <{html_spec_relative}#synchronization-semaphores-importing Importing Semaphore Payloads>,
--     there /must/ be no queue waiting on @semaphore@.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, @semaphore@ /must/ be signaled, or have an
--     associated
--     <{html_spec_relative}#synchronization-semaphores-signaling semaphore signal operation>
--     pending execution.
--
-- -   @handleType@ /must/ be defined as a POSIX file descriptor handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @semaphore@ /must/ be a valid @VkSemaphore@ handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Queue.VkSemaphore',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkGetSemaphoreFdKHR'
data VkSemaphoreGetFdInfoKHR = VkSemaphoreGetFdInfoKHR
  { -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "vkSemaphore"
  vkSemaphore :: VkSemaphore
  , -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "vkHandleType"
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkSemaphoreGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkSemaphoreGetFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSemaphoreGetFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSemaphoreGetFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkSemaphoreGetFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkSemaphoreGetFdInfoKHR))
