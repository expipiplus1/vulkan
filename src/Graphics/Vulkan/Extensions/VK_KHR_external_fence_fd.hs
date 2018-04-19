{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  , vkGetFenceFdKHR
  , vkImportFenceFdKHR
  , VkImportFenceFdInfoKHR(..)
  , VkFenceGetFdInfoKHR(..)
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
  ( VkFence
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlagBits(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR = VkStructureType 1000115000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR = VkStructureType 1000115001
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME = "VK_KHR_external_fence_fd"
-- | vkGetFenceFdKHR - Get a POSIX file descriptor handle for a fence
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that created the fence being
--     exported.
--
-- -   @pGetFdInfo@ is a pointer to an instance of the
--     'VkFenceGetFdInfoKHR' structure containing parameters of the export
--     operation.
--
-- -   @pFd@ will return the file descriptor representing the fence
--     payload.
--
-- = Description
-- #_description#
--
-- Each call to @vkGetFenceFdKHR@ /must/ create a new file descriptor and
-- transfer ownership of it to the application. To avoid leaking resources,
-- the application /must/ release ownership of the file descriptor when it
-- is no longer needed.
--
-- __Note__
--
-- Ownership can be released in many ways. For example, the application can
-- call @close@() on the file descriptor, or transfer ownership back to
-- Vulkan by using the file descriptor to import a fence payload.
--
-- If @pGetFdInfo@::@handleType@ is
-- @VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT@ and the fence is signaled at
-- the time @vkGetFenceFdKHR@ is called, @pFd@ /may/ return the value @-1@
-- instead of a valid file descriptor.
--
-- Where supported by the operating system, the implementation /must/ set
-- the file descriptor to be closed automatically when an @execve@ system
-- call is made.
--
-- Exporting a file descriptor from a fence /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <{html_spec_relative}#synchronization-fences-importing Importing Fence State>.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pGetFdInfo@ /must/ be a valid pointer to a valid
--     @VkFenceGetFdInfoKHR@ structure
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
-- 'VkFenceGetFdInfoKHR'
foreign import ccall "vkGetFenceFdKHR" vkGetFenceFdKHR :: ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkFenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
-- | vkImportFenceFdKHR - Import a fence from a POSIX file descriptor
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that created the fence.
--
-- -   @pImportFenceFdInfo@ points to a 'VkImportFenceFdInfoKHR' structure
--     specifying the fence and import parameters.
--
-- = Description
-- #_description#
--
-- Importing a fence payload from a file descriptor transfers ownership of
-- the file descriptor from the application to the Vulkan implementation.
-- The application /must/ not perform any operations on the file descriptor
-- after a successful import.
--
-- Applications /can/ import the same fence payload into multiple instances
-- of Vulkan, into the same instance from which it was exported, and
-- multiple times into a given Vulkan instance.
--
-- == Valid Usage
--
-- -   @fence@ /must/ not be associated with any queue command that has not
--     yet completed execution on that queue
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pImportFenceFdInfo@ /must/ be a valid pointer to a valid
--     @VkImportFenceFdInfoKHR@ structure
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
-- 'VkImportFenceFdInfoKHR'
foreign import ccall "vkImportFenceFdKHR" vkImportFenceFdKHR :: ("device" ::: VkDevice) -> ("pImportFenceFdInfo" ::: Ptr VkImportFenceFdInfoKHR) -> IO VkResult
-- | VkImportFenceFdInfoKHR - (None)
--
-- = Description
-- #_description#
--
-- The handle types supported by @handleType@ are:
--
-- > +-----------------------+-----------------------+-----------------------+
-- > | Handle Type           | Transference          | Permanence Supported  |
-- > +=======================+=======================+=======================+
-- > | @VK_EXTERNAL_FENCE_HA | Reference             | Temporary,Permanent   |
-- > | NDLE_TYPE_OPAQUE_FD_B |                       |                       |
-- > | IT@                   |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | @VK_EXTERNAL_FENCE_HA | Copy                  | Temporary             |
-- > | NDLE_TYPE_SYNC_FD_BIT |                       |                       |
-- > | @                     |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > Handle Types Supported by VkImportFenceFdInfoKHR
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <{html_spec_relative}#synchronization-fence-handletypes-fd Handle Types Supported by VkImportFenceFdInfoKHR>
--     table.
--
-- -   @fd@ /must/ obey any requirements listed for @handleType@ in
--     <{html_spec_relative}#external-fence-handle-types-compatibility external fence handle types compatibility>.
--
-- If @handleType@ is @VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT@, the
-- special value @-1@ for @fd@ is treated like a valid sync file descriptor
-- referring to an object that has already signaled. The import operation
-- will succeed and the @VkFence@ will have a temporarily imported payload
-- as if a valid file descriptor had been provided.
--
-- __Note__
--
-- This special behavior for importing an invalid sync file descriptor
-- allows easier interoperability with other system APIs which use the
-- convention that an invalid sync file descriptor represents work that has
-- already completed and doesn’t need to be waited for. It is consistent
-- with the option for implementations to return a @-1@ file descriptor
-- when exporting a @VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT@ from a
-- @VkFence@ which is signaled.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @fence@ /must/ be a valid @VkFence@ handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlagBits'
--     values
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
--     value
--
-- == Host Synchronization
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Queue.VkFence',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkImportFenceFdKHR'
data VkImportFenceFdInfoKHR = VkImportFenceFdInfoKHR
  { -- No documentation found for Nested "VkImportFenceFdInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "vkFence"
  vkFence :: VkFence
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "vkFlags"
  vkFlags :: VkFenceImportFlags
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "vkHandleType"
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "vkFd"
  vkFd :: CInt
  }
  deriving (Eq, Show)

instance Storable VkImportFenceFdInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkImportFenceFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 28)
                                    <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportFenceFdInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkFd (poked :: VkImportFenceFdInfoKHR))
-- | VkFenceGetFdInfoKHR - Structure describing a POSIX FD fence export
-- operation
--
-- = Description
-- #_description#
--
-- The properties of the file descriptor returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
-- for a description of the properties of the defined external fence handle
-- types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo'::@handleTypes@
--     when @fence@’s current payload was created.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, @fence@ /must/ be signaled, or have an
--     associated
--     <{html_spec_relative}#synchronization-fences-signaling fence signal operation>
--     pending execution.
--
-- -   @fence@ /must/ not currently have its payload replaced by an
--     imported payload as described below in
--     <{html_spec_relative}#synchronization-fences-importing Importing Fence Payloads>
--     unless that imported payload’s handle type was included in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'::@exportFromImportedHandleTypes@
--     for @handleType@.
--
-- -   @handleType@ /must/ be defined as a POSIX file descriptor handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @fence@ /must/ be a valid @VkFence@ handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
--     value
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Queue.VkFence',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkGetFenceFdKHR'
data VkFenceGetFdInfoKHR = VkFenceGetFdInfoKHR
  { -- No documentation found for Nested "VkFenceGetFdInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFenceGetFdInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFenceGetFdInfoKHR" "vkFence"
  vkFence :: VkFence
  , -- No documentation found for Nested "VkFenceGetFdInfoKHR" "vkHandleType"
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkFenceGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkFenceGetFdInfoKHR <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceGetFdInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceGetFdInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkFenceGetFdInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkFenceGetFdInfoKHR))
