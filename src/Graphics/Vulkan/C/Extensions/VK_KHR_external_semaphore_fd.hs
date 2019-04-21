{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  , FN_vkGetSemaphoreFdKHR
  , PFN_vkGetSemaphoreFdKHR
  , vkGetSemaphoreFdKHR
  , FN_vkImportSemaphoreFdKHR
  , PFN_vkImportSemaphoreFdKHR
  , vkImportSemaphoreFdKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.C.Types
  ( CInt(..)
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkSemaphore
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkSemaphoreImportFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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
-- > Handle Types Supported by 'VkImportSemaphoreFdInfoKHR'
--
-- == Valid Usage
--
-- Unresolved directive in VkImportSemaphoreFdInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkImportSemaphoreFdInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkImportSemaphoreFdInfoKHR = VkImportSemaphoreFdInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @semaphore@ is the semaphore into which the payload will be imported.
  vkSemaphore :: VkSemaphore
  , -- | @flags@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlagBits'
  -- specifying additional parameters for the semaphore payload import
  -- operation.
  vkFlags :: VkSemaphoreImportFlags
  , -- | @handleType@ /must/ be a value included in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphore-handletypes-fd Handle Types Supported by VkImportSemaphoreFdInfoKHR>
  -- table.
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  , -- | @fd@ /must/ obey any requirements listed for @handleType@ in
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-semaphore-handle-types-compatibility external semaphore handle types compatibility>.
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

instance Zero VkImportSemaphoreFdInfoKHR where
  zero = VkImportSemaphoreFdInfoKHR VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
                                    zero
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
-- Unresolved directive in VkSemaphoreGetFdInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkSemaphoreGetFdInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSemaphoreGetFdInfoKHR = VkSemaphoreGetFdInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @semaphore@ is the semaphore from which state will be exported.
  vkSemaphore :: VkSemaphore
  , -- | @handleType@ is the type of handle requested.
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

instance Zero VkSemaphoreGetFdInfoKHR where
  zero = VkSemaphoreGetFdInfoKHR VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
                                 zero
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
--     'VkSemaphoreGetFdInfoKHR' structure containing parameters of the
--     export operation.
--
-- -   @pFd@ will return the file descriptor representing the semaphore
--     payload.
--
-- = Description
--
-- Each call to 'vkGetSemaphoreFdKHR' /must/ create a new file descriptor
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
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore State>.
--
-- Unresolved directive in vkGetSemaphoreFdKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetSemaphoreFdKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetSemaphoreFdKHR" vkGetSemaphoreFdKHR :: ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
#else
vkGetSemaphoreFdKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
vkGetSemaphoreFdKHR deviceCmds = mkVkGetSemaphoreFdKHR (pVkGetSemaphoreFdKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreFdKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
#endif

type FN_vkGetSemaphoreFdKHR = ("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult
type PFN_vkGetSemaphoreFdKHR = FunPtr FN_vkGetSemaphoreFdKHR

-- | vkImportSemaphoreFdKHR - Import a semaphore from a POSIX file descriptor
--
-- = Parameters
--
-- -   @device@ is the logical device that created the semaphore.
--
-- -   @pImportSemaphoreFdInfo@ points to a 'VkImportSemaphoreFdInfoKHR'
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
-- == Valid Usage
--
-- Unresolved directive in vkImportSemaphoreFdKHR.txt -
-- include::{generated}\/validity\/protos\/vkImportSemaphoreFdKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkImportSemaphoreFdKHR" vkImportSemaphoreFdKHR :: ("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult
#else
vkImportSemaphoreFdKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult
vkImportSemaphoreFdKHR deviceCmds = mkVkImportSemaphoreFdKHR (pVkImportSemaphoreFdKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportSemaphoreFdKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult)
#endif

type FN_vkImportSemaphoreFdKHR = ("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult
type PFN_vkImportSemaphoreFdKHR = FunPtr FN_vkImportSemaphoreFdKHR

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = "VK_KHR_external_semaphore_fd"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR = VkStructureType 1000079000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR"
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR = VkStructureType 1000079001
