{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( withCStructFenceGetFdInfoKHR
  , fromCStructFenceGetFdInfoKHR
  , FenceGetFdInfoKHR(..)
  , withCStructImportFenceFdInfoKHR
  , fromCStructImportFenceFdInfoKHR
  , ImportFenceFdInfoKHR(..)
  , getFenceFdKHR
  , importFenceFdKHR
  , pattern KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  , pattern KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
  , vkGetFenceFdKHR
  , vkImportFenceFdKHR
  , pattern VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( Fence
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( FenceImportFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceHandleTypeFlagBits
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
  ( pattern STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  )



-- | VkFenceGetFdInfoKHR - Structure describing a POSIX FD fence export
-- operation
--
-- = Description
--
-- The properties of the file descriptor returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
-- for a description of the properties of the defined external fence handle
-- types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo'::@handleTypes@
--     when @fence@’s current payload was created.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, @fence@ /must/ be signaled, or have an
--     associated
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>
--     pending execution.
--
-- -   @fence@ /must/ not currently have its payload replaced by an
--     imported payload as described below in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence Payloads>
--     unless that imported payload’s handle type was included in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'::@exportFromImportedHandleTypes@
--     for @handleType@.
--
-- -   @handleType@ /must/ be defined as a POSIX file descriptor handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @fence@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Queue.VkFence'
--     handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.vkGetFenceFdKHR'
data FenceGetFdInfoKHR = FenceGetFdInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "FenceGetFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FenceGetFdInfoKHR" "fence"
  fence :: Fence
  , -- No documentation found for Nested "FenceGetFdInfoKHR" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkFenceGetFdInfoKHR' and
-- marshal a 'FenceGetFdInfoKHR' into it. The 'VkFenceGetFdInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructFenceGetFdInfoKHR :: FenceGetFdInfoKHR -> (VkFenceGetFdInfoKHR -> IO a) -> IO a
withCStructFenceGetFdInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: FenceGetFdInfoKHR)) (\pPNext -> cont (VkFenceGetFdInfoKHR VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR pPNext (fence (marshalled :: FenceGetFdInfoKHR)) (handleType (marshalled :: FenceGetFdInfoKHR))))

-- | A function to read a 'VkFenceGetFdInfoKHR' and all additional
-- structures in the pointer chain into a 'FenceGetFdInfoKHR'.
fromCStructFenceGetFdInfoKHR :: VkFenceGetFdInfoKHR -> IO FenceGetFdInfoKHR
fromCStructFenceGetFdInfoKHR c = FenceGetFdInfoKHR <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFenceGetFdInfoKHR)))
                                                   <*> pure (vkFence (c :: VkFenceGetFdInfoKHR))
                                                   <*> pure (vkHandleType (c :: VkFenceGetFdInfoKHR))

instance Zero FenceGetFdInfoKHR where
  zero = FenceGetFdInfoKHR Nothing
                           zero
                           zero



-- | VkImportFenceFdInfoKHR - (None)
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
-- > | _KHR_external_fence_c |                       |                       |
-- > | apabilities.VK_EXTERN |                       |                       |
-- > | AL_FENCE_HANDLE_TYPE_ |                       |                       |
-- > | OPAQUE_FD_BIT'        |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.Co | Copy                  | Temporary             |
-- > | re11.Promoted_from_VK |                       |                       |
-- > | _KHR_external_fence_c |                       |                       |
-- > | apabilities.VK_EXTERN |                       |                       |
-- > | AL_FENCE_HANDLE_TYPE_ |                       |                       |
-- > | SYNC_FD_BIT'          |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > Handle Types Supported by
-- > 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR'
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fence-handletypes-fd Handle Types Supported by VkImportFenceFdInfoKHR>
--     table.
--
-- -   @fd@ /must/ obey any requirements listed for @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-fence-handle-types-compatibility external fence handle types compatibility>.
--
-- If @handleType@ is
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT',
-- the special value @-1@ for @fd@ is treated like a valid sync file
-- descriptor referring to an object that has already signaled. The import
-- operation will succeed and the 'Graphics.Vulkan.C.Core10.Queue.VkFence'
-- will have a temporarily imported payload as if a valid file descriptor
-- had been provided.
--
-- __Note__
--
-- This special behavior for importing an invalid sync file descriptor
-- allows easier interoperability with other system APIs which use the
-- convention that an invalid sync file descriptor represents work that has
-- already completed and does not need to be waited for. It is consistent
-- with the option for implementations to return a @-1@ file descriptor
-- when exporting a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT'
-- from a 'Graphics.Vulkan.C.Core10.Queue.VkFence' which is signaled.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @fence@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Queue.VkFence'
--     handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlagBits'
--     values
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
--     value
--
-- == Host Synchronization
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.vkImportFenceFdKHR'
data ImportFenceFdInfoKHR = ImportFenceFdInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ImportFenceFdInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "fence"
  fence :: Fence
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "flags"
  flags :: FenceImportFlags
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "ImportFenceFdInfoKHR" "fd"
  fd :: CInt
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImportFenceFdInfoKHR' and
-- marshal a 'ImportFenceFdInfoKHR' into it. The 'VkImportFenceFdInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImportFenceFdInfoKHR :: ImportFenceFdInfoKHR -> (VkImportFenceFdInfoKHR -> IO a) -> IO a
withCStructImportFenceFdInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImportFenceFdInfoKHR)) (\pPNext -> cont (VkImportFenceFdInfoKHR VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR pPNext (fence (marshalled :: ImportFenceFdInfoKHR)) (flags (marshalled :: ImportFenceFdInfoKHR)) (handleType (marshalled :: ImportFenceFdInfoKHR)) (fd (marshalled :: ImportFenceFdInfoKHR))))

-- | A function to read a 'VkImportFenceFdInfoKHR' and all additional
-- structures in the pointer chain into a 'ImportFenceFdInfoKHR'.
fromCStructImportFenceFdInfoKHR :: VkImportFenceFdInfoKHR -> IO ImportFenceFdInfoKHR
fromCStructImportFenceFdInfoKHR c = ImportFenceFdInfoKHR <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportFenceFdInfoKHR)))
                                                         <*> pure (vkFence (c :: VkImportFenceFdInfoKHR))
                                                         <*> pure (vkFlags (c :: VkImportFenceFdInfoKHR))
                                                         <*> pure (vkHandleType (c :: VkImportFenceFdInfoKHR))
                                                         <*> pure (vkFd (c :: VkImportFenceFdInfoKHR))

instance Zero ImportFenceFdInfoKHR where
  zero = ImportFenceFdInfoKHR Nothing
                              zero
                              zero
                              zero
                              zero



-- | vkGetFenceFdKHR - Get a POSIX file descriptor handle for a fence
--
-- = Parameters
--
-- -   @device@ is the logical device that created the fence being
--     exported.
--
-- -   @pGetFdInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkFenceGetFdInfoKHR'
--     structure containing parameters of the export operation.
--
-- -   @pFd@ will return the file descriptor representing the fence
--     payload.
--
-- = Description
--
-- Each call to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.vkGetFenceFdKHR'
-- /must/ create a new file descriptor and transfer ownership of it to the
-- application. To avoid leaking resources, the application /must/ release
-- ownership of the file descriptor when it is no longer needed.
--
-- __Note__
--
-- Ownership can be released in many ways. For example, the application can
-- call @close@() on the file descriptor, or transfer ownership back to
-- Vulkan by using the file descriptor to import a fence payload.
--
-- If @pGetFdInfo@->@handleType@ is
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT'
-- and the fence is signaled at the time
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.vkGetFenceFdKHR'
-- is called, @pFd@ /may/ return the value @-1@ instead of a valid file
-- descriptor.
--
-- Where supported by the operating system, the implementation /must/ set
-- the file descriptor to be closed automatically when an @execve@ system
-- call is made.
--
-- Exporting a file descriptor from a fence /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence State>.
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkFenceGetFdInfoKHR'
getFenceFdKHR :: Device ->  FenceGetFdInfoKHR ->  IO (CInt)
getFenceFdKHR = \(Device device' commandTable) -> \getFdInfo' -> alloca (\pFd' -> (\marshalled -> withCStructFenceGetFdInfoKHR marshalled . flip with) getFdInfo' (\pGetFdInfo' -> vkGetFenceFdKHR commandTable device' pGetFdInfo' pFd' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pFd'))))


-- | vkImportFenceFdKHR - Import a fence from a POSIX file descriptor
--
-- = Parameters
--
-- -   @device@ is the logical device that created the fence.
--
-- -   @pImportFenceFdInfo@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR'
--     structure specifying the fence and import parameters.
--
-- = Description
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR'
importFenceFdKHR :: Device ->  ImportFenceFdInfoKHR ->  IO ()
importFenceFdKHR = \(Device device' commandTable) -> \importFenceFdInfo' -> (\marshalled -> withCStructImportFenceFdInfoKHR marshalled . flip with) importFenceFdInfo' (\pImportFenceFdInfo' -> vkImportFenceFdKHR commandTable device' pImportFenceFdInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME = VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_FD_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
