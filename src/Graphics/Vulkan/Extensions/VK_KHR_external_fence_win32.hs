{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( withCStructExportFenceWin32HandleInfoKHR
  , fromCStructExportFenceWin32HandleInfoKHR
  , ExportFenceWin32HandleInfoKHR(..)
  , withCStructFenceGetWin32HandleInfoKHR
  , fromCStructFenceGetWin32HandleInfoKHR
  , FenceGetWin32HandleInfoKHR(..)
  , withCStructImportFenceWin32HandleInfoKHR
  , fromCStructImportFenceWin32HandleInfoKHR
  , ImportFenceWin32HandleInfoKHR(..)
  , getFenceWin32HandleKHR
  , importFenceWin32HandleKHR
  , pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
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
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  , vkGetFenceWin32HandleKHR
  , vkImportFenceWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
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
  ( pattern STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  )



-- | VkExportFenceWin32HandleInfoKHR - Structure specifying additional
-- attributes of Windows handles exported from a fence
--
-- = Description
--
-- If this structure is not present, or if @pAttributes@ is set to @NULL@,
-- default security descriptor values will be used, and child processes
-- created by the application will not inherit the handle, as described in
-- the MSDN documentation for “Synchronization Object Security and Access
-- Rights”1. Further, if the structure is not present, the access rights
-- will be
--
-- @DXGI_SHARED_RESOURCE_READ@ | @DXGI_SHARED_RESOURCE_WRITE@
--
-- for handles of the following types:
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT'
--
-- [1]
--     <https://msdn.microsoft.com/en-us/library/windows/desktop/ms686670.aspx>
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo'::@handleTypes@
--     does not include
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkExportFenceWin32HandleInfoKHR'
--     /must/ not be in the @pNext@ chain of
--     'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR'
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.SECURITY_ATTRIBUTES'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExportFenceWin32HandleInfoKHR = ExportFenceWin32HandleInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "pAttributes"
  attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "dwAccess"
  dwAccess :: DWORD
  , -- No documentation found for Nested "ExportFenceWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExportFenceWin32HandleInfoKHR' and
-- marshal a 'ExportFenceWin32HandleInfoKHR' into it. The 'VkExportFenceWin32HandleInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExportFenceWin32HandleInfoKHR :: ExportFenceWin32HandleInfoKHR -> (VkExportFenceWin32HandleInfoKHR -> IO a) -> IO a
withCStructExportFenceWin32HandleInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExportFenceWin32HandleInfoKHR)) (\pPNext -> cont (VkExportFenceWin32HandleInfoKHR VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR pPNext (attributes (marshalled :: ExportFenceWin32HandleInfoKHR)) (dwAccess (marshalled :: ExportFenceWin32HandleInfoKHR)) (name (marshalled :: ExportFenceWin32HandleInfoKHR))))

-- | A function to read a 'VkExportFenceWin32HandleInfoKHR' and all additional
-- structures in the pointer chain into a 'ExportFenceWin32HandleInfoKHR'.
fromCStructExportFenceWin32HandleInfoKHR :: VkExportFenceWin32HandleInfoKHR -> IO ExportFenceWin32HandleInfoKHR
fromCStructExportFenceWin32HandleInfoKHR c = ExportFenceWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportFenceWin32HandleInfoKHR)))
                                                                           <*> pure (vkPAttributes (c :: VkExportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkDwAccess (c :: VkExportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkName (c :: VkExportFenceWin32HandleInfoKHR))

instance Zero ExportFenceWin32HandleInfoKHR where
  zero = ExportFenceWin32HandleInfoKHR Nothing
                                       zero
                                       zero
                                       zero



-- | VkFenceGetWin32HandleInfoKHR - Structure describing a Win32 handle fence
-- export operation
--
-- = Description
--
-- The properties of the handle returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
-- for a description of the properties of the defined external fence handle
-- types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo'::@handleTypes@
--     when the @fence@’s current payload was created.
--
-- -   If @handleType@ is defined as an NT handle,
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.vkGetFenceWin32HandleKHR'
--     /must/ be called no more than once for each valid unique combination
--     of @fence@ and @handleType@.
--
-- -   @fence@ /must/ not currently have its payload replaced by an
--     imported payload as described below in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence Payloads>
--     unless that imported payload’s handle type was included in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'::@exportFromImportedHandleTypes@
--     for @handleType@.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, @fence@ /must/ be signaled, or have an
--     associated
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>
--     pending execution.
--
-- -   @handleType@ /must/ be defined as an NT handle or a global share
--     handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR'
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.vkGetFenceWin32HandleKHR'
data FenceGetWin32HandleInfoKHR = FenceGetWin32HandleInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "FenceGetWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "FenceGetWin32HandleInfoKHR" "fence"
  fence :: Fence
  , -- No documentation found for Nested "FenceGetWin32HandleInfoKHR" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkFenceGetWin32HandleInfoKHR' and
-- marshal a 'FenceGetWin32HandleInfoKHR' into it. The 'VkFenceGetWin32HandleInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructFenceGetWin32HandleInfoKHR :: FenceGetWin32HandleInfoKHR -> (VkFenceGetWin32HandleInfoKHR -> IO a) -> IO a
withCStructFenceGetWin32HandleInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: FenceGetWin32HandleInfoKHR)) (\pPNext -> cont (VkFenceGetWin32HandleInfoKHR VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR pPNext (fence (marshalled :: FenceGetWin32HandleInfoKHR)) (handleType (marshalled :: FenceGetWin32HandleInfoKHR))))

-- | A function to read a 'VkFenceGetWin32HandleInfoKHR' and all additional
-- structures in the pointer chain into a 'FenceGetWin32HandleInfoKHR'.
fromCStructFenceGetWin32HandleInfoKHR :: VkFenceGetWin32HandleInfoKHR -> IO FenceGetWin32HandleInfoKHR
fromCStructFenceGetWin32HandleInfoKHR c = FenceGetWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkFenceGetWin32HandleInfoKHR)))
                                                                     <*> pure (vkFence (c :: VkFenceGetWin32HandleInfoKHR))
                                                                     <*> pure (vkHandleType (c :: VkFenceGetWin32HandleInfoKHR))

instance Zero FenceGetWin32HandleInfoKHR where
  zero = FenceGetWin32HandleInfoKHR Nothing
                                    zero
                                    zero



-- | VkImportFenceWin32HandleInfoKHR - (None)
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
-- > | OPAQUE_WIN32_BIT'     |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.Co | Reference             | Temporary,Permanent   |
-- > | re11.Promoted_from_VK |                       |                       |
-- > | _KHR_external_fence_c |                       |                       |
-- > | apabilities.VK_EXTERN |                       |                       |
-- > | AL_FENCE_HANDLE_TYPE_ |                       |                       |
-- > | OPAQUE_WIN32_KMT_BIT' |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > Handle Types Supported by
-- > 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR'
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fence-handletypes-win32 Handle Types Supported by VkImportFenceWin32HandleInfoKHR>
--     table.
--
-- -   If @handleType@ is not
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     @name@ /must/ be @NULL@.
--
-- -   If @handleType@ is not @0@ and @handle@ is @NULL@, @name@ /must/
--     name a valid synchronization primitive of the type specified by
--     @handleType@.
--
-- -   If @handleType@ is not @0@ and @name@ is @NULL@, @handle@ /must/ be
--     a valid handle of the type specified by @handleType@.
--
-- -   If @handle@ is not @NULL@, @name@ must be @NULL@.
--
-- -   If @handle@ is not @NULL@, it /must/ obey any requirements listed
--     for @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-fence-handle-types-compatibility external fence handle types compatibility>.
--
-- -   If @name@ is not @NULL@, it /must/ obey any requirements listed for
--     @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-fence-handle-types-compatibility external fence handle types compatibility>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR'
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
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.vkImportFenceWin32HandleKHR'
data ImportFenceWin32HandleInfoKHR = ImportFenceWin32HandleInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "fence"
  fence :: Fence
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "flags"
  flags :: FenceImportFlags
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "handleType"
  handleType :: ExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "handle"
  handle :: HANDLE
  , -- No documentation found for Nested "ImportFenceWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImportFenceWin32HandleInfoKHR' and
-- marshal a 'ImportFenceWin32HandleInfoKHR' into it. The 'VkImportFenceWin32HandleInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImportFenceWin32HandleInfoKHR :: ImportFenceWin32HandleInfoKHR -> (VkImportFenceWin32HandleInfoKHR -> IO a) -> IO a
withCStructImportFenceWin32HandleInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImportFenceWin32HandleInfoKHR)) (\pPNext -> cont (VkImportFenceWin32HandleInfoKHR VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR pPNext (fence (marshalled :: ImportFenceWin32HandleInfoKHR)) (flags (marshalled :: ImportFenceWin32HandleInfoKHR)) (handleType (marshalled :: ImportFenceWin32HandleInfoKHR)) (handle (marshalled :: ImportFenceWin32HandleInfoKHR)) (name (marshalled :: ImportFenceWin32HandleInfoKHR))))

-- | A function to read a 'VkImportFenceWin32HandleInfoKHR' and all additional
-- structures in the pointer chain into a 'ImportFenceWin32HandleInfoKHR'.
fromCStructImportFenceWin32HandleInfoKHR :: VkImportFenceWin32HandleInfoKHR -> IO ImportFenceWin32HandleInfoKHR
fromCStructImportFenceWin32HandleInfoKHR c = ImportFenceWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportFenceWin32HandleInfoKHR)))
                                                                           <*> pure (vkFence (c :: VkImportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkFlags (c :: VkImportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkHandleType (c :: VkImportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkHandle (c :: VkImportFenceWin32HandleInfoKHR))
                                                                           <*> pure (vkName (c :: VkImportFenceWin32HandleInfoKHR))

instance Zero ImportFenceWin32HandleInfoKHR where
  zero = ImportFenceWin32HandleInfoKHR Nothing
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero



-- | vkGetFenceWin32HandleKHR - Get a Windows HANDLE for a fence
--
-- = Parameters
--
-- -   @device@ is the logical device that created the fence being
--     exported.
--
-- -   @pGetWin32HandleInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkFenceGetWin32HandleInfoKHR'
--     structure containing parameters of the export operation.
--
-- -   @pHandle@ will return the Windows handle representing the fence
--     state.
--
-- = Description
--
-- For handle types defined as NT handles, the handles returned by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.vkGetFenceWin32HandleKHR'
-- are owned by the application. To avoid leaking resources, the
-- application /must/ release ownership of them using the @CloseHandle@
-- system call when they are no longer needed.
--
-- Exporting a Windows handle from a fence /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence Payloads>.
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkFenceGetWin32HandleInfoKHR'
getFenceWin32HandleKHR :: Device ->  FenceGetWin32HandleInfoKHR ->  IO (HANDLE)
getFenceWin32HandleKHR = \(Device device' commandTable) -> \getWin32HandleInfo' -> alloca (\pHandle' -> (\marshalled -> withCStructFenceGetWin32HandleInfoKHR marshalled . flip with) getWin32HandleInfo' (\pGetWin32HandleInfo' -> vkGetFenceWin32HandleKHR commandTable device' pGetWin32HandleInfo' pHandle' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pHandle'))))


-- | vkImportFenceWin32HandleKHR - Import a fence from a Windows HANDLE
--
-- = Parameters
--
-- -   @device@ is the logical device that created the fence.
--
-- -   @pImportFenceWin32HandleInfo@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR'
--     structure specifying the fence and import parameters.
--
-- = Description
--
-- Importing a fence payload from Windows handles does not transfer
-- ownership of the handle to the Vulkan implementation. For handle types
-- defined as NT handles, the application /must/ release ownership using
-- the @CloseHandle@ system call when the handle is no longer needed.
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR'
importFenceWin32HandleKHR :: Device ->  ImportFenceWin32HandleInfoKHR ->  IO ()
importFenceWin32HandleKHR = \(Device device' commandTable) -> \importFenceWin32HandleInfo' -> (\marshalled -> withCStructImportFenceWin32HandleInfoKHR marshalled . flip with) importFenceWin32HandleInfo' (\pImportFenceWin32HandleInfo' -> vkImportFenceWin32HandleKHR commandTable device' pImportFenceWin32HandleInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
