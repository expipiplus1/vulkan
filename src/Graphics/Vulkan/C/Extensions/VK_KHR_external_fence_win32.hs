{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  , FN_vkGetFenceWin32HandleKHR
  , PFN_vkGetFenceWin32HandleKHR
  , vkGetFenceWin32HandleKHR
  , FN_vkImportFenceWin32HandleKHR
  , PFN_vkImportFenceWin32HandleKHR
  , vkImportFenceWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  ) where

import Data.String
  ( IsString
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
  ( VkFence
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkFenceImportFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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
--     'VkExportFenceWin32HandleInfoKHR' /must/ not be in the @pNext@ chain
--     of 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR'
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.SECURITY_ATTRIBUTES'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkExportFenceWin32HandleInfoKHR = VkExportFenceWin32HandleInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pAttributes@ is a pointer to a Windows
  -- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.SECURITY_ATTRIBUTES'
  -- structure specifying security attributes of the handle.
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- | @dwAccess@ is a
  -- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.DWORD'
  -- specifying access rights of the handle.
  vkDwAccess :: DWORD
  , -- | @name@ is a NULL-terminated UTF-16 string to associate with the
  -- underlying synchronization primitive referenced by NT handles exported
  -- from the created fence.
  vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkExportFenceWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkExportFenceWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkPAttributes (poked :: VkExportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkExportFenceWin32HandleInfoKHR))

instance Zero VkExportFenceWin32HandleInfoKHR where
  zero = VkExportFenceWin32HandleInfoKHR VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
                                         zero
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
--     'vkGetFenceWin32HandleKHR' /must/ be called no more than once for
--     each valid unique combination of @fence@ and @handleType@.
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
--     'VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR'
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
-- 'vkGetFenceWin32HandleKHR'
data VkFenceGetWin32HandleInfoKHR = VkFenceGetWin32HandleInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @fence@ is the fence from which state will be exported.
  vkFence :: VkFence
  , -- | @handleType@ is the type of handle requested.
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkFenceGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkFenceGetWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFenceGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFenceGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkFenceGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkFenceGetWin32HandleInfoKHR))

instance Zero VkFenceGetWin32HandleInfoKHR where
  zero = VkFenceGetWin32HandleInfoKHR VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
                                      zero
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
-- > Handle Types Supported by 'VkImportFenceWin32HandleInfoKHR'
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
--     'VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR'
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
-- 'vkImportFenceWin32HandleKHR'
data VkImportFenceWin32HandleInfoKHR = VkImportFenceWin32HandleInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @fence@ is the fence into which the state will be imported.
  vkFence :: VkFence
  , -- | @flags@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlagBits'
  -- specifying additional parameters for the fence payload import operation.
  vkFlags :: VkFenceImportFlags
  , -- | @handleType@ specifies the type of @handle@.
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
  , -- | @handle@ is the external handle to import, or @NULL@.
  vkHandle :: HANDLE
  , -- | @name@ is the NULL-terminated UTF-16 string naming the underlying
  -- synchronization primitive to import, or @NULL@.
  vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkImportFenceWin32HandleInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkImportFenceWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 28)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFence (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkHandle (poked :: VkImportFenceWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkName (poked :: VkImportFenceWin32HandleInfoKHR))

instance Zero VkImportFenceWin32HandleInfoKHR where
  zero = VkImportFenceWin32HandleInfoKHR VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
                                         zero
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
--     'VkFenceGetWin32HandleInfoKHR' structure containing parameters of
--     the export operation.
--
-- -   @pHandle@ will return the Windows handle representing the fence
--     state.
--
-- = Description
--
-- For handle types defined as NT handles, the handles returned by
-- 'vkGetFenceWin32HandleKHR' are owned by the application. To avoid
-- leaking resources, the application /must/ release ownership of them
-- using the @CloseHandle@ system call when they are no longer needed.
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
-- 'VkFenceGetWin32HandleInfoKHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetFenceWin32HandleKHR" vkGetFenceWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
#else
vkGetFenceWin32HandleKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
vkGetFenceWin32HandleKHR deviceCmds = mkVkGetFenceWin32HandleKHR (pVkGetFenceWin32HandleKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceWin32HandleKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
#endif

type FN_vkGetFenceWin32HandleKHR = ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
type PFN_vkGetFenceWin32HandleKHR = FunPtr FN_vkGetFenceWin32HandleKHR

-- | vkImportFenceWin32HandleKHR - Import a fence from a Windows HANDLE
--
-- = Parameters
--
-- -   @device@ is the logical device that created the fence.
--
-- -   @pImportFenceWin32HandleInfo@ points to a
--     'VkImportFenceWin32HandleInfoKHR' structure specifying the fence and
--     import parameters.
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
-- 'VkImportFenceWin32HandleInfoKHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkImportFenceWin32HandleKHR" vkImportFenceWin32HandleKHR :: ("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult
#else
vkImportFenceWin32HandleKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult
vkImportFenceWin32HandleKHR deviceCmds = mkVkImportFenceWin32HandleKHR (pVkImportFenceWin32HandleKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportFenceWin32HandleKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult)
#endif

type FN_vkImportFenceWin32HandleKHR = ("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult
type PFN_vkImportFenceWin32HandleKHR = FunPtr FN_vkImportFenceWin32HandleKHR

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = "VK_KHR_external_fence_win32"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114000
