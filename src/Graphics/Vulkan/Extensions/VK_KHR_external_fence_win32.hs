{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
  , vkGetFenceWin32HandleKHR
  , vkImportFenceWin32HandleKHR
  , VkImportFenceWin32HandleInfoKHR(..)
  , VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
  ) where

import Data.String
  ( IsString
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
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , SECURITY_ATTRIBUTES
  , HANDLE
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000114002
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = "VK_KHR_external_fence_win32"
-- | vkGetFenceWin32HandleKHR - Get a Windows HANDLE for a fence
--
-- = Parameters
-- #_parameters#
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
-- #_description#
--
-- For handle types defined as NT handles, the handles returned by
-- @vkGetFenceWin32HandleKHR@ are owned by the application. To avoid
-- leaking resources, the application /must/ release ownership of them
-- using the @CloseHandle@ system call when they are no longer needed.
--
-- Exporting a Windows handle from a fence /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <{html_spec_relative}#synchronization-fences-importing Importing Fence Payloads>.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pGetWin32HandleInfo@ /must/ be a valid pointer to a valid
--     @VkFenceGetWin32HandleInfoKHR@ structure
--
-- -   @pHandle@ /must/ be a valid pointer to a @HANDLE@ value
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
-- 'VkFenceGetWin32HandleInfoKHR'
foreign import ccall "vkGetFenceWin32HandleKHR" vkGetFenceWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
-- | vkImportFenceWin32HandleKHR - Import a fence from a Windows HANDLE
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that created the fence.
--
-- -   @pImportFenceWin32HandleInfo@ points to a
--     'VkImportFenceWin32HandleInfoKHR' structure specifying the fence and
--     import parameters.
--
-- = Description
-- #_description#
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
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pImportFenceWin32HandleInfo@ /must/ be a valid pointer to a valid
--     @VkImportFenceWin32HandleInfoKHR@ structure
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
-- 'VkImportFenceWin32HandleInfoKHR'
foreign import ccall "vkImportFenceWin32HandleKHR" vkImportFenceWin32HandleKHR :: ("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult
-- | VkImportFenceWin32HandleInfoKHR - (None)
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
-- > | NDLE_TYPE_OPAQUE_WIN3 |                       |                       |
-- > | 2_BIT@                |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | @VK_EXTERNAL_FENCE_HA | Reference             | Temporary,Permanent   |
-- > | NDLE_TYPE_OPAQUE_WIN3 |                       |                       |
-- > | 2_KMT_BIT@            |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > Handle Types Supported by VkImportFenceWin32HandleInfoKHR
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <{html_spec_relative}#synchronization-fence-handletypes-win32 Handle Types Supported by VkImportFenceWin32HandleInfoKHR>
--     table.
--
-- -   If @handleType@ is not
--     @VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT@, @name@ /must/ be
--     @NULL@.
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
--     <{html_spec_relative}#external-fence-handle-types-compatibility external fence handle types compatibility>.
--
-- -   If @name@ is not @NULL@, it /must/ obey any requirements listed for
--     @handleType@ in
--     <{html_spec_relative}#external-fence-handle-types-compatibility external fence handle types compatibility>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @fence@ /must/ be a valid @VkFence@ handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence.VkFenceImportFlagBits'
--     values
--
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
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
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkImportFenceWin32HandleKHR'
data VkImportFenceWin32HandleInfoKHR = VkImportFenceWin32HandleInfoKHR
  { -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "vkFence"
  vkFence :: VkFence
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "vkFlags"
  vkFlags :: VkFenceImportFlags
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "vkHandleType"
  vkHandleType :: VkExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "vkHandle"
  vkHandle :: HANDLE
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "vkName"
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
-- | VkExportFenceWin32HandleInfoKHR - Structure specifying additional
-- attributes of Windows handles exported from a fence
--
-- = Description
-- #_description#
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
-- @VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT@
--
-- [1]
--     <https://msdn.microsoft.com/en-us/library/windows/desktop/ms686670.aspx>
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo'::@handleTypes@
--     does not include @VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT@,
--     VkExportFenceWin32HandleInfoKHR /must/ not be in the @pNext@ chain
--     of 'Graphics.Vulkan.Core10.Fence.VkFenceCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR@
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid @SECURITY_ATTRIBUTES@ value
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkExportFenceWin32HandleInfoKHR = VkExportFenceWin32HandleInfoKHR
  { -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "vkPAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "vkDwAccess"
  vkDwAccess :: DWORD
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "vkName"
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
-- | VkFenceGetWin32HandleInfoKHR - Structure describing a Win32 handle fence
-- export operation
--
-- = Description
-- #_description#
--
-- The properties of the handle returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceHandleTypeFlagBits'
-- for a description of the properties of the defined external fence handle
-- types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo'::@handleTypes@
--     when the @fence@’s current payload was created.
--
-- -   If @handleType@ is defined as an NT handle,
--     'vkGetFenceWin32HandleKHR' /must/ be called no more than once for
--     each valid unique combination of @fence@ and @handleType@.
--
-- -   @fence@ /must/ not currently have its payload replaced by an
--     imported payload as described below in
--     <{html_spec_relative}#synchronization-fences-importing Importing Fence Payloads>
--     unless that imported payload’s handle type was included in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties'::@exportFromImportedHandleTypes@
--     for @handleType@.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, @fence@ /must/ be signaled, or have an
--     associated
--     <{html_spec_relative}#synchronization-fences-signaling fence signal operation>
--     pending execution.
--
-- -   @handleType@ /must/ be defined as an NT handle or a global share
--     handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR@
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
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetFenceWin32HandleKHR'
data VkFenceGetWin32HandleInfoKHR = VkFenceGetWin32HandleInfoKHR
  { -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "vkFence"
  vkFence :: VkFence
  , -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "vkHandleType"
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
