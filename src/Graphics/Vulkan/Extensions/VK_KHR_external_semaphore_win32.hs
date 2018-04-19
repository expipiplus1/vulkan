{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  , vkGetSemaphoreWin32HandleKHR
  , vkImportSemaphoreWin32HandleKHR
  , VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkExportSemaphoreWin32HandleInfoKHR(..)
  , VkD3D12FenceSubmitInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word64
  , Word32
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
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( DWORD
  , SECURITY_ATTRIBUTES
  , HANDLE
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000078000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR = VkStructureType 1000078001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR"
pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR = VkStructureType 1000078002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR = VkStructureType 1000078003
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME = "VK_KHR_external_semaphore_win32"
-- | vkGetSemaphoreWin32HandleKHR - Get a Windows HANDLE for a semaphore
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that created the semaphore being
--     exported.
--
-- -   @pGetWin32HandleInfo@ is a pointer to an instance of the
--     'VkSemaphoreGetWin32HandleInfoKHR' structure containing parameters
--     of the export operation.
--
-- -   @pHandle@ will return the Windows handle representing the semaphore
--     state.
--
-- = Description
-- #_description#
--
-- For handle types defined as NT handles, the handles returned by
-- @vkGetSemaphoreWin32HandleKHR@ are owned by the application. To avoid
-- leaking resources, the application /must/ release ownership of them
-- using the @CloseHandle@ system call when they are no longer needed.
--
-- Exporting a Windows handle from a semaphore /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <{html_spec_relative}#synchronization-semaphores-importing Importing Semaphore Payloads>.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pGetWin32HandleInfo@ /must/ be a valid pointer to a valid
--     @VkSemaphoreGetWin32HandleInfoKHR@ structure
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
-- 'VkSemaphoreGetWin32HandleInfoKHR'
foreign import ccall "vkGetSemaphoreWin32HandleKHR" vkGetSemaphoreWin32HandleKHR :: ("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult
-- | vkImportSemaphoreWin32HandleKHR - Import a semaphore from a Windows
-- HANDLE
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that created the semaphore.
--
-- -   @pImportSemaphoreWin32HandleInfo@ points to a
--     'VkImportSemaphoreWin32HandleInfoKHR' structure specifying the
--     semaphore and import parameters.
--
-- = Description
-- #_description#
--
-- Importing a semaphore payload from Windows handles does not transfer
-- ownership of the handle to the Vulkan implementation. For handle types
-- defined as NT handles, the application /must/ release ownership using
-- the @CloseHandle@ system call when the handle is no longer needed.
--
-- Applications /can/ import the same semaphore payload into multiple
-- instances of Vulkan, into the same instance from which it was exported,
-- and multiple times into a given Vulkan instance.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pImportSemaphoreWin32HandleInfo@ /must/ be a valid pointer to a
--     valid @VkImportSemaphoreWin32HandleInfoKHR@ structure
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
-- 'VkImportSemaphoreWin32HandleInfoKHR'
foreign import ccall "vkImportSemaphoreWin32HandleKHR" vkImportSemaphoreWin32HandleKHR :: ("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult
-- | VkImportSemaphoreWin32HandleInfoKHR - Structure specifying Windows
-- handle to import to a semaphore
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
-- > | WIN32_BIT@            |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | @VK_EXTERNAL_SEMAPHOR | Reference             | Temporary,Permanent   |
-- > | E_HANDLE_TYPE_OPAQUE_ |                       |                       |
-- > | WIN32_KMT_BIT@        |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | @VK_EXTERNAL_SEMAPHOR | Reference             | Temporary,Permanent   |
-- > | E_HANDLE_TYPE_D3D12_F |                       |                       |
-- > | ENCE_BIT@             |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > Handle Types Supported by VkImportSemaphoreWin32HandleInfoKHR
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <{html_spec_relative}#synchronization-semaphore-handletypes-win32 Handle Types Supported by VkImportSemaphoreWin32HandleInfoKHR>
--     table.
--
-- -   If @handleType@ is not
--     @VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT@ or
--     @VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT@, @name@ /must/
--     be @NULL@.
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
--     <{html_spec_relative}#external-semaphore-handle-types-compatibility external semaphore handle types compatibility>.
--
-- -   If @name@ is not @NULL@, it /must/ obey any requirements listed for
--     @handleType@ in
--     <{html_spec_relative}#external-semaphore-handle-types-compatibility external semaphore handle types compatibility>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @semaphore@ /must/ be a valid @VkSemaphore@ handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlagBits'
--     values
--
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
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
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkImportSemaphoreWin32HandleKHR'
data VkImportSemaphoreWin32HandleInfoKHR = VkImportSemaphoreWin32HandleInfoKHR
  { -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "vkSemaphore"
  vkSemaphore :: VkSemaphore
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "vkFlags"
  vkFlags :: VkSemaphoreImportFlags
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "vkHandleType"
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "vkHandle"
  vkHandle :: HANDLE
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "vkName"
  vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkImportSemaphoreWin32HandleInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkImportSemaphoreWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
                                                 <*> peek (ptr `plusPtr` 32)
                                                 <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkFlags (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 28) (vkHandleType (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkHandle (poked :: VkImportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkName (poked :: VkImportSemaphoreWin32HandleInfoKHR))
-- | VkExportSemaphoreWin32HandleInfoKHR - Structure specifying additional
-- attributes of Windows handles exported from a semaphore
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
-- @VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT@
--
-- And
--
-- @GENERIC_ALL@
--
-- for handles of the following types:
--
-- @VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT@
--
-- [1]
--     <https://msdn.microsoft.com/en-us/library/windows/desktop/ms686670.aspx>
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo'::@handleTypes@
--     does not include
--     @VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT@ or
--     @VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT@,
--     @VkExportSemaphoreWin32HandleInfoKHR@ /must/ not be in the @pNext@
--     chain of
--     'Graphics.Vulkan.Core10.QueueSemaphore.VkSemaphoreCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR@
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid @SECURITY_ATTRIBUTES@ value
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkExportSemaphoreWin32HandleInfoKHR = VkExportSemaphoreWin32HandleInfoKHR
  { -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "vkPAttributes"
  vkPAttributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "vkDwAccess"
  vkDwAccess :: DWORD
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "vkName"
  vkName :: LPCWSTR
  }
  deriving (Eq, Show)

instance Storable VkExportSemaphoreWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkExportSemaphoreWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkPAttributes (poked :: VkExportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDwAccess (poked :: VkExportSemaphoreWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkName (poked :: VkExportSemaphoreWin32HandleInfoKHR))
-- | VkD3D12FenceSubmitInfoKHR - Structure specifying values for Direct3D 12
-- fence-backed semaphores
--
-- = Description
-- #_description#
--
-- If the semaphore in
-- 'Graphics.Vulkan.Core10.Queue.VkSubmitInfo'::@pWaitSemaphores@ or
-- 'Graphics.Vulkan.Core10.Queue.VkSubmitInfo'::@pSignalSemaphores@
-- corresponding to an entry in @pWaitSemaphoreValues@ or
-- @pSignalSemaphoreValues@ respectively does not currently have a
-- <{html_spec_relative}#synchronization-semaphores-payloads payload>
-- referring to a Direct3D 12 fence, the implementation /must/ ignore the
-- value in the @pWaitSemaphoreValues@ or @pSignalSemaphoreValues@ entry.
--
-- == Valid Usage
--
-- -   @waitSemaphoreValuesCount@ /must/ be the same value as
--     @VkSubmitInfo@::@waitSemaphoreCount@, where @VkSubmitInfo@ is in the
--     @pNext@ chain of this @VkD3D12FenceSubmitInfoKHR@ structure.
--
-- -   @signalSemaphoreValuesCount@ /must/ be the same value as
--     @VkSubmitInfo@::@signalSemaphoreCount@, where @VkSubmitInfo@ is in
--     the @pNext@ chain of this @VkD3D12FenceSubmitInfoKHR@ structure.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR@
--
-- -   If @waitSemaphoreValuesCount@ is not @0@, and @pWaitSemaphoreValues@
--     is not @NULL@, @pWaitSemaphoreValues@ /must/ be a valid pointer to
--     an array of @waitSemaphoreValuesCount@ @uint64_t@ values
--
-- -   If @signalSemaphoreValuesCount@ is not @0@, and
--     @pSignalSemaphoreValues@ is not @NULL@, @pSignalSemaphoreValues@
--     /must/ be a valid pointer to an array of
--     @signalSemaphoreValuesCount@ @uint64_t@ values
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkD3D12FenceSubmitInfoKHR = VkD3D12FenceSubmitInfoKHR
  { -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "vkWaitSemaphoreValuesCount"
  vkWaitSemaphoreValuesCount :: Word32
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "vkPWaitSemaphoreValues"
  vkPWaitSemaphoreValues :: Ptr Word64
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "vkSignalSemaphoreValuesCount"
  vkSignalSemaphoreValuesCount :: Word32
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "vkPSignalSemaphoreValues"
  vkPSignalSemaphoreValues :: Ptr Word64
  }
  deriving (Eq, Show)

instance Storable VkD3D12FenceSubmitInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkD3D12FenceSubmitInfoKHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
                                       <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreValuesCount (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphoreValues (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSignalSemaphoreValuesCount (poked :: VkD3D12FenceSubmitInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkPSignalSemaphoreValues (poked :: VkD3D12FenceSubmitInfoKHR))
-- | VkSemaphoreGetWin32HandleInfoKHR - Structure describing a Win32 handle
-- semaphore export operation
--
-- = Description
-- #_description#
--
-- The properties of the handle returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
-- for a description of the properties of the defined external semaphore
-- handle types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo'::@handleTypes@
--     when the @semaphore@’s current payload was created.
--
-- -   If @handleType@ is defined as an NT handle,
--     'vkGetSemaphoreWin32HandleKHR' /must/ be called no more than once
--     for each valid unique combination of @semaphore@ and @handleType@.
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
-- -   @handleType@ /must/ be defined as an NT handle or a global share
--     handle.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR@
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
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkGetSemaphoreWin32HandleKHR'
data VkSemaphoreGetWin32HandleInfoKHR = VkSemaphoreGetWin32HandleInfoKHR
  { -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "vkSemaphore"
  vkSemaphore :: VkSemaphore
  , -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "vkHandleType"
  vkHandleType :: VkExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Eq, Show)

instance Storable VkSemaphoreGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkSemaphoreGetWin32HandleInfoKHR <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSemaphoreGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSemaphoreGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSemaphore (poked :: VkSemaphoreGetWin32HandleInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkHandleType (poked :: VkSemaphoreGetWin32HandleInfoKHR))
