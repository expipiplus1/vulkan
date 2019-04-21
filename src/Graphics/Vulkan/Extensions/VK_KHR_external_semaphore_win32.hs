{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( withCStructD3D12FenceSubmitInfoKHR
  , fromCStructD3D12FenceSubmitInfoKHR
  , D3D12FenceSubmitInfoKHR(..)
  , withCStructExportSemaphoreWin32HandleInfoKHR
  , fromCStructExportSemaphoreWin32HandleInfoKHR
  , ExportSemaphoreWin32HandleInfoKHR(..)
  , withCStructImportSemaphoreWin32HandleInfoKHR
  , fromCStructImportSemaphoreWin32HandleInfoKHR
  , ImportSemaphoreWin32HandleInfoKHR(..)
  , withCStructSemaphoreGetWin32HandleInfoKHR
  , fromCStructSemaphoreGetWin32HandleInfoKHR
  , SemaphoreGetWin32HandleInfoKHR(..)
  , getSemaphoreWin32HandleKHR
  , importSemaphoreWin32HandleKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Function
  ( (&)
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word64
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
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( LPCWSTR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( VkD3D12FenceSubmitInfoKHR(..)
  , VkExportSemaphoreWin32HandleInfoKHR(..)
  , VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  , vkGetSemaphoreWin32HandleKHR
  , vkImportSemaphoreWin32HandleKHR
  , pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
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
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
  )



-- | VkD3D12FenceSubmitInfoKHR - Structure specifying values for Direct3D 12
-- fence-backed semaphores
--
-- = Description
--
-- If the semaphore in
-- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'::@pWaitSemaphores@ or
-- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'::@pSignalSemaphores@
-- corresponding to an entry in @pWaitSemaphoreValues@ or
-- @pSignalSemaphoreValues@ respectively does not currently have a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-payloads payload>
-- referring to a Direct3D 12 fence, the implementation /must/ ignore the
-- value in the @pWaitSemaphoreValues@ or @pSignalSemaphoreValues@ entry.
--
-- == Valid Usage
--
-- Unresolved directive in VkD3D12FenceSubmitInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkD3D12FenceSubmitInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data D3D12FenceSubmitInfoKHR = D3D12FenceSubmitInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "D3D12FenceSubmitInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  -- Optional length valued member elided
  , -- No documentation found for Nested "D3D12FenceSubmitInfoKHR" "pWaitSemaphoreValues"
  waitSemaphoreValues :: Maybe (Vector Word64)
  -- Optional length valued member elided
  , -- No documentation found for Nested "D3D12FenceSubmitInfoKHR" "pSignalSemaphoreValues"
  signalSemaphoreValues :: Maybe (Vector Word64)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkD3D12FenceSubmitInfoKHR' and
-- marshal a 'D3D12FenceSubmitInfoKHR' into it. The 'VkD3D12FenceSubmitInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructD3D12FenceSubmitInfoKHR :: D3D12FenceSubmitInfoKHR -> (VkD3D12FenceSubmitInfoKHR -> IO a) -> IO a
withCStructD3D12FenceSubmitInfoKHR marshalled cont = maybeWith (withVec (&)) (signalSemaphoreValues (marshalled :: D3D12FenceSubmitInfoKHR)) (\pPSignalSemaphoreValues -> maybeWith (withVec (&)) (waitSemaphoreValues (marshalled :: D3D12FenceSubmitInfoKHR)) (\pPWaitSemaphoreValues -> maybeWith withSomeVkStruct (next (marshalled :: D3D12FenceSubmitInfoKHR)) (\pPNext -> cont (VkD3D12FenceSubmitInfoKHR VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR pPNext (maybe 0 (fromIntegral . Data.Vector.length) (waitSemaphoreValues (marshalled :: D3D12FenceSubmitInfoKHR))) pPWaitSemaphoreValues (maybe 0 (fromIntegral . Data.Vector.length) (signalSemaphoreValues (marshalled :: D3D12FenceSubmitInfoKHR))) pPSignalSemaphoreValues))))

-- | A function to read a 'VkD3D12FenceSubmitInfoKHR' and all additional
-- structures in the pointer chain into a 'D3D12FenceSubmitInfoKHR'.
fromCStructD3D12FenceSubmitInfoKHR :: VkD3D12FenceSubmitInfoKHR -> IO D3D12FenceSubmitInfoKHR
fromCStructD3D12FenceSubmitInfoKHR c = D3D12FenceSubmitInfoKHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkD3D12FenceSubmitInfoKHR)))
                                                               -- Optional length valued member elided
                                                               <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkWaitSemaphoreValuesCount (c :: VkD3D12FenceSubmitInfoKHR))) (peekElemOff p)) (vkPWaitSemaphoreValues (c :: VkD3D12FenceSubmitInfoKHR))
                                                               -- Optional length valued member elided
                                                               <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkSignalSemaphoreValuesCount (c :: VkD3D12FenceSubmitInfoKHR))) (peekElemOff p)) (vkPSignalSemaphoreValues (c :: VkD3D12FenceSubmitInfoKHR))

instance Zero D3D12FenceSubmitInfoKHR where
  zero = D3D12FenceSubmitInfoKHR Nothing
                                 Nothing
                                 Nothing



-- | VkExportSemaphoreWin32HandleInfoKHR - Structure specifying additional
-- attributes of Windows handles exported from a semaphore
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
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT'
--
-- And
--
-- @GENERIC_ALL@
--
-- for handles of the following types:
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT'
--
-- [1]
--     <https://msdn.microsoft.com/en-us/library/windows/desktop/ms686670.aspx>
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo'::@handleTypes@
--     does not include
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkExportSemaphoreWin32HandleInfoKHR'
--     /must/ not be in the @pNext@ chain of
--     'Graphics.Vulkan.C.Core10.QueueSemaphore.VkSemaphoreCreateInfo'.
--
-- Unresolved directive in VkExportSemaphoreWin32HandleInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkExportSemaphoreWin32HandleInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data ExportSemaphoreWin32HandleInfoKHR = ExportSemaphoreWin32HandleInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "pAttributes"
  attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "dwAccess"
  dwAccess :: DWORD
  , -- No documentation found for Nested "ExportSemaphoreWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExportSemaphoreWin32HandleInfoKHR' and
-- marshal a 'ExportSemaphoreWin32HandleInfoKHR' into it. The 'VkExportSemaphoreWin32HandleInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExportSemaphoreWin32HandleInfoKHR :: ExportSemaphoreWin32HandleInfoKHR -> (VkExportSemaphoreWin32HandleInfoKHR -> IO a) -> IO a
withCStructExportSemaphoreWin32HandleInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExportSemaphoreWin32HandleInfoKHR)) (\pPNext -> cont (VkExportSemaphoreWin32HandleInfoKHR VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR pPNext (attributes (marshalled :: ExportSemaphoreWin32HandleInfoKHR)) (dwAccess (marshalled :: ExportSemaphoreWin32HandleInfoKHR)) (name (marshalled :: ExportSemaphoreWin32HandleInfoKHR))))

-- | A function to read a 'VkExportSemaphoreWin32HandleInfoKHR' and all additional
-- structures in the pointer chain into a 'ExportSemaphoreWin32HandleInfoKHR'.
fromCStructExportSemaphoreWin32HandleInfoKHR :: VkExportSemaphoreWin32HandleInfoKHR -> IO ExportSemaphoreWin32HandleInfoKHR
fromCStructExportSemaphoreWin32HandleInfoKHR c = ExportSemaphoreWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportSemaphoreWin32HandleInfoKHR)))
                                                                                   <*> pure (vkPAttributes (c :: VkExportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkDwAccess (c :: VkExportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkName (c :: VkExportSemaphoreWin32HandleInfoKHR))

instance Zero ExportSemaphoreWin32HandleInfoKHR where
  zero = ExportSemaphoreWin32HandleInfoKHR Nothing
                                           zero
                                           zero
                                           zero



-- | VkImportSemaphoreWin32HandleInfoKHR - Structure specifying Windows
-- handle to import to a semaphore
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
-- > | LE_TYPE_OPAQUE_WIN32_ |                       |                       |
-- > | BIT'                  |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.Co | Reference             | Temporary,Permanent   |
-- > | re11.Promoted_from_VK |                       |                       |
-- > | _KHR_external_semapho |                       |                       |
-- > | re_capabilities.VK_EX |                       |                       |
-- > | TERNAL_SEMAPHORE_HAND |                       |                       |
-- > | LE_TYPE_OPAQUE_WIN32_ |                       |                       |
-- > | KMT_BIT'              |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | 'Graphics.Vulkan.C.Co | Reference             | Temporary,Permanent   |
-- > | re11.Promoted_from_VK |                       |                       |
-- > | _KHR_external_semapho |                       |                       |
-- > | re_capabilities.VK_EX |                       |                       |
-- > | TERNAL_SEMAPHORE_HAND |                       |                       |
-- > | LE_TYPE_D3D12_FENCE_B |                       |                       |
-- > | IT'                   |                       |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > Handle Types Supported by
-- > 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkImportSemaphoreWin32HandleInfoKHR'
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphore-handletypes-win32 Handle Types Supported by VkImportSemaphoreWin32HandleInfoKHR>
--     table.
--
-- -   If @handleType@ is not
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT',
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
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-semaphore-handle-types-compatibility external semaphore handle types compatibility>.
--
-- -   If @name@ is not @NULL@, it /must/ obey any requirements listed for
--     @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#external-semaphore-handle-types-compatibility external semaphore handle types compatibility>.
--
-- Unresolved directive in VkImportSemaphoreWin32HandleInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkImportSemaphoreWin32HandleInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data ImportSemaphoreWin32HandleInfoKHR = ImportSemaphoreWin32HandleInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "semaphore"
  semaphore :: Semaphore
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "flags"
  flags :: SemaphoreImportFlags
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "handle"
  handle :: HANDLE
  , -- No documentation found for Nested "ImportSemaphoreWin32HandleInfoKHR" "name"
  name :: LPCWSTR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImportSemaphoreWin32HandleInfoKHR' and
-- marshal a 'ImportSemaphoreWin32HandleInfoKHR' into it. The 'VkImportSemaphoreWin32HandleInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImportSemaphoreWin32HandleInfoKHR :: ImportSemaphoreWin32HandleInfoKHR -> (VkImportSemaphoreWin32HandleInfoKHR -> IO a) -> IO a
withCStructImportSemaphoreWin32HandleInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImportSemaphoreWin32HandleInfoKHR)) (\pPNext -> cont (VkImportSemaphoreWin32HandleInfoKHR VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR pPNext (semaphore (marshalled :: ImportSemaphoreWin32HandleInfoKHR)) (flags (marshalled :: ImportSemaphoreWin32HandleInfoKHR)) (handleType (marshalled :: ImportSemaphoreWin32HandleInfoKHR)) (handle (marshalled :: ImportSemaphoreWin32HandleInfoKHR)) (name (marshalled :: ImportSemaphoreWin32HandleInfoKHR))))

-- | A function to read a 'VkImportSemaphoreWin32HandleInfoKHR' and all additional
-- structures in the pointer chain into a 'ImportSemaphoreWin32HandleInfoKHR'.
fromCStructImportSemaphoreWin32HandleInfoKHR :: VkImportSemaphoreWin32HandleInfoKHR -> IO ImportSemaphoreWin32HandleInfoKHR
fromCStructImportSemaphoreWin32HandleInfoKHR c = ImportSemaphoreWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImportSemaphoreWin32HandleInfoKHR)))
                                                                                   <*> pure (vkSemaphore (c :: VkImportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkFlags (c :: VkImportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkHandleType (c :: VkImportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkHandle (c :: VkImportSemaphoreWin32HandleInfoKHR))
                                                                                   <*> pure (vkName (c :: VkImportSemaphoreWin32HandleInfoKHR))

instance Zero ImportSemaphoreWin32HandleInfoKHR where
  zero = ImportSemaphoreWin32HandleInfoKHR Nothing
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero



-- | VkSemaphoreGetWin32HandleInfoKHR - Structure describing a Win32 handle
-- semaphore export operation
--
-- = Description
--
-- The properties of the handle returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
-- for a description of the properties of the defined external semaphore
-- handle types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo'::@handleTypes@
--     when the @semaphore@’s current payload was created.
--
-- -   If @handleType@ is defined as an NT handle,
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.vkGetSemaphoreWin32HandleKHR'
--     /must/ be called no more than once for each valid unique combination
--     of @semaphore@ and @handleType@.
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
-- -   @handleType@ /must/ be defined as an NT handle or a global share
--     handle.
--
-- Unresolved directive in VkSemaphoreGetWin32HandleInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkSemaphoreGetWin32HandleInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data SemaphoreGetWin32HandleInfoKHR = SemaphoreGetWin32HandleInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "SemaphoreGetWin32HandleInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SemaphoreGetWin32HandleInfoKHR" "semaphore"
  semaphore :: Semaphore
  , -- No documentation found for Nested "SemaphoreGetWin32HandleInfoKHR" "handleType"
  handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSemaphoreGetWin32HandleInfoKHR' and
-- marshal a 'SemaphoreGetWin32HandleInfoKHR' into it. The 'VkSemaphoreGetWin32HandleInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSemaphoreGetWin32HandleInfoKHR :: SemaphoreGetWin32HandleInfoKHR -> (VkSemaphoreGetWin32HandleInfoKHR -> IO a) -> IO a
withCStructSemaphoreGetWin32HandleInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SemaphoreGetWin32HandleInfoKHR)) (\pPNext -> cont (VkSemaphoreGetWin32HandleInfoKHR VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR pPNext (semaphore (marshalled :: SemaphoreGetWin32HandleInfoKHR)) (handleType (marshalled :: SemaphoreGetWin32HandleInfoKHR))))

-- | A function to read a 'VkSemaphoreGetWin32HandleInfoKHR' and all additional
-- structures in the pointer chain into a 'SemaphoreGetWin32HandleInfoKHR'.
fromCStructSemaphoreGetWin32HandleInfoKHR :: VkSemaphoreGetWin32HandleInfoKHR -> IO SemaphoreGetWin32HandleInfoKHR
fromCStructSemaphoreGetWin32HandleInfoKHR c = SemaphoreGetWin32HandleInfoKHR <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSemaphoreGetWin32HandleInfoKHR)))
                                                                             <*> pure (vkSemaphore (c :: VkSemaphoreGetWin32HandleInfoKHR))
                                                                             <*> pure (vkHandleType (c :: VkSemaphoreGetWin32HandleInfoKHR))

instance Zero SemaphoreGetWin32HandleInfoKHR where
  zero = SemaphoreGetWin32HandleInfoKHR Nothing
                                        zero
                                        zero



-- | vkGetSemaphoreWin32HandleKHR - Get a Windows HANDLE for a semaphore
--
-- = Parameters
--
-- -   @device@ is the logical device that created the semaphore being
--     exported.
--
-- -   @pGetWin32HandleInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkSemaphoreGetWin32HandleInfoKHR'
--     structure containing parameters of the export operation.
--
-- -   @pHandle@ will return the Windows handle representing the semaphore
--     state.
--
-- = Description
--
-- For handle types defined as NT handles, the handles returned by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.vkGetSemaphoreWin32HandleKHR'
-- are owned by the application. To avoid leaking resources, the
-- application /must/ release ownership of them using the @CloseHandle@
-- system call when they are no longer needed.
--
-- Exporting a Windows handle from a semaphore /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>.
--
-- Unresolved directive in vkGetSemaphoreWin32HandleKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetSemaphoreWin32HandleKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getSemaphoreWin32HandleKHR :: Device ->  SemaphoreGetWin32HandleInfoKHR ->  IO (HANDLE)
getSemaphoreWin32HandleKHR = \(Device device' commandTable) -> \getWin32HandleInfo' -> alloca (\pHandle' -> (\marshalled -> withCStructSemaphoreGetWin32HandleInfoKHR marshalled . flip with) getWin32HandleInfo' (\pGetWin32HandleInfo' -> vkGetSemaphoreWin32HandleKHR commandTable device' pGetWin32HandleInfo' pHandle' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pHandle'))))


-- | vkImportSemaphoreWin32HandleKHR - Import a semaphore from a Windows
-- HANDLE
--
-- = Parameters
--
-- -   @device@ is the logical device that created the semaphore.
--
-- -   @pImportSemaphoreWin32HandleInfo@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkImportSemaphoreWin32HandleInfoKHR'
--     structure specifying the semaphore and import parameters.
--
-- = Description
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
-- Unresolved directive in vkImportSemaphoreWin32HandleKHR.txt -
-- include::{generated}\/validity\/protos\/vkImportSemaphoreWin32HandleKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
importSemaphoreWin32HandleKHR :: Device ->  ImportSemaphoreWin32HandleInfoKHR ->  IO ()
importSemaphoreWin32HandleKHR = \(Device device' commandTable) -> \importSemaphoreWin32HandleInfo' -> (\marshalled -> withCStructImportSemaphoreWin32HandleInfoKHR marshalled . flip with) importSemaphoreWin32HandleInfo' (\pImportSemaphoreWin32HandleInfo' -> vkImportSemaphoreWin32HandleKHR commandTable device' pImportSemaphoreWin32HandleInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))
