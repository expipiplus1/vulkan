{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_external_fence_win32  ( getFenceWin32HandleKHR
                                                      , importFenceWin32HandleKHR
                                                      , ImportFenceWin32HandleInfoKHR(..)
                                                      , ExportFenceWin32HandleInfoKHR(..)
                                                      , FenceGetWin32HandleInfoKHR(..)
                                                      , KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
                                                      , pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
                                                      , KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
                                                      , pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
                                                      , HANDLE
                                                      , DWORD
                                                      , LPCWSTR
                                                      , SECURITY_ATTRIBUTES
                                                      ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetFenceWin32HandleKHR))
import Vulkan.Dynamic (DeviceCmds(pVkImportFenceWin32HandleKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Extensions.VK_KHR_external_memory_win32 (LPCWSTR)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Extensions.VK_KHR_external_memory_win32 (LPCWSTR)
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceWin32HandleKHR
  :: FunPtr (Ptr Device_T -> Ptr FenceGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result) -> Ptr Device_T -> Ptr FenceGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result

-- | vkGetFenceWin32HandleKHR - Get a Windows HANDLE for a fence
--
-- = Description
--
-- For handle types defined as NT handles, the handles returned by
-- 'getFenceWin32HandleKHR' are owned by the application. To avoid leaking
-- resources, the application /must/ release ownership of them using the
-- @CloseHandle@ system call when they are no longer needed.
--
-- Exporting a Windows handle from a fence /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence Payloads>.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'FenceGetWin32HandleInfoKHR'
getFenceWin32HandleKHR :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device that created the fence being exported.
                          --
                          -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                          Device
                       -> -- | @pGetWin32HandleInfo@ is a pointer to a 'FenceGetWin32HandleInfoKHR'
                          -- structure containing parameters of the export operation.
                          --
                          -- @pGetWin32HandleInfo@ /must/ be a valid pointer to a valid
                          -- 'FenceGetWin32HandleInfoKHR' structure
                          FenceGetWin32HandleInfoKHR
                       -> io (HANDLE)
getFenceWin32HandleKHR device getWin32HandleInfo = liftIO . evalContT $ do
  let vkGetFenceWin32HandleKHRPtr = pVkGetFenceWin32HandleKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetFenceWin32HandleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetFenceWin32HandleKHR is null" Nothing Nothing
  let vkGetFenceWin32HandleKHR' = mkVkGetFenceWin32HandleKHR vkGetFenceWin32HandleKHRPtr
  pGetWin32HandleInfo <- ContT $ withCStruct (getWin32HandleInfo)
  pPHandle <- ContT $ bracket (callocBytes @HANDLE 8) free
  r <- lift $ vkGetFenceWin32HandleKHR' (deviceHandle (device)) pGetWin32HandleInfo (pPHandle)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pHandle <- lift $ peek @HANDLE pPHandle
  pure $ (pHandle)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportFenceWin32HandleKHR
  :: FunPtr (Ptr Device_T -> Ptr ImportFenceWin32HandleInfoKHR -> IO Result) -> Ptr Device_T -> Ptr ImportFenceWin32HandleInfoKHR -> IO Result

-- | vkImportFenceWin32HandleKHR - Import a fence from a Windows HANDLE
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_EXTERNAL_HANDLE'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'ImportFenceWin32HandleInfoKHR'
importFenceWin32HandleKHR :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the logical device that created the fence.
                             --
                             -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                             Device
                          -> -- | @pImportFenceWin32HandleInfo@ is a pointer to a
                             -- 'ImportFenceWin32HandleInfoKHR' structure specifying the fence and
                             -- import parameters.
                             --
                             -- @pImportFenceWin32HandleInfo@ /must/ be a valid pointer to a valid
                             -- 'ImportFenceWin32HandleInfoKHR' structure
                             ImportFenceWin32HandleInfoKHR
                          -> io ()
importFenceWin32HandleKHR device importFenceWin32HandleInfo = liftIO . evalContT $ do
  let vkImportFenceWin32HandleKHRPtr = pVkImportFenceWin32HandleKHR (deviceCmds (device :: Device))
  lift $ unless (vkImportFenceWin32HandleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkImportFenceWin32HandleKHR is null" Nothing Nothing
  let vkImportFenceWin32HandleKHR' = mkVkImportFenceWin32HandleKHR vkImportFenceWin32HandleKHRPtr
  pImportFenceWin32HandleInfo <- ContT $ withCStruct (importFenceWin32HandleInfo)
  r <- lift $ vkImportFenceWin32HandleKHR' (deviceHandle (device)) pImportFenceWin32HandleInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkImportFenceWin32HandleInfoKHR - (None)
--
-- = Description
--
-- The handle types supported by @handleType@ are:
--
-- +-------------------------------------------------------------------------------------------------------+----------------------+-----------------------+
-- | Handle Type                                                                                           | Transference         | Permanence Supported  |
-- +=======================================================================================================+======================+=======================+
-- | 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT'     | Reference            | Temporary,Permanent   |
-- +-------------------------------------------------------------------------------------------------------+----------------------+-----------------------+
-- | 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT' | Reference            | Temporary,Permanent   |
-- +-------------------------------------------------------------------------------------------------------+----------------------+-----------------------+
--
-- Handle Types Supported by 'ImportFenceWin32HandleInfoKHR'
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-fence-handletypes-win32 Handle Types Supported by >
--     table
--
-- -   If @handleType@ is not
--     'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     @name@ /must/ be @NULL@
--
-- -   If @handleType@ is not @0@ and @handle@ is @NULL@, @name@ /must/
--     name a valid synchronization primitive of the type specified by
--     @handleType@
--
-- -   If @handleType@ is not @0@ and @name@ is @NULL@, @handle@ /must/ be
--     a valid handle of the type specified by @handleType@
--
-- -   If @handle@ is not @NULL@, @name@ /must/ be @NULL@
--
-- -   If @handle@ is not @NULL@, it /must/ obey any requirements listed
--     for @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-fence-handle-types-compatibility external fence handle types compatibility>
--
-- -   If @name@ is not @NULL@, it /must/ obey any requirements listed for
--     @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-fence-handle-types-compatibility external fence handle types compatibility>
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @fence@ /must/ be a valid 'Vulkan.Core10.Handles.Fence' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Vulkan.Core11.Enums.FenceImportFlagBits.FenceImportFlagBits' values
--
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits'
--     value
--
-- == Host Synchronization
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits',
-- 'Vulkan.Core10.Handles.Fence',
-- 'Vulkan.Core11.Enums.FenceImportFlagBits.FenceImportFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'importFenceWin32HandleKHR'
data ImportFenceWin32HandleInfoKHR = ImportFenceWin32HandleInfoKHR
  { -- | @fence@ is the fence into which the state will be imported.
    fence :: Fence
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core11.Enums.FenceImportFlagBits.FenceImportFlagBits' specifying
    -- additional parameters for the fence payload import operation.
    flags :: FenceImportFlags
  , -- | @handleType@ specifies the type of @handle@.
    handleType :: ExternalFenceHandleTypeFlagBits
  , -- | @handle@ is the external handle to import, or @NULL@.
    handle :: HANDLE
  , -- | @name@ is a null-terminated UTF-16 string naming the underlying
    -- synchronization primitive to import, or @NULL@.
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportFenceWin32HandleInfoKHR)
#endif
deriving instance Show ImportFenceWin32HandleInfoKHR

instance ToCStruct ImportFenceWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportFenceWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 24 :: Ptr FenceImportFlags)) (flags)
    poke ((p `plusPtr` 28 :: Ptr ExternalFenceHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 32 :: Ptr HANDLE)) (handle)
    poke ((p `plusPtr` 40 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (zero)
    f

instance FromCStruct ImportFenceWin32HandleInfoKHR where
  peekCStruct p = do
    fence <- peek @Fence ((p `plusPtr` 16 :: Ptr Fence))
    flags <- peek @FenceImportFlags ((p `plusPtr` 24 :: Ptr FenceImportFlags))
    handleType <- peek @ExternalFenceHandleTypeFlagBits ((p `plusPtr` 28 :: Ptr ExternalFenceHandleTypeFlagBits))
    handle <- peek @HANDLE ((p `plusPtr` 32 :: Ptr HANDLE))
    name <- peek @LPCWSTR ((p `plusPtr` 40 :: Ptr LPCWSTR))
    pure $ ImportFenceWin32HandleInfoKHR
             fence flags handleType handle name

instance Storable ImportFenceWin32HandleInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportFenceWin32HandleInfoKHR where
  zero = ImportFenceWin32HandleInfoKHR
           zero
           zero
           zero
           zero
           zero


-- | VkExportFenceWin32HandleInfoKHR - Structure specifying additional
-- attributes of Windows handles exported from a fence
--
-- = Description
--
-- If
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_fence.ExportFenceCreateInfo'
-- is not present in the same @pNext@ chain, this structure is ignored.
--
-- If
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_fence.ExportFenceCreateInfo'
-- is present in the @pNext@ chain of 'Vulkan.Core10.Fence.FenceCreateInfo'
-- with a Windows @handleType@, but either 'ExportFenceWin32HandleInfoKHR'
-- is not present in the @pNext@ chain, or if it is but @pAttributes@ is
-- set to @NULL@, default security descriptor values will be used, and
-- child processes created by the application will not inherit the handle,
-- as described in the MSDN documentation for “Synchronization Object
-- Security and Access Rights”1. Further, if the structure is not present,
-- the access rights will be
--
-- @DXGI_SHARED_RESOURCE_READ@ | @DXGI_SHARED_RESOURCE_WRITE@
--
-- for handles of the following types:
--
-- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT'
--
-- [1]
--     <https://docs.microsoft.com/en-us/windows/win32/sync/synchronization-object-security-and-access-rights>
--
-- == Valid Usage
--
-- -   If
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_fence.ExportFenceCreateInfo'::@handleTypes@
--     does not include
--     'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     a 'ExportFenceWin32HandleInfoKHR' structure /must/ not be included
--     in the @pNext@ chain of 'Vulkan.Core10.Fence.FenceCreateInfo'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR'
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Extensions.VK_NV_external_memory_win32.SECURITY_ATTRIBUTES'
--     value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportFenceWin32HandleInfoKHR = ExportFenceWin32HandleInfoKHR
  { -- | @pAttributes@ is a pointer to a Windows
    -- 'Vulkan.Extensions.VK_NV_external_memory_win32.SECURITY_ATTRIBUTES'
    -- structure specifying security attributes of the handle.
    attributes :: Ptr SECURITY_ATTRIBUTES
  , -- | @dwAccess@ is a 'Vulkan.Extensions.VK_NV_external_memory_win32.DWORD'
    -- specifying access rights of the handle.
    dwAccess :: DWORD
  , -- | @name@ is a null-terminated UTF-16 string to associate with the
    -- underlying synchronization primitive referenced by NT handles exported
    -- from the created fence.
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportFenceWin32HandleInfoKHR)
#endif
deriving instance Show ExportFenceWin32HandleInfoKHR

instance ToCStruct ExportFenceWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportFenceWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES))) (attributes)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (dwAccess)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (zero)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (zero)
    f

instance FromCStruct ExportFenceWin32HandleInfoKHR where
  peekCStruct p = do
    pAttributes <- peek @(Ptr SECURITY_ATTRIBUTES) ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)))
    dwAccess <- peek @DWORD ((p `plusPtr` 24 :: Ptr DWORD))
    name <- peek @LPCWSTR ((p `plusPtr` 32 :: Ptr LPCWSTR))
    pure $ ExportFenceWin32HandleInfoKHR
             pAttributes dwAccess name

instance Storable ExportFenceWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportFenceWin32HandleInfoKHR where
  zero = ExportFenceWin32HandleInfoKHR
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
-- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits'
-- for a description of the properties of the defined external fence handle
-- types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_fence.ExportFenceCreateInfo'::@handleTypes@
--     when the @fence@’s current payload was created
--
-- -   If @handleType@ is defined as an NT handle, 'getFenceWin32HandleKHR'
--     /must/ be called no more than once for each valid unique combination
--     of @fence@ and @handleType@
--
-- -   @fence@ /must/ not currently have its payload replaced by an
--     imported payload as described below in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-fences-importing Importing Fence Payloads>
--     unless that imported payload’s handle type was included in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities.ExternalFenceProperties'::@exportFromImportedHandleTypes@
--     for @handleType@
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, @fence@ /must/ be signaled, or have an
--     associated
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>
--     pending execution
--
-- -   @handleType@ /must/ be defined as an NT handle or a global share
--     handle
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @fence@ /must/ be a valid 'Vulkan.Core10.Handles.Fence' handle
--
-- -   @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits',
-- 'Vulkan.Core10.Handles.Fence',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getFenceWin32HandleKHR'
data FenceGetWin32HandleInfoKHR = FenceGetWin32HandleInfoKHR
  { -- | @fence@ is the fence from which state will be exported.
    fence :: Fence
  , -- | @handleType@ is the type of handle requested.
    handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FenceGetWin32HandleInfoKHR)
#endif
deriving instance Show FenceGetWin32HandleInfoKHR

instance ToCStruct FenceGetWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FenceGetWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (zero)
    f

instance FromCStruct FenceGetWin32HandleInfoKHR where
  peekCStruct p = do
    fence <- peek @Fence ((p `plusPtr` 16 :: Ptr Fence))
    handleType <- peek @ExternalFenceHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits))
    pure $ FenceGetWin32HandleInfoKHR
             fence handleType

instance Storable FenceGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FenceGetWin32HandleInfoKHR where
  zero = FenceGetWin32HandleInfoKHR
           zero
           zero


type KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1


type KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = "VK_KHR_external_fence_win32"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = "VK_KHR_external_fence_win32"

