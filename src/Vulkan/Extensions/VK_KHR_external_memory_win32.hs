{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_external_memory_win32  ( getMemoryWin32HandleKHR
                                                       , getMemoryWin32HandlePropertiesKHR
                                                       , ImportMemoryWin32HandleInfoKHR(..)
                                                       , ExportMemoryWin32HandleInfoKHR(..)
                                                       , MemoryWin32HandlePropertiesKHR(..)
                                                       , MemoryGetWin32HandleInfoKHR(..)
                                                       , KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
                                                       , pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
                                                       , KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
                                                       , pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
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
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Extensions.WSITypes (DWORD)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryWin32HandleKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryWin32HandlePropertiesKHR))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Extensions.WSITypes (HANDLE)
import Vulkan.Extensions.WSITypes (LPCWSTR)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.WSITypes (SECURITY_ATTRIBUTES)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.WSITypes (DWORD)
import Vulkan.Extensions.WSITypes (HANDLE)
import Vulkan.Extensions.WSITypes (LPCWSTR)
import Vulkan.Extensions.WSITypes (SECURITY_ATTRIBUTES)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandleKHR
  :: FunPtr (Ptr Device_T -> Ptr MemoryGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result) -> Ptr Device_T -> Ptr MemoryGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result

-- | vkGetMemoryWin32HandleKHR - Get a Windows HANDLE for a memory object
--
-- = Parameters
--
-- -   @device@ is the logical device that created the device memory being
--     exported.
--
-- -   @pGetWin32HandleInfo@ is a pointer to a
--     'MemoryGetWin32HandleInfoKHR' structure containing parameters of the
--     export operation.
--
-- -   @pHandle@ will return the Windows handle representing the underlying
--     resources of the device memory object.
--
-- = Description
--
-- For handle types defined as NT handles, the handles returned by
-- 'getMemoryWin32HandleKHR' are owned by the application. To avoid leaking
-- resources, the application /must/ release ownership of them using the
-- @CloseHandle@ system call when they are no longer needed.
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
-- 'Vulkan.Core10.Handles.Device', 'MemoryGetWin32HandleInfoKHR'
getMemoryWin32HandleKHR :: forall io . MonadIO io => Device -> MemoryGetWin32HandleInfoKHR -> io (HANDLE)
getMemoryWin32HandleKHR device getWin32HandleInfo = liftIO . evalContT $ do
  let vkGetMemoryWin32HandleKHRPtr = pVkGetMemoryWin32HandleKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryWin32HandleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryWin32HandleKHR is null" Nothing Nothing
  let vkGetMemoryWin32HandleKHR' = mkVkGetMemoryWin32HandleKHR vkGetMemoryWin32HandleKHRPtr
  pGetWin32HandleInfo <- ContT $ withCStruct (getWin32HandleInfo)
  pPHandle <- ContT $ bracket (callocBytes @HANDLE 8) free
  r <- lift $ vkGetMemoryWin32HandleKHR' (deviceHandle (device)) pGetWin32HandleInfo (pPHandle)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pHandle <- lift $ peek @HANDLE pPHandle
  pure $ (pHandle)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandlePropertiesKHR
  :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> HANDLE -> Ptr MemoryWin32HandlePropertiesKHR -> IO Result) -> Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> HANDLE -> Ptr MemoryWin32HandlePropertiesKHR -> IO Result

-- | vkGetMemoryWin32HandlePropertiesKHR - Get Properties of External Memory
-- Win32 Handles
--
-- = Parameters
--
-- -   @device@ is the logical device that will be importing @handle@.
--
-- -   @handleType@ is the type of the handle @handle@.
--
-- -   @handle@ is the handle which will be imported.
--
-- -   @pMemoryWin32HandleProperties@ will return properties of @handle@.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_EXTERNAL_HANDLE'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'MemoryWin32HandlePropertiesKHR'
getMemoryWin32HandlePropertiesKHR :: forall io . MonadIO io => Device -> ExternalMemoryHandleTypeFlagBits -> HANDLE -> io (MemoryWin32HandlePropertiesKHR)
getMemoryWin32HandlePropertiesKHR device handleType handle = liftIO . evalContT $ do
  let vkGetMemoryWin32HandlePropertiesKHRPtr = pVkGetMemoryWin32HandlePropertiesKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryWin32HandlePropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryWin32HandlePropertiesKHR is null" Nothing Nothing
  let vkGetMemoryWin32HandlePropertiesKHR' = mkVkGetMemoryWin32HandlePropertiesKHR vkGetMemoryWin32HandlePropertiesKHRPtr
  pPMemoryWin32HandleProperties <- ContT (withZeroCStruct @MemoryWin32HandlePropertiesKHR)
  r <- lift $ vkGetMemoryWin32HandlePropertiesKHR' (deviceHandle (device)) (handleType) (handle) (pPMemoryWin32HandleProperties)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryWin32HandleProperties <- lift $ peekCStruct @MemoryWin32HandlePropertiesKHR pPMemoryWin32HandleProperties
  pure $ (pMemoryWin32HandleProperties)


-- | VkImportMemoryWin32HandleInfoKHR - import Win32 memory created on the
-- same physical device
--
-- = Description
--
-- Importing memory objects from Windows handles does not transfer
-- ownership of the handle to the Vulkan implementation. For handle types
-- defined as NT handles, the application /must/ release ownership using
-- the @CloseHandle@ system call when the handle is no longer needed.
--
-- Applications /can/ import the same underlying memory into multiple
-- instances of Vulkan, into the same instance from which it was exported,
-- and multiple times into a given Vulkan instance. In all cases, each
-- import operation /must/ create a distinct
-- 'Vulkan.Core10.Handles.DeviceMemory' object.
--
-- == Valid Usage
--
-- -   If @handleType@ is not @0@, it /must/ be supported for import, as
--     reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'
--
-- -   The memory from which @handle@ was exported, or the memory named by
--     @name@ /must/ have been created on the same underlying physical
--     device as @device@
--
-- -   If @handleType@ is not @0@, it /must/ be defined as an NT handle or
--     a global share handle
--
-- -   If @handleType@ is not
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     @name@ /must/ be @NULL@
--
-- -   If @handleType@ is not @0@ and @handle@ is @NULL@, @name@ /must/
--     name a valid memory resource of the type specified by @handleType@
--
-- -   If @handleType@ is not @0@ and @name@ is @NULL@, @handle@ /must/ be
--     a valid handle of the type specified by @handleType@
--
-- -   if @handle@ is not @NULL@, @name@ /must/ be @NULL@
--
-- -   If @handle@ is not @NULL@, it /must/ obey any requirements listed
--     for @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>
--
-- -   If @name@ is not @NULL@, it /must/ obey any requirements listed for
--     @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
-- -   If @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMemoryWin32HandleInfoKHR = ImportMemoryWin32HandleInfoKHR
  { -- | @handleType@ specifies the type of @handle@ or @name@.
    handleType :: ExternalMemoryHandleTypeFlagBits
  , -- | @handle@ is the external handle to import, or @NULL@.
    handle :: HANDLE
  , -- | @name@ is a null-terminated UTF-16 string naming the underlying memory
    -- resource to import, or @NULL@.
    name :: LPCWSTR
  }
  deriving (Typeable)
deriving instance Show ImportMemoryWin32HandleInfoKHR

instance ToCStruct ImportMemoryWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemoryWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 24 :: Ptr HANDLE)) (handle)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImportMemoryWin32HandleInfoKHR where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits))
    handle <- peek @HANDLE ((p `plusPtr` 24 :: Ptr HANDLE))
    name <- peek @LPCWSTR ((p `plusPtr` 32 :: Ptr LPCWSTR))
    pure $ ImportMemoryWin32HandleInfoKHR
             handleType handle name

instance Storable ImportMemoryWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemoryWin32HandleInfoKHR where
  zero = ImportMemoryWin32HandleInfoKHR
           zero
           zero
           zero


-- | VkExportMemoryWin32HandleInfoKHR - Structure specifying additional
-- attributes of Windows handles exported from a memory
--
-- = Description
--
-- If
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'
-- is not present in the same @pNext@ chain, this structure is ignored.
--
-- If
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'
-- is present in the @pNext@ chain of
-- 'Vulkan.Core10.Memory.MemoryAllocateInfo' with a Windows @handleType@,
-- but either 'ExportMemoryWin32HandleInfoKHR' is not present in the
-- @pNext@ chain, or if it is but @pAttributes@ is set to @NULL@, default
-- security descriptor values will be used, and child processes created by
-- the application will not inherit the handle, as described in the MSDN
-- documentation for “Synchronization Object Security and Access Rights”1.
-- Further, if the structure is not present, the access rights used depend
-- on the handle type.
--
-- For handles of the following types:
--
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT'
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT'
--
-- The implementation /must/ ensure the access rights allow read and write
-- access to the memory.
--
-- For handles of the following types:
--
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT'
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT'
--
-- The access rights /must/ be:
--
-- @GENERIC_ALL@
--
-- [1]
--     <https://docs.microsoft.com/en-us/windows/win32/sync/synchronization-object-security-and-access-rights>
--
-- == Valid Usage
--
-- -   If
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     does not include
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     a 'ExportMemoryWin32HandleInfoKHR' structure /must/ not be included
--     in the @pNext@ chain of 'Vulkan.Core10.Memory.MemoryAllocateInfo'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
-- -   If @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid
--     pointer to a valid 'Vulkan.Extensions.WSITypes.SECURITY_ATTRIBUTES'
--     value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMemoryWin32HandleInfoKHR = ExportMemoryWin32HandleInfoKHR
  { -- | @pAttributes@ is a pointer to a Windows
    -- 'Vulkan.Extensions.WSITypes.SECURITY_ATTRIBUTES' structure specifying
    -- security attributes of the handle.
    attributes :: Ptr SECURITY_ATTRIBUTES
  , -- | @dwAccess@ is a 'Vulkan.Extensions.WSITypes.DWORD' specifying access
    -- rights of the handle.
    dwAccess :: DWORD
  , -- | @name@ is a null-terminated UTF-16 string to associate with the
    -- underlying resource referenced by NT handles exported from the created
    -- memory.
    name :: LPCWSTR
  }
  deriving (Typeable)
deriving instance Show ExportMemoryWin32HandleInfoKHR

instance ToCStruct ExportMemoryWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMemoryWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES))) (attributes)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (dwAccess)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (zero)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (zero)
    f

instance FromCStruct ExportMemoryWin32HandleInfoKHR where
  peekCStruct p = do
    pAttributes <- peek @(Ptr SECURITY_ATTRIBUTES) ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)))
    dwAccess <- peek @DWORD ((p `plusPtr` 24 :: Ptr DWORD))
    name <- peek @LPCWSTR ((p `plusPtr` 32 :: Ptr LPCWSTR))
    pure $ ExportMemoryWin32HandleInfoKHR
             pAttributes dwAccess name

instance Storable ExportMemoryWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMemoryWin32HandleInfoKHR where
  zero = ExportMemoryWin32HandleInfoKHR
           zero
           zero
           zero


-- | VkMemoryWin32HandlePropertiesKHR - Properties of External Memory Windows
-- Handles
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryWin32HandlePropertiesKHR'
data MemoryWin32HandlePropertiesKHR = MemoryWin32HandlePropertiesKHR
  { -- | @memoryTypeBits@ is a bitmask containing one bit set for every memory
    -- type which the specified windows handle /can/ be imported as.
    memoryTypeBits :: Word32 }
  deriving (Typeable)
deriving instance Show MemoryWin32HandlePropertiesKHR

instance ToCStruct MemoryWin32HandlePropertiesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryWin32HandlePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryWin32HandlePropertiesKHR where
  peekCStruct p = do
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ MemoryWin32HandlePropertiesKHR
             memoryTypeBits

instance Storable MemoryWin32HandlePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryWin32HandlePropertiesKHR where
  zero = MemoryWin32HandlePropertiesKHR
           zero


-- | VkMemoryGetWin32HandleInfoKHR - Structure describing a Win32 handle
-- semaphore export operation
--
-- = Description
--
-- The properties of the handle returned depend on the value of
-- @handleType@. See
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
-- for a description of the properties of the defined external memory
-- handle types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     when @memory@ was created
--
-- -   If @handleType@ is defined as an NT handle,
--     'getMemoryWin32HandleKHR' /must/ be called no more than once for
--     each valid unique combination of @memory@ and @handleType@
--
-- -   @handleType@ /must/ be defined as an NT handle or a global share
--     handle
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @memory@ /must/ be a valid 'Vulkan.Core10.Handles.DeviceMemory'
--     handle
--
-- -   @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryWin32HandleKHR'
data MemoryGetWin32HandleInfoKHR = MemoryGetWin32HandleInfoKHR
  { -- | @memory@ is the memory object from which the handle will be exported.
    memory :: DeviceMemory
  , -- | @handleType@ is the type of handle requested.
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable)
deriving instance Show MemoryGetWin32HandleInfoKHR

instance ToCStruct MemoryGetWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryGetWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    f

instance FromCStruct MemoryGetWin32HandleInfoKHR where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ MemoryGetWin32HandleInfoKHR
             memory handleType

instance Storable MemoryGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryGetWin32HandleInfoKHR where
  zero = MemoryGetWin32HandleInfoKHR
           zero
           zero


type KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1


type KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_KHR_external_memory_win32"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_KHR_external_memory_win32"

