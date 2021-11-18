{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_memory_win32 - device extension
--
-- == VK_KHR_external_memory_win32
--
-- [__Name String__]
--     @VK_KHR_external_memory_win32@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     74
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory@
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_memory_win32] @cubanismo%0A<<Here describe the issue or question you have about the VK_KHR_external_memory_win32 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-10-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Carsten Rohde, NVIDIA
--
-- == Description
--
-- An application may wish to reference device memory in multiple Vulkan
-- logical devices or instances, in multiple processes, and\/or in multiple
-- APIs. This extension enables an application to export Windows handles
-- from Vulkan memory objects and to import Vulkan memory objects from
-- Windows handles exported from other Vulkan memory objects or from
-- similar resources in other APIs.
--
-- == New Commands
--
-- -   'getMemoryWin32HandleKHR'
--
-- -   'getMemoryWin32HandlePropertiesKHR'
--
-- == New Structures
--
-- -   'MemoryGetWin32HandleInfoKHR'
--
-- -   'MemoryWin32HandlePropertiesKHR'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ExportMemoryWin32HandleInfoKHR'
--
--     -   'ImportMemoryWin32HandleInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR'
--
-- == Issues
--
-- 1) Do applications need to call @CloseHandle@() on the values returned
-- from 'getMemoryWin32HandleKHR' when @handleType@ is
-- 'Vulkan.Extensions.VK_KHR_external_memory_capabilities.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR'?
--
-- __RESOLVED__: Yes, unless it is passed back in to another driver
-- instance to import the object. A successful get call transfers ownership
-- of the handle to the application. Destroying the memory object will not
-- destroy the handle or the handle’s reference to the underlying memory
-- resource.
--
-- 2) Should the language regarding KMT\/Windows 7 handles be moved to a
-- separate extension so that it can be deprecated over time?
--
-- __RESOLVED__: No. Support for them can be deprecated by drivers if they
-- choose, by no longer returning them in the supported handle types of the
-- instance level queries.
--
-- 3) How should the valid size and memory type for windows memory handles
-- created outside of Vulkan be specified?
--
-- __RESOLVED__: The valid memory types are queried directly from the
-- external handle. The size is determined by the associated image or
-- buffer memory requirements for external handle types that require
-- dedicated allocations, and by the size specified when creating the
-- object from which the handle was exported for other external handle
-- types.
--
-- == Version History
--
-- -   Revision 1, 2016-10-21 (James Jones)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ExportMemoryWin32HandleInfoKHR', 'ImportMemoryWin32HandleInfoKHR',
-- 'MemoryGetWin32HandleInfoKHR', 'MemoryWin32HandlePropertiesKHR',
-- 'getMemoryWin32HandleKHR', 'getMemoryWin32HandlePropertiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_win32 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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
                                                       , LPCWSTR
                                                       , HANDLE
                                                       , DWORD
                                                       , SECURITY_ATTRIBUTES
                                                       ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CWchar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryWin32HandleKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryWin32HandlePropertiesKHR))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandleKHR
  :: FunPtr (Ptr Device_T -> Ptr MemoryGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result) -> Ptr Device_T -> Ptr MemoryGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result

-- | vkGetMemoryWin32HandleKHR - Get a Windows HANDLE for a memory object
--
-- = Description
--
-- For handle types defined as NT handles, the handles returned by
-- 'getMemoryWin32HandleKHR' are owned by the application and hold a
-- reference to their payload. To avoid leaking resources, the application
-- /must/ release ownership of them using the @CloseHandle@ system call
-- when they are no longer needed.
--
-- Note
--
-- Non-NT handle types do not add a reference to their associated payload.
-- If the original object owning the payload is destroyed, all resources
-- and handles sharing that payload will become invalid.
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_win32 VK_KHR_external_memory_win32>,
-- 'Vulkan.Core10.Handles.Device', 'MemoryGetWin32HandleInfoKHR'
getMemoryWin32HandleKHR :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device that created the device memory being
                           -- exported.
                           --
                           -- #VUID-vkGetMemoryWin32HandleKHR-device-parameter# @device@ /must/ be a
                           -- valid 'Vulkan.Core10.Handles.Device' handle
                           Device
                        -> -- | @pGetWin32HandleInfo@ is a pointer to a 'MemoryGetWin32HandleInfoKHR'
                           -- structure containing parameters of the export operation.
                           --
                           -- #VUID-vkGetMemoryWin32HandleKHR-pGetWin32HandleInfo-parameter#
                           -- @pGetWin32HandleInfo@ /must/ be a valid pointer to a valid
                           -- 'MemoryGetWin32HandleInfoKHR' structure
                           MemoryGetWin32HandleInfoKHR
                        -> io (HANDLE)
getMemoryWin32HandleKHR device getWin32HandleInfo = liftIO . evalContT $ do
  let vkGetMemoryWin32HandleKHRPtr = pVkGetMemoryWin32HandleKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetMemoryWin32HandleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryWin32HandleKHR is null" Nothing Nothing
  let vkGetMemoryWin32HandleKHR' = mkVkGetMemoryWin32HandleKHR vkGetMemoryWin32HandleKHRPtr
  pGetWin32HandleInfo <- ContT $ withCStruct (getWin32HandleInfo)
  pPHandle <- ContT $ bracket (callocBytes @HANDLE 8) free
  r <- lift $ traceAroundEvent "vkGetMemoryWin32HandleKHR" (vkGetMemoryWin32HandleKHR' (deviceHandle (device)) pGetWin32HandleInfo (pPHandle))
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_win32 VK_KHR_external_memory_win32>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'MemoryWin32HandlePropertiesKHR'
getMemoryWin32HandlePropertiesKHR :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the logical device that will be importing @handle@.
                                     --
                                     -- #VUID-vkGetMemoryWin32HandlePropertiesKHR-device-parameter# @device@
                                     -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                     Device
                                  -> -- | @handleType@ is a
                                     -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
                                     -- value specifying the type of the handle @handle@.
                                     --
                                     -- #VUID-vkGetMemoryWin32HandlePropertiesKHR-handleType-00666# @handleType@
                                     -- /must/ not be one of the handle types defined as opaque
                                     --
                                     -- #VUID-vkGetMemoryWin32HandlePropertiesKHR-handleType-parameter#
                                     -- @handleType@ /must/ be a valid
                                     -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
                                     -- value
                                     ExternalMemoryHandleTypeFlagBits
                                  -> -- | @handle@ is the handle which will be imported.
                                     --
                                     -- #VUID-vkGetMemoryWin32HandlePropertiesKHR-handle-00665# @handle@ /must/
                                     -- be an external memory handle created outside of the Vulkan API
                                     HANDLE
                                  -> io (MemoryWin32HandlePropertiesKHR)
getMemoryWin32HandlePropertiesKHR device handleType handle = liftIO . evalContT $ do
  let vkGetMemoryWin32HandlePropertiesKHRPtr = pVkGetMemoryWin32HandlePropertiesKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetMemoryWin32HandlePropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryWin32HandlePropertiesKHR is null" Nothing Nothing
  let vkGetMemoryWin32HandlePropertiesKHR' = mkVkGetMemoryWin32HandlePropertiesKHR vkGetMemoryWin32HandlePropertiesKHRPtr
  pPMemoryWin32HandleProperties <- ContT (withZeroCStruct @MemoryWin32HandlePropertiesKHR)
  r <- lift $ traceAroundEvent "vkGetMemoryWin32HandlePropertiesKHR" (vkGetMemoryWin32HandlePropertiesKHR' (deviceHandle (device)) (handleType) (handle) (pPMemoryWin32HandleProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryWin32HandleProperties <- lift $ peekCStruct @MemoryWin32HandlePropertiesKHR pPMemoryWin32HandleProperties
  pure $ (pMemoryWin32HandleProperties)


-- | VkImportMemoryWin32HandleInfoKHR - Import Win32 memory created on the
-- same physical device
--
-- = Description
--
-- Importing memory object payloads from Windows handles does not transfer
-- ownership of the handle to the Vulkan implementation. For handle types
-- defined as NT handles, the application /must/ release handle ownership
-- using the @CloseHandle@ system call when the handle is no longer needed.
-- For handle types defined as NT handles, the imported memory object holds
-- a reference to its payload.
--
-- Note
--
-- Non-NT handle import operations do not add a reference to their
-- associated payload. If the original object owning the payload is
-- destroyed, all resources and handles sharing that payload will become
-- invalid.
--
-- Applications /can/ import the same payload into multiple instances of
-- Vulkan, into the same instance from which it was exported, and multiple
-- times into a given Vulkan instance. In all cases, each import operation
-- /must/ create a distinct 'Vulkan.Core10.Handles.DeviceMemory' object.
--
-- == Valid Usage
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-handleType-00658# If
--     @handleType@ is not @0@, it /must/ be supported for import, as
--     reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-handle-00659# The memory from
--     which @handle@ was exported, or the memory named by @name@ /must/
--     have been created on the same underlying physical device as @device@
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-handleType-00660# If
--     @handleType@ is not @0@, it /must/ be defined as an NT handle or a
--     global share handle
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-handleType-01439# If
--     @handleType@ is not
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     @name@ /must/ be @NULL@
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-handleType-01440# If
--     @handleType@ is not @0@ and @handle@ is @NULL@, @name@ /must/ name a
--     valid memory resource of the type specified by @handleType@
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-handleType-00661# If
--     @handleType@ is not @0@ and @name@ is @NULL@, @handle@ /must/ be a
--     valid handle of the type specified by @handleType@
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-handle-01441# if @handle@ is
--     not @NULL@, @name@ /must/ be @NULL@
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-handle-01518# If @handle@ is
--     not @NULL@, it /must/ obey any requirements listed for @handleType@
--     in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-name-01519# If @name@ is not
--     @NULL@, it /must/ obey any requirements listed for @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
-- -   #VUID-VkImportMemoryWin32HandleInfoKHR-handleType-parameter# If
--     @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_win32 VK_KHR_external_memory_win32>,
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMemoryWin32HandleInfoKHR = ImportMemoryWin32HandleInfoKHR
  { -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the type of @handle@ or @name@.
    handleType :: ExternalMemoryHandleTypeFlagBits
  , -- | @handle@ is @NULL@ or the external handle to import.
    handle :: HANDLE
  , -- | @name@ is @NULL@ or a null-terminated UTF-16 string naming the payload
    -- to import.
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemoryWin32HandleInfoKHR)
#endif
deriving instance Show ImportMemoryWin32HandleInfoKHR

instance ToCStruct ImportMemoryWin32HandleInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
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
-- is not included in the same @pNext@ chain, this structure is ignored.
--
-- If
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'
-- is included in the @pNext@ chain of
-- 'Vulkan.Core10.Memory.MemoryAllocateInfo' with a Windows @handleType@,
-- but either 'ExportMemoryWin32HandleInfoKHR' is not included in the
-- @pNext@ chain, or if it is but @pAttributes@ is set to @NULL@, default
-- security descriptor values will be used, and child processes created by
-- the application will not inherit the handle, as described in the MSDN
-- documentation for “Synchronization Object Security and Access Rights”1.
-- Further, if the structure is not present, the access rights used depend
-- on the handle type.
--
-- For handles of the following types:
--
-- -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT'
--
-- The implementation /must/ ensure the access rights allow read and write
-- access to the memory.
--
-- [1]
--     <https://docs.microsoft.com/en-us/windows/win32/sync/synchronization-object-security-and-access-rights>
--
-- == Valid Usage
--
-- -   #VUID-VkExportMemoryWin32HandleInfoKHR-handleTypes-00657# If
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     does not include
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     a 'ExportMemoryWin32HandleInfoKHR' structure /must/ not be included
--     in the @pNext@ chain of 'Vulkan.Core10.Memory.MemoryAllocateInfo'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportMemoryWin32HandleInfoKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
-- -   #VUID-VkExportMemoryWin32HandleInfoKHR-pAttributes-parameter# If
--     @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid pointer
--     to a valid
--     'Vulkan.Extensions.VK_NV_external_memory_win32.SECURITY_ATTRIBUTES'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_win32 VK_KHR_external_memory_win32>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMemoryWin32HandleInfoKHR = ExportMemoryWin32HandleInfoKHR
  { -- | @pAttributes@ is a pointer to a Windows
    -- 'Vulkan.Extensions.VK_NV_external_memory_win32.SECURITY_ATTRIBUTES'
    -- structure specifying security attributes of the handle.
    attributes :: Ptr SECURITY_ATTRIBUTES
  , -- | @dwAccess@ is a 'Vulkan.Extensions.VK_NV_external_memory_win32.DWORD'
    -- specifying access rights of the handle.
    dwAccess :: DWORD
  , -- | @name@ is a null-terminated UTF-16 string to associate with the payload
    -- referenced by NT handles exported from the created memory.
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMemoryWin32HandleInfoKHR)
#endif
deriving instance Show ExportMemoryWin32HandleInfoKHR

instance ToCStruct ExportMemoryWin32HandleInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_win32 VK_KHR_external_memory_win32>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryWin32HandlePropertiesKHR'
data MemoryWin32HandlePropertiesKHR = MemoryWin32HandlePropertiesKHR
  { -- | @memoryTypeBits@ is a bitmask containing one bit set for every memory
    -- type which the specified windows handle /can/ be imported as.
    memoryTypeBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryWin32HandlePropertiesKHR)
#endif
deriving instance Show MemoryWin32HandlePropertiesKHR

instance ToCStruct MemoryWin32HandlePropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
-- -   #VUID-VkMemoryGetWin32HandleInfoKHR-handleType-00662# @handleType@
--     /must/ have been included in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     when @memory@ was created
--
-- -   #VUID-VkMemoryGetWin32HandleInfoKHR-handleType-00663# If
--     @handleType@ is defined as an NT handle, 'getMemoryWin32HandleKHR'
--     /must/ be called no more than once for each valid unique combination
--     of @memory@ and @handleType@
--
-- -   #VUID-VkMemoryGetWin32HandleInfoKHR-handleType-00664# @handleType@
--     /must/ be defined as an NT handle or a global share handle
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryGetWin32HandleInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR'
--
-- -   #VUID-VkMemoryGetWin32HandleInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkMemoryGetWin32HandleInfoKHR-memory-parameter# @memory@
--     /must/ be a valid 'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- -   #VUID-VkMemoryGetWin32HandleInfoKHR-handleType-parameter#
--     @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_win32 VK_KHR_external_memory_win32>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryWin32HandleKHR'
data MemoryGetWin32HandleInfoKHR = MemoryGetWin32HandleInfoKHR
  { -- | @memory@ is the memory object from which the handle will be exported.
    memory :: DeviceMemory
  , -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the type of handle requested.
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryGetWin32HandleInfoKHR)
#endif
deriving instance Show MemoryGetWin32HandleInfoKHR

instance ToCStruct MemoryGetWin32HandleInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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


type LPCWSTR = Ptr CWchar

