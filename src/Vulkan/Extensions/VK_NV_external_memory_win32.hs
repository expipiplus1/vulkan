{-# language CPP #-}
module Vulkan.Extensions.VK_NV_external_memory_win32  ( getMemoryWin32HandleNV
                                                      , ImportMemoryWin32HandleInfoNV(..)
                                                      , ExportMemoryWin32HandleInfoNV(..)
                                                      , NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
                                                      , pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
                                                      , NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
                                                      , pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
                                                      , HANDLE
                                                      , DWORD
                                                      , SECURITY_ATTRIBUTES
                                                      , ExternalMemoryHandleTypeFlagBitsNV(..)
                                                      , ExternalMemoryHandleTypeFlagsNV
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryWin32HandleNV))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (DeviceMemory(..))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagBitsNV(..))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagsNV)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagBitsNV(..))
import Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagsNV)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandleNV
  :: FunPtr (Ptr Device_T -> DeviceMemory -> ExternalMemoryHandleTypeFlagsNV -> Ptr HANDLE -> IO Result) -> Ptr Device_T -> DeviceMemory -> ExternalMemoryHandleTypeFlagsNV -> Ptr HANDLE -> IO Result

-- | vkGetMemoryWin32HandleNV - retrieve Win32 handle to a device memory
-- object
--
-- == Valid Usage
--
-- -   #VUID-vkGetMemoryWin32HandleNV-handleType-01326# @handleType@ /must/
--     be a flag specified in
--     'Vulkan.Extensions.VK_NV_external_memory.ExportMemoryAllocateInfoNV'::@handleTypes@
--     when allocating @memory@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetMemoryWin32HandleNV-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetMemoryWin32HandleNV-memory-parameter# @memory@ /must/ be
--     a valid 'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- -   #VUID-vkGetMemoryWin32HandleNV-handleType-parameter# @handleType@
--     /must/ be a valid combination of
--     'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
--     values
--
-- -   #VUID-vkGetMemoryWin32HandleNV-handleType-requiredbitmask#
--     @handleType@ /must/ not be @0@
--
-- -   #VUID-vkGetMemoryWin32HandleNV-pHandle-parameter# @pHandle@ /must/
--     be a valid pointer to a 'HANDLE' value
--
-- -   #VUID-vkGetMemoryWin32HandleNV-memory-parent# @memory@ /must/ have
--     been created, allocated, or retrieved from @device@
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
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagsNV'
getMemoryWin32HandleNV :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device that owns the memory.
                          Device
                       -> -- | @memory@ is the 'Vulkan.Core10.Handles.DeviceMemory' object.
                          DeviceMemory
                       -> -- | @handleType@ is a bitmask of
                          -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
                          -- containing a single bit specifying the type of handle requested.
                          ExternalMemoryHandleTypeFlagsNV
                       -> io (HANDLE)
getMemoryWin32HandleNV device memory handleType = liftIO . evalContT $ do
  let vkGetMemoryWin32HandleNVPtr = pVkGetMemoryWin32HandleNV (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryWin32HandleNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryWin32HandleNV is null" Nothing Nothing
  let vkGetMemoryWin32HandleNV' = mkVkGetMemoryWin32HandleNV vkGetMemoryWin32HandleNVPtr
  pPHandle <- ContT $ bracket (callocBytes @HANDLE 8) free
  r <- lift $ vkGetMemoryWin32HandleNV' (deviceHandle (device)) (memory) (handleType) (pPHandle)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pHandle <- lift $ peek @HANDLE pPHandle
  pure $ (pHandle)


-- | VkImportMemoryWin32HandleInfoNV - import Win32 memory created on the
-- same physical device
--
-- = Description
--
-- If @handleType@ is @0@, this structure is ignored by consumers of the
-- 'Vulkan.Core10.Memory.MemoryAllocateInfo' structure it is chained from.
--
-- == Valid Usage
--
-- -   #VUID-VkImportMemoryWin32HandleInfoNV-handleType-01327# @handleType@
--     /must/ not have more than one bit set
--
-- -   #VUID-VkImportMemoryWin32HandleInfoNV-handle-01328# @handle@ /must/
--     be a valid handle to memory, obtained as specified by @handleType@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImportMemoryWin32HandleInfoNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV'
--
-- -   #VUID-VkImportMemoryWin32HandleInfoNV-handleType-parameter#
--     @handleType@ /must/ be a valid combination of
--     'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
--     values
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagsNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMemoryWin32HandleInfoNV = ImportMemoryWin32HandleInfoNV
  { -- | @handleType@ is @0@ or a
    -- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.ExternalMemoryHandleTypeFlagBitsNV'
    -- value specifying the type of memory handle in @handle@.
    handleType :: ExternalMemoryHandleTypeFlagsNV
  , -- | @handle@ is a Windows 'HANDLE' referring to the memory.
    handle :: HANDLE
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemoryWin32HandleInfoNV)
#endif
deriving instance Show ImportMemoryWin32HandleInfoNV

instance ToCStruct ImportMemoryWin32HandleInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemoryWin32HandleInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV)) (handleType)
    poke ((p `plusPtr` 24 :: Ptr HANDLE)) (handle)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImportMemoryWin32HandleInfoNV where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagsNV ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagsNV))
    handle <- peek @HANDLE ((p `plusPtr` 24 :: Ptr HANDLE))
    pure $ ImportMemoryWin32HandleInfoNV
             handleType handle

instance Storable ImportMemoryWin32HandleInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemoryWin32HandleInfoNV where
  zero = ImportMemoryWin32HandleInfoNV
           zero
           zero


-- | VkExportMemoryWin32HandleInfoNV - specify security attributes and access
-- rights for Win32 memory handles
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
-- [1]
--     <https://docs.microsoft.com/en-us/windows/win32/sync/synchronization-object-security-and-access-rights>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkExportMemoryWin32HandleInfoNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV'
--
-- -   #VUID-VkExportMemoryWin32HandleInfoNV-pAttributes-parameter# If
--     @pAttributes@ is not @NULL@, @pAttributes@ /must/ be a valid pointer
--     to a valid 'SECURITY_ATTRIBUTES' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ExportMemoryWin32HandleInfoNV = ExportMemoryWin32HandleInfoNV
  { -- | @pAttributes@ is a pointer to a Windows 'SECURITY_ATTRIBUTES' structure
    -- specifying security attributes of the handle.
    attributes :: Ptr SECURITY_ATTRIBUTES
  , -- | @dwAccess@ is a 'DWORD' specifying access rights of the handle.
    dwAccess :: DWORD
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMemoryWin32HandleInfoNV)
#endif
deriving instance Show ExportMemoryWin32HandleInfoNV

instance ToCStruct ExportMemoryWin32HandleInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMemoryWin32HandleInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES))) (attributes)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (dwAccess)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ExportMemoryWin32HandleInfoNV where
  peekCStruct p = do
    pAttributes <- peek @(Ptr SECURITY_ATTRIBUTES) ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)))
    dwAccess <- peek @DWORD ((p `plusPtr` 24 :: Ptr DWORD))
    pure $ ExportMemoryWin32HandleInfoNV
             pAttributes dwAccess

instance Storable ExportMemoryWin32HandleInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMemoryWin32HandleInfoNV where
  zero = ExportMemoryWin32HandleInfoNV
           zero
           zero


type NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1


type NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_NV_external_memory_win32"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_NV_external_memory_win32"


type HANDLE = Ptr ()


type DWORD = Word32


data SECURITY_ATTRIBUTES

