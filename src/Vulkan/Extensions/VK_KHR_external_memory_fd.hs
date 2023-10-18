{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_memory_fd - device extension
--
-- == VK_KHR_external_memory_fd
--
-- [__Name String__]
--     @VK_KHR_external_memory_fd@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     75
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_memory_fd] @cubanismo%0A*Here describe the issue or question you have about the VK_KHR_external_memory_fd extension* >
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
-- == Description
--
-- An application may wish to reference device memory in multiple Vulkan
-- logical devices or instances, in multiple processes, and\/or in multiple
-- APIs. This extension enables an application to export POSIX file
-- descriptor handles from Vulkan memory objects and to import Vulkan
-- memory objects from POSIX file descriptor handles exported from other
-- Vulkan memory objects or from similar resources in other APIs.
--
-- == New Commands
--
-- -   'getMemoryFdKHR'
--
-- -   'getMemoryFdPropertiesKHR'
--
-- == New Structures
--
-- -   'MemoryFdPropertiesKHR'
--
-- -   'MemoryGetFdInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMemoryFdInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR'
--
-- == Issues
--
-- 1) Does the application need to close the file descriptor returned by
-- 'getMemoryFdKHR'?
--
-- __RESOLVED__: Yes, unless it is passed back in to a driver instance to
-- import the memory. A successful get call transfers ownership of the file
-- descriptor to the application, and a successful import transfers it back
-- to the driver. Destroying the original memory object will not close the
-- file descriptor or remove its reference to the underlying memory
-- resource associated with it.
--
-- 2) Do drivers ever need to expose multiple file descriptors per memory
-- object?
--
-- __RESOLVED__: No. This would indicate there are actually multiple memory
-- objects, rather than a single memory object.
--
-- 3) How should the valid size and memory type for POSIX file descriptor
-- memory handles created outside of Vulkan be specified?
--
-- __RESOLVED__: The valid memory types are queried directly from the
-- external handle. The size will be specified by future extensions that
-- introduce such external memory handle types.
--
-- == Version History
--
-- -   Revision 1, 2016-10-21 (James Jones)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ImportMemoryFdInfoKHR', 'MemoryFdPropertiesKHR', 'MemoryGetFdInfoKHR',
-- 'getMemoryFdKHR', 'getMemoryFdPropertiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_external_memory_fd Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_memory_fd  ( getMemoryFdKHR
                                                    , getMemoryFdPropertiesKHR
                                                    , ImportMemoryFdInfoKHR(..)
                                                    , MemoryFdPropertiesKHR(..)
                                                    , MemoryGetFdInfoKHR(..)
                                                    , KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
                                                    , pattern KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION
                                                    , KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
                                                    , pattern KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME
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
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CInt(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)
import Foreign.C.Types (CInt(..))
import Foreign.C.Types (CInt(CInt))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryFdKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryFdPropertiesKHR))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryFdKHR
  :: FunPtr (Ptr Device_T -> Ptr MemoryGetFdInfoKHR -> Ptr CInt -> IO Result) -> Ptr Device_T -> Ptr MemoryGetFdInfoKHR -> Ptr CInt -> IO Result

-- | vkGetMemoryFdKHR - Get a POSIX file descriptor for a memory object
--
-- = Description
--
-- Each call to 'getMemoryFdKHR' /must/ create a new file descriptor
-- holding a reference to the memory object’s payload and transfer
-- ownership of the file descriptor to the application. To avoid leaking
-- resources, the application /must/ release ownership of the file
-- descriptor using the @close@ system call when it is no longer needed, or
-- by importing a Vulkan memory object from it. Where supported by the
-- operating system, the implementation /must/ set the file descriptor to
-- be closed automatically when an @execve@ system call is made.
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_fd VK_KHR_external_memory_fd>,
-- 'Vulkan.Core10.Handles.Device', 'MemoryGetFdInfoKHR'
getMemoryFdKHR :: forall io
                . (MonadIO io)
               => -- | @device@ is the logical device that created the device memory being
                  -- exported.
                  --
                  -- #VUID-vkGetMemoryFdKHR-device-parameter# @device@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Device' handle
                  Device
               -> -- | @pGetFdInfo@ is a pointer to a 'MemoryGetFdInfoKHR' structure containing
                  -- parameters of the export operation.
                  --
                  -- #VUID-vkGetMemoryFdKHR-pGetFdInfo-parameter# @pGetFdInfo@ /must/ be a
                  -- valid pointer to a valid 'MemoryGetFdInfoKHR' structure
                  MemoryGetFdInfoKHR
               -> io (("fd" ::: Int32))
getMemoryFdKHR device getFdInfo = liftIO . evalContT $ do
  let vkGetMemoryFdKHRPtr = pVkGetMemoryFdKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetMemoryFdKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryFdKHR is null" Nothing Nothing
  let vkGetMemoryFdKHR' = mkVkGetMemoryFdKHR vkGetMemoryFdKHRPtr
  pGetFdInfo <- ContT $ withCStruct (getFdInfo)
  pPFd <- ContT $ bracket (callocBytes @CInt 4) free
  r <- lift $ traceAroundEvent "vkGetMemoryFdKHR" (vkGetMemoryFdKHR'
                                                     (deviceHandle (device))
                                                     pGetFdInfo
                                                     (pPFd))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFd <- lift $ peek @CInt pPFd
  pure $ ((coerce @CInt @Int32 pFd))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryFdPropertiesKHR
  :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> CInt -> Ptr MemoryFdPropertiesKHR -> IO Result) -> Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> CInt -> Ptr MemoryFdPropertiesKHR -> IO Result

-- | vkGetMemoryFdPropertiesKHR - Get Properties of External Memory File
-- Descriptors
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_fd VK_KHR_external_memory_fd>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'MemoryFdPropertiesKHR'
getMemoryFdPropertiesKHR :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that will be importing @fd@.
                            --
                            -- #VUID-vkGetMemoryFdPropertiesKHR-device-parameter# @device@ /must/ be a
                            -- valid 'Vulkan.Core10.Handles.Device' handle
                            Device
                         -> -- | @handleType@ is a
                            -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
                            -- value specifying the type of the handle @fd@.
                            --
                            -- #VUID-vkGetMemoryFdPropertiesKHR-handleType-00674# @handleType@ /must/
                            -- not be
                            -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT'
                            --
                            -- #VUID-vkGetMemoryFdPropertiesKHR-handleType-parameter# @handleType@
                            -- /must/ be a valid
                            -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
                            -- value
                            ExternalMemoryHandleTypeFlagBits
                         -> -- | @fd@ is the handle which will be imported.
                            --
                            -- #VUID-vkGetMemoryFdPropertiesKHR-fd-00673# @fd@ /must/ point to a valid
                            -- POSIX file descriptor memory handle
                            ("fd" ::: Int32)
                         -> io (MemoryFdPropertiesKHR)
getMemoryFdPropertiesKHR device handleType fd = liftIO . evalContT $ do
  let vkGetMemoryFdPropertiesKHRPtr = pVkGetMemoryFdPropertiesKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetMemoryFdPropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryFdPropertiesKHR is null" Nothing Nothing
  let vkGetMemoryFdPropertiesKHR' = mkVkGetMemoryFdPropertiesKHR vkGetMemoryFdPropertiesKHRPtr
  pPMemoryFdProperties <- ContT (withZeroCStruct @MemoryFdPropertiesKHR)
  r <- lift $ traceAroundEvent "vkGetMemoryFdPropertiesKHR" (vkGetMemoryFdPropertiesKHR'
                                                               (deviceHandle (device))
                                                               (handleType)
                                                               (CInt (fd))
                                                               (pPMemoryFdProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryFdProperties <- lift $ peekCStruct @MemoryFdPropertiesKHR pPMemoryFdProperties
  pure $ (pMemoryFdProperties)


-- | VkImportMemoryFdInfoKHR - Import memory created on the same physical
-- device from a file descriptor
--
-- = Description
--
-- Importing memory from a file descriptor transfers ownership of the file
-- descriptor from the application to the Vulkan implementation. The
-- application /must/ not perform any operations on the file descriptor
-- after a successful import. The imported memory object holds a reference
-- to its payload.
--
-- Applications /can/ import the same payload into multiple instances of
-- Vulkan, into the same instance from which it was exported, and multiple
-- times into a given Vulkan instance. In all cases, each import operation
-- /must/ create a distinct 'Vulkan.Core10.Handles.DeviceMemory' object.
--
-- == Valid Usage
--
-- -   #VUID-VkImportMemoryFdInfoKHR-handleType-00667# If @handleType@ is
--     not @0@, it /must/ be supported for import, as reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'
--
-- -   #VUID-VkImportMemoryFdInfoKHR-fd-00668# The memory from which @fd@
--     was exported /must/ have been created on the same underlying
--     physical device as @device@
--
-- -   #VUID-VkImportMemoryFdInfoKHR-handleType-00669# If @handleType@ is
--     not @0@, it /must/ be
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT'
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT'
--
-- -   #VUID-VkImportMemoryFdInfoKHR-handleType-00670# If @handleType@ is
--     not @0@, @fd@ /must/ be a valid handle of the type specified by
--     @handleType@
--
-- -   #VUID-VkImportMemoryFdInfoKHR-fd-01746# The memory represented by
--     @fd@ /must/ have been created from a physical device and driver that
--     is compatible with @device@ and @handleType@, as described in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#external-memory-handle-types-compatibility>
--
-- -   #VUID-VkImportMemoryFdInfoKHR-fd-01520# @fd@ /must/ obey any
--     requirements listed for @handleType@ in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImportMemoryFdInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR'
--
-- -   #VUID-VkImportMemoryFdInfoKHR-handleType-parameter# If @handleType@
--     is not @0@, @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_fd VK_KHR_external_memory_fd>,
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMemoryFdInfoKHR = ImportMemoryFdInfoKHR
  { -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the handle type of @fd@.
    handleType :: ExternalMemoryHandleTypeFlagBits
  , -- | @fd@ is the external handle to import.
    fd :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemoryFdInfoKHR)
#endif
deriving instance Show ImportMemoryFdInfoKHR

instance ToCStruct ImportMemoryFdInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemoryFdInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 20 :: Ptr CInt)) (CInt (fd))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr CInt)) (CInt (zero))
    f

instance FromCStruct ImportMemoryFdInfoKHR where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits))
    fd <- peek @CInt ((p `plusPtr` 20 :: Ptr CInt))
    pure $ ImportMemoryFdInfoKHR
             handleType (coerce @CInt @Int32 fd)

instance Storable ImportMemoryFdInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemoryFdInfoKHR where
  zero = ImportMemoryFdInfoKHR
           zero
           zero


-- | VkMemoryFdPropertiesKHR - Properties of External Memory File Descriptors
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_fd VK_KHR_external_memory_fd>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryFdPropertiesKHR'
data MemoryFdPropertiesKHR = MemoryFdPropertiesKHR
  { -- | @memoryTypeBits@ is a bitmask containing one bit set for every memory
    -- type which the specified file descriptor /can/ be imported as.
    memoryTypeBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryFdPropertiesKHR)
#endif
deriving instance Show MemoryFdPropertiesKHR

instance ToCStruct MemoryFdPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryFdPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryFdPropertiesKHR where
  peekCStruct p = do
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ MemoryFdPropertiesKHR
             memoryTypeBits

instance Storable MemoryFdPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryFdPropertiesKHR where
  zero = MemoryFdPropertiesKHR
           zero


-- | VkMemoryGetFdInfoKHR - Structure describing a POSIX FD memory export
-- operation
--
-- = Description
--
-- The properties of the file descriptor exported depend on the value of
-- @handleType@. See
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
-- for a description of the properties of the defined external memory
-- handle types.
--
-- Note
--
-- The size of the exported file /may/ be larger than the size requested by
-- 'Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@. If
-- @handleType@ is
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT',
-- then the application /can/ query the file’s actual size with
-- <https://man7.org/linux/man-pages/man2/lseek.2.html lseek>.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_fd VK_KHR_external_memory_fd>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'getMemoryFdKHR'
data MemoryGetFdInfoKHR = MemoryGetFdInfoKHR
  { -- | @memory@ is the memory object from which the handle will be exported.
    --
    -- #VUID-VkMemoryGetFdInfoKHR-memory-parameter# @memory@ /must/ be a valid
    -- 'Vulkan.Core10.Handles.DeviceMemory' handle
    memory :: DeviceMemory
  , -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the type of handle requested.
    --
    -- #VUID-VkMemoryGetFdInfoKHR-handleType-00671# @handleType@ /must/ have
    -- been included in
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
    -- when @memory@ was created
    --
    -- #VUID-VkMemoryGetFdInfoKHR-handleType-00672# @handleType@ /must/ be
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT'
    -- or
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT'
    --
    -- #VUID-VkMemoryGetFdInfoKHR-handleType-parameter# @handleType@ /must/ be
    -- a valid
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryGetFdInfoKHR)
#endif
deriving instance Show MemoryGetFdInfoKHR

instance ToCStruct MemoryGetFdInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryGetFdInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    f

instance FromCStruct MemoryGetFdInfoKHR where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ MemoryGetFdInfoKHR
             memory handleType

instance Storable MemoryGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryGetFdInfoKHR where
  zero = MemoryGetFdInfoKHR
           zero
           zero


type KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION = 1


type KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME = "VK_KHR_external_memory_fd"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME = "VK_KHR_external_memory_fd"

