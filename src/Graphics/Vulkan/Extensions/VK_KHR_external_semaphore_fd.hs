{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd  ( getSemaphoreFdKHR
                                                                , importSemaphoreFdKHR
                                                                , ImportSemaphoreFdInfoKHR(..)
                                                                , SemaphoreGetFdInfoKHR(..)
                                                                , KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
                                                                , pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
                                                                , KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
                                                                , pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
                                                                ) where

import Control.Exception.Base (bracket)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CInt(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)
import Foreign.C.Types (CInt(CInt))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetSemaphoreFdKHR))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkImportSemaphoreFdKHR))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Handles (Semaphore)
import Graphics.Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreFdKHR
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreGetFdInfoKHR -> Ptr CInt -> IO Result) -> Ptr Device_T -> Ptr SemaphoreGetFdInfoKHR -> Ptr CInt -> IO Result

-- | vkGetSemaphoreFdKHR - Get a POSIX file descriptor handle for a semaphore
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     created the semaphore being exported.
--
-- -   @pGetFdInfo@ is a pointer to a 'SemaphoreGetFdInfoKHR' structure
--     containing parameters of the export operation.
--
-- -   @pFd@ will return the file descriptor representing the semaphore
--     payload.
--
-- = Description
--
-- Each call to 'getSemaphoreFdKHR' /must/ create a new file descriptor and
-- transfer ownership of it to the application. To avoid leaking resources,
-- the application /must/ release ownership of the file descriptor when it
-- is no longer needed.
--
-- Note
--
-- Ownership can be released in many ways. For example, the application can
-- call @close@() on the file descriptor, or transfer ownership back to
-- Vulkan by using the file descriptor to import a semaphore payload.
--
-- Where supported by the operating system, the implementation /must/ set
-- the file descriptor to be closed automatically when an @execve@ system
-- call is made.
--
-- Exporting a file descriptor from a semaphore /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore State>.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device', 'SemaphoreGetFdInfoKHR'
getSemaphoreFdKHR :: Device -> SemaphoreGetFdInfoKHR -> IO (("fd" ::: Int32))
getSemaphoreFdKHR device getFdInfo = evalContT $ do
  let vkGetSemaphoreFdKHR' = mkVkGetSemaphoreFdKHR (pVkGetSemaphoreFdKHR (deviceCmds (device :: Device)))
  pGetFdInfo <- ContT $ withCStruct (getFdInfo)
  pPFd <- ContT $ bracket (callocBytes @CInt 4) free
  r <- lift $ vkGetSemaphoreFdKHR' (deviceHandle (device)) pGetFdInfo (pPFd)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFd <- lift $ peek @CInt pPFd
  pure $ (((\(CInt a) -> a) pFd))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportSemaphoreFdKHR
  :: FunPtr (Ptr Device_T -> Ptr ImportSemaphoreFdInfoKHR -> IO Result) -> Ptr Device_T -> Ptr ImportSemaphoreFdInfoKHR -> IO Result

-- | vkImportSemaphoreFdKHR - Import a semaphore from a POSIX file descriptor
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     created the semaphore.
--
-- -   @pImportSemaphoreFdInfo@ is a pointer to a
--     'ImportSemaphoreFdInfoKHR' structure specifying the semaphore and
--     import parameters.
--
-- = Description
--
-- Importing a semaphore payload from a file descriptor transfers ownership
-- of the file descriptor from the application to the Vulkan
-- implementation. The application /must/ not perform any operations on the
-- file descriptor after a successful import.
--
-- Applications /can/ import the same semaphore payload into multiple
-- instances of Vulkan, into the same instance from which it was exported,
-- and multiple times into a given Vulkan instance.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_INVALID_EXTERNAL_HANDLE'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device', 'ImportSemaphoreFdInfoKHR'
importSemaphoreFdKHR :: Device -> ImportSemaphoreFdInfoKHR -> IO ()
importSemaphoreFdKHR device importSemaphoreFdInfo = evalContT $ do
  let vkImportSemaphoreFdKHR' = mkVkImportSemaphoreFdKHR (pVkImportSemaphoreFdKHR (deviceCmds (device :: Device)))
  pImportSemaphoreFdInfo <- ContT $ withCStruct (importSemaphoreFdInfo)
  r <- lift $ vkImportSemaphoreFdKHR' (deviceHandle (device)) pImportSemaphoreFdInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkImportSemaphoreFdInfoKHR - Structure specifying POSIX file descriptor
-- to import to a semaphore
--
-- = Description
--
-- The handle types supported by @handleType@ are:
--
-- +-----------------------------------------------------------------------------------------------------------------+----------------------+-----------------------+
-- | Handle Type                                                                                                     | Transference         | Permanence Supported  |
-- +=================================================================================================================+======================+=======================+
-- | 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT' | Reference            | Temporary,Permanent   |
-- +-----------------------------------------------------------------------------------------------------------------+----------------------+-----------------------+
-- | 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT'   | Copy                 | Temporary             |
-- +-----------------------------------------------------------------------------------------------------------------+----------------------+-----------------------+
--
-- Handle Types Supported by 'ImportSemaphoreFdInfoKHR'
--
-- == Valid Usage
--
-- -   @handleType@ /must/ be a value included in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphore-handletypes-fd Handle Types Supported by VkImportSemaphoreFdInfoKHR>
--     table.
--
-- -   @fd@ /must/ obey any requirements listed for @handleType@ in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#external-semaphore-handle-types-compatibility external semaphore handle types compatibility>.
--
-- -   If @handleType@ is
--     'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT',
--     the
--     'Graphics.Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     field /must/ match that of the semaphore from which @fd@ was
--     exported.
--
-- -   If @handleType@ is
--     'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT',
--     the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'::'Graphics.Vulkan.Core12.Enums.SemaphoreType.SemaphoreType'
--     field /must/ match that of the semaphore from which @fd@ was
--     exported.
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' contains
--     'Graphics.Vulkan.Core11.Enums.SemaphoreImportFlagBits.SEMAPHORE_IMPORT_TEMPORARY_BIT',
--     the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'::'Graphics.Vulkan.Core12.Enums.SemaphoreType.SemaphoreType'
--     field of the semaphore from which @fd@ was exported /must/ not be
--     'Graphics.Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   'Graphics.Vulkan.Core10.Handles.Semaphore' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Semaphore' handle
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core11.Enums.SemaphoreImportFlagBits.SemaphoreImportFlagBits'
--     values
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.Semaphore' /must/ be
--     externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Handles.Semaphore',
-- 'Graphics.Vulkan.Core11.Enums.SemaphoreImportFlagBits.SemaphoreImportFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'importSemaphoreFdKHR'
data ImportSemaphoreFdInfoKHR = ImportSemaphoreFdInfoKHR
  { -- | 'Graphics.Vulkan.Core10.Handles.Semaphore' is the semaphore into which
    -- the payload will be imported.
    semaphore :: Semaphore
  , -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
    -- 'Graphics.Vulkan.Core11.Enums.SemaphoreImportFlagBits.SemaphoreImportFlagBits'
    -- specifying additional parameters for the semaphore payload import
    -- operation.
    flags :: SemaphoreImportFlags
  , -- | @handleType@ specifies the type of @fd@.
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- | @fd@ is the external handle to import.
    fd :: Int32
  }
  deriving (Typeable)
deriving instance Show ImportSemaphoreFdInfoKHR

instance ToCStruct ImportSemaphoreFdInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportSemaphoreFdInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr SemaphoreImportFlags)) (flags)
    poke ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 32 :: Ptr CInt)) (CInt (fd))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    poke ((p `plusPtr` 32 :: Ptr CInt)) (CInt (zero))
    f

instance FromCStruct ImportSemaphoreFdInfoKHR where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    flags <- peek @SemaphoreImportFlags ((p `plusPtr` 24 :: Ptr SemaphoreImportFlags))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    fd <- peek @CInt ((p `plusPtr` 32 :: Ptr CInt))
    pure $ ImportSemaphoreFdInfoKHR
             semaphore flags handleType ((\(CInt a) -> a) fd)

instance Storable ImportSemaphoreFdInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportSemaphoreFdInfoKHR where
  zero = ImportSemaphoreFdInfoKHR
           zero
           zero
           zero
           zero


-- | VkSemaphoreGetFdInfoKHR - Structure describing a POSIX FD semaphore
-- export operation
--
-- = Description
--
-- The properties of the file descriptor returned depend on the value of
-- @handleType@. See
-- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
-- for a description of the properties of the defined external semaphore
-- handle types.
--
-- == Valid Usage
--
-- -   @handleType@ /must/ have been included in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore.ExportSemaphoreCreateInfo'::@handleTypes@
--     when 'Graphics.Vulkan.Core10.Handles.Semaphore'’s current payload
--     was created.
--
-- -   'Graphics.Vulkan.Core10.Handles.Semaphore' /must/ not currently have
--     its payload replaced by an imported payload as described below in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>
--     unless that imported payload’s handle type was included in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.ExternalSemaphoreProperties'::@exportFromImportedHandleTypes@
--     for @handleType@.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, as defined below in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>,
--     there /must/ be no queue waiting on
--     'Graphics.Vulkan.Core10.Handles.Semaphore'.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, 'Graphics.Vulkan.Core10.Handles.Semaphore'
--     /must/ be signaled, or have an associated
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
--     pending execution.
--
-- -   @handleType@ /must/ be defined as a POSIX file descriptor handle.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, 'Graphics.Vulkan.Core10.Handles.Semaphore'
--     /must/ have been created with a
--     'Graphics.Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Graphics.Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'.
--
-- -   If @handleType@ refers to a handle type with copy payload
--     transference semantics, 'Graphics.Vulkan.Core10.Handles.Semaphore'
--     /must/ have an associated semaphore signal operation that has been
--     submitted for execution and any semaphore signal operations on which
--     it depends (if any) /must/ have also been submitted for execution.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   'Graphics.Vulkan.Core10.Handles.Semaphore' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Semaphore' handle
--
-- -   @handleType@ /must/ be a valid
--     'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits',
-- 'Graphics.Vulkan.Core10.Handles.Semaphore',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getSemaphoreFdKHR'
data SemaphoreGetFdInfoKHR = SemaphoreGetFdInfoKHR
  { -- | 'Graphics.Vulkan.Core10.Handles.Semaphore' is the semaphore from which
    -- state will be exported.
    semaphore :: Semaphore
  , -- | @handleType@ is the type of handle requested.
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Typeable)
deriving instance Show SemaphoreGetFdInfoKHR

instance ToCStruct SemaphoreGetFdInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreGetFdInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    f

instance FromCStruct SemaphoreGetFdInfoKHR where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    pure $ SemaphoreGetFdInfoKHR
             semaphore handleType

instance Storable SemaphoreGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreGetFdInfoKHR where
  zero = SemaphoreGetFdInfoKHR
           zero
           zero


type KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1


type KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = "VK_KHR_external_semaphore_fd"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = "VK_KHR_external_semaphore_fd"

