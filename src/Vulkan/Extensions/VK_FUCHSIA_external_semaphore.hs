{-# language CPP #-}
-- | = Name
--
-- VK_FUCHSIA_external_semaphore - device extension
--
-- == VK_FUCHSIA_external_semaphore
--
-- [__Name String__]
--     @VK_FUCHSIA_external_semaphore@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     366
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_external_semaphore_capabilities@ to be enabled
--         for any device-level functionality
--
--     -   Requires @VK_KHR_external_semaphore@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   John Rosasco
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_FUCHSIA_external_semaphore] @rosasco%0A<<Here describe the issue or question you have about the VK_FUCHSIA_external_semaphore extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-03-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Craig Stout, Google
--
--     -   John Bauman, Google
--
--     -   John Rosasco, Google
--
-- == Description
--
-- An application using external memory may wish to synchronize access to
-- that memory using semaphores. This extension enables an application to
-- export semaphore payload to and import semaphore payload from Zircon
-- event handles.
--
-- == New Commands
--
-- -   'getSemaphoreZirconHandleFUCHSIA'
--
-- -   'importSemaphoreZirconHandleFUCHSIA'
--
-- == New Structures
--
-- -   'ImportSemaphoreZirconHandleInfoFUCHSIA'
--
-- -   'SemaphoreGetZirconHandleInfoFUCHSIA'
--
-- == New Enum Constants
--
-- -   'FUCHSIA_EXTERNAL_SEMAPHORE_EXTENSION_NAME'
--
-- -   'FUCHSIA_EXTERNAL_SEMAPHORE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.EXTERNAL_SEMAPHORE_HANDLE_TYPE_ZIRCON_EVENT_BIT_FUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_SEMAPHORE_ZIRCON_HANDLE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_GET_ZIRCON_HANDLE_INFO_FUCHSIA'
--
-- == Issues
--
-- 1) Does the application need to close the Zircon event handle returned
-- by 'getSemaphoreZirconHandleFUCHSIA'?
--
-- __RESOLVED__: Yes, unless it is passed back in to a driver instance to
-- import the semaphore. A successful get call transfers ownership of the
-- Zircon event handle to the application, and a successful import
-- transfers it back to the driver. Destroying the original semaphore
-- object will not close the Zircon event handle nor remove its reference
-- to the underlying semaphore resource associated with it.
--
-- == Version History
--
-- -   Revision 1, 2021-03-08 (John Rosasco)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ImportSemaphoreZirconHandleInfoFUCHSIA',
-- 'SemaphoreGetZirconHandleInfoFUCHSIA',
-- 'getSemaphoreZirconHandleFUCHSIA', 'importSemaphoreZirconHandleFUCHSIA'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_FUCHSIA_external_semaphore Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_FUCHSIA_external_semaphore  ( getSemaphoreZirconHandleFUCHSIA
                                                        , importSemaphoreZirconHandleFUCHSIA
                                                        , ImportSemaphoreZirconHandleInfoFUCHSIA(..)
                                                        , SemaphoreGetZirconHandleInfoFUCHSIA(..)
                                                        , FUCHSIA_EXTERNAL_SEMAPHORE_SPEC_VERSION
                                                        , pattern FUCHSIA_EXTERNAL_SEMAPHORE_SPEC_VERSION
                                                        , FUCHSIA_EXTERNAL_SEMAPHORE_EXTENSION_NAME
                                                        , pattern FUCHSIA_EXTERNAL_SEMAPHORE_EXTENSION_NAME
                                                        , Zx_handle_t
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetSemaphoreZirconHandleFUCHSIA))
import Vulkan.Dynamic (DeviceCmds(pVkImportSemaphoreZirconHandleFUCHSIA))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (Zx_handle_t)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_SEMAPHORE_ZIRCON_HANDLE_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_GET_ZIRCON_HANDLE_INFO_FUCHSIA))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (Zx_handle_t)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreZirconHandleFUCHSIA
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreGetZirconHandleInfoFUCHSIA -> Ptr Zx_handle_t -> IO Result) -> Ptr Device_T -> Ptr SemaphoreGetZirconHandleInfoFUCHSIA -> Ptr Zx_handle_t -> IO Result

-- | vkGetSemaphoreZirconHandleFUCHSIA - Get a Zircon event handle for a
-- semaphore
--
-- = Description
--
-- Each call to 'getSemaphoreZirconHandleFUCHSIA' /must/ create a Zircon
-- event handle and transfer ownership of it to the application. To avoid
-- leaking resources, the application /must/ release ownership of the
-- Zircon event handle when it is no longer needed.
--
-- Note
--
-- Ownership can be released in many ways. For example, the application can
-- call zx_handle_close() on the file descriptor, or transfer ownership
-- back to Vulkan by using the file descriptor to import a semaphore
-- payload.
--
-- Exporting a Zircon event handle from a semaphore /may/ have side effects
-- depending on the transference of the specified handle type, as described
-- in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore State>.
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_external_semaphore VK_FUCHSIA_external_semaphore>,
-- 'Vulkan.Core10.Handles.Device', 'SemaphoreGetZirconHandleInfoFUCHSIA'
getSemaphoreZirconHandleFUCHSIA :: forall io
                                 . (MonadIO io)
                                => -- | @device@ is the logical device that created the semaphore being
                                   -- exported.
                                   --
                                   -- #VUID-vkGetSemaphoreZirconHandleFUCHSIA-device-parameter# @device@
                                   -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                   Device
                                -> -- | @pGetZirconHandleInfo@ is a pointer to a
                                   -- 'SemaphoreGetZirconHandleInfoFUCHSIA' structure containing parameters of
                                   -- the export operation.
                                   --
                                   -- #VUID-vkGetSemaphoreZirconHandleFUCHSIA-pGetZirconHandleInfo-parameter#
                                   -- @pGetZirconHandleInfo@ /must/ be a valid pointer to a valid
                                   -- 'SemaphoreGetZirconHandleInfoFUCHSIA' structure
                                   SemaphoreGetZirconHandleInfoFUCHSIA
                                -> io (("zirconHandle" ::: Zx_handle_t))
getSemaphoreZirconHandleFUCHSIA device getZirconHandleInfo = liftIO . evalContT $ do
  let vkGetSemaphoreZirconHandleFUCHSIAPtr = pVkGetSemaphoreZirconHandleFUCHSIA (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetSemaphoreZirconHandleFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSemaphoreZirconHandleFUCHSIA is null" Nothing Nothing
  let vkGetSemaphoreZirconHandleFUCHSIA' = mkVkGetSemaphoreZirconHandleFUCHSIA vkGetSemaphoreZirconHandleFUCHSIAPtr
  pGetZirconHandleInfo <- ContT $ withCStruct (getZirconHandleInfo)
  pPZirconHandle <- ContT $ bracket (callocBytes @Zx_handle_t 4) free
  r <- lift $ traceAroundEvent "vkGetSemaphoreZirconHandleFUCHSIA" (vkGetSemaphoreZirconHandleFUCHSIA' (deviceHandle (device)) pGetZirconHandleInfo (pPZirconHandle))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pZirconHandle <- lift $ peek @Zx_handle_t pPZirconHandle
  pure $ (pZirconHandle)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportSemaphoreZirconHandleFUCHSIA
  :: FunPtr (Ptr Device_T -> Ptr ImportSemaphoreZirconHandleInfoFUCHSIA -> IO Result) -> Ptr Device_T -> Ptr ImportSemaphoreZirconHandleInfoFUCHSIA -> IO Result

-- | vkImportSemaphoreZirconHandleFUCHSIA - Import a semaphore from a Zircon
-- event handle
--
-- = Description
--
-- Importing a semaphore payload from a Zircon event handle transfers
-- ownership of the handle from the application to the Vulkan
-- implementation. The application /must/ not perform any operations on the
-- handle after a successful import.
--
-- Applications /can/ import the same semaphore payload into multiple
-- instances of Vulkan, into the same instance from which it was exported,
-- and multiple times into a given Vulkan instance.
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_external_semaphore VK_FUCHSIA_external_semaphore>,
-- 'Vulkan.Core10.Handles.Device', 'ImportSemaphoreZirconHandleInfoFUCHSIA'
importSemaphoreZirconHandleFUCHSIA :: forall io
                                    . (MonadIO io)
                                   => -- | @device@ is the logical device that created the semaphore.
                                      --
                                      -- #VUID-vkImportSemaphoreZirconHandleFUCHSIA-device-parameter# @device@
                                      -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                      Device
                                   -> -- | @pImportSemaphoreZirconHandleInfo@ is a pointer to a
                                      -- 'ImportSemaphoreZirconHandleInfoFUCHSIA' structure specifying the
                                      -- semaphore and import parameters.
                                      --
                                      -- #VUID-vkImportSemaphoreZirconHandleFUCHSIA-pImportSemaphoreZirconHandleInfo-parameter#
                                      -- @pImportSemaphoreZirconHandleInfo@ /must/ be a valid pointer to a valid
                                      -- 'ImportSemaphoreZirconHandleInfoFUCHSIA' structure
                                      ImportSemaphoreZirconHandleInfoFUCHSIA
                                   -> io ()
importSemaphoreZirconHandleFUCHSIA device importSemaphoreZirconHandleInfo = liftIO . evalContT $ do
  let vkImportSemaphoreZirconHandleFUCHSIAPtr = pVkImportSemaphoreZirconHandleFUCHSIA (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkImportSemaphoreZirconHandleFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkImportSemaphoreZirconHandleFUCHSIA is null" Nothing Nothing
  let vkImportSemaphoreZirconHandleFUCHSIA' = mkVkImportSemaphoreZirconHandleFUCHSIA vkImportSemaphoreZirconHandleFUCHSIAPtr
  pImportSemaphoreZirconHandleInfo <- ContT $ withCStruct (importSemaphoreZirconHandleInfo)
  r <- lift $ traceAroundEvent "vkImportSemaphoreZirconHandleFUCHSIA" (vkImportSemaphoreZirconHandleFUCHSIA' (deviceHandle (device)) pImportSemaphoreZirconHandleInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkImportSemaphoreZirconHandleInfoFUCHSIA - Structure specifying Zircon
-- event handle to import to a semaphore
--
-- = Description
--
-- The handle types supported by @handleType@ are:
--
-- +-------------------------------------------------------------------------------------------------------------------+------------------+---------------------+
-- | Handle Type                                                                                                       | Transference     | Permanence          |
-- |                                                                                                                   |                  | Supported           |
-- +===================================================================================================================+==================+=====================+
-- | 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.EXTERNAL_SEMAPHORE_HANDLE_TYPE_ZIRCON_EVENT_BIT_FUCHSIA' | Reference        | Temporary,Permanent |
-- +-------------------------------------------------------------------------------------------------------------------+------------------+---------------------+
--
-- Handle Types Supported by 'ImportSemaphoreZirconHandleInfoFUCHSIA'
--
-- == Valid Usage
--
-- -   #VUID-VkImportSemaphoreZirconHandleInfoFUCHSIA-handleType-04765#
--     @handleType@ /must/ be a value included in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphore-handletypes-fuchsia Handle Types Supported by >
--     table
--
-- -   #VUID-VkImportSemaphoreZirconHandleInfoFUCHSIA-zirconHandle-04766#
--     @zirconHandle@ /must/ obey any requirements listed for @handleType@
--     in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#external-semaphore-handle-types-compatibility external semaphore handle types compatibility>
--
-- -   #VUID-VkImportSemaphoreZirconHandleInfoFUCHSIA-zirconHandle-04767#
--     @zirconHandle@ /must/ have @ZX_RIGHTS_BASIC@ and @ZX_RIGHTS_SIGNAL@
--     rights
--
-- -   #VUID-VkImportSemaphoreZirconHandleInfoFUCHSIA-semaphoreType-04768#
--     The
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'::@semaphoreType@
--     field /must/ not be
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImportSemaphoreZirconHandleInfoFUCHSIA-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_SEMAPHORE_ZIRCON_HANDLE_INFO_FUCHSIA'
--
-- -   #VUID-VkImportSemaphoreZirconHandleInfoFUCHSIA-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkImportSemaphoreZirconHandleInfoFUCHSIA-semaphore-parameter#
--     @semaphore@ /must/ be a valid 'Vulkan.Core10.Handles.Semaphore'
--     handle
--
-- -   #VUID-VkImportSemaphoreZirconHandleInfoFUCHSIA-flags-parameter#
--     @flags@ /must/ be a valid combination of
--     'Vulkan.Core11.Enums.SemaphoreImportFlagBits.SemaphoreImportFlagBits'
--     values
--
-- -   #VUID-VkImportSemaphoreZirconHandleInfoFUCHSIA-handleType-parameter#
--     @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- == Host Synchronization
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_external_semaphore VK_FUCHSIA_external_semaphore>,
-- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits',
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core11.Enums.SemaphoreImportFlagBits.SemaphoreImportFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'importSemaphoreZirconHandleFUCHSIA'
data ImportSemaphoreZirconHandleInfoFUCHSIA = ImportSemaphoreZirconHandleInfoFUCHSIA
  { -- | @semaphore@ is the semaphore into which the payload will be imported.
    semaphore :: Semaphore
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core11.Enums.SemaphoreImportFlagBits.SemaphoreImportFlagBits'
    -- specifying additional parameters for the semaphore payload import
    -- operation.
    flags :: SemaphoreImportFlags
  , -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- value specifying the type of @zirconHandle@.
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- | @zirconHandle@ is the external handle to import.
    zirconHandle :: Zx_handle_t
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportSemaphoreZirconHandleInfoFUCHSIA)
#endif
deriving instance Show ImportSemaphoreZirconHandleInfoFUCHSIA

instance ToCStruct ImportSemaphoreZirconHandleInfoFUCHSIA where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportSemaphoreZirconHandleInfoFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_ZIRCON_HANDLE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr SemaphoreImportFlags)) (flags)
    poke ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 32 :: Ptr Zx_handle_t)) (zirconHandle)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_ZIRCON_HANDLE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Zx_handle_t)) (zero)
    f

instance FromCStruct ImportSemaphoreZirconHandleInfoFUCHSIA where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    flags <- peek @SemaphoreImportFlags ((p `plusPtr` 24 :: Ptr SemaphoreImportFlags))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    zirconHandle <- peek @Zx_handle_t ((p `plusPtr` 32 :: Ptr Zx_handle_t))
    pure $ ImportSemaphoreZirconHandleInfoFUCHSIA
             semaphore flags handleType zirconHandle

instance Storable ImportSemaphoreZirconHandleInfoFUCHSIA where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportSemaphoreZirconHandleInfoFUCHSIA where
  zero = ImportSemaphoreZirconHandleInfoFUCHSIA
           zero
           zero
           zero
           zero


-- | VkSemaphoreGetZirconHandleInfoFUCHSIA - Structure describing a Zircon
-- event handle semaphore export operation
--
-- = Description
--
-- The properties of the Zircon event handle returned depend on the value
-- of @handleType@. See
-- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
-- for a description of the properties of the defined external semaphore
-- handle types.
--
-- == Valid Usage
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-handleType-04758#
--     @handleType@ /must/ have been included in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore.ExportSemaphoreCreateInfo'::@handleTypes@
--     when @semaphore@’s current payload was created
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-semaphore-04759#
--     @semaphore@ /must/ not currently have its payload replaced by an
--     imported payload as described below in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>
--     unless that imported payload’s handle type was included in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.ExternalSemaphoreProperties'::@exportFromImportedHandleTypes@
--     for @handleType@
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-handleType-04760# If
--     @handleType@ refers to a handle type with copy payload transference
--     semantics, as defined below in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>,
--     there /must/ be no queue waiting on @semaphore@
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-handleType-04761# If
--     @handleType@ refers to a handle type with copy payload transference
--     semantics, @semaphore@ /must/ be signaled, or have an associated
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
--     pending execution
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-handleType-04762#
--     @handleType@ /must/ be defined as a Zircon event handle
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-semaphore-04763#
--     @semaphore@ /must/ have been created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_GET_ZIRCON_HANDLE_INFO_FUCHSIA'
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-semaphore-parameter#
--     @semaphore@ /must/ be a valid 'Vulkan.Core10.Handles.Semaphore'
--     handle
--
-- -   #VUID-VkSemaphoreGetZirconHandleInfoFUCHSIA-handleType-parameter#
--     @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_external_semaphore VK_FUCHSIA_external_semaphore>,
-- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits',
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getSemaphoreZirconHandleFUCHSIA'
data SemaphoreGetZirconHandleInfoFUCHSIA = SemaphoreGetZirconHandleInfoFUCHSIA
  { -- | @semaphore@ is the semaphore from which state will be exported.
    semaphore :: Semaphore
  , -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits'
    -- value specifying the type of handle requested.
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreGetZirconHandleInfoFUCHSIA)
#endif
deriving instance Show SemaphoreGetZirconHandleInfoFUCHSIA

instance ToCStruct SemaphoreGetZirconHandleInfoFUCHSIA where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreGetZirconHandleInfoFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_ZIRCON_HANDLE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_ZIRCON_HANDLE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    f

instance FromCStruct SemaphoreGetZirconHandleInfoFUCHSIA where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    pure $ SemaphoreGetZirconHandleInfoFUCHSIA
             semaphore handleType

instance Storable SemaphoreGetZirconHandleInfoFUCHSIA where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreGetZirconHandleInfoFUCHSIA where
  zero = SemaphoreGetZirconHandleInfoFUCHSIA
           zero
           zero


type FUCHSIA_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_FUCHSIA_EXTERNAL_SEMAPHORE_SPEC_VERSION"
pattern FUCHSIA_EXTERNAL_SEMAPHORE_SPEC_VERSION :: forall a . Integral a => a
pattern FUCHSIA_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1


type FUCHSIA_EXTERNAL_SEMAPHORE_EXTENSION_NAME = "VK_FUCHSIA_external_semaphore"

-- No documentation found for TopLevel "VK_FUCHSIA_EXTERNAL_SEMAPHORE_EXTENSION_NAME"
pattern FUCHSIA_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern FUCHSIA_EXTERNAL_SEMAPHORE_EXTENSION_NAME = "VK_FUCHSIA_external_semaphore"

