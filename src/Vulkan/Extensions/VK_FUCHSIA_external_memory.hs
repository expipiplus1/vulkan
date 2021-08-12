{-# language CPP #-}
-- | = Name
--
-- VK_FUCHSIA_external_memory - device extension
--
-- == VK_FUCHSIA_external_memory
--
-- [__Name String__]
--     @VK_FUCHSIA_external_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     365
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory_capabilities@
--
--     -   Requires @VK_KHR_external_memory@
--
-- [__Contact__]
--
--     -   John Rosasco
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_FUCHSIA_external_memory:%20&body=@rosasco%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-03-01
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
-- Vulkan apps may wish to export or import device memory handles to or
-- from other logical devices, instances or APIs.
--
-- This memory sharing can eliminate copies of memory buffers when
-- different subsystems need to interoperate on them. Sharing memory
-- buffers may also facilitate a better distribution of processing workload
-- for more complex memory manipulation pipelines.
--
-- == New Commands
--
-- -   'getMemoryZirconHandleFUCHSIA'
--
-- -   'getMemoryZirconHandlePropertiesFUCHSIA'
--
-- == New Structures
--
-- -   'MemoryGetZirconHandleInfoFUCHSIA'
--
-- -   'MemoryZirconHandlePropertiesFUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMemoryZirconHandleInfoFUCHSIA'
--
-- == New Enum Constants
--
-- -   'FUCHSIA_EXTERNAL_MEMORY_EXTENSION_NAME'
--
-- -   'FUCHSIA_EXTERNAL_MEMORY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ZIRCON_VMO_BIT_FUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_ZIRCON_HANDLE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_ZIRCON_HANDLE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_ZIRCON_HANDLE_PROPERTIES_FUCHSIA'
--
-- == Issues
--
-- See @VK_KHR_external_memory@ issues list for further information.
--
-- == Version History
--
-- -   Revision 1, 2021-03-01 (John Rosasco)
--
--     -   Initial draft
--
-- = See Also
--
-- 'ImportMemoryZirconHandleInfoFUCHSIA',
-- 'MemoryGetZirconHandleInfoFUCHSIA',
-- 'MemoryZirconHandlePropertiesFUCHSIA', 'getMemoryZirconHandleFUCHSIA',
-- 'getMemoryZirconHandlePropertiesFUCHSIA'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_external_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_FUCHSIA_external_memory  ( getMemoryZirconHandleFUCHSIA
                                                     , getMemoryZirconHandlePropertiesFUCHSIA
                                                     , ImportMemoryZirconHandleInfoFUCHSIA(..)
                                                     , MemoryZirconHandlePropertiesFUCHSIA(..)
                                                     , MemoryGetZirconHandleInfoFUCHSIA(..)
                                                     , FUCHSIA_EXTERNAL_MEMORY_SPEC_VERSION
                                                     , pattern FUCHSIA_EXTERNAL_MEMORY_SPEC_VERSION
                                                     , FUCHSIA_EXTERNAL_MEMORY_EXTENSION_NAME
                                                     , pattern FUCHSIA_EXTERNAL_MEMORY_EXTENSION_NAME
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryZirconHandleFUCHSIA))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryZirconHandlePropertiesFUCHSIA))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (Zx_handle_t)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_ZIRCON_HANDLE_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_GET_ZIRCON_HANDLE_INFO_FUCHSIA))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_ZIRCON_HANDLE_PROPERTIES_FUCHSIA))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (Zx_handle_t)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryZirconHandleFUCHSIA
  :: FunPtr (Ptr Device_T -> Ptr MemoryGetZirconHandleInfoFUCHSIA -> Ptr Zx_handle_t -> IO Result) -> Ptr Device_T -> Ptr MemoryGetZirconHandleInfoFUCHSIA -> Ptr Zx_handle_t -> IO Result

-- | vkGetMemoryZirconHandleFUCHSIA - Get a Zircon handle for an external
-- memory object
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
-- 'Vulkan.Core10.Handles.Device', 'MemoryGetZirconHandleInfoFUCHSIA'
getMemoryZirconHandleFUCHSIA :: forall io
                              . (MonadIO io)
                             => -- | @device@ is the 'Vulkan.Core10.Handles.Device'.
                                --
                                -- #VUID-vkGetMemoryZirconHandleFUCHSIA-device-parameter# @device@ /must/
                                -- be a valid 'Vulkan.Core10.Handles.Device' handle
                                Device
                             -> -- | @pGetZirconHandleInfo@ is a pointer to a
                                -- 'MemoryGetZirconHandleInfoFUCHSIA' structure.
                                --
                                -- #VUID-vkGetMemoryZirconHandleFUCHSIA-pGetZirconHandleInfo-parameter#
                                -- @pGetZirconHandleInfo@ /must/ be a valid pointer to a valid
                                -- 'MemoryGetZirconHandleInfoFUCHSIA' structure
                                MemoryGetZirconHandleInfoFUCHSIA
                             -> io (("zirconHandle" ::: Zx_handle_t))
getMemoryZirconHandleFUCHSIA device getZirconHandleInfo = liftIO . evalContT $ do
  let vkGetMemoryZirconHandleFUCHSIAPtr = pVkGetMemoryZirconHandleFUCHSIA (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryZirconHandleFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryZirconHandleFUCHSIA is null" Nothing Nothing
  let vkGetMemoryZirconHandleFUCHSIA' = mkVkGetMemoryZirconHandleFUCHSIA vkGetMemoryZirconHandleFUCHSIAPtr
  pGetZirconHandleInfo <- ContT $ withCStruct (getZirconHandleInfo)
  pPZirconHandle <- ContT $ bracket (callocBytes @Zx_handle_t 4) free
  r <- lift $ traceAroundEvent "vkGetMemoryZirconHandleFUCHSIA" (vkGetMemoryZirconHandleFUCHSIA' (deviceHandle (device)) pGetZirconHandleInfo (pPZirconHandle))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pZirconHandle <- lift $ peek @Zx_handle_t pPZirconHandle
  pure $ (pZirconHandle)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryZirconHandlePropertiesFUCHSIA
  :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> Zx_handle_t -> Ptr MemoryZirconHandlePropertiesFUCHSIA -> IO Result) -> Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> Zx_handle_t -> Ptr MemoryZirconHandlePropertiesFUCHSIA -> IO Result

-- | vkGetMemoryZirconHandlePropertiesFUCHSIA - Get a Zircon handle
-- properties for an external memory object
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
-- 'MemoryZirconHandlePropertiesFUCHSIA'
getMemoryZirconHandlePropertiesFUCHSIA :: forall io
                                        . (MonadIO io)
                                       => -- | @device@ is the 'Vulkan.Core10.Handles.Device'.
                                          --
                                          -- #VUID-vkGetMemoryZirconHandlePropertiesFUCHSIA-device-parameter#
                                          -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                          Device
                                       -> -- | @handleType@ is a
                                          -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
                                          -- value specifying the type of @zirconHandle@
                                          --
                                          -- #VUID-vkGetMemoryZirconHandlePropertiesFUCHSIA-handleType-04773#
                                          -- @handleType@ /must/ be
                                          -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ZIRCON_VMO_BIT_FUCHSIA'
                                          --
                                          -- #VUID-vkGetMemoryZirconHandlePropertiesFUCHSIA-handleType-parameter#
                                          -- @handleType@ /must/ be a valid
                                          -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
                                          -- value
                                          ExternalMemoryHandleTypeFlagBits
                                       -> -- | @zirconHandle@ is a @zx_handle_t@ (Zircon) handle to the external
                                          -- resource.
                                          --
                                          -- #VUID-vkGetMemoryZirconHandlePropertiesFUCHSIA-zirconHandle-04774#
                                          -- @zirconHandle@ must reference a valid VMO
                                          ("zirconHandle" ::: Zx_handle_t)
                                       -> io (MemoryZirconHandlePropertiesFUCHSIA)
getMemoryZirconHandlePropertiesFUCHSIA device handleType zirconHandle = liftIO . evalContT $ do
  let vkGetMemoryZirconHandlePropertiesFUCHSIAPtr = pVkGetMemoryZirconHandlePropertiesFUCHSIA (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryZirconHandlePropertiesFUCHSIAPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryZirconHandlePropertiesFUCHSIA is null" Nothing Nothing
  let vkGetMemoryZirconHandlePropertiesFUCHSIA' = mkVkGetMemoryZirconHandlePropertiesFUCHSIA vkGetMemoryZirconHandlePropertiesFUCHSIAPtr
  pPMemoryZirconHandleProperties <- ContT (withZeroCStruct @MemoryZirconHandlePropertiesFUCHSIA)
  r <- lift $ traceAroundEvent "vkGetMemoryZirconHandlePropertiesFUCHSIA" (vkGetMemoryZirconHandlePropertiesFUCHSIA' (deviceHandle (device)) (handleType) (zirconHandle) (pPMemoryZirconHandleProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryZirconHandleProperties <- lift $ peekCStruct @MemoryZirconHandlePropertiesFUCHSIA pPMemoryZirconHandleProperties
  pure $ (pMemoryZirconHandleProperties)


-- | VkImportMemoryZirconHandleInfoFUCHSIA - Structure specifying import
-- parameters for Zircon handle to external memory
--
-- == Valid Usage
--
-- -   #VUID-VkImportMemoryZirconHandleInfoFUCHSIA-handleType-04771#
--     @handleType@ /must/ be
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ZIRCON_VMO_BIT_FUCHSIA'
--
-- -   #VUID-VkImportMemoryZirconHandleInfoFUCHSIA-handle-04772# @handle@
--     must be a valid VMO handle
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImportMemoryZirconHandleInfoFUCHSIA-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_ZIRCON_HANDLE_INFO_FUCHSIA'
--
-- -   #VUID-VkImportMemoryZirconHandleInfoFUCHSIA-handleType-parameter# If
--     @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMemoryZirconHandleInfoFUCHSIA = ImportMemoryZirconHandleInfoFUCHSIA
  { -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the type of @handle@.
    handleType :: ExternalMemoryHandleTypeFlagBits
  , -- | @handle@ is a @zx_handle_t@ (Zircon) handle to the external memory.
    handle :: Zx_handle_t
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemoryZirconHandleInfoFUCHSIA)
#endif
deriving instance Show ImportMemoryZirconHandleInfoFUCHSIA

instance ToCStruct ImportMemoryZirconHandleInfoFUCHSIA where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemoryZirconHandleInfoFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_ZIRCON_HANDLE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 20 :: Ptr Zx_handle_t)) (handle)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_ZIRCON_HANDLE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImportMemoryZirconHandleInfoFUCHSIA where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits))
    handle <- peek @Zx_handle_t ((p `plusPtr` 20 :: Ptr Zx_handle_t))
    pure $ ImportMemoryZirconHandleInfoFUCHSIA
             handleType handle

instance Storable ImportMemoryZirconHandleInfoFUCHSIA where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemoryZirconHandleInfoFUCHSIA where
  zero = ImportMemoryZirconHandleInfoFUCHSIA
           zero
           zero


-- | VkMemoryZirconHandlePropertiesFUCHSIA - Structure specifying Zircon
-- handle compatible external memory
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryZirconHandlePropertiesFUCHSIA'
data MemoryZirconHandlePropertiesFUCHSIA = MemoryZirconHandlePropertiesFUCHSIA
  { -- | @memoryTypeBits@ a bitmask containing one bit set for every memory type
    -- which the specified handle can be imported as.
    memoryTypeBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryZirconHandlePropertiesFUCHSIA)
#endif
deriving instance Show MemoryZirconHandlePropertiesFUCHSIA

instance ToCStruct MemoryZirconHandlePropertiesFUCHSIA where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryZirconHandlePropertiesFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_ZIRCON_HANDLE_PROPERTIES_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_ZIRCON_HANDLE_PROPERTIES_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryZirconHandlePropertiesFUCHSIA where
  peekCStruct p = do
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ MemoryZirconHandlePropertiesFUCHSIA
             memoryTypeBits

instance Storable MemoryZirconHandlePropertiesFUCHSIA where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryZirconHandlePropertiesFUCHSIA where
  zero = MemoryZirconHandlePropertiesFUCHSIA
           zero


-- | VkMemoryGetZirconHandleInfoFUCHSIA - Structure specifying export
-- parameters for Zircon handle to device memory
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryZirconHandleFUCHSIA'
data MemoryGetZirconHandleInfoFUCHSIA = MemoryGetZirconHandleInfoFUCHSIA
  { -- | @memory@ the 'Vulkan.Core10.Handles.DeviceMemory' being exported.
    --
    -- #VUID-VkMemoryGetZirconHandleInfoFUCHSIA-memory-parameter# @memory@
    -- /must/ be a valid 'Vulkan.Core10.Handles.DeviceMemory' handle
    memory :: DeviceMemory
  , -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the type of the handle pointed to by
    -- 'getMemoryZirconHandleFUCHSIA'::@pZirconHandle@.
    --
    -- #VUID-VkMemoryGetZirconHandleInfoFUCHSIA-handleType-04775# @handleType@
    -- /must/ be
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ZIRCON_VMO_BIT_FUCHSIA'
    --
    -- #VUID-VkMemoryGetZirconHandleInfoFUCHSIA-handleType-04776# @handleType@
    -- /must/ have been included in the @handleTypes@ field of the
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'
    -- structure when the external memory was allocated
    --
    -- #VUID-VkMemoryGetZirconHandleInfoFUCHSIA-handleType-parameter#
    -- @handleType@ /must/ be a valid
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryGetZirconHandleInfoFUCHSIA)
#endif
deriving instance Show MemoryGetZirconHandleInfoFUCHSIA

instance ToCStruct MemoryGetZirconHandleInfoFUCHSIA where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryGetZirconHandleInfoFUCHSIA{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_ZIRCON_HANDLE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_ZIRCON_HANDLE_INFO_FUCHSIA)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    f

instance FromCStruct MemoryGetZirconHandleInfoFUCHSIA where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ MemoryGetZirconHandleInfoFUCHSIA
             memory handleType

instance Storable MemoryGetZirconHandleInfoFUCHSIA where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryGetZirconHandleInfoFUCHSIA where
  zero = MemoryGetZirconHandleInfoFUCHSIA
           zero
           zero


type FUCHSIA_EXTERNAL_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_FUCHSIA_EXTERNAL_MEMORY_SPEC_VERSION"
pattern FUCHSIA_EXTERNAL_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern FUCHSIA_EXTERNAL_MEMORY_SPEC_VERSION = 1


type FUCHSIA_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_FUCHSIA_external_memory"

-- No documentation found for TopLevel "VK_FUCHSIA_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern FUCHSIA_EXTERNAL_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern FUCHSIA_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_FUCHSIA_external_memory"

