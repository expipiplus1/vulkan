{-# language CPP #-}
-- | = Name
--
-- VK_EXT_external_memory_metal - device extension
--
-- = VK_EXT_external_memory_metal
--
-- [__Name String__]
--     @VK_EXT_external_memory_metal@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     603
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Aitor Camacho Larrondo
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_external_memory_metal] @aitor-lunarg%0A*Here describe the issue or question you have about the VK_EXT_external_memory_metal extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_external_memory_metal.adoc VK_EXT_external_memory_metal>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-07-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Aitor Camacho Larrondo, LunarG Inc.
--
-- == Description
--
-- An application may wish to reference device memory in multiple Vulkan
-- device instances, in multiple processes, and\/or in Metal API. This
-- extension enables an application to export and import Metal handles from
-- Vulkan memory objects such that the underlying resources can be
-- referenced outside the scope of the Vulkan device instance that created
-- them.
--
-- == New Commands
--
-- -   'getMemoryMetalHandleEXT'
--
-- -   'getMemoryMetalHandlePropertiesEXT'
--
-- == New Structures
--
-- -   'MemoryGetMetalHandleInfoEXT'
--
-- -   'MemoryMetalHandlePropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMemoryMetalHandleInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTERNAL_MEMORY_METAL_EXTENSION_NAME'
--
-- -   'EXT_EXTERNAL_MEMORY_METAL_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLBUFFER_BIT_EXT'
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLHEAP_BIT_EXT'
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLTEXTURE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_METAL_HANDLE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_METAL_HANDLE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_METAL_HANDLE_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2024-07-18 (Aitor Camacho Larrondo)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_external_memory_metal Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_external_memory_metal  ( getMemoryMetalHandleEXT
                                                       , getMemoryMetalHandlePropertiesEXT
                                                       , ImportMemoryMetalHandleInfoEXT(..)
                                                       , MemoryMetalHandlePropertiesEXT(..)
                                                       , MemoryGetMetalHandleInfoEXT(..)
                                                       , EXT_EXTERNAL_MEMORY_METAL_SPEC_VERSION
                                                       , pattern EXT_EXTERNAL_MEMORY_METAL_SPEC_VERSION
                                                       , EXT_EXTERNAL_MEMORY_METAL_EXTENSION_NAME
                                                       , pattern EXT_EXTERNAL_MEMORY_METAL_EXTENSION_NAME
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
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryMetalHandleEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryMetalHandlePropertiesEXT))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_METAL_HANDLE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_GET_METAL_HANDLE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_METAL_HANDLE_PROPERTIES_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryMetalHandleEXT
  :: FunPtr (Ptr Device_T -> Ptr MemoryGetMetalHandleInfoEXT -> Ptr (Ptr ()) -> IO Result) -> Ptr Device_T -> Ptr MemoryGetMetalHandleInfoEXT -> Ptr (Ptr ()) -> IO Result

-- | vkGetMemoryMetalHandleEXT - Get a Metal handle for a memory object
--
-- = Description
--
-- Unless the app retains the handle object returned by the call, the
-- lifespan will be the same as the associated
-- 'Vulkan.Core10.Handles.DeviceMemory'.
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_external_memory_metal VK_EXT_external_memory_metal>,
-- 'Vulkan.Core10.Handles.Device', 'MemoryGetMetalHandleInfoEXT'
getMemoryMetalHandleEXT :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device that created the device memory being
                           -- exported.
                           --
                           -- #VUID-vkGetMemoryMetalHandleEXT-device-parameter# @device@ /must/ be a
                           -- valid 'Vulkan.Core10.Handles.Device' handle
                           Device
                        -> -- | @pGetMetalHandleInfo@ is a pointer to a 'MemoryGetMetalHandleInfoEXT'
                           -- structure containing parameters of the export operation.
                           --
                           -- #VUID-vkGetMemoryMetalHandleEXT-pGetMetalHandleInfo-parameter#
                           -- @pGetMetalHandleInfo@ /must/ be a valid pointer to a valid
                           -- 'MemoryGetMetalHandleInfoEXT' structure
                           MemoryGetMetalHandleInfoEXT
                        -> io (("handle" ::: Ptr ()))
getMemoryMetalHandleEXT device getMetalHandleInfo = liftIO . evalContT $ do
  let vkGetMemoryMetalHandleEXTPtr = pVkGetMemoryMetalHandleEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetMemoryMetalHandleEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryMetalHandleEXT is null" Nothing Nothing
  let vkGetMemoryMetalHandleEXT' = mkVkGetMemoryMetalHandleEXT vkGetMemoryMetalHandleEXTPtr
  pGetMetalHandleInfo <- ContT $ withCStruct (getMetalHandleInfo)
  pPHandle <- ContT $ bracket (callocBytes @(Ptr ()) 8) free
  r <- lift $ traceAroundEvent "vkGetMemoryMetalHandleEXT" (vkGetMemoryMetalHandleEXT'
                                                              (deviceHandle (device))
                                                              pGetMetalHandleInfo
                                                              (pPHandle))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pHandle <- lift $ peek @(Ptr ()) pPHandle
  pure $ (pHandle)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryMetalHandlePropertiesEXT
  :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> Ptr () -> Ptr MemoryMetalHandlePropertiesEXT -> IO Result) -> Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> Ptr () -> Ptr MemoryMetalHandlePropertiesEXT -> IO Result

-- | vkGetMemoryMetalHandlePropertiesEXT - Get Properties of External Memory
-- Metal Handles
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_external_memory_metal VK_EXT_external_memory_metal>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'MemoryMetalHandlePropertiesEXT'
getMemoryMetalHandlePropertiesEXT :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the logical device that will be importing @pHandle@.
                                     --
                                     -- #VUID-vkGetMemoryMetalHandlePropertiesEXT-device-parameter# @device@
                                     -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                     Device
                                  -> -- | @handleType@ is a
                                     -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
                                     -- value specifying the type of the handle @pHandle@.
                                     --
                                     -- #VUID-vkGetMemoryMetalHandlePropertiesEXT-handleType-10417# @handleType@
                                     -- /must/ be
                                     -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLBUFFER_BIT_EXT',
                                     -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLTEXTURE_BIT_EXT'
                                     -- or
                                     -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLHEAP_BIT_EXT'
                                     --
                                     -- #VUID-vkGetMemoryMetalHandlePropertiesEXT-handleType-parameter#
                                     -- @handleType@ /must/ be a valid
                                     -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
                                     -- value
                                     ExternalMemoryHandleTypeFlagBits
                                  -> -- | @pHandle@ is the handle which will be imported.
                                     --
                                     -- #VUID-vkGetMemoryMetalHandlePropertiesEXT-handle-10416# @pHandle@ /must/
                                     -- point to a valid id\<MTLBuffer>, id\<MTLTexture> or id\<MTLDevice>
                                     --
                                     -- #VUID-vkGetMemoryMetalHandlePropertiesEXT-pHandle-parameter# @pHandle@
                                     -- /must/ be a pointer value
                                     ("handle" ::: Ptr ())
                                  -> io (MemoryMetalHandlePropertiesEXT)
getMemoryMetalHandlePropertiesEXT device
                                    handleType
                                    handle = liftIO . evalContT $ do
  let vkGetMemoryMetalHandlePropertiesEXTPtr = pVkGetMemoryMetalHandlePropertiesEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetMemoryMetalHandlePropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryMetalHandlePropertiesEXT is null" Nothing Nothing
  let vkGetMemoryMetalHandlePropertiesEXT' = mkVkGetMemoryMetalHandlePropertiesEXT vkGetMemoryMetalHandlePropertiesEXTPtr
  pPMemoryMetalHandleProperties <- ContT (withZeroCStruct @MemoryMetalHandlePropertiesEXT)
  r <- lift $ traceAroundEvent "vkGetMemoryMetalHandlePropertiesEXT" (vkGetMemoryMetalHandlePropertiesEXT'
                                                                        (deviceHandle (device))
                                                                        (handleType)
                                                                        (handle)
                                                                        (pPMemoryMetalHandleProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryMetalHandleProperties <- lift $ peekCStruct @MemoryMetalHandlePropertiesEXT pPMemoryMetalHandleProperties
  pure $ (pMemoryMetalHandleProperties)


-- | VkImportMemoryMetalHandleInfoEXT - Import Metal memory created on the
-- same physical device
--
-- = Description
--
-- Importing memory object payloads from Metal handles shares the ownership
-- of the handle to the Vulkan implementation.
--
-- Applications /can/ import the same payload into multiple instances of
-- Vulkan, into the same instance from which it was exported, and multiple
-- times into a given Vulkan instance. In all cases, each import operation
-- /must/ create a distinct 'Vulkan.Core10.Handles.DeviceMemory' object.
--
-- == Valid Usage
--
-- -   #VUID-VkImportMemoryMetalHandleInfoEXT-handleType-10408# If
--     @handleType@ is not @0@, it /must/ be supported for import, as
--     reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties'
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'
--
-- -   #VUID-VkImportMemoryMetalHandleInfoEXT-handle-10409# The memory from
--     which @handle@ was exported /must/ have been created on the same
--     underlying physical device as @device@
--
-- -   #VUID-VkImportMemoryMetalHandleInfoEXT-handleType-10410# If
--     @handleType@ is not @0@, it /must/ be
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLBUFFER_BIT_EXT',
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLTEXTURE_BIT_EXT'
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLHEAP_BIT_EXT'
--
-- -   #VUID-VkImportMemoryMetalHandleInfoEXT-handleType-10411# If
--     @handleType@ is not @0@ , @handle@ /must/ be a valid non-NULL handle
--     of the type specified by @handleType@
--
-- -   #VUID-VkImportMemoryMetalHandleInfoEXT-handle-10412# @handle@ /must/
--     obey any requirements listed for @handleType@ in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#external-memory-handle-types-compatibility external memory handle types compatibility>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImportMemoryMetalHandleInfoEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_METAL_HANDLE_INFO_EXT'
--
-- -   #VUID-VkImportMemoryMetalHandleInfoEXT-handleType-parameter# If
--     @handleType@ is not @0@, @handleType@ /must/ be a valid
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_external_memory_metal VK_EXT_external_memory_metal>,
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImportMemoryMetalHandleInfoEXT = ImportMemoryMetalHandleInfoEXT
  { -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the type of @handle@ or @name@.
    handleType :: ExternalMemoryHandleTypeFlagBits
  , -- | @handle@ is @NULL@ or the external handle to import.
    handle :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemoryMetalHandleInfoEXT)
#endif
deriving instance Show ImportMemoryMetalHandleInfoEXT

instance ToCStruct ImportMemoryMetalHandleInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemoryMetalHandleInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_METAL_HANDLE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (handle)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_METAL_HANDLE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImportMemoryMetalHandleInfoEXT where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits))
    handle <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ ImportMemoryMetalHandleInfoEXT
             handleType handle

instance Storable ImportMemoryMetalHandleInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemoryMetalHandleInfoEXT where
  zero = ImportMemoryMetalHandleInfoEXT
           zero
           zero


-- | VkMemoryMetalHandlePropertiesEXT - Properties of External Memory Metal
-- Handles
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_external_memory_metal VK_EXT_external_memory_metal>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryMetalHandlePropertiesEXT'
data MemoryMetalHandlePropertiesEXT = MemoryMetalHandlePropertiesEXT
  { -- | @memoryTypeBits@ is a bitmask containing one bit set for every memory
    -- type which the specified Metal handle /can/ be imported as.
    memoryTypeBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryMetalHandlePropertiesEXT)
#endif
deriving instance Show MemoryMetalHandlePropertiesEXT

instance ToCStruct MemoryMetalHandlePropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryMetalHandlePropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_METAL_HANDLE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_METAL_HANDLE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryMetalHandlePropertiesEXT where
  peekCStruct p = do
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ MemoryMetalHandlePropertiesEXT
             memoryTypeBits

instance Storable MemoryMetalHandlePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryMetalHandlePropertiesEXT where
  zero = MemoryMetalHandlePropertiesEXT
           zero


-- | VkMemoryGetMetalHandleInfoEXT - Structure describing a Metal handle
-- memory export operation
--
-- = Description
--
-- The properties of the handle returned depend on the value of
-- @handleType@. See
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
-- for a description of the properties of the defined external memory
-- handle types.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_external_memory_metal VK_EXT_external_memory_metal>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryMetalHandleEXT'
data MemoryGetMetalHandleInfoEXT = MemoryGetMetalHandleInfoEXT
  { -- | @memory@ is the memory object from which the handle will be exported.
    --
    -- #VUID-VkMemoryGetMetalHandleInfoEXT-memory-10413# @memory@ /must/ have
    -- been created with a valid
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'
    --
    -- #VUID-VkMemoryGetMetalHandleInfoEXT-memory-parameter# @memory@ /must/ be
    -- a valid 'Vulkan.Core10.Handles.DeviceMemory' handle
    memory :: DeviceMemory
  , -- | @handleType@ is a
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value specifying the type of handle requested.
    --
    -- #VUID-VkMemoryGetMetalHandleInfoEXT-handleType-10414# @handleType@
    -- /must/ have been included in
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
    -- when @memory@ was created
    --
    -- #VUID-VkMemoryGetMetalHandleInfoEXT-handleType-10415# @handleType@
    -- /must/ be
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLBUFFER_BIT_EXT',
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLTEXTURE_BIT_EXT'
    -- or
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLHEAP_BIT_EXT'
    --
    -- #VUID-VkMemoryGetMetalHandleInfoEXT-handleType-parameter# @handleType@
    -- /must/ be a valid
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryGetMetalHandleInfoEXT)
#endif
deriving instance Show MemoryGetMetalHandleInfoEXT

instance ToCStruct MemoryGetMetalHandleInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryGetMetalHandleInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_METAL_HANDLE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_METAL_HANDLE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    f

instance FromCStruct MemoryGetMetalHandleInfoEXT where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ MemoryGetMetalHandleInfoEXT
             memory handleType

instance Storable MemoryGetMetalHandleInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryGetMetalHandleInfoEXT where
  zero = MemoryGetMetalHandleInfoEXT
           zero
           zero


type EXT_EXTERNAL_MEMORY_METAL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_METAL_SPEC_VERSION"
pattern EXT_EXTERNAL_MEMORY_METAL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTERNAL_MEMORY_METAL_SPEC_VERSION = 1


type EXT_EXTERNAL_MEMORY_METAL_EXTENSION_NAME = "VK_EXT_external_memory_metal"

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_METAL_EXTENSION_NAME"
pattern EXT_EXTERNAL_MEMORY_METAL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTERNAL_MEMORY_METAL_EXTENSION_NAME = "VK_EXT_external_memory_metal"

