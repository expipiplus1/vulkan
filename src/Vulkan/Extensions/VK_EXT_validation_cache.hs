{-# language CPP #-}
-- | = Name
--
-- VK_EXT_validation_cache - device extension
--
-- == VK_EXT_validation_cache
--
-- [__Name String__]
--     @VK_EXT_validation_cache@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     161
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Cort Stratton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_validation_cache:%20&body=@cdwfs%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-08-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Cort Stratton, Google
--
--     -   Chris Forbes, Google
--
-- == Description
--
-- This extension provides a mechanism for caching the results of
-- potentially expensive internal validation operations across multiple
-- runs of a Vulkan application. At the core is the
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT' object type, which is
-- managed similarly to the existing 'Vulkan.Core10.Handles.PipelineCache'.
--
-- The new struct 'ShaderModuleValidationCacheCreateInfoEXT' can be
-- included in the @pNext@ chain at
-- 'Vulkan.Core10.Shader.createShaderModule' time. It contains a
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT' to use when validating
-- the 'Vulkan.Core10.Handles.ShaderModule'.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.ValidationCacheEXT'
--
-- == New Commands
--
-- -   'createValidationCacheEXT'
--
-- -   'destroyValidationCacheEXT'
--
-- -   'getValidationCacheDataEXT'
--
-- -   'mergeValidationCachesEXT'
--
-- == New Structures
--
-- -   'ValidationCacheCreateInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Shader.ShaderModuleCreateInfo':
--
--     -   'ShaderModuleValidationCacheCreateInfoEXT'
--
-- == New Enums
--
-- -   'ValidationCacheHeaderVersionEXT'
--
-- == New Bitmasks
--
-- -   'ValidationCacheCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VALIDATION_CACHE_EXTENSION_NAME'
--
-- -   'EXT_VALIDATION_CACHE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_VALIDATION_CACHE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2017-08-29 (Cort Stratton)
--
--     -   Initial draft
--
-- = See Also
--
-- 'ShaderModuleValidationCacheCreateInfoEXT',
-- 'ValidationCacheCreateFlagsEXT', 'ValidationCacheCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT',
-- 'ValidationCacheHeaderVersionEXT', 'createValidationCacheEXT',
-- 'destroyValidationCacheEXT', 'getValidationCacheDataEXT',
-- 'mergeValidationCachesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_validation_cache Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_validation_cache  ( createValidationCacheEXT
                                                  , withValidationCacheEXT
                                                  , destroyValidationCacheEXT
                                                  , getValidationCacheDataEXT
                                                  , mergeValidationCachesEXT
                                                  , ValidationCacheCreateInfoEXT(..)
                                                  , ShaderModuleValidationCacheCreateInfoEXT(..)
                                                  , ValidationCacheCreateFlagsEXT(..)
                                                  , ValidationCacheHeaderVersionEXT( VALIDATION_CACHE_HEADER_VERSION_ONE_EXT
                                                                                   , ..
                                                                                   )
                                                  , EXT_VALIDATION_CACHE_SPEC_VERSION
                                                  , pattern EXT_VALIDATION_CACHE_SPEC_VERSION
                                                  , EXT_VALIDATION_CACHE_EXTENSION_NAME
                                                  , pattern EXT_VALIDATION_CACHE_EXTENSION_NAME
                                                  , ValidationCacheEXT(..)
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (packCStringLen)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateValidationCacheEXT))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyValidationCacheEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetValidationCacheDataEXT))
import Vulkan.Dynamic (DeviceCmds(pVkMergeValidationCachesEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (ValidationCacheEXT)
import Vulkan.Extensions.Handles (ValidationCacheEXT(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (ValidationCacheEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateValidationCacheEXT
  :: FunPtr (Ptr Device_T -> Ptr ValidationCacheCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr ValidationCacheEXT -> IO Result) -> Ptr Device_T -> Ptr ValidationCacheCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr ValidationCacheEXT -> IO Result

-- | vkCreateValidationCacheEXT - Creates a new validation cache
--
-- = Description
--
-- Note
--
-- Applications /can/ track and manage the total host memory size of a
-- validation cache object using the @pAllocator@. Applications /can/ limit
-- the amount of data retrieved from a validation cache object in
-- 'getValidationCacheDataEXT'. Implementations /should/ not internally
-- limit the total number of entries added to a validation cache object or
-- the total host memory consumed.
--
-- Once created, a validation cache /can/ be passed to the
-- 'Vulkan.Core10.Shader.createShaderModule' command by adding this object
-- to the 'Vulkan.Core10.Shader.ShaderModuleCreateInfo' structure’s @pNext@
-- chain. If a 'ShaderModuleValidationCacheCreateInfoEXT' object is
-- included in the 'Vulkan.Core10.Shader.ShaderModuleCreateInfo'::@pNext@
-- chain, and its @validationCache@ field is not
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', the implementation will query
-- it for possible reuse opportunities and update it with new content. The
-- use of the validation cache object in these commands is internally
-- synchronized, and the same validation cache object /can/ be used in
-- multiple threads simultaneously.
--
-- Note
--
-- Implementations /should/ make every effort to limit any critical
-- sections to the actual accesses to the cache, which is expected to be
-- significantly shorter than the duration of the
-- 'Vulkan.Core10.Shader.createShaderModule' command.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateValidationCacheEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateValidationCacheEXT-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'ValidationCacheCreateInfoEXT' structure
--
-- -   #VUID-vkCreateValidationCacheEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateValidationCacheEXT-pValidationCache-parameter#
--     @pValidationCache@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.ValidationCacheEXT' handle
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
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'ValidationCacheCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT'
createValidationCacheEXT :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that creates the validation cache object.
                            Device
                         -> -- | @pCreateInfo@ is a pointer to a 'ValidationCacheCreateInfoEXT' structure
                            -- containing the initial parameters for the validation cache object.
                            ValidationCacheCreateInfoEXT
                         -> -- | @pAllocator@ controls host memory allocation as described in the
                            -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                            -- chapter.
                            ("allocator" ::: Maybe AllocationCallbacks)
                         -> io (ValidationCacheEXT)
createValidationCacheEXT device createInfo allocator = liftIO . evalContT $ do
  let vkCreateValidationCacheEXTPtr = pVkCreateValidationCacheEXT (deviceCmds (device :: Device))
  lift $ unless (vkCreateValidationCacheEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateValidationCacheEXT is null" Nothing Nothing
  let vkCreateValidationCacheEXT' = mkVkCreateValidationCacheEXT vkCreateValidationCacheEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPValidationCache <- ContT $ bracket (callocBytes @ValidationCacheEXT 8) free
  r <- lift $ traceAroundEvent "vkCreateValidationCacheEXT" (vkCreateValidationCacheEXT' (deviceHandle (device)) pCreateInfo pAllocator (pPValidationCache))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pValidationCache <- lift $ peek @ValidationCacheEXT pPValidationCache
  pure $ (pValidationCache)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createValidationCacheEXT' and 'destroyValidationCacheEXT'
--
-- To ensure that 'destroyValidationCacheEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withValidationCacheEXT :: forall io r . MonadIO io => Device -> ValidationCacheCreateInfoEXT -> Maybe AllocationCallbacks -> (io ValidationCacheEXT -> (ValidationCacheEXT -> io ()) -> r) -> r
withValidationCacheEXT device pCreateInfo pAllocator b =
  b (createValidationCacheEXT device pCreateInfo pAllocator)
    (\(o0) -> destroyValidationCacheEXT device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyValidationCacheEXT
  :: FunPtr (Ptr Device_T -> ValidationCacheEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> ValidationCacheEXT -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyValidationCacheEXT - Destroy a validation cache object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyValidationCacheEXT-validationCache-01537# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @validationCache@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyValidationCacheEXT-validationCache-01538# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @validationCache@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyValidationCacheEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyValidationCacheEXT-validationCache-parameter# If
--     @validationCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @validationCache@ /must/ be a valid
--     'Vulkan.Extensions.Handles.ValidationCacheEXT' handle
--
-- -   #VUID-vkDestroyValidationCacheEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyValidationCacheEXT-validationCache-parent# If
--     @validationCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @validationCache@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT'
destroyValidationCacheEXT :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the logical device that destroys the validation cache
                             -- object.
                             Device
                          -> -- | @validationCache@ is the handle of the validation cache to destroy.
                             ValidationCacheEXT
                          -> -- | @pAllocator@ controls host memory allocation as described in the
                             -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                             -- chapter.
                             ("allocator" ::: Maybe AllocationCallbacks)
                          -> io ()
destroyValidationCacheEXT device validationCache allocator = liftIO . evalContT $ do
  let vkDestroyValidationCacheEXTPtr = pVkDestroyValidationCacheEXT (deviceCmds (device :: Device))
  lift $ unless (vkDestroyValidationCacheEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyValidationCacheEXT is null" Nothing Nothing
  let vkDestroyValidationCacheEXT' = mkVkDestroyValidationCacheEXT vkDestroyValidationCacheEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyValidationCacheEXT" (vkDestroyValidationCacheEXT' (deviceHandle (device)) (validationCache) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetValidationCacheDataEXT
  :: FunPtr (Ptr Device_T -> ValidationCacheEXT -> Ptr CSize -> Ptr () -> IO Result) -> Ptr Device_T -> ValidationCacheEXT -> Ptr CSize -> Ptr () -> IO Result

-- | vkGetValidationCacheDataEXT - Get the data store from a validation cache
--
-- = Description
--
-- If @pData@ is @NULL@, then the maximum size of the data that /can/ be
-- retrieved from the validation cache, in bytes, is returned in
-- @pDataSize@. Otherwise, @pDataSize@ /must/ point to a variable set by
-- the user to the size of the buffer, in bytes, pointed to by @pData@, and
-- on return the variable is overwritten with the amount of data actually
-- written to @pData@.
--
-- If @pDataSize@ is less than the maximum size that /can/ be retrieved by
-- the validation cache, at most @pDataSize@ bytes will be written to
-- @pData@, and 'getValidationCacheDataEXT' will return
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE'. Any data written to @pData@ is
-- valid and /can/ be provided as the @pInitialData@ member of the
-- 'ValidationCacheCreateInfoEXT' structure passed to
-- 'createValidationCacheEXT'.
--
-- Two calls to 'getValidationCacheDataEXT' with the same parameters /must/
-- retrieve the same data unless a command that modifies the contents of
-- the cache is called between them.
--
-- Applications /can/ store the data retrieved from the validation cache,
-- and use these data, possibly in a future run of the application, to
-- populate new validation cache objects. The results of validation,
-- however, /may/ depend on the vendor ID, device ID, driver version, and
-- other details of the device. To enable applications to detect when
-- previously retrieved data is incompatible with the device, the initial
-- bytes written to @pData@ /must/ be a header consisting of the following
-- members:
--
-- +--------+----------------------------------------+------------------------------------------+
-- | Offset | Size                                   | Meaning                                  |
-- +========+========================================+==========================================+
-- | 0      | 4                                      | length in bytes of the entire validation |
-- |        |                                        | cache header written as a stream of      |
-- |        |                                        | bytes, with the least significant byte   |
-- |        |                                        | first                                    |
-- +--------+----------------------------------------+------------------------------------------+
-- | 4      | 4                                      | a 'ValidationCacheHeaderVersionEXT'      |
-- |        |                                        | value written as a stream of bytes, with |
-- |        |                                        | the least significant byte first         |
-- +--------+----------------------------------------+------------------------------------------+
-- | 8      | 'Vulkan.Core10.APIConstants.UUID_SIZE' | a layer commit ID expressed as a UUID,   |
-- |        |                                        | which uniquely identifies the version of |
-- |        |                                        | the validation layers used to generate   |
-- |        |                                        | these validation results                 |
-- +--------+----------------------------------------+------------------------------------------+
--
-- Layout for validation cache header version
-- 'VALIDATION_CACHE_HEADER_VERSION_ONE_EXT'
--
-- The first four bytes encode the length of the entire validation cache
-- header, in bytes. This value includes all fields in the header including
-- the validation cache version field and the size of the length field.
--
-- The next four bytes encode the validation cache version, as described
-- for 'ValidationCacheHeaderVersionEXT'. A consumer of the validation
-- cache /should/ use the cache version to interpret the remainder of the
-- cache header.
--
-- If @pDataSize@ is less than what is necessary to store this header,
-- nothing will be written to @pData@ and zero will be written to
-- @pDataSize@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetValidationCacheDataEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetValidationCacheDataEXT-validationCache-parameter#
--     @validationCache@ /must/ be a valid
--     'Vulkan.Extensions.Handles.ValidationCacheEXT' handle
--
-- -   #VUID-vkGetValidationCacheDataEXT-pDataSize-parameter# @pDataSize@
--     /must/ be a valid pointer to a @size_t@ value
--
-- -   #VUID-vkGetValidationCacheDataEXT-pData-parameter# If the value
--     referenced by @pDataSize@ is not @0@, and @pData@ is not @NULL@,
--     @pData@ /must/ be a valid pointer to an array of @pDataSize@ bytes
--
-- -   #VUID-vkGetValidationCacheDataEXT-validationCache-parent#
--     @validationCache@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT'
getValidationCacheDataEXT :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the logical device that owns the validation cache.
                             Device
                          -> -- | @validationCache@ is the validation cache to retrieve data from.
                             ValidationCacheEXT
                          -> io (Result, ("data" ::: ByteString))
getValidationCacheDataEXT device validationCache = liftIO . evalContT $ do
  let vkGetValidationCacheDataEXTPtr = pVkGetValidationCacheDataEXT (deviceCmds (device :: Device))
  lift $ unless (vkGetValidationCacheDataEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetValidationCacheDataEXT is null" Nothing Nothing
  let vkGetValidationCacheDataEXT' = mkVkGetValidationCacheDataEXT vkGetValidationCacheDataEXTPtr
  let device' = deviceHandle (device)
  pPDataSize <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ traceAroundEvent "vkGetValidationCacheDataEXT" (vkGetValidationCacheDataEXT' device' (validationCache) (pPDataSize) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDataSize <- lift $ peek @CSize pPDataSize
  pPData <- ContT $ bracket (callocBytes @(()) (fromIntegral ((coerce @CSize @Word64 pDataSize)))) free
  r' <- lift $ traceAroundEvent "vkGetValidationCacheDataEXT" (vkGetValidationCacheDataEXT' device' (validationCache) (pPDataSize) (pPData))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pDataSize'' <- lift $ peek @CSize pPDataSize
  pData' <- lift $ packCStringLen  (castPtr @() @CChar pPData, (fromIntegral ((coerce @CSize @Word64 pDataSize''))))
  pure $ ((r'), pData')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMergeValidationCachesEXT
  :: FunPtr (Ptr Device_T -> ValidationCacheEXT -> Word32 -> Ptr ValidationCacheEXT -> IO Result) -> Ptr Device_T -> ValidationCacheEXT -> Word32 -> Ptr ValidationCacheEXT -> IO Result

-- | vkMergeValidationCachesEXT - Combine the data stores of validation
-- caches
--
-- = Description
--
-- Note
--
-- The details of the merge operation are implementation dependent, but
-- implementations /should/ merge the contents of the specified validation
-- caches and prune duplicate entries.
--
-- == Valid Usage
--
-- -   #VUID-vkMergeValidationCachesEXT-dstCache-01536# @dstCache@ /must/
--     not appear in the list of source caches
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkMergeValidationCachesEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkMergeValidationCachesEXT-dstCache-parameter# @dstCache@
--     /must/ be a valid 'Vulkan.Extensions.Handles.ValidationCacheEXT'
--     handle
--
-- -   #VUID-vkMergeValidationCachesEXT-pSrcCaches-parameter# @pSrcCaches@
--     /must/ be a valid pointer to an array of @srcCacheCount@ valid
--     'Vulkan.Extensions.Handles.ValidationCacheEXT' handles
--
-- -   #VUID-vkMergeValidationCachesEXT-srcCacheCount-arraylength#
--     @srcCacheCount@ /must/ be greater than @0@
--
-- -   #VUID-vkMergeValidationCachesEXT-dstCache-parent# @dstCache@ /must/
--     have been created, allocated, or retrieved from @device@
--
-- -   #VUID-vkMergeValidationCachesEXT-pSrcCaches-parent# Each element of
--     @pSrcCaches@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @dstCache@ /must/ be externally synchronized
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT'
mergeValidationCachesEXT :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that owns the validation cache objects.
                            Device
                         -> -- | @dstCache@ is the handle of the validation cache to merge results into.
                            ("dstCache" ::: ValidationCacheEXT)
                         -> -- | @pSrcCaches@ is a pointer to an array of validation cache handles, which
                            -- will be merged into @dstCache@. The previous contents of @dstCache@ are
                            -- included after the merge.
                            ("srcCaches" ::: Vector ValidationCacheEXT)
                         -> io ()
mergeValidationCachesEXT device dstCache srcCaches = liftIO . evalContT $ do
  let vkMergeValidationCachesEXTPtr = pVkMergeValidationCachesEXT (deviceCmds (device :: Device))
  lift $ unless (vkMergeValidationCachesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkMergeValidationCachesEXT is null" Nothing Nothing
  let vkMergeValidationCachesEXT' = mkVkMergeValidationCachesEXT vkMergeValidationCachesEXTPtr
  pPSrcCaches <- ContT $ allocaBytesAligned @ValidationCacheEXT ((Data.Vector.length (srcCaches)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPSrcCaches `plusPtr` (8 * (i)) :: Ptr ValidationCacheEXT) (e)) (srcCaches)
  r <- lift $ traceAroundEvent "vkMergeValidationCachesEXT" (vkMergeValidationCachesEXT' (deviceHandle (device)) (dstCache) ((fromIntegral (Data.Vector.length $ (srcCaches)) :: Word32)) (pPSrcCaches))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkValidationCacheCreateInfoEXT - Structure specifying parameters of a
-- newly created validation cache
--
-- == Valid Usage
--
-- -   #VUID-VkValidationCacheCreateInfoEXT-initialDataSize-01534# If
--     @initialDataSize@ is not @0@, it /must/ be equal to the size of
--     @pInitialData@, as returned by 'getValidationCacheDataEXT' when
--     @pInitialData@ was originally retrieved
--
-- -   #VUID-VkValidationCacheCreateInfoEXT-initialDataSize-01535# If
--     @initialDataSize@ is not @0@, @pInitialData@ /must/ have been
--     retrieved from a previous call to 'getValidationCacheDataEXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkValidationCacheCreateInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT'
--
-- -   #VUID-VkValidationCacheCreateInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkValidationCacheCreateInfoEXT-flags-zerobitmask# @flags@
--     /must/ be @0@
--
-- -   #VUID-VkValidationCacheCreateInfoEXT-pInitialData-parameter# If
--     @initialDataSize@ is not @0@, @pInitialData@ /must/ be a valid
--     pointer to an array of @initialDataSize@ bytes
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'ValidationCacheCreateFlagsEXT', 'createValidationCacheEXT'
data ValidationCacheCreateInfoEXT = ValidationCacheCreateInfoEXT
  { -- | @flags@ is reserved for future use.
    flags :: ValidationCacheCreateFlagsEXT
  , -- | @initialDataSize@ is the number of bytes in @pInitialData@. If
    -- @initialDataSize@ is zero, the validation cache will initially be empty.
    initialDataSize :: Word64
  , -- | @pInitialData@ is a pointer to previously retrieved validation cache
    -- data. If the validation cache data is incompatible (as defined below)
    -- with the device, the validation cache will be initially empty. If
    -- @initialDataSize@ is zero, @pInitialData@ is ignored.
    initialData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ValidationCacheCreateInfoEXT)
#endif
deriving instance Show ValidationCacheCreateInfoEXT

instance ToCStruct ValidationCacheCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ValidationCacheCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ValidationCacheCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (initialDataSize))
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (initialData)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct ValidationCacheCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @ValidationCacheCreateFlagsEXT ((p `plusPtr` 16 :: Ptr ValidationCacheCreateFlagsEXT))
    initialDataSize <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    pInitialData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ ValidationCacheCreateInfoEXT
             flags (coerce @CSize @Word64 initialDataSize) pInitialData

instance Storable ValidationCacheCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ValidationCacheCreateInfoEXT where
  zero = ValidationCacheCreateInfoEXT
           zero
           zero
           zero


-- | VkShaderModuleValidationCacheCreateInfoEXT - Specify validation cache to
-- use during shader module creation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.ValidationCacheEXT'
data ShaderModuleValidationCacheCreateInfoEXT = ShaderModuleValidationCacheCreateInfoEXT
  { -- | @validationCache@ is the validation cache object from which the results
    -- of prior validation attempts will be written, and to which new
    -- validation results for this 'Vulkan.Core10.Handles.ShaderModule' will be
    -- written (if not already present).
    --
    -- #VUID-VkShaderModuleValidationCacheCreateInfoEXT-validationCache-parameter#
    -- @validationCache@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.ValidationCacheEXT' handle
    validationCache :: ValidationCacheEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderModuleValidationCacheCreateInfoEXT)
#endif
deriving instance Show ShaderModuleValidationCacheCreateInfoEXT

instance ToCStruct ShaderModuleValidationCacheCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderModuleValidationCacheCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ValidationCacheEXT)) (validationCache)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ValidationCacheEXT)) (zero)
    f

instance FromCStruct ShaderModuleValidationCacheCreateInfoEXT where
  peekCStruct p = do
    validationCache <- peek @ValidationCacheEXT ((p `plusPtr` 16 :: Ptr ValidationCacheEXT))
    pure $ ShaderModuleValidationCacheCreateInfoEXT
             validationCache

instance Storable ShaderModuleValidationCacheCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ShaderModuleValidationCacheCreateInfoEXT where
  zero = ShaderModuleValidationCacheCreateInfoEXT
           zero


-- | VkValidationCacheCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'ValidationCacheCreateFlagsEXT' is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
--
-- 'ValidationCacheCreateInfoEXT'
newtype ValidationCacheCreateFlagsEXT = ValidationCacheCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameValidationCacheCreateFlagsEXT :: String
conNameValidationCacheCreateFlagsEXT = "ValidationCacheCreateFlagsEXT"

enumPrefixValidationCacheCreateFlagsEXT :: String
enumPrefixValidationCacheCreateFlagsEXT = ""

showTableValidationCacheCreateFlagsEXT :: [(ValidationCacheCreateFlagsEXT, String)]
showTableValidationCacheCreateFlagsEXT = []

instance Show ValidationCacheCreateFlagsEXT where
  showsPrec = enumShowsPrec enumPrefixValidationCacheCreateFlagsEXT
                            showTableValidationCacheCreateFlagsEXT
                            conNameValidationCacheCreateFlagsEXT
                            (\(ValidationCacheCreateFlagsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ValidationCacheCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixValidationCacheCreateFlagsEXT
                          showTableValidationCacheCreateFlagsEXT
                          conNameValidationCacheCreateFlagsEXT
                          ValidationCacheCreateFlagsEXT


-- | VkValidationCacheHeaderVersionEXT - Encode validation cache version
--
-- = See Also
--
-- 'createValidationCacheEXT', 'getValidationCacheDataEXT'
newtype ValidationCacheHeaderVersionEXT = ValidationCacheHeaderVersionEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'VALIDATION_CACHE_HEADER_VERSION_ONE_EXT' specifies version one of the
-- validation cache.
pattern VALIDATION_CACHE_HEADER_VERSION_ONE_EXT = ValidationCacheHeaderVersionEXT 1
{-# complete VALIDATION_CACHE_HEADER_VERSION_ONE_EXT :: ValidationCacheHeaderVersionEXT #-}

conNameValidationCacheHeaderVersionEXT :: String
conNameValidationCacheHeaderVersionEXT = "ValidationCacheHeaderVersionEXT"

enumPrefixValidationCacheHeaderVersionEXT :: String
enumPrefixValidationCacheHeaderVersionEXT = "VALIDATION_CACHE_HEADER_VERSION_ONE_EXT"

showTableValidationCacheHeaderVersionEXT :: [(ValidationCacheHeaderVersionEXT, String)]
showTableValidationCacheHeaderVersionEXT = [(VALIDATION_CACHE_HEADER_VERSION_ONE_EXT, "")]

instance Show ValidationCacheHeaderVersionEXT where
  showsPrec = enumShowsPrec enumPrefixValidationCacheHeaderVersionEXT
                            showTableValidationCacheHeaderVersionEXT
                            conNameValidationCacheHeaderVersionEXT
                            (\(ValidationCacheHeaderVersionEXT x) -> x)
                            (showsPrec 11)

instance Read ValidationCacheHeaderVersionEXT where
  readPrec = enumReadPrec enumPrefixValidationCacheHeaderVersionEXT
                          showTableValidationCacheHeaderVersionEXT
                          conNameValidationCacheHeaderVersionEXT
                          ValidationCacheHeaderVersionEXT


type EXT_VALIDATION_CACHE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_SPEC_VERSION"
pattern EXT_VALIDATION_CACHE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_VALIDATION_CACHE_SPEC_VERSION = 1


type EXT_VALIDATION_CACHE_EXTENSION_NAME = "VK_EXT_validation_cache"

-- No documentation found for TopLevel "VK_EXT_VALIDATION_CACHE_EXTENSION_NAME"
pattern EXT_VALIDATION_CACHE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VALIDATION_CACHE_EXTENSION_NAME = "VK_EXT_validation_cache"

