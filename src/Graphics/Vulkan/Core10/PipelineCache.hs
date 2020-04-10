{-# language CPP #-}
module Graphics.Vulkan.Core10.PipelineCache  ( createPipelineCache
                                             , withPipelineCache
                                             , destroyPipelineCache
                                             , getPipelineCacheData
                                             , mergePipelineCaches
                                             , PipelineCacheCreateInfo(..)
                                             ) where

import Control.Exception.Base (bracket)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCStringLen)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreatePipelineCache))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyPipelineCache))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetPipelineCacheData))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkMergePipelineCaches))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (PipelineCache)
import Graphics.Vulkan.Core10.Handles (PipelineCache(..))
import Graphics.Vulkan.Core10.Enums.PipelineCacheCreateFlags (PipelineCacheCreateFlags)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineCache
  :: FunPtr (Ptr Device_T -> Ptr PipelineCacheCreateInfo -> Ptr AllocationCallbacks -> Ptr PipelineCache -> IO Result) -> Ptr Device_T -> Ptr PipelineCacheCreateInfo -> Ptr AllocationCallbacks -> Ptr PipelineCache -> IO Result

-- | vkCreatePipelineCache - Creates a new pipeline cache
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the pipeline cache
--     object.
--
-- -   @pCreateInfo@ is a pointer to a 'PipelineCacheCreateInfo' structure
--     containing initial parameters for the pipeline cache object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelineCache@ is a pointer to a
--     'Graphics.Vulkan.Core10.Handles.PipelineCache' handle in which the
--     resulting pipeline cache object is returned.
--
-- = Description
--
-- Note
--
-- Applications /can/ track and manage the total host memory size of a
-- pipeline cache object using the @pAllocator@. Applications /can/ limit
-- the amount of data retrieved from a pipeline cache object in
-- 'getPipelineCacheData'. Implementations /should/ not internally limit
-- the total number of entries added to a pipeline cache object or the
-- total host memory consumed.
--
-- Once created, a pipeline cache /can/ be passed to the
-- 'Graphics.Vulkan.Core10.Pipeline.createGraphicsPipelines' and
-- 'Graphics.Vulkan.Core10.Pipeline.createComputePipelines' commands. If
-- the pipeline cache passed into these commands is not
-- 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', the implementation
-- will query it for possible reuse opportunities and update it with new
-- content. The use of the pipeline cache object in these commands is
-- internally synchronized, and the same pipeline cache object /can/ be
-- used in multiple threads simultaneously.
--
-- Note
--
-- Implementations /should/ make every effort to limit any critical
-- sections to the actual accesses to the cache, which is expected to be
-- significantly shorter than the duration of the
-- 'Graphics.Vulkan.Core10.Pipeline.createGraphicsPipelines' and
-- 'Graphics.Vulkan.Core10.Pipeline.createComputePipelines' commands.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'PipelineCacheCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pPipelineCache@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.PipelineCache' handle
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
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.PipelineCache',
-- 'PipelineCacheCreateInfo'
createPipelineCache :: Device -> PipelineCacheCreateInfo -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (PipelineCache)
createPipelineCache device createInfo allocator = evalContT $ do
  let vkCreatePipelineCache' = mkVkCreatePipelineCache (pVkCreatePipelineCache (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelineCache <- ContT $ bracket (callocBytes @PipelineCache 8) free
  r <- lift $ vkCreatePipelineCache' (deviceHandle (device)) pCreateInfo pAllocator (pPPipelineCache)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelineCache <- lift $ peek @PipelineCache pPPipelineCache
  pure $ (pPipelineCache)

-- | A safe wrapper for 'createPipelineCache' and 'destroyPipelineCache'
-- using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withPipelineCache :: Device -> PipelineCacheCreateInfo -> Maybe AllocationCallbacks -> ((PipelineCache) -> IO r) -> IO r
withPipelineCache device pCreateInfo pAllocator =
  bracket
    (createPipelineCache device pCreateInfo pAllocator)
    (\(o0) -> destroyPipelineCache device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineCache
  :: FunPtr (Ptr Device_T -> PipelineCache -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> PipelineCache -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyPipelineCache - Destroy a pipeline cache object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the pipeline cache
--     object.
--
-- -   @pipelineCache@ is the handle of the pipeline cache to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @pipelineCache@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @pipelineCache@ was created, @pAllocator@ /must/
--     be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   If @pipelineCache@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @pipelineCache@
--     /must/ be a valid 'Graphics.Vulkan.Core10.Handles.PipelineCache'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @pipelineCache@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.PipelineCache'
destroyPipelineCache :: Device -> PipelineCache -> ("allocator" ::: Maybe AllocationCallbacks) -> IO ()
destroyPipelineCache device pipelineCache allocator = evalContT $ do
  let vkDestroyPipelineCache' = mkVkDestroyPipelineCache (pVkDestroyPipelineCache (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyPipelineCache' (deviceHandle (device)) (pipelineCache) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineCacheData
  :: FunPtr (Ptr Device_T -> PipelineCache -> Ptr CSize -> Ptr () -> IO Result) -> Ptr Device_T -> PipelineCache -> Ptr CSize -> Ptr () -> IO Result

-- | vkGetPipelineCacheData - Get the data store from a pipeline cache
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the pipeline cache.
--
-- -   @pipelineCache@ is the pipeline cache to retrieve data from.
--
-- -   @pDataSize@ is a pointer to a @size_t@ value related to the amount
--     of data in the pipeline cache, as described below.
--
-- -   @pData@ is either @NULL@ or a pointer to a buffer.
--
-- = Description
--
-- If @pData@ is @NULL@, then the maximum size of the data that /can/ be
-- retrieved from the pipeline cache, in bytes, is returned in @pDataSize@.
-- Otherwise, @pDataSize@ /must/ point to a variable set by the user to the
-- size of the buffer, in bytes, pointed to by @pData@, and on return the
-- variable is overwritten with the amount of data actually written to
-- @pData@.
--
-- If @pDataSize@ is less than the maximum size that /can/ be retrieved by
-- the pipeline cache, at most @pDataSize@ bytes will be written to
-- @pData@, and 'getPipelineCacheData' will return
-- 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'. Any data written to
-- @pData@ is valid and /can/ be provided as the @pInitialData@ member of
-- the 'PipelineCacheCreateInfo' structure passed to 'createPipelineCache'.
--
-- Two calls to 'getPipelineCacheData' with the same parameters /must/
-- retrieve the same data unless a command that modifies the contents of
-- the cache is called between them.
--
-- Applications /can/ store the data retrieved from the pipeline cache, and
-- use these data, possibly in a future run of the application, to populate
-- new pipeline cache objects. The results of pipeline compiles, however,
-- /may/ depend on the vendor ID, device ID, driver version, and other
-- details of the device. To enable applications to detect when previously
-- retrieved data is incompatible with the device, the initial bytes
-- written to @pData@ /must/ be a header consisting of the following
-- members:
--
-- +--------+-------------------------------------------------+---------------------------------------------------------------------------------------------+
-- | Offset | Size                                            | Meaning                                                                                     |
-- +========+=================================================+=============================================================================================+
-- | 0      | 4                                               | length in bytes of the entire pipeline cache header written as a stream of bytes, with the  |
-- |        |                                                 | least significant byte first                                                                |
-- +--------+-------------------------------------------------+---------------------------------------------------------------------------------------------+
-- | 4      | 4                                               | a 'Graphics.Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'      |
-- |        |                                                 | value written as a stream of bytes, with the least significant byte first                   |
-- +--------+-------------------------------------------------+---------------------------------------------------------------------------------------------+
-- | 8      | 4                                               | a vendor ID equal to                                                                        |
-- |        |                                                 | 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@vendorID@ written  |
-- |        |                                                 | as a stream of bytes, with the least significant byte first                                 |
-- +--------+-------------------------------------------------+---------------------------------------------------------------------------------------------+
-- | 12     | 4                                               | a device ID equal to                                                                        |
-- |        |                                                 | 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@deviceID@ written  |
-- |        |                                                 | as a stream of bytes, with the least significant byte first                                 |
-- +--------+-------------------------------------------------+---------------------------------------------------------------------------------------------+
-- | 16     | 'Graphics.Vulkan.Core10.APIConstants.UUID_SIZE' | a pipeline cache ID equal to                                                                |
-- |        |                                                 | 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@pipelineCacheUUID@ |
-- +--------+-------------------------------------------------+---------------------------------------------------------------------------------------------+
--
-- Layout for pipeline cache header version
-- 'Graphics.Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PIPELINE_CACHE_HEADER_VERSION_ONE'
--
-- The first four bytes encode the length of the entire pipeline cache
-- header, in bytes. This value includes all fields in the header including
-- the pipeline cache version field and the size of the length field.
--
-- The next four bytes encode the pipeline cache version, as described for
-- 'Graphics.Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'.
-- A consumer of the pipeline cache /should/ use the cache version to
-- interpret the remainder of the cache header.
--
-- If @pDataSize@ is less than what is necessary to store this header,
-- nothing will be written to @pData@ and zero will be written to
-- @pDataSize@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pipelineCache@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   @pDataSize@ /must/ be a valid pointer to a @size_t@ value
--
-- -   If the value referenced by @pDataSize@ is not @0@, and @pData@ is
--     not @NULL@, @pData@ /must/ be a valid pointer to an array of
--     @pDataSize@ bytes
--
-- -   @pipelineCache@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.PipelineCache'
getPipelineCacheData :: Device -> PipelineCache -> IO (Result, ("data" ::: ByteString))
getPipelineCacheData device pipelineCache = evalContT $ do
  let vkGetPipelineCacheData' = mkVkGetPipelineCacheData (pVkGetPipelineCacheData (deviceCmds (device :: Device)))
  let device' = deviceHandle (device)
  pPDataSize <- ContT $ bracket (callocBytes @CSize 8) free
  r <- lift $ vkGetPipelineCacheData' device' (pipelineCache) (pPDataSize) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDataSize <- lift $ peek @CSize pPDataSize
  pPData <- ContT $ bracket (callocBytes @(()) (fromIntegral (((\(CSize a) -> a) pDataSize)))) free
  r' <- lift $ vkGetPipelineCacheData' device' (pipelineCache) (pPDataSize) (pPData)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pDataSize'' <- lift $ peek @CSize pPDataSize
  pData' <- lift $ packCStringLen  (castPtr @() @CChar pPData, (fromIntegral (((\(CSize a) -> a) pDataSize''))))
  pure $ ((r'), pData')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMergePipelineCaches
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr PipelineCache -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr PipelineCache -> IO Result

-- | vkMergePipelineCaches - Combine the data stores of pipeline caches
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the pipeline cache objects.
--
-- -   @dstCache@ is the handle of the pipeline cache to merge results
--     into.
--
-- -   @srcCacheCount@ is the length of the @pSrcCaches@ array.
--
-- -   @pSrcCaches@ is a pointer to an array of pipeline cache handles,
--     which will be merged into @dstCache@. The previous contents of
--     @dstCache@ are included after the merge.
--
-- = Description
--
-- Note
--
-- The details of the merge operation are implementation dependent, but
-- implementations /should/ merge the contents of the specified pipelines
-- and prune duplicate entries.
--
-- == Valid Usage
--
-- -   @dstCache@ /must/ not appear in the list of source caches
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @dstCache@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   @pSrcCaches@ /must/ be a valid pointer to an array of
--     @srcCacheCount@ valid 'Graphics.Vulkan.Core10.Handles.PipelineCache'
--     handles
--
-- -   @srcCacheCount@ /must/ be greater than @0@
--
-- -   @dstCache@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- -   Each element of @pSrcCaches@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @dstCache@ /must/ be externally synchronized
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
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.PipelineCache'
mergePipelineCaches :: Device -> ("dstCache" ::: PipelineCache) -> ("srcCaches" ::: Vector PipelineCache) -> IO ()
mergePipelineCaches device dstCache srcCaches = evalContT $ do
  let vkMergePipelineCaches' = mkVkMergePipelineCaches (pVkMergePipelineCaches (deviceCmds (device :: Device)))
  pPSrcCaches <- ContT $ allocaBytesAligned @PipelineCache ((Data.Vector.length (srcCaches)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPSrcCaches `plusPtr` (8 * (i)) :: Ptr PipelineCache) (e)) (srcCaches)
  r <- lift $ vkMergePipelineCaches' (deviceHandle (device)) (dstCache) ((fromIntegral (Data.Vector.length $ (srcCaches)) :: Word32)) (pPSrcCaches)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkPipelineCacheCreateInfo - Structure specifying parameters of a newly
-- created pipeline cache
--
-- == Valid Usage
--
-- -   If @initialDataSize@ is not @0@, it /must/ be equal to the size of
--     @pInitialData@, as returned by 'getPipelineCacheData' when
--     @pInitialData@ was originally retrieved
--
-- -   If @initialDataSize@ is not @0@, @pInitialData@ /must/ have been
--     retrieved from a previous call to 'getPipelineCacheData'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   If @initialDataSize@ is not @0@, @pInitialData@ /must/ be a valid
--     pointer to an array of @initialDataSize@ bytes
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.PipelineCacheCreateFlags.PipelineCacheCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createPipelineCache'
data PipelineCacheCreateInfo = PipelineCacheCreateInfo
  { -- | @flags@ is reserved for future use.
    flags :: PipelineCacheCreateFlags
  , -- | @initialDataSize@ is the number of bytes in @pInitialData@. If
    -- @initialDataSize@ is zero, the pipeline cache will initially be empty.
    initialDataSize :: Word64
  , -- | @pInitialData@ is a pointer to previously retrieved pipeline cache data.
    -- If the pipeline cache data is incompatible (as defined below) with the
    -- device, the pipeline cache will be initially empty. If @initialDataSize@
    -- is zero, @pInitialData@ is ignored.
    initialData :: Ptr ()
  }
  deriving (Typeable)
deriving instance Show PipelineCacheCreateInfo

instance ToCStruct PipelineCacheCreateInfo where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCacheCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineCacheCreateFlags)) (flags)
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (initialDataSize))
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (initialData)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct PipelineCacheCreateInfo where
  peekCStruct p = do
    flags <- peek @PipelineCacheCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCacheCreateFlags))
    initialDataSize <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    pInitialData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ PipelineCacheCreateInfo
             flags ((\(CSize a) -> a) initialDataSize) pInitialData

instance Storable PipelineCacheCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCacheCreateInfo where
  zero = PipelineCacheCreateInfo
           zero
           zero
           zero

