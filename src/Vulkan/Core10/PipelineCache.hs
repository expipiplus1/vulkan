{-# language CPP #-}
module Vulkan.Core10.PipelineCache  ( createPipelineCache
                                    , withPipelineCache
                                    , destroyPipelineCache
                                    , getPipelineCacheData
                                    , mergePipelineCaches
                                    , PipelineCacheCreateInfo(..)
                                    ) where

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
import Data.ByteString (packCStringLen)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
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
import Vulkan.Dynamic (DeviceCmds(pVkCreatePipelineCache))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyPipelineCache))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineCacheData))
import Vulkan.Dynamic (DeviceCmds(pVkMergePipelineCaches))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Enums.PipelineCacheCreateFlagBits (PipelineCacheCreateFlags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineCache
  :: FunPtr (Ptr Device_T -> Ptr PipelineCacheCreateInfo -> Ptr AllocationCallbacks -> Ptr PipelineCache -> IO Result) -> Ptr Device_T -> Ptr PipelineCacheCreateInfo -> Ptr AllocationCallbacks -> Ptr PipelineCache -> IO Result

-- | vkCreatePipelineCache - Creates a new pipeline cache
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
-- 'Vulkan.Core10.Pipeline.createGraphicsPipelines'
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.createRayTracingPipelinesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV', and
-- 'Vulkan.Core10.Pipeline.createComputePipelines' commands. If the
-- pipeline cache passed into these commands is not
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', the implementation will query
-- it for possible reuse opportunities and update it with new content. The
-- use of the pipeline cache object in these commands is internally
-- synchronized, and the same pipeline cache object /can/ be used in
-- multiple threads simultaneously.
--
-- If @flags@ of @pCreateInfo@ includes
-- 'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT',
-- all commands that modify the returned pipeline cache object /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>.
--
-- Note
--
-- Implementations /should/ make every effort to limit any critical
-- sections to the actual accesses to the cache, which is expected to be
-- significantly shorter than the duration of the @vkCreate*Pipelines@
-- commands.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'PipelineCacheCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pPipelineCache@ /must/ be a valid pointer to a
--     'Vulkan.Core10.Handles.PipelineCache' handle
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
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.PipelineCache',
-- 'PipelineCacheCreateInfo'
createPipelineCache :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the logical device that creates the pipeline cache object.
                       Device
                    -> -- | @pCreateInfo@ is a pointer to a 'PipelineCacheCreateInfo' structure
                       -- containing initial parameters for the pipeline cache object.
                       PipelineCacheCreateInfo
                    -> -- | @pAllocator@ controls host memory allocation as described in the
                       -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                       -- chapter.
                       (("allocator" ::: Maybe AllocationCallbacks))
                    -> io (PipelineCache)
createPipelineCache device createInfo allocator = liftIO . evalContT $ do
  let vkCreatePipelineCachePtr = pVkCreatePipelineCache (deviceCmds (device :: Device))
  lift $ unless (vkCreatePipelineCachePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreatePipelineCache is null" Nothing Nothing
  let vkCreatePipelineCache' = mkVkCreatePipelineCache vkCreatePipelineCachePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelineCache <- ContT $ bracket (callocBytes @PipelineCache 8) free
  r <- lift $ vkCreatePipelineCache' (deviceHandle (device)) pCreateInfo pAllocator (pPPipelineCache)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelineCache <- lift $ peek @PipelineCache pPPipelineCache
  pure $ (pPipelineCache)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createPipelineCache' and 'destroyPipelineCache'
--
-- To ensure that 'destroyPipelineCache' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withPipelineCache :: forall io r . MonadIO io => Device -> PipelineCacheCreateInfo -> Maybe AllocationCallbacks -> (io (PipelineCache) -> ((PipelineCache) -> io ()) -> r) -> r
withPipelineCache device pCreateInfo pAllocator b =
  b (createPipelineCache device pCreateInfo pAllocator)
    (\(o0) -> destroyPipelineCache device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineCache
  :: FunPtr (Ptr Device_T -> PipelineCache -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> PipelineCache -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyPipelineCache - Destroy a pipeline cache object
--
-- == Valid Usage
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipelineCache@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipelineCache@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
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
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.PipelineCache'
destroyPipelineCache :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the logical device that destroys the pipeline cache object.
                        Device
                     -> -- | @pipelineCache@ is the handle of the pipeline cache to destroy.
                        PipelineCache
                     -> -- | @pAllocator@ controls host memory allocation as described in the
                        -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                        -- chapter.
                        (("allocator" ::: Maybe AllocationCallbacks))
                     -> io ()
destroyPipelineCache device pipelineCache allocator = liftIO . evalContT $ do
  let vkDestroyPipelineCachePtr = pVkDestroyPipelineCache (deviceCmds (device :: Device))
  lift $ unless (vkDestroyPipelineCachePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyPipelineCache is null" Nothing Nothing
  let vkDestroyPipelineCache' = mkVkDestroyPipelineCache vkDestroyPipelineCachePtr
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
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE'. Any data written to @pData@ is
-- valid and /can/ be provided as the @pInitialData@ member of the
-- 'PipelineCacheCreateInfo' structure passed to 'createPipelineCache'.
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
-- +--------+----------------------------------------+------------------------------------------------------------------------------------+
-- | Offset | Size                                   | Meaning                                                                            |
-- +========+========================================+====================================================================================+
-- | 0      | 4                                      | length in bytes of the entire pipeline cache header written as a stream of bytes,  |
-- |        |                                        | with the least significant byte first                                              |
-- +--------+----------------------------------------+------------------------------------------------------------------------------------+
-- | 4      | 4                                      | a 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'      |
-- |        |                                        | value written as a stream of bytes, with the least significant byte first          |
-- +--------+----------------------------------------+------------------------------------------------------------------------------------+
-- | 8      | 4                                      | a vendor ID equal to                                                               |
-- |        |                                        | 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@vendorID@ written  |
-- |        |                                        | as a stream of bytes, with the least significant byte first                        |
-- +--------+----------------------------------------+------------------------------------------------------------------------------------+
-- | 12     | 4                                      | a device ID equal to                                                               |
-- |        |                                        | 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@deviceID@ written  |
-- |        |                                        | as a stream of bytes, with the least significant byte first                        |
-- +--------+----------------------------------------+------------------------------------------------------------------------------------+
-- | 16     | 'Vulkan.Core10.APIConstants.UUID_SIZE' | a pipeline cache ID equal to                                                       |
-- |        |                                        | 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@pipelineCacheUUID@ |
-- +--------+----------------------------------------+------------------------------------------------------------------------------------+
--
-- Layout for pipeline cache header version
-- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PIPELINE_CACHE_HEADER_VERSION_ONE'
--
-- The first four bytes encode the length of the entire pipeline cache
-- header, in bytes. This value includes all fields in the header including
-- the pipeline cache version field and the size of the length field.
--
-- The next four bytes encode the pipeline cache version, as described for
-- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'.
-- A consumer of the pipeline cache /should/ use the cache version to
-- interpret the remainder of the cache header.
--
-- If @pDataSize@ is less than what is necessary to store this header,
-- nothing will be written to @pData@ and zero will be written to
-- @pDataSize@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
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
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.PipelineCache'
getPipelineCacheData :: forall io
                      . (MonadIO io)
                     => -- | @device@ is the logical device that owns the pipeline cache.
                        Device
                     -> -- | @pipelineCache@ is the pipeline cache to retrieve data from.
                        PipelineCache
                     -> io (Result, ("data" ::: ByteString))
getPipelineCacheData device pipelineCache = liftIO . evalContT $ do
  let vkGetPipelineCacheDataPtr = pVkGetPipelineCacheData (deviceCmds (device :: Device))
  lift $ unless (vkGetPipelineCacheDataPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineCacheData is null" Nothing Nothing
  let vkGetPipelineCacheData' = mkVkGetPipelineCacheData vkGetPipelineCacheDataPtr
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
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @dstCache@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineCache'
--     handle
--
-- -   @pSrcCaches@ /must/ be a valid pointer to an array of
--     @srcCacheCount@ valid 'Vulkan.Core10.Handles.PipelineCache' handles
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
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.PipelineCache'
mergePipelineCaches :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the logical device that owns the pipeline cache objects.
                       Device
                    -> -- | @dstCache@ is the handle of the pipeline cache to merge results into.
                       (("dstCache" ::: PipelineCache))
                    -> -- | @pSrcCaches@ is a pointer to an array of pipeline cache handles, which
                       -- will be merged into @dstCache@. The previous contents of @dstCache@ are
                       -- included after the merge.
                       (("srcCaches" ::: Vector PipelineCache))
                    -> io ()
mergePipelineCaches device dstCache srcCaches = liftIO . evalContT $ do
  let vkMergePipelineCachesPtr = pVkMergePipelineCaches (deviceCmds (device :: Device))
  lift $ unless (vkMergePipelineCachesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkMergePipelineCaches is null" Nothing Nothing
  let vkMergePipelineCaches' = mkVkMergePipelineCaches vkMergePipelineCachesPtr
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
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlagBits'
--     values
--
-- -   If @initialDataSize@ is not @0@, @pInitialData@ /must/ be a valid
--     pointer to an array of @initialDataSize@ bytes
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createPipelineCache'
data PipelineCacheCreateInfo = PipelineCacheCreateInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlagBits'
    -- specifying the behavior of the pipeline cache.
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

