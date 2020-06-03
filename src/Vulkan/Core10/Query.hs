{-# language CPP #-}
module Vulkan.Core10.Query  ( createQueryPool
                            , withQueryPool
                            , destroyQueryPool
                            , getQueryPoolResults
                            , QueryPoolCreateInfo(..)
                            , QueryPool(..)
                            , QueryPoolCreateFlags(..)
                            , QueryType(..)
                            , QueryResultFlagBits(..)
                            , QueryResultFlags
                            , QueryPipelineStatisticFlagBits(..)
                            , QueryPipelineStatisticFlags
                            ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateQueryPool))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyQueryPool))
import Vulkan.Dynamic (DeviceCmds(pVkGetQueryPoolResults))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits (QueryPipelineStatisticFlags)
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryPoolCreateFlags (QueryPoolCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (QueryPoolPerformanceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (QueryPoolPerformanceQueryCreateInfoINTEL)
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlagBits(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Vulkan.Core10.Enums.QueryType (QueryType)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits (QueryPipelineStatisticFlagBits(..))
import Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits (QueryPipelineStatisticFlags)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryPoolCreateFlags (QueryPoolCreateFlags(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlagBits(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Vulkan.Core10.Enums.QueryType (QueryType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateQueryPool
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct QueryPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr QueryPool -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct QueryPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr QueryPool -> IO Result

-- | vkCreateQueryPool - Create a new query pool object
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'QueryPoolCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pQueryPool@ /must/ be a valid pointer to a
--     'Vulkan.Core10.Handles.QueryPool' handle
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
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.QueryPool',
-- 'QueryPoolCreateInfo'
createQueryPool :: forall a io
                 . (Extendss QueryPoolCreateInfo a, PokeChain a, MonadIO io)
                => -- | @device@ is the logical device that creates the query pool.
                   Device
                -> -- | @pCreateInfo@ is a pointer to a 'QueryPoolCreateInfo' structure
                   -- containing the number and type of queries to be managed by the pool.
                   (QueryPoolCreateInfo a)
                -> -- | @pAllocator@ controls host memory allocation as described in the
                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                   -- chapter.
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io (QueryPool)
createQueryPool device createInfo allocator = liftIO . evalContT $ do
  let vkCreateQueryPoolPtr = pVkCreateQueryPool (deviceCmds (device :: Device))
  lift $ unless (vkCreateQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateQueryPool is null" Nothing Nothing
  let vkCreateQueryPool' = mkVkCreateQueryPool vkCreateQueryPoolPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPQueryPool <- ContT $ bracket (callocBytes @QueryPool 8) free
  r <- lift $ vkCreateQueryPool' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPQueryPool)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pQueryPool <- lift $ peek @QueryPool pPQueryPool
  pure $ (pQueryPool)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createQueryPool' and 'destroyQueryPool'
--
-- To ensure that 'destroyQueryPool' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withQueryPool :: forall a io r . (Extendss QueryPoolCreateInfo a, PokeChain a, MonadIO io) => Device -> QueryPoolCreateInfo a -> Maybe AllocationCallbacks -> (io (QueryPool) -> ((QueryPool) -> io ()) -> r) -> r
withQueryPool device pCreateInfo pAllocator b =
  b (createQueryPool device pCreateInfo pAllocator)
    (\(o0) -> destroyQueryPool device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyQueryPool
  :: FunPtr (Ptr Device_T -> QueryPool -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> QueryPool -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyQueryPool - Destroy a query pool object
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @queryPool@ /must/ have
--     completed execution
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @queryPool@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @queryPool@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @queryPool@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @queryPool@ /must/ be a valid 'Vulkan.Core10.Handles.QueryPool'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   If @queryPool@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @queryPool@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.QueryPool'
destroyQueryPool :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that destroys the query pool.
                    Device
                 -> -- | @queryPool@ is the query pool to destroy.
                    QueryPool
                 -> -- | @pAllocator@ controls host memory allocation as described in the
                    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                    -- chapter.
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io ()
destroyQueryPool device queryPool allocator = liftIO . evalContT $ do
  let vkDestroyQueryPoolPtr = pVkDestroyQueryPool (deviceCmds (device :: Device))
  lift $ unless (vkDestroyQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyQueryPool is null" Nothing Nothing
  let vkDestroyQueryPool' = mkVkDestroyQueryPool vkDestroyQueryPoolPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyQueryPool' (deviceHandle (device)) (queryPool) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetQueryPoolResults
  :: FunPtr (Ptr Device_T -> QueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> DeviceSize -> QueryResultFlags -> IO Result) -> Ptr Device_T -> QueryPool -> Word32 -> Word32 -> CSize -> Ptr () -> DeviceSize -> QueryResultFlags -> IO Result

-- | vkGetQueryPoolResults - Copy results of queries in a query pool to a
-- host memory region
--
-- = Description
--
-- The range of queries read is defined by [@firstQuery@, @firstQuery@ +
-- @queryCount@ - 1]. For pipeline statistics queries, each query index in
-- the pool contains one integer value for each bit that is enabled in
-- 'QueryPoolCreateInfo'::@pipelineStatistics@ when the pool is created.
--
-- If no bits are set in @flags@, and all requested queries are in the
-- available state, results are written as an array of 32-bit unsigned
-- integer values. The behavior when not all queries are available, is
-- described
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-wait-bit-not-set below>.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is not
-- set and the result overflows a 32-bit value, the value /may/ either wrap
-- or saturate. Similarly, if
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is set and
-- the result overflows a 64-bit value, the value /may/ either wrap or
-- saturate.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' is
-- set, Vulkan will wait for each query to be in the available state before
-- retrieving the numerical results for that query. In this case,
-- 'getQueryPoolResults' is guaranteed to succeed and return
-- 'Vulkan.Core10.Enums.Result.SUCCESS' if the queries become available in
-- a finite time (i.e. if they have been issued and not reset). If queries
-- will never finish (e.g. due to being reset but not issued), then
-- 'getQueryPoolResults' /may/ not return in finite time.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' and
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT' are
-- both not set then no result values are written to @pData@ for queries
-- that are in the unavailable state at the time of the call, and
-- 'getQueryPoolResults' returns 'Vulkan.Core10.Enums.Result.NOT_READY'.
-- However, availability state is still written to @pData@ for those
-- queries if
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set.
--
-- Note
--
-- Applications /must/ take care to ensure that use of the
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' bit has
-- the desired effect.
--
-- For example, if a query has been used previously and a command buffer
-- records the commands
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery', and
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery' for that query, then
-- the query will remain in the available state until
-- 'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool' is
-- called or the 'Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool'
-- command executes on a queue. Applications /can/ use fences or events to
-- ensure that a query has already been reset before checking for its
-- results or availability status. Otherwise, a stale value could be
-- returned from a previous use of the query.
--
-- The above also applies when
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' is used
-- in combination with
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'.
-- In this case, the returned availability status /may/ reflect the result
-- of a previous use of the query unless
-- 'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool' is
-- called or the 'Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool'
-- command has been executed since the last use of the query.
--
-- Note
--
-- Applications /can/ double-buffer query pool usage, with a pool per
-- frame, and reset queries at the end of the frame in which they are read.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT' is
-- set, 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' is
-- not set, and the query’s status is unavailable, an intermediate result
-- value between zero and the final result value is written to @pData@ for
-- that query.
--
-- If
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set, the final integer value written for each query is non-zero if
-- the query’s status was available or zero if the status was unavailable.
-- When
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is used, implementations /must/ guarantee that if they return a non-zero
-- availability value then the numerical results /must/ be valid, assuming
-- the results are not reset by a subsequent command.
--
-- Note
--
-- Satisfying this guarantee /may/ require careful ordering by the
-- application, e.g. to read the availability status before reading the
-- results.
--
-- == Valid Usage
--
-- -   @firstQuery@ /must/ be less than the number of queries in
--     @queryPool@
--
-- -   If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is
--     not set in @flags@, then @pData@ and @stride@ /must/ be multiples of
--     @4@
--
-- -   If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is
--     not set in @flags@ and the @queryType@ used to create @queryPool@
--     was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     then @pData@ and @stride@ /must/ be multiples of @4@
--
-- -   If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is
--     set in @flags@ then @pData@ and @stride@ /must/ be multiples of @8@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     then @pData@ and @stride@ /must/ be multiples of the size of
--     'Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterResultKHR'
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in @queryPool@
--
-- -   @dataSize@ /must/ be large enough to contain the result of each
--     query, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-memorylayout here>
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP', @flags@ /must/
--     not contain
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT',
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--     or 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the @queryPool@ /must/ have been recorded once for each pass as
--     retrieved via a call to
--     'Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @queryPool@ /must/ be a valid 'Vulkan.Core10.Handles.QueryPool'
--     handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlagBits' values
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   @queryPool@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.NOT_READY'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Handles.QueryPool',
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlags'
getQueryPoolResults :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the logical device that owns the query pool.
                       Device
                    -> -- | @queryPool@ is the query pool managing the queries containing the
                       -- desired results.
                       QueryPool
                    -> -- | @firstQuery@ is the initial query index.
                       ("firstQuery" ::: Word32)
                    -> -- | @queryCount@ is the number of queries to read.
                       ("queryCount" ::: Word32)
                    -> -- | @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
                       ("dataSize" ::: Word64)
                    -> -- | @pData@ is a pointer to a user-allocated buffer where the results will
                       -- be written
                       ("data" ::: Ptr ())
                    -> -- | @stride@ is the stride in bytes between results for individual queries
                       -- within @pData@.
                       ("stride" ::: DeviceSize)
                    -> -- | @flags@ is a bitmask of
                       -- 'Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlagBits' specifying
                       -- how and when results are returned.
                       QueryResultFlags
                    -> io (Result)
getQueryPoolResults device queryPool firstQuery queryCount dataSize data' stride flags = liftIO $ do
  let vkGetQueryPoolResultsPtr = pVkGetQueryPoolResults (deviceCmds (device :: Device))
  unless (vkGetQueryPoolResultsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetQueryPoolResults is null" Nothing Nothing
  let vkGetQueryPoolResults' = mkVkGetQueryPoolResults vkGetQueryPoolResultsPtr
  r <- vkGetQueryPoolResults' (deviceHandle (device)) (queryPool) (firstQuery) (queryCount) (CSize (dataSize)) (data') (stride) (flags)
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


-- | VkQueryPoolCreateInfo - Structure specifying parameters of a newly
-- created query pool
--
-- = Description
--
-- @pipelineStatistics@ is ignored if @queryType@ is not
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineStatisticsQuery pipeline statistics queries>
--     feature is not enabled, @queryType@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'
--
-- -   If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS',
--     @pipelineStatistics@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlagBits'
--     values
--
-- -   If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the @pNext@ chain /must/ include a structure of type
--     'Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR'
--
-- -   @queryCount@ /must/ be greater than 0
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR'
--     or
--     'Vulkan.Extensions.VK_INTEL_performance_query.QueryPoolPerformanceQueryCreateInfoINTEL'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @queryType@ /must/ be a valid
--     'Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlags',
-- 'Vulkan.Core10.Enums.QueryPoolCreateFlags.QueryPoolCreateFlags',
-- 'Vulkan.Core10.Enums.QueryType.QueryType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createQueryPool'
data QueryPoolCreateInfo (es :: [Type]) = QueryPoolCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: QueryPoolCreateFlags
  , -- | @queryType@ is a 'Vulkan.Core10.Enums.QueryType.QueryType' value
    -- specifying the type of queries managed by the pool.
    queryType :: QueryType
  , -- | @queryCount@ is the number of queries managed by the pool.
    queryCount :: Word32
  , -- | @pipelineStatistics@ is a bitmask of
    -- 'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlagBits'
    -- specifying which counters will be returned in queries on the new pool,
    -- as described below in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-pipestats>.
    pipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueryPoolCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (QueryPoolCreateInfo es)

instance Extensible QueryPoolCreateInfo where
  extensibleType = STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  setNext x next = x{next = next}
  getNext QueryPoolCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends QueryPoolCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @QueryPoolPerformanceQueryCreateInfoINTEL = Just f
    | Just Refl <- eqT @e @QueryPoolPerformanceCreateInfoKHR = Just f
    | otherwise = Nothing

instance (Extendss QueryPoolCreateInfo es, PokeChain es) => ToCStruct (QueryPoolCreateInfo es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueryPoolCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr QueryPoolCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr QueryType)) (queryType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (queryCount)
    lift $ poke ((p `plusPtr` 28 :: Ptr QueryPipelineStatisticFlags)) (pipelineStatistics)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr QueryType)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ f

instance (Extendss QueryPoolCreateInfo es, PeekChain es) => FromCStruct (QueryPoolCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @QueryPoolCreateFlags ((p `plusPtr` 16 :: Ptr QueryPoolCreateFlags))
    queryType <- peek @QueryType ((p `plusPtr` 20 :: Ptr QueryType))
    queryCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pipelineStatistics <- peek @QueryPipelineStatisticFlags ((p `plusPtr` 28 :: Ptr QueryPipelineStatisticFlags))
    pure $ QueryPoolCreateInfo
             next flags queryType queryCount pipelineStatistics

instance es ~ '[] => Zero (QueryPoolCreateInfo es) where
  zero = QueryPoolCreateInfo
           ()
           zero
           zero
           zero
           zero

