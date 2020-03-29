{-# language CPP #-}
module Graphics.Vulkan.Core10.Query  ( createQueryPool
                                     , withQueryPool
                                     , destroyQueryPool
                                     , getQueryPoolResults
                                     , QueryPoolCreateInfo(..)
                                     ) where

import Control.Exception.Base (bracket)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateQueryPool))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyQueryPool))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetQueryPoolResults))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits (QueryPipelineStatisticFlags)
import Graphics.Vulkan.Core10.Handles (QueryPool)
import Graphics.Vulkan.Core10.Handles (QueryPool(..))
import Graphics.Vulkan.Core10.Enums.QueryPoolCreateFlags (QueryPoolCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_INTEL_performance_query (QueryPoolCreateInfoINTEL)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_KHR_performance_query (QueryPoolPerformanceCreateInfoKHR)
import Graphics.Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Graphics.Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Graphics.Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlagBits(..))
import Graphics.Vulkan.Core10.Enums.QueryType (QueryType)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateQueryPool
  :: FunPtr (Ptr Device_T -> Ptr (QueryPoolCreateInfo a) -> Ptr AllocationCallbacks -> Ptr QueryPool -> IO Result) -> Ptr Device_T -> Ptr (QueryPoolCreateInfo a) -> Ptr AllocationCallbacks -> Ptr QueryPool -> IO Result

-- | vkCreateQueryPool - Create a new query pool object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     creates the query pool.
--
-- -   @pCreateInfo@ is a pointer to a 'QueryPoolCreateInfo' structure
--     containing the number and type of queries to be managed by the pool.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pQueryPool@ is a pointer to a
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle in which the
--     resulting query pool object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'QueryPoolCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pQueryPool@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
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
-- 'Graphics.Vulkan.Core10.Handles.QueryPool', 'QueryPoolCreateInfo'
createQueryPool :: PokeChain a => Device -> QueryPoolCreateInfo a -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (QueryPool)
createQueryPool device createInfo allocator = evalContT $ do
  let vkCreateQueryPool' = mkVkCreateQueryPool (pVkCreateQueryPool (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPQueryPool <- ContT $ bracket (callocBytes @QueryPool 8) free
  r <- lift $ vkCreateQueryPool' (deviceHandle (device)) pCreateInfo pAllocator (pPQueryPool)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pQueryPool <- lift $ peek @QueryPool pPQueryPool
  pure $ (pQueryPool)

-- | A safe wrapper for 'createQueryPool' and 'destroyQueryPool' using
-- 'bracket'
--
-- The allocated value must not be returned from the provided computation
withQueryPool :: PokeChain a => Device -> QueryPoolCreateInfo a -> Maybe AllocationCallbacks -> (QueryPool -> IO r) -> IO r
withQueryPool device queryPoolCreateInfo allocationCallbacks =
  bracket
    (createQueryPool device queryPoolCreateInfo allocationCallbacks)
    (\o -> destroyQueryPool device o allocationCallbacks)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyQueryPool
  :: FunPtr (Ptr Device_T -> QueryPool -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> QueryPool -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyQueryPool - Destroy a query pool object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     destroys the query pool.
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' is the query pool to
--     destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to
--     'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have completed
--     execution
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' is a valid handle, it
--     /must/ have been created, allocated, or retrieved from
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ be
--     externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.QueryPool'
destroyQueryPool :: Device -> QueryPool -> ("allocator" ::: Maybe AllocationCallbacks) -> IO ()
destroyQueryPool device queryPool allocator = evalContT $ do
  let vkDestroyQueryPool' = mkVkDestroyQueryPool (pVkDestroyQueryPool (deviceCmds (device :: Device)))
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
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the query pool.
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' is the query pool
--     managing the queries containing the desired results.
--
-- -   @firstQuery@ is the initial query index.
--
-- -   @queryCount@ is the number of queries to read.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written
--
-- -   @stride@ is the stride in bytes between results for individual
--     queries within @pData@.
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlagBits'
--     specifying how and when results are returned.
--
-- = Description
--
-- The range of queries read is defined by [@firstQuery@, @firstQuery@ +
-- @queryCount@ - 1]. For pipeline statistics queries, each query index in
-- the pool contains one integer value for each bit that is enabled in
-- 'QueryPoolCreateInfo'::@pipelineStatistics@ when the pool is created.
--
-- If no bits are set in 'Graphics.Vulkan.Core10.BaseType.Flags', and all
-- requested queries are in the available state, results are written as an
-- array of 32-bit unsigned integer values. The behavior when not all
-- queries are available, is described
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-wait-bit-not-set below>.
--
-- If
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
-- is not set and the result overflows a 32-bit value, the value /may/
-- either wrap or saturate. Similarly, if
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
-- is set and the result overflows a 64-bit value, the value /may/ either
-- wrap or saturate.
--
-- If
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT'
-- is set, Vulkan will wait for each query to be in the available state
-- before retrieving the numerical results for that query. In this case,
-- 'getQueryPoolResults' is guaranteed to succeed and return
-- 'Graphics.Vulkan.Core10.Enums.Result.SUCCESS' if the queries become
-- available in a finite time (i.e. if they have been issued and not
-- reset). If queries will never finish (e.g. due to being reset but not
-- issued), then 'getQueryPoolResults' /may/ not return in finite time.
--
-- If
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT'
-- and
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
-- are both not set then no result values are written to @pData@ for
-- queries that are in the unavailable state at the time of the call, and
-- 'getQueryPoolResults' returns
-- 'Graphics.Vulkan.Core10.Enums.Result.NOT_READY'. However, availability
-- state is still written to @pData@ for those queries if
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set.
--
-- Note
--
-- Applications /must/ take care to ensure that use of the
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT'
-- bit has the desired effect.
--
-- For example, if a query has been used previously and a command buffer
-- records the commands
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery', and
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdEndQuery' for that
-- query, then the query will remain in the available state until
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool'
-- is called or the
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool' command
-- executes on a queue. Applications /can/ use fences or events to ensure
-- that a query has already been reset before checking for its results or
-- availability status. Otherwise, a stale value could be returned from a
-- previous use of the query.
--
-- The above also applies when
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT'
-- is used in combination with
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'.
-- In this case, the returned availability status /may/ reflect the result
-- of a previous use of the query unless
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool'
-- is called or the
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool' command
-- has been executed since the last use of the query.
--
-- Note
--
-- Applications /can/ double-buffer query pool usage, with a pool per
-- frame, and reset queries at the end of the frame in which they are read.
--
-- If
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
-- is set,
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT'
-- is not set, and the query’s status is unavailable, an intermediate
-- result value between zero and the final result value is written to
-- @pData@ for that query.
--
-- If
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set, the final integer value written for each query is non-zero if
-- the query’s status was available or zero if the status was unavailable.
-- When
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
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
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--     is not set in 'Graphics.Vulkan.Core10.BaseType.Flags', then @pData@
--     and @stride@ /must/ be multiples of @4@
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--     is not set in 'Graphics.Vulkan.Core10.BaseType.Flags' and the
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to create
--     'Graphics.Vulkan.Core10.Handles.QueryPool' was not
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     then @pData@ and @stride@ /must/ be multiples of @4@
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--     is set in 'Graphics.Vulkan.Core10.BaseType.Flags' then @pData@ and
--     @stride@ /must/ be multiples of @8@
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     then @pData@ and @stride@ /must/ be multiples of the size of
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterResultKHR'
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   @dataSize@ /must/ be large enough to contain the result of each
--     query, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-memorylayout here>
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP',
--     'Graphics.Vulkan.Core10.BaseType.Flags' /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     'Graphics.Vulkan.Core10.BaseType.Flags' /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT',
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the 'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been
--     recorded once for each pass as retrieved via a call to
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlagBits'
--     values
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been created,
--     allocated, or retrieved from 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.NOT_READY'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Handles.QueryPool',
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlags'
getQueryPoolResults :: Device -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: Word64) -> ("data" ::: Ptr ()) -> ("stride" ::: DeviceSize) -> QueryResultFlags -> IO (Result)
getQueryPoolResults device queryPool firstQuery queryCount dataSize data' stride flags = do
  let vkGetQueryPoolResults' = mkVkGetQueryPoolResults (pVkGetQueryPoolResults (deviceCmds (device :: Device)))
  r <- vkGetQueryPoolResults' (deviceHandle (device)) (queryPool) (firstQuery) (queryCount) (CSize (dataSize)) (data') (stride) (flags)
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


-- | VkQueryPoolCreateInfo - Structure specifying parameters of a newly
-- created query pool
--
-- = Description
--
-- @pipelineStatistics@ is ignored if
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' is not
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineStatisticsQuery pipeline statistics queries>
--     feature is not enabled,
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' /must/ not be
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'
--
-- -   If 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' is
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS',
--     @pipelineStatistics@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlagBits'
--     values
--
-- -   If 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' is
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the @pNext@ chain /must/ include a structure of type
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR'
--
-- -   @queryCount@ /must/ be greater than 0
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.QueryPoolCreateInfoINTEL'
--     or
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be @0@
--
-- -   'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlags',
-- 'Graphics.Vulkan.Core10.Enums.QueryPoolCreateFlags.QueryPoolCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createQueryPool'
data QueryPoolCreateInfo (es :: [Type]) = QueryPoolCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is reserved for future use.
    flags :: QueryPoolCreateFlags
  , -- | 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' is a
    -- 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' value specifying the
    -- type of queries managed by the pool.
    queryType :: QueryType
  , -- | @queryCount@ is the number of queries managed by the pool.
    queryCount :: Word32
  , -- | @pipelineStatistics@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlagBits'
    -- specifying which counters will be returned in queries on the new pool,
    -- as described below in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-pipestats>.
    pipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (QueryPoolCreateInfo es)

instance Extensible QueryPoolCreateInfo where
  extensibleType = STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  setNext x next = x{next = next}
  getNext QueryPoolCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends QueryPoolCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @QueryPoolCreateInfoINTEL = Just f
    | Just Refl <- eqT @e @QueryPoolPerformanceCreateInfoKHR = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (QueryPoolCreateInfo es) where
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

instance PeekChain es => FromCStruct (QueryPoolCreateInfo es) where
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

