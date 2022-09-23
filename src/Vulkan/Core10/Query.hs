{-# language CPP #-}
-- No documentation found for Chapter "Query"
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

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateQueryPool))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyQueryPool))
import Vulkan.Dynamic (DeviceCmds(pVkGetQueryPoolResults))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
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
import Vulkan.Exception (VulkanException(..))
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
-- -   #VUID-vkCreateQueryPool-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateQueryPool-pCreateInfo-parameter# @pCreateInfo@ /must/
--     be a valid pointer to a valid 'QueryPoolCreateInfo' structure
--
-- -   #VUID-vkCreateQueryPool-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateQueryPool-pQueryPool-parameter# @pQueryPool@ /must/ be
--     a valid pointer to a 'Vulkan.Core10.Handles.QueryPool' handle
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                   -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                   -- chapter.
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io (QueryPool)
createQueryPool device createInfo allocator = liftIO . evalContT $ do
  let vkCreateQueryPoolPtr = pVkCreateQueryPool (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateQueryPool is null" Nothing Nothing
  let vkCreateQueryPool' = mkVkCreateQueryPool vkCreateQueryPoolPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPQueryPool <- ContT $ bracket (callocBytes @QueryPool 8) free
  r <- lift $ traceAroundEvent "vkCreateQueryPool" (vkCreateQueryPool' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPQueryPool))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pQueryPool <- lift $ peek @QueryPool pPQueryPool
  pure $ (pQueryPool)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createQueryPool' and 'destroyQueryPool'
--
-- To ensure that 'destroyQueryPool' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withQueryPool :: forall a io r . (Extendss QueryPoolCreateInfo a, PokeChain a, MonadIO io) => Device -> QueryPoolCreateInfo a -> Maybe AllocationCallbacks -> (io QueryPool -> (QueryPool -> io ()) -> r) -> r
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
-- -   #VUID-vkDestroyQueryPool-queryPool-00793# All submitted commands
--     that refer to @queryPool@ /must/ have completed execution
--
-- -   #VUID-vkDestroyQueryPool-queryPool-00794# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @queryPool@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyQueryPool-queryPool-00795# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @queryPool@ was created, @pAllocator@ /must/ be @NULL@
--
-- Note
--
-- Applications /can/ verify that @queryPool@ /can/ be destroyed by
-- checking that 'getQueryPoolResults'() without the
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT' flag
-- returns 'Vulkan.Core10.Enums.Result.SUCCESS' for all queries that are
-- used in command buffers submitted for execution.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyQueryPool-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyQueryPool-queryPool-parameter# If @queryPool@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @queryPool@ /must/ be a
--     valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkDestroyQueryPool-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyQueryPool-queryPool-parent# If @queryPool@ is a valid
--     handle, it /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @queryPool@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.QueryPool'
destroyQueryPool :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that destroys the query pool.
                    Device
                 -> -- | @queryPool@ is the query pool to destroy.
                    QueryPool
                 -> -- | @pAllocator@ controls host memory allocation as described in the
                    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                    -- chapter.
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io ()
destroyQueryPool device queryPool allocator = liftIO . evalContT $ do
  let vkDestroyQueryPoolPtr = pVkDestroyQueryPool (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyQueryPool is null" Nothing Nothing
  let vkDestroyQueryPool' = mkVkDestroyQueryPool vkDestroyQueryPoolPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyQueryPool" (vkDestroyQueryPool' (deviceHandle (device)) (queryPool) pAllocator)
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
-- Any results written for a query are written according to
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-memorylayout a layout dependent on the query type>.
--
-- If no bits are set in @flags@, and all requested queries are in the
-- available state, results are written as an array of 32-bit unsigned
-- integer values. Behavior when not all queries are available is described
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-wait-bit-not-set below>.
--
-- If
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set, results for all queries in @queryPool@ identified by
-- @firstQuery@ and @queryCount@ are copied to @pData@, along with an extra
-- availability value written directly after the results of each query and
-- interpreted as an unsigned integer. A value of zero indicates that the
-- results are not yet available, otherwise the query is complete and
-- results are available.
--
-- If @VK_QUERY_RESULT_WITH_STATUS_BIT_KHR@ is set, results for all queries
-- in @queryPool@ identified by @firstQuery@ and @queryCount@ are copied to
-- @pData@, along with an extra status value written directly after the
-- results of each query and interpreted as a signed integer. A value of
-- zero indicates that the results are not yet available. Positive values
-- indicate that the operations within the query completed successfully,
-- and the query results are valid. Negative values indicate that the
-- operations within the query completed unsuccessfully.
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryResultStatusKHR VkQueryResultStatusKHR>
-- defines specific meaning for values returned here, though
-- implementations are free to return other values.
--
-- Results for any available query written by this command are final and
-- represent the final result of the query. If
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT' is
-- set, then for any query that is unavailable, an intermediate result
-- between zero and the final result value is written for that query.
-- Otherwise, any result written by this command is undefined.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is set,
-- results and availability or status values for all queries are written as
-- an array of 64-bit values. If the @queryPool@ was created with
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
-- results for each query are written as an array of the type indicated by
-- 'Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterKHR'::@storage@
-- for the counter being queried. Otherwise, results and availability or
-- status values are written as an array of 32-bit values. If an unsigned
-- integer query’s value overflows the result type, the value /may/ either
-- wrap or saturate. If a signed integer query’s value overflows the result
-- type, the value is undefined. If a floating point query’s value is not
-- representable as the result type, the value is undefined.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' is
-- set, this command defines an execution dependency with any earlier
-- commands that writes one of the identified queries. The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all instances of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWriteTimestamp2',
-- and 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp' that
-- reference any query in @queryPool@ indicated by @firstQuery@ and
-- @queryCount@. The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes the host operations of this command.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' is
-- not set, 'getQueryPoolResults' /may/ return
-- 'Vulkan.Core10.Enums.Result.NOT_READY' if there are queries in the
-- unavailable state.
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
-- A similar situation can arise with the
-- @VK_QUERY_RESULT_WITH_STATUS_BIT_KHR@ flag.
--
-- Note
--
-- Applications /can/ double-buffer query pool usage, with a pool per
-- frame, and reset queries at the end of the frame in which they are read.
--
-- == Valid Usage
--
-- -   #VUID-vkGetQueryPoolResults-firstQuery-00813# @firstQuery@ /must/ be
--     less than the number of queries in @queryPool@
--
-- -   #VUID-vkGetQueryPoolResults-flags-02828# If
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is not
--     set in @flags@ and the @queryType@ used to create @queryPool@ was
--     not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     then @pData@ and @stride@ /must/ be multiples of @4@
--
-- -   #VUID-vkGetQueryPoolResults-flags-00815# If
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is set
--     in @flags@ then @pData@ and @stride@ /must/ be multiples of @8@
--
-- -   #VUID-vkGetQueryPoolResults-queryType-03229# If the @queryType@ used
--     to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     then @pData@ and @stride@ /must/ be multiples of the size of
--     'Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterResultKHR'
--
-- -   #VUID-vkGetQueryPoolResults-queryType-04519# If the @queryType@ used
--     to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     then @stride@ /must/ be large enough to contain
--     'Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR'::@counterIndexCount@
--     used to create @queryPool@ times the size of
--     'Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterResultKHR'
--
-- -   #VUID-vkGetQueryPoolResults-firstQuery-00816# The sum of
--     @firstQuery@ and @queryCount@ /must/ be less than or equal to the
--     number of queries in @queryPool@
--
-- -   #VUID-vkGetQueryPoolResults-dataSize-00817# @dataSize@ /must/ be
--     large enough to contain the result of each query, as described
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-memorylayout here>
--
-- -   #VUID-vkGetQueryPoolResults-queryType-00818# If the @queryType@ used
--     to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP', @flags@ /must/
--     not contain
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--
-- -   #VUID-vkGetQueryPoolResults-queryType-03230# If the @queryType@ used
--     to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT',
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--     or 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--
-- -   #VUID-vkGetQueryPoolResults-queryType-03231# If the @queryType@ used
--     to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the @queryPool@ /must/ have been recorded once for each pass as
--     retrieved via a call to
--     'Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
--
-- -   #VUID-vkGetQueryPoolResults-queryType-04810# If the @queryType@ used
--     to create @queryPool@ was @VK_QUERY_TYPE_RESULT_STATUS_ONLY_KHR@,
--     @flags@ /must/ include @VK_QUERY_RESULT_WITH_STATUS_BIT_KHR@
--
-- -   #VUID-vkGetQueryPoolResults-flags-04811# If @flags@ includes
--     @VK_QUERY_RESULT_WITH_STATUS_BIT_KHR@, it /must/ not include
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
--
-- -   #VUID-vkGetQueryPoolResults-queryType-06900# If the @queryType@ used
--     to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     @flags@ /must/ not contain @VK_QUERY_RESULT_WITH_STATUS_BIT_KHR@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetQueryPoolResults-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetQueryPoolResults-queryPool-parameter# @queryPool@ /must/
--     be a valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkGetQueryPoolResults-pData-parameter# @pData@ /must/ be a
--     valid pointer to an array of @dataSize@ bytes
--
-- -   #VUID-vkGetQueryPoolResults-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlagBits' values
--
-- -   #VUID-vkGetQueryPoolResults-dataSize-arraylength# @dataSize@ /must/
--     be greater than @0@
--
-- -   #VUID-vkGetQueryPoolResults-queryPool-parent# @queryPool@ /must/
--     have been created, allocated, or retrieved from @device@
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkGetQueryPoolResultsPtr = pVkGetQueryPoolResults (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkGetQueryPoolResultsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetQueryPoolResults is null" Nothing Nothing
  let vkGetQueryPoolResults' = mkVkGetQueryPoolResults vkGetQueryPoolResultsPtr
  r <- traceAroundEvent "vkGetQueryPoolResults" (vkGetQueryPoolResults' (deviceHandle (device)) (queryPool) (firstQuery) (queryCount) (CSize (dataSize)) (data') (stride) (flags))
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
-- -   #VUID-VkQueryPoolCreateInfo-queryType-00791# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelineStatisticsQuery pipelineStatisticsQuery>
--     feature is not enabled, @queryType@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'
--
-- -   #VUID-VkQueryPoolCreateInfo-meshShaderQueries-07068# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-meshShaderQueries meshShaderQueries>
--     feature is not enabled, @queryType@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT'
--
-- -   #VUID-VkQueryPoolCreateInfo-meshShaderQueries-07069# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-meshShaderQueries meshShaderQueries>
--     feature is not enabled, and @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS',
--     @pipelineStatistics@ /must/ not contain
--     'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QUERY_PIPELINE_STATISTIC_TASK_SHADER_INVOCATIONS_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QUERY_PIPELINE_STATISTIC_MESH_SHADER_INVOCATIONS_BIT_EXT'
--
-- -   #VUID-VkQueryPoolCreateInfo-queryType-00792# If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS',
--     @pipelineStatistics@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlagBits'
--     values
--
-- -   #VUID-VkQueryPoolCreateInfo-queryType-03222# If @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the @pNext@ chain /must/ include a
--     'Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR'
--     structure
--
-- -   #VUID-VkQueryPoolCreateInfo-queryCount-02763# @queryCount@ /must/ be
--     greater than 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkQueryPoolCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO'
--
-- -   #VUID-VkQueryPoolCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR',
--     'Vulkan.Extensions.VK_INTEL_performance_query.QueryPoolPerformanceQueryCreateInfoINTEL',
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeH264ProfileInfoEXT VkVideoDecodeH264ProfileInfoEXT>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeH265ProfileInfoEXT VkVideoDecodeH265ProfileInfoEXT>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeUsageInfoKHR VkVideoDecodeUsageInfoKHR>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH264ProfileInfoEXT VkVideoEncodeH264ProfileInfoEXT>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeH265ProfileInfoEXT VkVideoEncodeH265ProfileInfoEXT>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoEncodeUsageInfoKHR VkVideoEncodeUsageInfoKHR>,
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>
--
-- -   #VUID-VkQueryPoolCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkQueryPoolCreateInfo-flags-zerobitmask# @flags@ /must/ be @0@
--
-- -   #VUID-VkQueryPoolCreateInfo-queryType-parameter# @queryType@ /must/
--     be a valid 'Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-pipestats>.
    pipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueryPoolCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (QueryPoolCreateInfo es)

instance Extensible QueryPoolCreateInfo where
  extensibleTypeName = "QueryPoolCreateInfo"
  setNext QueryPoolCreateInfo{..} next' = QueryPoolCreateInfo{next = next', ..}
  getNext QueryPoolCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends QueryPoolCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @QueryPoolPerformanceQueryCreateInfoINTEL = Just f
    | Just Refl <- eqT @e @QueryPoolPerformanceCreateInfoKHR = Just f
    | otherwise = Nothing

instance (Extendss QueryPoolCreateInfo es, PokeChain es) => ToCStruct (QueryPoolCreateInfo es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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

