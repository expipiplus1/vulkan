{-# language CPP #-}
-- | = Name
--
-- VK_KHR_performance_query - device extension
--
-- == VK_KHR_performance_query
--
-- [__Name String__]
--     @VK_KHR_performance_query@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     117
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Alon Or-bach
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_performance_query:%20&body=@alonorbach%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-10-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jesse Barker, Unity Technologies
--
--     -   Kenneth Benzie, Codeplay
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jesse Hall, Google
--
--     -   Tobias Hector, AMD
--
--     -   Neil Henning, Codeplay
--
--     -   Baldur Karlsson
--
--     -   Lionel Landwerlin, Intel
--
--     -   Peter Lohrmann, AMD
--
--     -   Alon Or-bach, Samsung
--
--     -   Daniel Rakos, AMD
--
--     -   Niklas Smedberg, Unity Technologies
--
--     -   Igor Ostrowski, Intel
--
-- == Description
--
-- The @VK_KHR_performance_query@ extension adds a mechanism to allow
-- querying of performance counters for use in applications and by
-- profiling tools.
--
-- Each queue family /may/ expose counters that /can/ be enabled on a queue
-- of that family. We extend 'Vulkan.Core10.Enums.QueryType.QueryType' to
-- add a new query type for performance queries, and chain a structure on
-- 'Vulkan.Core10.Query.QueryPoolCreateInfo' to specify the performance
-- queries to enable.
--
-- == New Commands
--
-- -   'acquireProfilingLockKHR'
--
-- -   'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR'
--
-- -   'getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
--
-- -   'releaseProfilingLockKHR'
--
-- == New Structures
--
-- -   'AcquireProfilingLockInfoKHR'
--
-- -   'PerformanceCounterDescriptionKHR'
--
-- -   'PerformanceCounterKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePerformanceQueryFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePerformanceQueryPropertiesKHR'
--
-- -   Extending 'Vulkan.Core10.Query.QueryPoolCreateInfo':
--
--     -   'QueryPoolPerformanceCreateInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo':
--
--     -   'PerformanceQuerySubmitInfoKHR'
--
-- == New Unions
--
-- -   'PerformanceCounterResultKHR'
--
-- == New Enums
--
-- -   'AcquireProfilingLockFlagBitsKHR'
--
-- -   'PerformanceCounterDescriptionFlagBitsKHR'
--
-- -   'PerformanceCounterScopeKHR'
--
-- -   'PerformanceCounterStorageKHR'
--
-- -   'PerformanceCounterUnitKHR'
--
-- == New Bitmasks
--
-- -   'AcquireProfilingLockFlagsKHR'
--
-- -   'PerformanceCounterDescriptionFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PERFORMANCE_QUERY_EXTENSION_NAME'
--
-- -   'KHR_PERFORMANCE_QUERY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Should this extension include a mechanism to begin a query in command
-- buffer /A/ and end the query in command buffer /B/?
--
-- __RESOLVED__ No - queries are tied to command buffer creation and thus
-- have to be encapsulated within a single command buffer.
--
-- 2) Should this extension include a mechanism to begin and end queries
-- globally on the queue, not using the existing command buffer commands?
--
-- __RESOLVED__ No - for the same reasoning as the resolution of 1).
--
-- 3) Should this extension expose counters that require multiple passes?
--
-- __RESOLVED__ Yes - users should re-submit a command buffer with the same
-- commands in it multiple times, specifying the pass to count as the query
-- parameter in VkPerformanceQuerySubmitInfoKHR.
--
-- 4) How to handle counters across parallel workloads?
--
-- __RESOLVED__ In the spirit of Vulkan, a counter description flag
-- 'PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR' denotes
-- that the accuracy of a counter result is affected by parallel workloads.
--
-- 5) How to handle secondary command buffers?
--
-- __RESOLVED__ Secondary command buffers inherit any counter pass index
-- specified in the parent primary command buffer. Note: this is no longer
-- an issue after change from issue 10 resolution
--
-- 6) What commands does the profiling lock have to be held for?
--
-- __RESOLVED__ For any command buffer that is being queried with a
-- performance query pool, the profiling lock /must/ be held while that
-- command buffer is in the /recording/, /executable/, or /pending state/.
--
-- 7) Should we support
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'?
--
-- __RESOLVED__ Yes.
--
-- 8) Should we allow performance queries to interact with multiview?
--
-- __RESOLVED__ Yes, but the performance queries must be performed once for
-- each pass per view.
--
-- 9) Should a @queryCount > 1@ be usable for performance queries?
--
-- __RESOLVED__ Yes. Some vendors will have costly performance counter
-- query pool creation, and would rather if a certain set of counters were
-- to be used multiple times that a @queryCount > 1@ can be used to
-- amortize the instantiation cost.
--
-- 10) Should we introduce an indirect mechanism to set the counter pass
-- index?
--
-- __RESOLVED__ Specify the counter pass index at submit time instead to
-- avoid requiring re-recording of command buffers when multiple counter
-- passes needed.
--
-- == Examples
--
-- The following example shows how to find what performance counters a
-- queue family supports, setup a query pool to record these performance
-- counters, how to add the query pool to the command buffer to record
-- information, and how to get the results from the query pool.
--
-- > // A previously created physical device
-- > VkPhysicalDevice physicalDevice;
-- >
-- > // One of the queue families our device supports
-- > uint32_t queueFamilyIndex;
-- >
-- > uint32_t counterCount;
-- >
-- > // Get the count of counters supported
-- > vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR(
-- >   physicalDevice,
-- >   queueFamilyIndex,
-- >   &counterCount,
-- >   NULL,
-- >   NULL);
-- >
-- > VkPerformanceCounterKHR* counters =
-- >   malloc(sizeof(VkPerformanceCounterKHR) * counterCount);
-- > VkPerformanceCounterDescriptionKHR* counterDescriptions =
-- >   malloc(sizeof(VkPerformanceCounterDescriptionKHR) * counterCount);
-- >
-- > // Get the counters supported
-- > vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR(
-- >   physicalDevice,
-- >   queueFamilyIndex,
-- >   &counterCount,
-- >   counters,
-- >   counterDescriptions);
-- >
-- > // Try to enable the first 8 counters
-- > uint32_t enabledCounters[8];
-- >
-- > const uint32_t enabledCounterCount = min(counterCount, 8));
-- >
-- > for (uint32_t i = 0; i < enabledCounterCount; i++) {
-- >   enabledCounters[i] = i;
-- > }
-- >
-- > // A previously created device that had the performanceCounterQueryPools feature
-- > // set to VK_TRUE
-- > VkDevice device;
-- >
-- > VkQueryPoolPerformanceCreateInfoKHR performanceQueryCreateInfo = {
-- >   VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR,
-- >   NULL,
-- >
-- >   // Specify the queue family that this performance query is performed on
-- >   queueFamilyIndex,
-- >
-- >   // The number of counters to enable
-- >   enabledCounterCount,
-- >
-- >   // The array of indices of counters to enable
-- >   enabledCounters
-- > };
-- >
-- >
-- > // Get the number of passes our counters will require.
-- > uint32_t numPasses;
-- >
-- > vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR(
-- >   physicalDevice,
-- >   &performanceQueryCreateInfo,
-- >   &numPasses);
-- >
-- > VkQueryPoolCreateInfo queryPoolCreateInfo = {
-- >   VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO,
-- >   &performanceQueryCreateInfo,
-- >   0,
-- >
-- >   // Using our new query type here
-- >   VK_QUERY_TYPE_PERFORMANCE_QUERY_KHR,
-- >
-- >   1,
-- >
-- >   0
-- > };
-- >
-- > VkQueryPool queryPool;
-- >
-- > VkResult result = vkCreateQueryPool(
-- >   device,
-- >   &queryPoolCreateInfo,
-- >   NULL,
-- >   &queryPool);
-- >
-- > assert(VK_SUCCESS == result);
-- >
-- > // A queue from queueFamilyIndex
-- > VkQueue queue;
-- >
-- > // A command buffer we want to record counters on
-- > VkCommandBuffer commandBuffer;
-- >
-- > VkCommandBufferBeginInfo commandBufferBeginInfo = {
-- >   VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
-- >   NULL,
-- >   0,
-- >   NULL
-- > };
-- >
-- > VkAcquireProfilingLockInfoKHR lockInfo = {
-- >   VK_STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR,
-- >   NULL,
-- >   0,
-- >   UINT64_MAX // Wait forever for the lock
-- > };
-- >
-- > // Acquire the profiling lock before we record command buffers
-- > // that will use performance queries
-- >
-- > result = vkAcquireProfilingLockKHR(device, &lockInfo);
-- >
-- > assert(VK_SUCCESS == result);
-- >
-- > result = vkBeginCommandBuffer(commandBuffer, &commandBufferBeginInfo);
-- >
-- > assert(VK_SUCCESS == result);
-- >
-- > vkCmdResetQueryPool(
-- >   commandBuffer,
-- >   queryPool,
-- >   0,
-- >   1);
-- >
-- > vkCmdBeginQuery(
-- >   commandBuffer,
-- >   queryPool,
-- >   0,
-- >   0);
-- >
-- > // Perform the commands you want to get performance information on
-- > // ...
-- >
-- > // Perform a barrier to ensure all previous commands were complete before
-- > // ending the query
-- > vkCmdPipelineBarrier(commandBuffer,
-- >   VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT,
-- >   VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT,
-- >   0,
-- >   0,
-- >   NULL,
-- >   0,
-- >   NULL,
-- >   0,
-- >   NULL);
-- >
-- > vkCmdEndQuery(
-- >   commandBuffer,
-- >   queryPool,
-- >   0);
-- >
-- > result = vkEndCommandBuffer(commandBuffer);
-- >
-- > assert(VK_SUCCESS == result);
-- >
-- > for (uint32_t counterPass = 0; counterPass < numPasses; counterPass++) {
-- >
-- >   VkPerformanceQuerySubmitInfoKHR performanceQuerySubmitInfo = {
-- >     VK_STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR,
-- >     NULL,
-- >     counterPass
-- >   };
-- >
-- >
-- >   // Submit the command buffer and wait for its completion
-- >   // ...
-- > }
-- >
-- > // Release the profiling lock after the command buffer is no longer in the
-- > // pending state.
-- > vkReleaseProfilingLockKHR(device);
-- >
-- > result = vkResetCommandBuffer(commandBuffer, 0);
-- >
-- > assert(VK_SUCCESS == result);
-- >
-- > // Create an array to hold the results of all counters
-- > VkPerformanceCounterResultKHR* recordedCounters = malloc(
-- >   sizeof(VkPerformanceCounterResultKHR) * enabledCounterCount);
-- >
-- > result = vkGetQueryPoolResults(
-- >   device,
-- >   queryPool,
-- >   0,
-- >   1,
-- >   sizeof(VkPerformanceCounterResultKHR) * enabledCounterCount,
-- >   recordedCounters,
-- >   sizeof(VkPerformanceCounterResultKHR),
-- >   NULL);
-- >
-- > // recordedCounters is filled with our counters, we'll look at one for posterity
-- > switch (counters[0].storage) {
-- >   case VK_PERFORMANCE_COUNTER_STORAGE_INT32:
-- >     // use recordCounters[0].int32 to get at the counter result!
-- >     break;
-- >   case VK_PERFORMANCE_COUNTER_STORAGE_INT64:
-- >     // use recordCounters[0].int64 to get at the counter result!
-- >     break;
-- >   case VK_PERFORMANCE_COUNTER_STORAGE_UINT32:
-- >     // use recordCounters[0].uint32 to get at the counter result!
-- >     break;
-- >   case VK_PERFORMANCE_COUNTER_STORAGE_UINT64:
-- >     // use recordCounters[0].uint64 to get at the counter result!
-- >     break;
-- >   case VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32:
-- >     // use recordCounters[0].float32 to get at the counter result!
-- >     break;
-- >   case VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64:
-- >     // use recordCounters[0].float64 to get at the counter result!
-- >     break;
-- > }
--
-- == Version History
--
-- -   Revision 1, 2019-10-08
--
-- = See Also
--
-- 'AcquireProfilingLockFlagBitsKHR', 'AcquireProfilingLockFlagsKHR',
-- 'AcquireProfilingLockInfoKHR',
-- 'PerformanceCounterDescriptionFlagBitsKHR',
-- 'PerformanceCounterDescriptionFlagsKHR',
-- 'PerformanceCounterDescriptionKHR', 'PerformanceCounterKHR',
-- 'PerformanceCounterResultKHR', 'PerformanceCounterScopeKHR',
-- 'PerformanceCounterStorageKHR', 'PerformanceCounterUnitKHR',
-- 'PerformanceQuerySubmitInfoKHR',
-- 'PhysicalDevicePerformanceQueryFeaturesKHR',
-- 'PhysicalDevicePerformanceQueryPropertiesKHR',
-- 'QueryPoolPerformanceCreateInfoKHR', 'acquireProfilingLockKHR',
-- 'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR',
-- 'getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR',
-- 'releaseProfilingLockKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_performance_query Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_performance_query  ( enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
                                                   , getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
                                                   , acquireProfilingLockKHR
                                                   , releaseProfilingLockKHR
                                                   , pattern QUERY_SCOPE_COMMAND_BUFFER_KHR
                                                   , pattern QUERY_SCOPE_RENDER_PASS_KHR
                                                   , pattern QUERY_SCOPE_COMMAND_KHR
                                                   , pattern PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR
                                                   , pattern PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR
                                                   , PhysicalDevicePerformanceQueryFeaturesKHR(..)
                                                   , PhysicalDevicePerformanceQueryPropertiesKHR(..)
                                                   , PerformanceCounterKHR(..)
                                                   , PerformanceCounterDescriptionKHR(..)
                                                   , QueryPoolPerformanceCreateInfoKHR(..)
                                                   , AcquireProfilingLockInfoKHR(..)
                                                   , PerformanceQuerySubmitInfoKHR(..)
                                                   , PerformanceCounterResultKHR(..)
                                                   , PerformanceCounterScopeKHR( PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR
                                                                               , PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR
                                                                               , PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR
                                                                               , ..
                                                                               )
                                                   , PerformanceCounterUnitKHR( PERFORMANCE_COUNTER_UNIT_GENERIC_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_BYTES_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_KELVIN_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_WATTS_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_VOLTS_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_AMPS_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_HERTZ_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_CYCLES_KHR
                                                                              , ..
                                                                              )
                                                   , PerformanceCounterStorageKHR( PERFORMANCE_COUNTER_STORAGE_INT32_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_INT64_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_UINT32_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_UINT64_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR
                                                                                 , ..
                                                                                 )
                                                   , PerformanceCounterDescriptionFlagsKHR
                                                   , PerformanceCounterDescriptionFlagBitsKHR( PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR
                                                                                             , PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR
                                                                                             , ..
                                                                                             )
                                                   , AcquireProfilingLockFlagsKHR
                                                   , AcquireProfilingLockFlagBitsKHR(..)
                                                   , KHR_PERFORMANCE_QUERY_SPEC_VERSION
                                                   , pattern KHR_PERFORMANCE_QUERY_SPEC_VERSION
                                                   , KHR_PERFORMANCE_QUERY_EXTENSION_NAME
                                                   , pattern KHR_PERFORMANCE_QUERY_EXTENSION_NAME
                                                   ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Foreign.C.Types (CDouble)
import Foreign.C.Types (CDouble(CDouble))
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Data.Int (Int64)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAcquireProfilingLockKHR))
import Vulkan.Dynamic (DeviceCmds(pVkReleaseProfilingLockKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Dynamic (InstanceCmds(pVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR))
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr PerformanceCounterKHR -> Ptr PerformanceCounterDescriptionKHR -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr PerformanceCounterKHR -> Ptr PerformanceCounterDescriptionKHR -> IO Result

-- | vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR -
-- Reports properties of the performance query counters available on a
-- queue family of a device
--
-- = Description
--
-- If @pCounters@ is @NULL@ and @pCounterDescriptions@ is @NULL@, then the
-- number of counters available is returned in @pCounterCount@. Otherwise,
-- @pCounterCount@ /must/ point to a variable set by the user to the number
-- of elements in the @pCounters@, @pCounterDescriptions@, or both arrays
-- and on return the variable is overwritten with the number of structures
-- actually written out. If @pCounterCount@ is less than the number of
-- counters available, at most @pCounterCount@ structures will be written
-- and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR-pCounterCount-parameter#
--     @pCounterCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR-pCounters-parameter#
--     If the value referenced by @pCounterCount@ is not @0@, and
--     @pCounters@ is not @NULL@, @pCounters@ /must/ be a valid pointer to
--     an array of @pCounterCount@ 'PerformanceCounterKHR' structures
--
-- -   #VUID-vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR-pCounterDescriptions-parameter#
--     If the value referenced by @pCounterCount@ is not @0@, and
--     @pCounterDescriptions@ is not @NULL@, @pCounterDescriptions@ /must/
--     be a valid pointer to an array of @pCounterCount@
--     'PerformanceCounterDescriptionKHR' structures
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'PerformanceCounterDescriptionKHR', 'PerformanceCounterKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR :: forall io
                                                               . (MonadIO io)
                                                              => -- | @physicalDevice@ is the handle to the physical device whose queue family
                                                                 -- performance query counter properties will be queried.
                                                                 PhysicalDevice
                                                              -> -- | @queueFamilyIndex@ is the index into the queue family of the physical
                                                                 -- device we want to get properties for.
                                                                 ("queueFamilyIndex" ::: Word32)
                                                              -> io (Result, ("counters" ::: Vector PerformanceCounterKHR), ("counterDescriptions" ::: Vector PerformanceCounterDescriptionKHR))
enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR physicalDevice queueFamilyIndex = liftIO . evalContT $ do
  let vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRPtr = pVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR is null" Nothing Nothing
  let vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR' = mkVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPCounterCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR" (vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR' physicalDevice' (queueFamilyIndex) (pPCounterCount) (nullPtr) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCounterCount <- lift $ peek @Word32 pPCounterCount
  pPCounters <- ContT $ bracket (callocBytes @PerformanceCounterKHR ((fromIntegral (pCounterCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCounters `advancePtrBytes` (i * 48) :: Ptr PerformanceCounterKHR) . ($ ())) [0..(fromIntegral (pCounterCount)) - 1]
  pPCounterDescriptions <- ContT $ bracket (callocBytes @PerformanceCounterDescriptionKHR ((fromIntegral (pCounterCount)) * 792)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCounterDescriptions `advancePtrBytes` (i * 792) :: Ptr PerformanceCounterDescriptionKHR) . ($ ())) [0..(fromIntegral (pCounterCount)) - 1]
  r' <- lift $ traceAroundEvent "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR" (vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR' physicalDevice' (queueFamilyIndex) (pPCounterCount) ((pPCounters)) ((pPCounterDescriptions)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pCounterCount' <- lift $ peek @Word32 pPCounterCount
  let x33 = pCounterCount'
  pCounters' <- lift $ generateM (fromIntegral x33) (\i -> peekCStruct @PerformanceCounterKHR (((pPCounters) `advancePtrBytes` (48 * (i)) :: Ptr PerformanceCounterKHR)))
  pCounterDescriptions' <- lift $ generateM (fromIntegral x33) (\i -> peekCStruct @PerformanceCounterDescriptionKHR (((pPCounterDescriptions) `advancePtrBytes` (792 * (i)) :: Ptr PerformanceCounterDescriptionKHR)))
  pure $ ((r'), pCounters', pCounterDescriptions')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr QueryPoolPerformanceCreateInfoKHR -> Ptr Word32 -> IO ()) -> Ptr PhysicalDevice_T -> Ptr QueryPoolPerformanceCreateInfoKHR -> Ptr Word32 -> IO ()

-- | vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR - Reports the
-- number of passes require for a performance query pool type
--
-- = Description
--
-- The @pPerformanceQueryCreateInfo@ member
-- 'QueryPoolPerformanceCreateInfoKHR'::@queueFamilyIndex@ /must/ be a
-- queue family of @physicalDevice@. The number of passes required to
-- capture the counters specified in the @pPerformanceQueryCreateInfo@
-- member 'QueryPoolPerformanceCreateInfoKHR'::@pCounters@ is returned in
-- @pNumPasses@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'QueryPoolPerformanceCreateInfoKHR'
getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR :: forall io
                                                       . (MonadIO io)
                                                      => -- | @physicalDevice@ is the handle to the physical device whose queue family
                                                         -- performance query counter properties will be queried.
                                                         --
                                                         -- #VUID-vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR-physicalDevice-parameter#
                                                         -- @physicalDevice@ /must/ be a valid
                                                         -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                         PhysicalDevice
                                                      -> -- | @pPerformanceQueryCreateInfo@ is a pointer to a
                                                         -- 'QueryPoolPerformanceCreateInfoKHR' of the performance query that is to
                                                         -- be created.
                                                         --
                                                         -- #VUID-vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR-pPerformanceQueryCreateInfo-parameter#
                                                         -- @pPerformanceQueryCreateInfo@ /must/ be a valid pointer to a valid
                                                         -- 'QueryPoolPerformanceCreateInfoKHR' structure
                                                         ("performanceQueryCreateInfo" ::: QueryPoolPerformanceCreateInfoKHR)
                                                      -> io (("numPasses" ::: Word32))
getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR physicalDevice performanceQueryCreateInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRPtr = pVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR' = mkVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRPtr
  pPerformanceQueryCreateInfo <- ContT $ withCStruct (performanceQueryCreateInfo)
  pPNumPasses <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR" (vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR' (physicalDeviceHandle (physicalDevice)) pPerformanceQueryCreateInfo (pPNumPasses))
  pNumPasses <- lift $ peek @Word32 pPNumPasses
  pure $ (pNumPasses)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireProfilingLockKHR
  :: FunPtr (Ptr Device_T -> Ptr AcquireProfilingLockInfoKHR -> IO Result) -> Ptr Device_T -> Ptr AcquireProfilingLockInfoKHR -> IO Result

-- | vkAcquireProfilingLockKHR - Acquires the profiling lock
--
-- = Description
--
-- Implementations /may/ allow multiple actors to hold the profiling lock
-- concurrently.
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
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
--
-- = See Also
--
-- 'AcquireProfilingLockInfoKHR', 'Vulkan.Core10.Handles.Device'
acquireProfilingLockKHR :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device to profile.
                           --
                           -- #VUID-vkAcquireProfilingLockKHR-device-parameter# @device@ /must/ be a
                           -- valid 'Vulkan.Core10.Handles.Device' handle
                           Device
                        -> -- | @pInfo@ is a pointer to a 'AcquireProfilingLockInfoKHR' structure which
                           -- contains information about how the profiling is to be acquired.
                           --
                           -- #VUID-vkAcquireProfilingLockKHR-pInfo-parameter# @pInfo@ /must/ be a
                           -- valid pointer to a valid 'AcquireProfilingLockInfoKHR' structure
                           AcquireProfilingLockInfoKHR
                        -> io ()
acquireProfilingLockKHR device info = liftIO . evalContT $ do
  let vkAcquireProfilingLockKHRPtr = pVkAcquireProfilingLockKHR (deviceCmds (device :: Device))
  lift $ unless (vkAcquireProfilingLockKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireProfilingLockKHR is null" Nothing Nothing
  let vkAcquireProfilingLockKHR' = mkVkAcquireProfilingLockKHR vkAcquireProfilingLockKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkAcquireProfilingLockKHR" (vkAcquireProfilingLockKHR' (deviceHandle (device)) pInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseProfilingLockKHR
  :: FunPtr (Ptr Device_T -> IO ()) -> Ptr Device_T -> IO ()

-- | vkReleaseProfilingLockKHR - Releases the profiling lock
--
-- == Valid Usage
--
-- -   #VUID-vkReleaseProfilingLockKHR-device-03235# The profiling lock of
--     @device@ /must/ have been held via a previous successful call to
--     'acquireProfilingLockKHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkReleaseProfilingLockKHR-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device'
releaseProfilingLockKHR :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device to cease profiling on.
                           Device
                        -> io ()
releaseProfilingLockKHR device = liftIO $ do
  let vkReleaseProfilingLockKHRPtr = pVkReleaseProfilingLockKHR (deviceCmds (device :: Device))
  unless (vkReleaseProfilingLockKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseProfilingLockKHR is null" Nothing Nothing
  let vkReleaseProfilingLockKHR' = mkVkReleaseProfilingLockKHR vkReleaseProfilingLockKHRPtr
  traceAroundEvent "vkReleaseProfilingLockKHR" (vkReleaseProfilingLockKHR' (deviceHandle (device)))
  pure $ ()


-- No documentation found for TopLevel "VK_QUERY_SCOPE_COMMAND_BUFFER_KHR"
pattern QUERY_SCOPE_COMMAND_BUFFER_KHR = PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR


-- No documentation found for TopLevel "VK_QUERY_SCOPE_RENDER_PASS_KHR"
pattern QUERY_SCOPE_RENDER_PASS_KHR = PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR


-- No documentation found for TopLevel "VK_QUERY_SCOPE_COMMAND_KHR"
pattern QUERY_SCOPE_COMMAND_KHR = PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR


-- No documentation found for TopLevel "VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR"
pattern PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR = PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR


-- No documentation found for TopLevel "VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR"
pattern PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR = PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR


-- | VkPhysicalDevicePerformanceQueryFeaturesKHR - Structure describing
-- performance query support for an implementation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePerformanceQueryFeaturesKHR = PhysicalDevicePerformanceQueryFeaturesKHR
  { -- | #features-performanceCounterQueryPools# @performanceCounterQueryPools@
    -- indicates whether the implementation supports performance counter query
    -- pools.
    performanceCounterQueryPools :: Bool
  , -- | #features-performanceCounterMultipleQueryPools#
    -- @performanceCounterMultipleQueryPools@ indicates whether the
    -- implementation supports using multiple performance query pools in a
    -- primary command buffer and secondary command buffers executed within it.
    performanceCounterMultipleQueryPools :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePerformanceQueryFeaturesKHR)
#endif
deriving instance Show PhysicalDevicePerformanceQueryFeaturesKHR

instance ToCStruct PhysicalDevicePerformanceQueryFeaturesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePerformanceQueryFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (performanceCounterQueryPools))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (performanceCounterMultipleQueryPools))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePerformanceQueryFeaturesKHR where
  peekCStruct p = do
    performanceCounterQueryPools <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    performanceCounterMultipleQueryPools <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDevicePerformanceQueryFeaturesKHR
             (bool32ToBool performanceCounterQueryPools) (bool32ToBool performanceCounterMultipleQueryPools)

instance Storable PhysicalDevicePerformanceQueryFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePerformanceQueryFeaturesKHR where
  zero = PhysicalDevicePerformanceQueryFeaturesKHR
           zero
           zero


-- | VkPhysicalDevicePerformanceQueryPropertiesKHR - Structure describing
-- performance query properties for an implementation
--
-- = Members
--
-- The members of the 'PhysicalDevicePerformanceQueryPropertiesKHR'
-- structure describe the following implementation-dependent properties:
--
-- == Valid Usage (Implicit)
--
-- If the 'PhysicalDevicePerformanceQueryPropertiesKHR' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent properties.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePerformanceQueryPropertiesKHR = PhysicalDevicePerformanceQueryPropertiesKHR
  { -- | @allowCommandBufferQueryCopies@ is 'Vulkan.Core10.FundamentalTypes.TRUE'
    -- if the performance query pools are allowed to be used with
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'.
    allowCommandBufferQueryCopies :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePerformanceQueryPropertiesKHR)
#endif
deriving instance Show PhysicalDevicePerformanceQueryPropertiesKHR

instance ToCStruct PhysicalDevicePerformanceQueryPropertiesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePerformanceQueryPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (allowCommandBufferQueryCopies))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePerformanceQueryPropertiesKHR where
  peekCStruct p = do
    allowCommandBufferQueryCopies <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePerformanceQueryPropertiesKHR
             (bool32ToBool allowCommandBufferQueryCopies)

instance Storable PhysicalDevicePerformanceQueryPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePerformanceQueryPropertiesKHR where
  zero = PhysicalDevicePerformanceQueryPropertiesKHR
           zero


-- | VkPerformanceCounterKHR - Structure providing information about a
-- counter
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PerformanceCounterScopeKHR', 'PerformanceCounterStorageKHR',
-- 'PerformanceCounterUnitKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR'
data PerformanceCounterKHR = PerformanceCounterKHR
  { -- | @unit@ is a 'PerformanceCounterUnitKHR' specifying the unit that the
    -- counter data will record.
    unit :: PerformanceCounterUnitKHR
  , -- | @scope@ is a 'PerformanceCounterScopeKHR' specifying the scope that the
    -- counter belongs to.
    scope :: PerformanceCounterScopeKHR
  , -- | @storage@ is a 'PerformanceCounterStorageKHR' specifying the storage
    -- type that the counter’s data uses.
    storage :: PerformanceCounterStorageKHR
  , -- | @uuid@ is an array of size 'Vulkan.Core10.APIConstants.UUID_SIZE',
    -- containing 8-bit values that represent a universally unique identifier
    -- for the counter of the physical device.
    uuid :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceCounterKHR)
#endif
deriving instance Show PerformanceCounterKHR

instance ToCStruct PerformanceCounterKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceCounterKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceCounterUnitKHR)) (unit)
    poke ((p `plusPtr` 20 :: Ptr PerformanceCounterScopeKHR)) (scope)
    poke ((p `plusPtr` 24 :: Ptr PerformanceCounterStorageKHR)) (storage)
    pokeFixedLengthByteString ((p `plusPtr` 28 :: Ptr (FixedArray UUID_SIZE Word8))) (uuid)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceCounterUnitKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr PerformanceCounterScopeKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PerformanceCounterStorageKHR)) (zero)
    pokeFixedLengthByteString ((p `plusPtr` 28 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    f

instance FromCStruct PerformanceCounterKHR where
  peekCStruct p = do
    unit <- peek @PerformanceCounterUnitKHR ((p `plusPtr` 16 :: Ptr PerformanceCounterUnitKHR))
    scope <- peek @PerformanceCounterScopeKHR ((p `plusPtr` 20 :: Ptr PerformanceCounterScopeKHR))
    storage <- peek @PerformanceCounterStorageKHR ((p `plusPtr` 24 :: Ptr PerformanceCounterStorageKHR))
    uuid <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 28 :: Ptr (FixedArray UUID_SIZE Word8)))
    pure $ PerformanceCounterKHR
             unit scope storage uuid

instance Storable PerformanceCounterKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceCounterKHR where
  zero = PerformanceCounterKHR
           zero
           zero
           zero
           mempty


-- | VkPerformanceCounterDescriptionKHR - Structure providing more detailed
-- information about a counter
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PerformanceCounterDescriptionFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR'
data PerformanceCounterDescriptionKHR = PerformanceCounterDescriptionKHR
  { -- | @flags@ is a bitmask of 'PerformanceCounterDescriptionFlagBitsKHR'
    -- indicating the usage behavior for the counter.
    flags :: PerformanceCounterDescriptionFlagsKHR
  , -- | @name@ is an array of size
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE', containing a
    -- null-terminated UTF-8 string specifying the name of the counter.
    name :: ByteString
  , -- | @category@ is an array of size
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE', containing a
    -- null-terminated UTF-8 string specifying the category of the counter.
    category :: ByteString
  , -- | @description@ is an array of size
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE', containing a
    -- null-terminated UTF-8 string specifying the description of the counter.
    description :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceCounterDescriptionKHR)
#endif
deriving instance Show PerformanceCounterDescriptionKHR

instance ToCStruct PerformanceCounterDescriptionKHR where
  withCStruct x f = allocaBytesAligned 792 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceCounterDescriptionKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceCounterDescriptionFlagsKHR)) (flags)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (category)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    f
  cStructSize = 792
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    f

instance FromCStruct PerformanceCounterDescriptionKHR where
  peekCStruct p = do
    flags <- peek @PerformanceCounterDescriptionFlagsKHR ((p `plusPtr` 16 :: Ptr PerformanceCounterDescriptionFlagsKHR))
    name <- packCString (lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    category <- packCString (lowerArrayPtr ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    pure $ PerformanceCounterDescriptionKHR
             flags name category description

instance Storable PerformanceCounterDescriptionKHR where
  sizeOf ~_ = 792
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceCounterDescriptionKHR where
  zero = PerformanceCounterDescriptionKHR
           zero
           mempty
           mempty
           mempty


-- | VkQueryPoolPerformanceCreateInfoKHR - Structure specifying parameters of
-- a newly created performance query pool
--
-- == Valid Usage
--
-- -   #VUID-VkQueryPoolPerformanceCreateInfoKHR-queueFamilyIndex-03236#
--     @queueFamilyIndex@ /must/ be a valid queue family index of the
--     device
--
-- -   #VUID-VkQueryPoolPerformanceCreateInfoKHR-performanceCounterQueryPools-03237#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-performanceCounterQueryPools performanceCounterQueryPools>
--     feature /must/ be enabled
--
-- -   #VUID-VkQueryPoolPerformanceCreateInfoKHR-pCounterIndices-03321#
--     Each element of @pCounterIndices@ /must/ be in the range of counters
--     reported by
--     'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR' for
--     the queue family specified in @queueFamilyIndex@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkQueryPoolPerformanceCreateInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR'
--
-- -   #VUID-VkQueryPoolPerformanceCreateInfoKHR-pCounterIndices-parameter#
--     @pCounterIndices@ /must/ be a valid pointer to an array of
--     @counterIndexCount@ @uint32_t@ values
--
-- -   #VUID-VkQueryPoolPerformanceCreateInfoKHR-counterIndexCount-arraylength#
--     @counterIndexCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
data QueryPoolPerformanceCreateInfoKHR = QueryPoolPerformanceCreateInfoKHR
  { -- | @queueFamilyIndex@ is the queue family index to create this performance
    -- query pool for.
    queueFamilyIndex :: Word32
  , -- | @pCounterIndices@ is the array of indices into the
    -- 'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR'::@pCounters@
    -- to enable in this performance query pool.
    counterIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueryPoolPerformanceCreateInfoKHR)
#endif
deriving instance Show QueryPoolPerformanceCreateInfoKHR

instance ToCStruct QueryPoolPerformanceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueryPoolPerformanceCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (queueFamilyIndex)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (counterIndices)) :: Word32))
    pPCounterIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (counterIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (counterIndices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPCounterIndices')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct QueryPoolPerformanceCreateInfoKHR where
  peekCStruct p = do
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    counterIndexCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pCounterIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pCounterIndices' <- generateM (fromIntegral counterIndexCount) (\i -> peek @Word32 ((pCounterIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ QueryPoolPerformanceCreateInfoKHR
             queueFamilyIndex pCounterIndices'

instance Zero QueryPoolPerformanceCreateInfoKHR where
  zero = QueryPoolPerformanceCreateInfoKHR
           zero
           mempty


-- | VkAcquireProfilingLockInfoKHR - Structure specifying parameters to
-- acquire the profiling lock
--
-- == Valid Usage (Implicit)
--
-- If @timeout@ is 0, 'acquireProfilingLockKHR' will not block while
-- attempting to acquire the profling lock. If @timeout@ is @UINT64_MAX@,
-- the function will not return until the profiling lock was acquired.
--
-- = See Also
--
-- 'AcquireProfilingLockFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'acquireProfilingLockKHR'
data AcquireProfilingLockInfoKHR = AcquireProfilingLockInfoKHR
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkAcquireProfilingLockInfoKHR-flags-zerobitmask# @flags@ /must/ be
    -- @0@
    flags :: AcquireProfilingLockFlagsKHR
  , -- | @timeout@ indicates how long the function waits, in nanoseconds, if the
    -- profiling lock is not available.
    timeout :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AcquireProfilingLockInfoKHR)
#endif
deriving instance Show AcquireProfilingLockInfoKHR

instance ToCStruct AcquireProfilingLockInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AcquireProfilingLockInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AcquireProfilingLockFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (timeout)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct AcquireProfilingLockInfoKHR where
  peekCStruct p = do
    flags <- peek @AcquireProfilingLockFlagsKHR ((p `plusPtr` 16 :: Ptr AcquireProfilingLockFlagsKHR))
    timeout <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ AcquireProfilingLockInfoKHR
             flags timeout

instance Storable AcquireProfilingLockInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AcquireProfilingLockInfoKHR where
  zero = AcquireProfilingLockInfoKHR
           zero
           zero


-- | VkPerformanceQuerySubmitInfoKHR - Structure indicating which counter
-- pass index is active for performance queries
--
-- = Description
--
-- If the 'Vulkan.Core10.Queue.SubmitInfo'::@pNext@ chain does not include
-- this structure, the batch defaults to use counter pass index 0.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PerformanceQuerySubmitInfoKHR = PerformanceQuerySubmitInfoKHR
  { -- | @counterPassIndex@ specifies which counter pass index is active.
    --
    -- #VUID-VkPerformanceQuerySubmitInfoKHR-counterPassIndex-03221#
    -- @counterPassIndex@ /must/ be less than the number of counter passes
    -- required by any queries within the batch. The required number of counter
    -- passes for a performance query is obtained by calling
    -- 'getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
    counterPassIndex :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceQuerySubmitInfoKHR)
#endif
deriving instance Show PerformanceQuerySubmitInfoKHR

instance ToCStruct PerformanceQuerySubmitInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceQuerySubmitInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (counterPassIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PerformanceQuerySubmitInfoKHR where
  peekCStruct p = do
    counterPassIndex <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PerformanceQuerySubmitInfoKHR
             counterPassIndex

instance Storable PerformanceQuerySubmitInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceQuerySubmitInfoKHR where
  zero = PerformanceQuerySubmitInfoKHR
           zero


data PerformanceCounterResultKHR
  = Int32Counter Int32
  | Int64Counter Int64
  | Uint32Counter Word32
  | Uint64Counter Word64
  | Float32Counter Float
  | Float64Counter Double
  deriving (Show)

instance ToCStruct PerformanceCounterResultKHR where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr PerformanceCounterResultKHR -> PerformanceCounterResultKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Int32Counter v -> lift $ poke (castPtr @_ @Int32 p) (v)
    Int64Counter v -> lift $ poke (castPtr @_ @Int64 p) (v)
    Uint32Counter v -> lift $ poke (castPtr @_ @Word32 p) (v)
    Uint64Counter v -> lift $ poke (castPtr @_ @Word64 p) (v)
    Float32Counter v -> lift $ poke (castPtr @_ @CFloat p) (CFloat (v))
    Float64Counter v -> lift $ poke (castPtr @_ @CDouble p) (CDouble (v))
  pokeZeroCStruct :: Ptr PerformanceCounterResultKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero PerformanceCounterResultKHR where
  zero = Int64Counter zero


-- | VkPerformanceCounterScopeKHR - Supported counter scope types
--
-- = See Also
--
-- 'PerformanceCounterKHR'
newtype PerformanceCounterScopeKHR = PerformanceCounterScopeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR' - the performance counter
-- scope is a single complete command buffer.
pattern PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR = PerformanceCounterScopeKHR 0
-- | 'PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR' - the performance counter
-- scope is zero or more complete render passes. The performance query
-- containing the performance counter /must/ begin and end outside a render
-- pass instance.
pattern PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR    = PerformanceCounterScopeKHR 1
-- | 'PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR' - the performance counter scope
-- is zero or more commands.
pattern PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR        = PerformanceCounterScopeKHR 2
{-# complete PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR,
             PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR,
             PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR :: PerformanceCounterScopeKHR #-}

conNamePerformanceCounterScopeKHR :: String
conNamePerformanceCounterScopeKHR = "PerformanceCounterScopeKHR"

enumPrefixPerformanceCounterScopeKHR :: String
enumPrefixPerformanceCounterScopeKHR = "PERFORMANCE_COUNTER_SCOPE_"

showTablePerformanceCounterScopeKHR :: [(PerformanceCounterScopeKHR, String)]
showTablePerformanceCounterScopeKHR =
  [ (PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR, "COMMAND_BUFFER_KHR")
  , (PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR   , "RENDER_PASS_KHR")
  , (PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR       , "COMMAND_KHR")
  ]

instance Show PerformanceCounterScopeKHR where
  showsPrec = enumShowsPrec enumPrefixPerformanceCounterScopeKHR
                            showTablePerformanceCounterScopeKHR
                            conNamePerformanceCounterScopeKHR
                            (\(PerformanceCounterScopeKHR x) -> x)
                            (showsPrec 11)

instance Read PerformanceCounterScopeKHR where
  readPrec = enumReadPrec enumPrefixPerformanceCounterScopeKHR
                          showTablePerformanceCounterScopeKHR
                          conNamePerformanceCounterScopeKHR
                          PerformanceCounterScopeKHR


-- | VkPerformanceCounterUnitKHR - Supported counter unit types
--
-- = See Also
--
-- 'PerformanceCounterKHR'
newtype PerformanceCounterUnitKHR = PerformanceCounterUnitKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PERFORMANCE_COUNTER_UNIT_GENERIC_KHR' - the performance counter unit is
-- a generic data point.
pattern PERFORMANCE_COUNTER_UNIT_GENERIC_KHR          = PerformanceCounterUnitKHR 0
-- | 'PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR' - the performance counter unit
-- is a percentage (%).
pattern PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR       = PerformanceCounterUnitKHR 1
-- | 'PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR' - the performance counter
-- unit is a value of nanoseconds (ns).
pattern PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR      = PerformanceCounterUnitKHR 2
-- | 'PERFORMANCE_COUNTER_UNIT_BYTES_KHR' - the performance counter unit is a
-- value of bytes.
pattern PERFORMANCE_COUNTER_UNIT_BYTES_KHR            = PerformanceCounterUnitKHR 3
-- | 'PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR' - the performance
-- counter unit is a value of bytes\/s.
pattern PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR = PerformanceCounterUnitKHR 4
-- | 'PERFORMANCE_COUNTER_UNIT_KELVIN_KHR' - the performance counter unit is
-- a temperature reported in Kelvin.
pattern PERFORMANCE_COUNTER_UNIT_KELVIN_KHR           = PerformanceCounterUnitKHR 5
-- | 'PERFORMANCE_COUNTER_UNIT_WATTS_KHR' - the performance counter unit is a
-- value of watts (W).
pattern PERFORMANCE_COUNTER_UNIT_WATTS_KHR            = PerformanceCounterUnitKHR 6
-- | 'PERFORMANCE_COUNTER_UNIT_VOLTS_KHR' - the performance counter unit is a
-- value of volts (V).
pattern PERFORMANCE_COUNTER_UNIT_VOLTS_KHR            = PerformanceCounterUnitKHR 7
-- | 'PERFORMANCE_COUNTER_UNIT_AMPS_KHR' - the performance counter unit is a
-- value of amps (A).
pattern PERFORMANCE_COUNTER_UNIT_AMPS_KHR             = PerformanceCounterUnitKHR 8
-- | 'PERFORMANCE_COUNTER_UNIT_HERTZ_KHR' - the performance counter unit is a
-- value of hertz (Hz).
pattern PERFORMANCE_COUNTER_UNIT_HERTZ_KHR            = PerformanceCounterUnitKHR 9
-- | 'PERFORMANCE_COUNTER_UNIT_CYCLES_KHR' - the performance counter unit is
-- a value of cycles.
pattern PERFORMANCE_COUNTER_UNIT_CYCLES_KHR           = PerformanceCounterUnitKHR 10
{-# complete PERFORMANCE_COUNTER_UNIT_GENERIC_KHR,
             PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR,
             PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR,
             PERFORMANCE_COUNTER_UNIT_BYTES_KHR,
             PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR,
             PERFORMANCE_COUNTER_UNIT_KELVIN_KHR,
             PERFORMANCE_COUNTER_UNIT_WATTS_KHR,
             PERFORMANCE_COUNTER_UNIT_VOLTS_KHR,
             PERFORMANCE_COUNTER_UNIT_AMPS_KHR,
             PERFORMANCE_COUNTER_UNIT_HERTZ_KHR,
             PERFORMANCE_COUNTER_UNIT_CYCLES_KHR :: PerformanceCounterUnitKHR #-}

conNamePerformanceCounterUnitKHR :: String
conNamePerformanceCounterUnitKHR = "PerformanceCounterUnitKHR"

enumPrefixPerformanceCounterUnitKHR :: String
enumPrefixPerformanceCounterUnitKHR = "PERFORMANCE_COUNTER_UNIT_"

showTablePerformanceCounterUnitKHR :: [(PerformanceCounterUnitKHR, String)]
showTablePerformanceCounterUnitKHR =
  [ (PERFORMANCE_COUNTER_UNIT_GENERIC_KHR         , "GENERIC_KHR")
  , (PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR      , "PERCENTAGE_KHR")
  , (PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR     , "NANOSECONDS_KHR")
  , (PERFORMANCE_COUNTER_UNIT_BYTES_KHR           , "BYTES_KHR")
  , (PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR, "BYTES_PER_SECOND_KHR")
  , (PERFORMANCE_COUNTER_UNIT_KELVIN_KHR          , "KELVIN_KHR")
  , (PERFORMANCE_COUNTER_UNIT_WATTS_KHR           , "WATTS_KHR")
  , (PERFORMANCE_COUNTER_UNIT_VOLTS_KHR           , "VOLTS_KHR")
  , (PERFORMANCE_COUNTER_UNIT_AMPS_KHR            , "AMPS_KHR")
  , (PERFORMANCE_COUNTER_UNIT_HERTZ_KHR           , "HERTZ_KHR")
  , (PERFORMANCE_COUNTER_UNIT_CYCLES_KHR          , "CYCLES_KHR")
  ]

instance Show PerformanceCounterUnitKHR where
  showsPrec = enumShowsPrec enumPrefixPerformanceCounterUnitKHR
                            showTablePerformanceCounterUnitKHR
                            conNamePerformanceCounterUnitKHR
                            (\(PerformanceCounterUnitKHR x) -> x)
                            (showsPrec 11)

instance Read PerformanceCounterUnitKHR where
  readPrec = enumReadPrec enumPrefixPerformanceCounterUnitKHR
                          showTablePerformanceCounterUnitKHR
                          conNamePerformanceCounterUnitKHR
                          PerformanceCounterUnitKHR


-- | VkPerformanceCounterStorageKHR - Supported counter storage types
--
-- = See Also
--
-- 'PerformanceCounterKHR'
newtype PerformanceCounterStorageKHR = PerformanceCounterStorageKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PERFORMANCE_COUNTER_STORAGE_INT32_KHR' - the performance counter
-- storage is a 32-bit signed integer.
pattern PERFORMANCE_COUNTER_STORAGE_INT32_KHR   = PerformanceCounterStorageKHR 0
-- | 'PERFORMANCE_COUNTER_STORAGE_INT64_KHR' - the performance counter
-- storage is a 64-bit signed integer.
pattern PERFORMANCE_COUNTER_STORAGE_INT64_KHR   = PerformanceCounterStorageKHR 1
-- | 'PERFORMANCE_COUNTER_STORAGE_UINT32_KHR' - the performance counter
-- storage is a 32-bit unsigned integer.
pattern PERFORMANCE_COUNTER_STORAGE_UINT32_KHR  = PerformanceCounterStorageKHR 2
-- | 'PERFORMANCE_COUNTER_STORAGE_UINT64_KHR' - the performance counter
-- storage is a 64-bit unsigned integer.
pattern PERFORMANCE_COUNTER_STORAGE_UINT64_KHR  = PerformanceCounterStorageKHR 3
-- | 'PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR' - the performance counter
-- storage is a 32-bit floating-point.
pattern PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR = PerformanceCounterStorageKHR 4
-- | 'PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR' - the performance counter
-- storage is a 64-bit floating-point.
pattern PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR = PerformanceCounterStorageKHR 5
{-# complete PERFORMANCE_COUNTER_STORAGE_INT32_KHR,
             PERFORMANCE_COUNTER_STORAGE_INT64_KHR,
             PERFORMANCE_COUNTER_STORAGE_UINT32_KHR,
             PERFORMANCE_COUNTER_STORAGE_UINT64_KHR,
             PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR,
             PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR :: PerformanceCounterStorageKHR #-}

conNamePerformanceCounterStorageKHR :: String
conNamePerformanceCounterStorageKHR = "PerformanceCounterStorageKHR"

enumPrefixPerformanceCounterStorageKHR :: String
enumPrefixPerformanceCounterStorageKHR = "PERFORMANCE_COUNTER_STORAGE_"

showTablePerformanceCounterStorageKHR :: [(PerformanceCounterStorageKHR, String)]
showTablePerformanceCounterStorageKHR =
  [ (PERFORMANCE_COUNTER_STORAGE_INT32_KHR  , "INT32_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_INT64_KHR  , "INT64_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_UINT32_KHR , "UINT32_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_UINT64_KHR , "UINT64_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR, "FLOAT32_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR, "FLOAT64_KHR")
  ]

instance Show PerformanceCounterStorageKHR where
  showsPrec = enumShowsPrec enumPrefixPerformanceCounterStorageKHR
                            showTablePerformanceCounterStorageKHR
                            conNamePerformanceCounterStorageKHR
                            (\(PerformanceCounterStorageKHR x) -> x)
                            (showsPrec 11)

instance Read PerformanceCounterStorageKHR where
  readPrec = enumReadPrec enumPrefixPerformanceCounterStorageKHR
                          showTablePerformanceCounterStorageKHR
                          conNamePerformanceCounterStorageKHR
                          PerformanceCounterStorageKHR


type PerformanceCounterDescriptionFlagsKHR = PerformanceCounterDescriptionFlagBitsKHR

-- | VkPerformanceCounterDescriptionFlagBitsKHR - Bitmask specifying usage
-- behavior for a counter
--
-- = See Also
--
-- 'PerformanceCounterDescriptionFlagsKHR'
newtype PerformanceCounterDescriptionFlagBitsKHR = PerformanceCounterDescriptionFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR'
-- specifies that recording the counter /may/ have a noticeable performance
-- impact.
pattern PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR =
  PerformanceCounterDescriptionFlagBitsKHR 0x00000001
-- | 'PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR'
-- specifies that concurrently recording the counter while other submitted
-- command buffers are running /may/ impact the accuracy of the recording.
pattern PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR =
  PerformanceCounterDescriptionFlagBitsKHR 0x00000002

conNamePerformanceCounterDescriptionFlagBitsKHR :: String
conNamePerformanceCounterDescriptionFlagBitsKHR = "PerformanceCounterDescriptionFlagBitsKHR"

enumPrefixPerformanceCounterDescriptionFlagBitsKHR :: String
enumPrefixPerformanceCounterDescriptionFlagBitsKHR = "PERFORMANCE_COUNTER_DESCRIPTION_"

showTablePerformanceCounterDescriptionFlagBitsKHR :: [(PerformanceCounterDescriptionFlagBitsKHR, String)]
showTablePerformanceCounterDescriptionFlagBitsKHR =
  [ (PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR, "PERFORMANCE_IMPACTING_BIT_KHR")
  , (PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR, "CONCURRENTLY_IMPACTED_BIT_KHR")
  ]

instance Show PerformanceCounterDescriptionFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixPerformanceCounterDescriptionFlagBitsKHR
                            showTablePerformanceCounterDescriptionFlagBitsKHR
                            conNamePerformanceCounterDescriptionFlagBitsKHR
                            (\(PerformanceCounterDescriptionFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PerformanceCounterDescriptionFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixPerformanceCounterDescriptionFlagBitsKHR
                          showTablePerformanceCounterDescriptionFlagBitsKHR
                          conNamePerformanceCounterDescriptionFlagBitsKHR
                          PerformanceCounterDescriptionFlagBitsKHR


type AcquireProfilingLockFlagsKHR = AcquireProfilingLockFlagBitsKHR

-- | VkAcquireProfilingLockFlagBitsKHR - Reserved for future use
--
-- = See Also
--
-- 'AcquireProfilingLockFlagsKHR'
newtype AcquireProfilingLockFlagBitsKHR = AcquireProfilingLockFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameAcquireProfilingLockFlagBitsKHR :: String
conNameAcquireProfilingLockFlagBitsKHR = "AcquireProfilingLockFlagBitsKHR"

enumPrefixAcquireProfilingLockFlagBitsKHR :: String
enumPrefixAcquireProfilingLockFlagBitsKHR = ""

showTableAcquireProfilingLockFlagBitsKHR :: [(AcquireProfilingLockFlagBitsKHR, String)]
showTableAcquireProfilingLockFlagBitsKHR = []

instance Show AcquireProfilingLockFlagBitsKHR where
  showsPrec = enumShowsPrec enumPrefixAcquireProfilingLockFlagBitsKHR
                            showTableAcquireProfilingLockFlagBitsKHR
                            conNameAcquireProfilingLockFlagBitsKHR
                            (\(AcquireProfilingLockFlagBitsKHR x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read AcquireProfilingLockFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixAcquireProfilingLockFlagBitsKHR
                          showTableAcquireProfilingLockFlagBitsKHR
                          conNameAcquireProfilingLockFlagBitsKHR
                          AcquireProfilingLockFlagBitsKHR


type KHR_PERFORMANCE_QUERY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION"
pattern KHR_PERFORMANCE_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PERFORMANCE_QUERY_SPEC_VERSION = 1


type KHR_PERFORMANCE_QUERY_EXTENSION_NAME = "VK_KHR_performance_query"

-- No documentation found for TopLevel "VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME"
pattern KHR_PERFORMANCE_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PERFORMANCE_QUERY_EXTENSION_NAME = "VK_KHR_performance_query"

