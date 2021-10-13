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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_performance_query] @alonorbach%0A<<Here describe the issue or question you have about the VK_KHR_performance_query extension>> >
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
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo',
--     'Vulkan.Extensions.VK_KHR_synchronization2.SubmitInfo2KHR':
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
-- __RESOLVED__ Specify the counter pass index at submit time instead, to
-- avoid requiring re-recording of command buffers when multiple counter
-- passes are needed.
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
-- > // recordedCounters is filled with our counters, we will look at one for posterity
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
module Vulkan.Extensions.VK_KHR_performance_query  ( AcquireProfilingLockInfoKHR
                                                   , PerformanceCounterDescriptionKHR
                                                   , PerformanceCounterKHR
                                                   , PerformanceQuerySubmitInfoKHR
                                                   , PhysicalDevicePerformanceQueryFeaturesKHR
                                                   , PhysicalDevicePerformanceQueryPropertiesKHR
                                                   , QueryPoolPerformanceCreateInfoKHR
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AcquireProfilingLockInfoKHR

instance ToCStruct AcquireProfilingLockInfoKHR
instance Show AcquireProfilingLockInfoKHR

instance FromCStruct AcquireProfilingLockInfoKHR


data PerformanceCounterDescriptionKHR

instance ToCStruct PerformanceCounterDescriptionKHR
instance Show PerformanceCounterDescriptionKHR

instance FromCStruct PerformanceCounterDescriptionKHR


data PerformanceCounterKHR

instance ToCStruct PerformanceCounterKHR
instance Show PerformanceCounterKHR

instance FromCStruct PerformanceCounterKHR


data PerformanceQuerySubmitInfoKHR

instance ToCStruct PerformanceQuerySubmitInfoKHR
instance Show PerformanceQuerySubmitInfoKHR

instance FromCStruct PerformanceQuerySubmitInfoKHR


data PhysicalDevicePerformanceQueryFeaturesKHR

instance ToCStruct PhysicalDevicePerformanceQueryFeaturesKHR
instance Show PhysicalDevicePerformanceQueryFeaturesKHR

instance FromCStruct PhysicalDevicePerformanceQueryFeaturesKHR


data PhysicalDevicePerformanceQueryPropertiesKHR

instance ToCStruct PhysicalDevicePerformanceQueryPropertiesKHR
instance Show PhysicalDevicePerformanceQueryPropertiesKHR

instance FromCStruct PhysicalDevicePerformanceQueryPropertiesKHR


data QueryPoolPerformanceCreateInfoKHR

instance ToCStruct QueryPoolPerformanceCreateInfoKHR
instance Show QueryPoolPerformanceCreateInfoKHR

instance FromCStruct QueryPoolPerformanceCreateInfoKHR

