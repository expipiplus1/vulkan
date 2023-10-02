{-# language CPP #-}
-- | = Name
--
-- VK_INTEL_performance_query - device extension
--
-- == VK_INTEL_performance_query
--
-- [__Name String__]
--     @VK_INTEL_performance_query@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     211
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__; __Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Lionel Landwerlin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_INTEL_performance_query] @llandwerlin%0A*Here describe the issue or question you have about the VK_INTEL_performance_query extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-05-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Lionel Landwerlin, Intel
--
--     -   Piotr Maciejewski, Intel
--
-- == Description
--
-- This extension allows an application to capture performance data to be
-- interpreted by a external application or library.
--
-- Such a library is available at :
-- <https://github.com/intel/metrics-discovery>
--
-- Performance analysis tools such as
-- <https://software.intel.com/content/www/us/en/develop/tools/graphics-performance-analyzers.html Graphics Performance Analyzers>
-- make use of this extension and the metrics-discovery library to present
-- the data in a human readable way.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.PerformanceConfigurationINTEL'
--
-- == New Commands
--
-- -   'acquirePerformanceConfigurationINTEL'
--
-- -   'cmdSetPerformanceMarkerINTEL'
--
-- -   'cmdSetPerformanceOverrideINTEL'
--
-- -   'cmdSetPerformanceStreamMarkerINTEL'
--
-- -   'getPerformanceParameterINTEL'
--
-- -   'initializePerformanceApiINTEL'
--
-- -   'queueSetPerformanceConfigurationINTEL'
--
-- -   'releasePerformanceConfigurationINTEL'
--
-- -   'uninitializePerformanceApiINTEL'
--
-- == New Structures
--
-- -   'InitializePerformanceApiInfoINTEL'
--
-- -   'PerformanceConfigurationAcquireInfoINTEL'
--
-- -   'PerformanceMarkerInfoINTEL'
--
-- -   'PerformanceOverrideInfoINTEL'
--
-- -   'PerformanceStreamMarkerInfoINTEL'
--
-- -   'PerformanceValueINTEL'
--
-- -   Extending 'Vulkan.Core10.Query.QueryPoolCreateInfo':
--
--     -   'QueryPoolCreateInfoINTEL'
--
--     -   'QueryPoolPerformanceQueryCreateInfoINTEL'
--
-- == New Unions
--
-- -   'PerformanceValueDataINTEL'
--
-- == New Enums
--
-- -   'PerformanceConfigurationTypeINTEL'
--
-- -   'PerformanceOverrideTypeINTEL'
--
-- -   'PerformanceParameterTypeINTEL'
--
-- -   'PerformanceValueTypeINTEL'
--
-- -   'QueryPoolSamplingModeINTEL'
--
-- == New Enum Constants
--
-- -   'INTEL_PERFORMANCE_QUERY_EXTENSION_NAME'
--
-- -   'INTEL_PERFORMANCE_QUERY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_INTEL'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL'
--
--     -   'STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO_INTEL'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL'
--
-- == Example Code
--
-- > // A previously created device
-- > VkDevice device;
-- >
-- > // A queue derived from the device
-- > VkQueue queue;
-- >
-- > VkInitializePerformanceApiInfoINTEL performanceApiInfoIntel = {
-- >   VK_STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL,
-- >   NULL,
-- >   NULL
-- > };
-- >
-- > vkInitializePerformanceApiINTEL(
-- >   device,
-- >   &performanceApiInfoIntel);
-- >
-- > VkQueryPoolPerformanceQueryCreateInfoINTEL queryPoolIntel = {
-- >   VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO_INTEL,
-- >   NULL,
-- >   VK_QUERY_POOL_SAMPLING_MODE_MANUAL_INTEL,
-- > };
-- >
-- > VkQueryPoolCreateInfo queryPoolCreateInfo = {
-- >   VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO,
-- >   &queryPoolIntel,
-- >   0,
-- >   VK_QUERY_TYPE_PERFORMANCE_QUERY_INTEL,
-- >   1,
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
-- > // A command buffer we want to record counters on
-- > VkCommandBuffer commandBuffer;
-- >
-- > VkCommandBufferBeginInfo commandBufferBeginInfo = {
-- >   VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
-- >   NULL,
-- >   VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
-- >   NULL
-- > };
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
-- > VkPerformanceConfigurationAcquireInfoINTEL performanceConfigurationAcquireInfo = {
-- >   VK_STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL,
-- >   NULL,
-- >   VK_PERFORMANCE_CONFIGURATION_TYPE_COMMAND_QUEUE_METRICS_DISCOVERY_ACTIVATED_INTEL
-- > };
-- >
-- > VkPerformanceConfigurationINTEL performanceConfigurationIntel;
-- >
-- > result = vkAcquirePerformanceConfigurationINTEL(
-- >   device,
-- >   &performanceConfigurationAcquireInfo,
-- >   &performanceConfigurationIntel);
-- >
-- > vkQueueSetPerformanceConfigurationINTEL(queue, performanceConfigurationIntel);
-- >
-- > assert(VK_SUCCESS == result);
-- >
-- > // Submit the command buffer and wait for its completion
-- > // ...
-- >
-- > result = vkReleasePerformanceConfigurationINTEL(
-- >   device,
-- >   performanceConfigurationIntel);
-- >
-- > assert(VK_SUCCESS == result);
-- >
-- > // Get the report size from metrics-discovery's QueryReportSize
-- >
-- > result = vkGetQueryPoolResults(
-- >   device,
-- >   queryPool,
-- >   0, 1, QueryReportSize,
-- >   data, QueryReportSize, 0);
-- >
-- > assert(VK_SUCCESS == result);
-- >
-- > // The data can then be passed back to metrics-discovery from which
-- > // human readable values can be queried.
--
-- == Version History
--
-- -   Revision 2, 2020-03-06 (Lionel Landwerlin)
--
--     -   Rename VkQueryPoolCreateInfoINTEL in
--         VkQueryPoolPerformanceQueryCreateInfoINTEL
--
-- -   Revision 1, 2018-05-16 (Lionel Landwerlin)
--
--     -   Initial revision
--
-- == See Also
--
-- 'InitializePerformanceApiInfoINTEL',
-- 'PerformanceConfigurationAcquireInfoINTEL',
-- 'Vulkan.Extensions.Handles.PerformanceConfigurationINTEL',
-- 'PerformanceConfigurationTypeINTEL', 'PerformanceMarkerInfoINTEL',
-- 'PerformanceOverrideInfoINTEL', 'PerformanceOverrideTypeINTEL',
-- 'PerformanceParameterTypeINTEL', 'PerformanceStreamMarkerInfoINTEL',
-- 'PerformanceValueDataINTEL', 'PerformanceValueINTEL',
-- 'PerformanceValueTypeINTEL', 'QueryPoolCreateInfoINTEL',
-- 'QueryPoolPerformanceQueryCreateInfoINTEL',
-- 'QueryPoolSamplingModeINTEL', 'acquirePerformanceConfigurationINTEL',
-- 'cmdSetPerformanceMarkerINTEL', 'cmdSetPerformanceOverrideINTEL',
-- 'cmdSetPerformanceStreamMarkerINTEL', 'getPerformanceParameterINTEL',
-- 'initializePerformanceApiINTEL',
-- 'queueSetPerformanceConfigurationINTEL',
-- 'releasePerformanceConfigurationINTEL',
-- 'uninitializePerformanceApiINTEL'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_INTEL_performance_query Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_INTEL_performance_query  ( InitializePerformanceApiInfoINTEL
                                                     , PerformanceConfigurationAcquireInfoINTEL
                                                     , PerformanceMarkerInfoINTEL
                                                     , PerformanceOverrideInfoINTEL
                                                     , PerformanceStreamMarkerInfoINTEL
                                                     , PerformanceValueINTEL
                                                     , QueryPoolPerformanceQueryCreateInfoINTEL
                                                     , PerformanceParameterTypeINTEL
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data InitializePerformanceApiInfoINTEL

instance ToCStruct InitializePerformanceApiInfoINTEL
instance Show InitializePerformanceApiInfoINTEL

instance FromCStruct InitializePerformanceApiInfoINTEL


data PerformanceConfigurationAcquireInfoINTEL

instance ToCStruct PerformanceConfigurationAcquireInfoINTEL
instance Show PerformanceConfigurationAcquireInfoINTEL

instance FromCStruct PerformanceConfigurationAcquireInfoINTEL


data PerformanceMarkerInfoINTEL

instance ToCStruct PerformanceMarkerInfoINTEL
instance Show PerformanceMarkerInfoINTEL

instance FromCStruct PerformanceMarkerInfoINTEL


data PerformanceOverrideInfoINTEL

instance ToCStruct PerformanceOverrideInfoINTEL
instance Show PerformanceOverrideInfoINTEL

instance FromCStruct PerformanceOverrideInfoINTEL


data PerformanceStreamMarkerInfoINTEL

instance ToCStruct PerformanceStreamMarkerInfoINTEL
instance Show PerformanceStreamMarkerInfoINTEL

instance FromCStruct PerformanceStreamMarkerInfoINTEL


data PerformanceValueINTEL

instance ToCStruct PerformanceValueINTEL
instance Show PerformanceValueINTEL

instance FromCStruct PerformanceValueINTEL


data QueryPoolPerformanceQueryCreateInfoINTEL

instance ToCStruct QueryPoolPerformanceQueryCreateInfoINTEL
instance Show QueryPoolPerformanceQueryCreateInfoINTEL

instance FromCStruct QueryPoolPerformanceQueryCreateInfoINTEL


data PerformanceParameterTypeINTEL

