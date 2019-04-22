{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Query
  ( VkQueryPipelineStatisticFlagBits
  , VkQueryPipelineStatisticFlags
  , VkQueryPool
  , VkQueryPoolCreateFlags
  , VkQueryPoolCreateInfo
  , VkQueryResultFlagBits
  , VkQueryResultFlags
  , VkQueryType
  , FN_vkCreateQueryPool
  , PFN_vkCreateQueryPool
  , FN_vkDestroyQueryPool
  , PFN_vkDestroyQueryPool
  , FN_vkGetQueryPoolResults
  , PFN_vkGetQueryPoolResults
  ) where

import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  , VkDeviceSize
  )


data VkQueryPipelineStatisticFlagBits

-- | VkQueryPipelineStatisticFlags - Bitmask of
-- VkQueryPipelineStatisticFlagBits
--
-- = Description
--
-- 'VkQueryPipelineStatisticFlags' is a bitmask type for setting a mask of
-- zero or more 'VkQueryPipelineStatisticFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'VkQueryPipelineStatisticFlagBits', 'VkQueryPoolCreateInfo'
type VkQueryPipelineStatisticFlags = VkQueryPipelineStatisticFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkQueryPool_T
-- | VkQueryPool - Opaque handle to a query pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBeginQueryIndexedEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdEndQueryIndexedEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdWriteAccelerationStructuresPropertiesNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp',
-- 'vkCreateQueryPool', 'vkDestroyQueryPool', 'vkGetQueryPoolResults',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset.vkResetQueryPoolEXT'
type VkQueryPool = Ptr VkQueryPool_T

data VkQueryPoolCreateFlags

data VkQueryPoolCreateInfo

data VkQueryResultFlagBits

-- | VkQueryResultFlags - Bitmask of VkQueryResultFlagBits
--
-- = Description
--
-- 'VkQueryResultFlags' is a bitmask type for setting a mask of zero or
-- more 'VkQueryResultFlagBits'.
--
-- = See Also
--
-- 'VkQueryResultFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'vkGetQueryPoolResults'
type VkQueryResultFlags = VkQueryResultFlagBits

data VkQueryType

type FN_vkCreateQueryPool = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult
type PFN_vkCreateQueryPool = FunPtr FN_vkCreateQueryPool

type FN_vkDestroyQueryPool = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyQueryPool = FunPtr FN_vkDestroyQueryPool

type FN_vkGetQueryPoolResults = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult
type PFN_vkGetQueryPoolResults = FunPtr FN_vkGetQueryPoolResults
