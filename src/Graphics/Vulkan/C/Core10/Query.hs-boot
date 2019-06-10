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

-- No documentation found for TopLevel "VkQueryPipelineStatisticFlags"
type VkQueryPipelineStatisticFlags = VkQueryPipelineStatisticFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkQueryPool_T
-- No documentation found for TopLevel "VkQueryPool"
type VkQueryPool = Ptr VkQueryPool_T

data VkQueryPoolCreateFlags

data VkQueryPoolCreateInfo

data VkQueryResultFlagBits

-- No documentation found for TopLevel "VkQueryResultFlags"
type VkQueryResultFlags = VkQueryResultFlagBits

data VkQueryType

type FN_vkCreateQueryPool = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult
type PFN_vkCreateQueryPool = FunPtr FN_vkCreateQueryPool

type FN_vkDestroyQueryPool = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyQueryPool = FunPtr FN_vkDestroyQueryPool

type FN_vkGetQueryPoolResults = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult
type PFN_vkGetQueryPoolResults = FunPtr FN_vkGetQueryPoolResults
