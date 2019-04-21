{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( VkPhysicalDeviceTransformFeedbackFeaturesEXT
  , VkPhysicalDeviceTransformFeedbackPropertiesEXT
  , VkPipelineRasterizationStateStreamCreateFlagsEXT
  , VkPipelineRasterizationStateStreamCreateInfoEXT
  , FN_vkCmdBeginQueryIndexedEXT
  , PFN_vkCmdBeginQueryIndexedEXT
  , FN_vkCmdBeginTransformFeedbackEXT
  , PFN_vkCmdBeginTransformFeedbackEXT
  , FN_vkCmdBindTransformFeedbackBuffersEXT
  , PFN_vkCmdBindTransformFeedbackBuffersEXT
  , FN_vkCmdDrawIndirectByteCountEXT
  , PFN_vkCmdDrawIndirectByteCountEXT
  , FN_vkCmdEndQueryIndexedEXT
  , PFN_vkCmdEndQueryIndexedEXT
  , FN_vkCmdEndTransformFeedbackEXT
  , PFN_vkCmdEndTransformFeedbackEXT
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkQueryControlFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Query
  ( VkQueryPool
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkPhysicalDeviceTransformFeedbackFeaturesEXT

data VkPhysicalDeviceTransformFeedbackPropertiesEXT

data VkPipelineRasterizationStateStreamCreateFlagsEXT

data VkPipelineRasterizationStateStreamCreateInfoEXT

type FN_vkCmdBeginQueryIndexedEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ()
type PFN_vkCmdBeginQueryIndexedEXT = FunPtr FN_vkCmdBeginQueryIndexedEXT

type FN_vkCmdBeginTransformFeedbackEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkCmdBeginTransformFeedbackEXT = FunPtr FN_vkCmdBeginTransformFeedbackEXT

type FN_vkCmdBindTransformFeedbackBuffersEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkCmdBindTransformFeedbackBuffersEXT = FunPtr FN_vkCmdBindTransformFeedbackBuffersEXT

type FN_vkCmdDrawIndirectByteCountEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndirectByteCountEXT = FunPtr FN_vkCmdDrawIndirectByteCountEXT

type FN_vkCmdEndQueryIndexedEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ()
type PFN_vkCmdEndQueryIndexedEXT = FunPtr FN_vkCmdEndQueryIndexedEXT

type FN_vkCmdEndTransformFeedbackEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkCmdEndTransformFeedbackEXT = FunPtr FN_vkCmdEndTransformFeedbackEXT
