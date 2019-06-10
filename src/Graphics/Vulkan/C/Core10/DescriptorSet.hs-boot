{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkCopyDescriptorSet
  , VkDescriptorBufferInfo
  , VkDescriptorImageInfo
  , VkDescriptorPool
  , VkDescriptorPoolCreateFlagBits
  , VkDescriptorPoolCreateFlags
  , VkDescriptorPoolCreateInfo
  , VkDescriptorPoolResetFlags
  , VkDescriptorPoolSize
  , VkDescriptorSet
  , VkDescriptorSetAllocateInfo
  , VkDescriptorSetLayoutBinding
  , VkDescriptorSetLayoutCreateFlagBits
  , VkDescriptorSetLayoutCreateFlags
  , VkDescriptorSetLayoutCreateInfo
  , VkDescriptorType
  , VkWriteDescriptorSet
  , FN_vkAllocateDescriptorSets
  , PFN_vkAllocateDescriptorSets
  , FN_vkCreateDescriptorPool
  , PFN_vkCreateDescriptorPool
  , FN_vkCreateDescriptorSetLayout
  , PFN_vkCreateDescriptorSetLayout
  , FN_vkDestroyDescriptorPool
  , PFN_vkDestroyDescriptorPool
  , FN_vkDestroyDescriptorSetLayout
  , PFN_vkDestroyDescriptorSetLayout
  , FN_vkFreeDescriptorSets
  , PFN_vkFreeDescriptorSets
  , FN_vkResetDescriptorPool
  , PFN_vkResetDescriptorPool
  , FN_vkUpdateDescriptorSets
  , PFN_vkUpdateDescriptorSets
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
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  )


data VkCopyDescriptorSet

data VkDescriptorBufferInfo

data VkDescriptorImageInfo

-- | Dummy data to tag the 'Ptr' with
data VkDescriptorPool_T
-- No documentation found for TopLevel "VkDescriptorPool"
type VkDescriptorPool = Ptr VkDescriptorPool_T

data VkDescriptorPoolCreateFlagBits

-- No documentation found for TopLevel "VkDescriptorPoolCreateFlags"
type VkDescriptorPoolCreateFlags = VkDescriptorPoolCreateFlagBits

data VkDescriptorPoolCreateInfo

data VkDescriptorPoolResetFlags

data VkDescriptorPoolSize

-- | Dummy data to tag the 'Ptr' with
data VkDescriptorSet_T
-- No documentation found for TopLevel "VkDescriptorSet"
type VkDescriptorSet = Ptr VkDescriptorSet_T

data VkDescriptorSetAllocateInfo

data VkDescriptorSetLayoutBinding

data VkDescriptorSetLayoutCreateFlagBits

-- No documentation found for TopLevel "VkDescriptorSetLayoutCreateFlags"
type VkDescriptorSetLayoutCreateFlags = VkDescriptorSetLayoutCreateFlagBits

data VkDescriptorSetLayoutCreateInfo

data VkDescriptorType

data VkWriteDescriptorSet

type FN_vkAllocateDescriptorSets = ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
type PFN_vkAllocateDescriptorSets = FunPtr FN_vkAllocateDescriptorSets

type FN_vkCreateDescriptorPool = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult
type PFN_vkCreateDescriptorPool = FunPtr FN_vkCreateDescriptorPool

type FN_vkCreateDescriptorSetLayout = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult
type PFN_vkCreateDescriptorSetLayout = FunPtr FN_vkCreateDescriptorSetLayout

type FN_vkDestroyDescriptorPool = ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDescriptorPool = FunPtr FN_vkDestroyDescriptorPool

type FN_vkDestroyDescriptorSetLayout = ("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDescriptorSetLayout = FunPtr FN_vkDestroyDescriptorSetLayout

type FN_vkFreeDescriptorSets = ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
type PFN_vkFreeDescriptorSets = FunPtr FN_vkFreeDescriptorSets

type FN_vkResetDescriptorPool = ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult
type PFN_vkResetDescriptorPool = FunPtr FN_vkResetDescriptorPool

type FN_vkUpdateDescriptorSets = ("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()
type PFN_vkUpdateDescriptorSets = FunPtr FN_vkUpdateDescriptorSets
