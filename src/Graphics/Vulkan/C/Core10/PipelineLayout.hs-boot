{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  , VkPipelineLayoutCreateFlags
  , VkPipelineLayoutCreateInfo
  , VkPushConstantRange
  , VkShaderStageFlags
  , FN_vkCreatePipelineLayout
  , PFN_vkCreatePipelineLayout
  , FN_vkDestroyPipelineLayout
  , PFN_vkDestroyPipelineLayout
  ) where

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
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipelineLayout
  , VkShaderStageFlagBits
  )


-- | Dummy data to tag the 'Ptr' with
data VkDescriptorSetLayout_T
-- No documentation found for TopLevel "VkDescriptorSetLayout"
type VkDescriptorSetLayout = Ptr VkDescriptorSetLayout_T

data VkPipelineLayoutCreateFlags

data VkPipelineLayoutCreateInfo

data VkPushConstantRange

-- No documentation found for TopLevel "VkShaderStageFlags"
type VkShaderStageFlags = VkShaderStageFlagBits

type FN_vkCreatePipelineLayout = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult
type PFN_vkCreatePipelineLayout = FunPtr FN_vkCreatePipelineLayout

type FN_vkDestroyPipelineLayout = ("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyPipelineLayout = FunPtr FN_vkDestroyPipelineLayout
