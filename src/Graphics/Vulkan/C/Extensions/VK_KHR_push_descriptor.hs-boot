{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( VkPhysicalDevicePushDescriptorPropertiesKHR
  , FN_vkCmdPushDescriptorSetKHR
  , PFN_vkCmdPushDescriptorSetKHR
  , FN_vkCmdPushDescriptorSetWithTemplateKHR
  , PFN_vkCmdPushDescriptorSetWithTemplateKHR
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
import {-# source #-} Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkWriteDescriptorSet
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pass
  ( VkPipelineBindPoint
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipelineLayout
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplate
  )


data VkPhysicalDevicePushDescriptorPropertiesKHR

type FN_vkCmdPushDescriptorSetKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ()
type PFN_vkCmdPushDescriptorSetKHR = FunPtr FN_vkCmdPushDescriptorSetKHR

type FN_vkCmdPushDescriptorSetWithTemplateKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ()
type PFN_vkCmdPushDescriptorSetWithTemplateKHR = FunPtr FN_vkCmdPushDescriptorSetWithTemplateKHR
