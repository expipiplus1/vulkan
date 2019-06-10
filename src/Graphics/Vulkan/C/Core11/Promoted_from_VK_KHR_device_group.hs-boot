{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkDeviceGroupBindSparseInfo
  , VkDeviceGroupCommandBufferBeginInfo
  , VkDeviceGroupRenderPassBeginInfo
  , VkDeviceGroupSubmitInfo
  , VkMemoryAllocateFlagBits
  , VkMemoryAllocateFlags
  , VkMemoryAllocateFlagsInfo
  , VkPeerMemoryFeatureFlagBits
  , VkPeerMemoryFeatureFlags
  , FN_vkCmdDispatchBase
  , PFN_vkCmdDispatchBase
  , FN_vkCmdSetDeviceMask
  , PFN_vkCmdSetDeviceMask
  , FN_vkGetDeviceGroupPeerMemoryFeatures
  , PFN_vkGetDeviceGroupPeerMemoryFeatures
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
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkDeviceGroupBindSparseInfo

data VkDeviceGroupCommandBufferBeginInfo

data VkDeviceGroupRenderPassBeginInfo

data VkDeviceGroupSubmitInfo

data VkMemoryAllocateFlagBits

-- No documentation found for TopLevel "VkMemoryAllocateFlags"
type VkMemoryAllocateFlags = VkMemoryAllocateFlagBits

data VkMemoryAllocateFlagsInfo

data VkPeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "VkPeerMemoryFeatureFlags"
type VkPeerMemoryFeatureFlags = VkPeerMemoryFeatureFlagBits

type FN_vkCmdDispatchBase = ("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
type PFN_vkCmdDispatchBase = FunPtr FN_vkCmdDispatchBase

type FN_vkCmdSetDeviceMask = ("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()
type PFN_vkCmdSetDeviceMask = FunPtr FN_vkCmdSetDeviceMask

type FN_vkGetDeviceGroupPeerMemoryFeatures = ("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()
type PFN_vkGetDeviceGroupPeerMemoryFeatures = FunPtr FN_vkGetDeviceGroupPeerMemoryFeatures
