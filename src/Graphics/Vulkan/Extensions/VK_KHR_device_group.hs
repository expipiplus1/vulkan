{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_device_group
  ( pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION
  , pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME
  , vkGetDeviceGroupPeerMemoryFeaturesKHR
  , vkCmdSetDeviceMaskKHR
  , vkCmdDispatchBaseKHR
  , VkPeerMemoryFeatureFlagBitsKHR
  , VkMemoryAllocateFlagBitsKHR
  , VkPeerMemoryFeatureFlagsKHR
  , VkMemoryAllocateFlagsKHR
  , VkMemoryAllocateFlagsInfoKHR
  , pattern VkMemoryAllocateFlagsInfoKHR
  , VkBindBufferMemoryDeviceGroupInfoKHR
  , pattern VkBindBufferMemoryDeviceGroupInfoKHR
  , VkBindImageMemoryDeviceGroupInfoKHR
  , pattern VkBindImageMemoryDeviceGroupInfoKHR
  , VkDeviceGroupRenderPassBeginInfoKHR
  , pattern VkDeviceGroupRenderPassBeginInfoKHR
  , VkDeviceGroupCommandBufferBeginInfoKHR
  , pattern VkDeviceGroupCommandBufferBeginInfoKHR
  , VkDeviceGroupSubmitInfoKHR
  , pattern VkDeviceGroupSubmitInfoKHR
  , VkDeviceGroupBindSparseInfoKHR
  , pattern VkDeviceGroupBindSparseInfoKHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Pass
  ( VkDependencyFlagBits(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkPipelineCreateFlagBits(..)
  , VkRect2D(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  , VkDeviceGroupBindSparseInfo(..)
  , VkDeviceGroupSubmitInfo(..)
  , VkDeviceGroupCommandBufferBeginInfo(..)
  , VkDeviceGroupRenderPassBeginInfo(..)
  , VkMemoryAllocateFlagsInfo(..)
  , VkMemoryAllocateFlags
  , VkMemoryAllocateFlagBits(..)
  , VkPeerMemoryFeatureFlagBits(..)
  , vkCmdDispatchBase
  , vkCmdSetDeviceMask
  , VkPeerMemoryFeatureFlags
  , vkGetDeviceGroupPeerMemoryFeatures
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , VkBindImageMemoryDeviceGroupInfo(..)
  , VkBindBufferMemoryDeviceGroupInfo(..)
  )


pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION = 3
pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME = "VK_KHR_device_group"
vkGetDeviceGroupPeerMemoryFeaturesKHR :: ("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()
vkGetDeviceGroupPeerMemoryFeaturesKHR = vkGetDeviceGroupPeerMemoryFeatures
vkCmdSetDeviceMaskKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()
vkCmdSetDeviceMaskKHR = vkCmdSetDeviceMask
vkCmdDispatchBaseKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
vkCmdDispatchBaseKHR = vkCmdDispatchBase
type VkPeerMemoryFeatureFlagBitsKHR = VkPeerMemoryFeatureFlagBits
type VkMemoryAllocateFlagBitsKHR = VkMemoryAllocateFlagBits
type VkPeerMemoryFeatureFlagsKHR = VkPeerMemoryFeatureFlags
type VkMemoryAllocateFlagsKHR = VkMemoryAllocateFlags
type VkMemoryAllocateFlagsInfoKHR = VkMemoryAllocateFlagsInfo


pattern VkMemoryAllocateFlagsInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("flags" ::: VkMemoryAllocateFlags) -> ("deviceMask" ::: Word32) -> VkMemoryAllocateFlagsInfoKHR
pattern VkMemoryAllocateFlagsInfoKHR vkSType vkNext vkFlags vkDeviceMask = VkMemoryAllocateFlagsInfo vkSType vkNext vkFlags vkDeviceMask
type VkBindBufferMemoryDeviceGroupInfoKHR = VkBindBufferMemoryDeviceGroupInfo


pattern VkBindBufferMemoryDeviceGroupInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceIndexCount" ::: Word32) -> ("pDeviceIndices" ::: Ptr Word32) -> VkBindBufferMemoryDeviceGroupInfoKHR
pattern VkBindBufferMemoryDeviceGroupInfoKHR vkSType vkNext vkDeviceIndexCount vkDeviceIndices = VkBindBufferMemoryDeviceGroupInfo vkSType vkNext vkDeviceIndexCount vkDeviceIndices
type VkBindImageMemoryDeviceGroupInfoKHR = VkBindImageMemoryDeviceGroupInfo


pattern VkBindImageMemoryDeviceGroupInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceIndexCount" ::: Word32) -> ("pDeviceIndices" ::: Ptr Word32) -> ("splitInstanceBindRegionCount" ::: Word32) -> ("pSplitInstanceBindRegions" ::: Ptr VkRect2D) -> VkBindImageMemoryDeviceGroupInfoKHR
pattern VkBindImageMemoryDeviceGroupInfoKHR vkSType vkNext vkDeviceIndexCount vkDeviceIndices vkSplitInstanceBindRegionCount vkSplitInstanceBindRegions = VkBindImageMemoryDeviceGroupInfo vkSType vkNext vkDeviceIndexCount vkDeviceIndices vkSplitInstanceBindRegionCount vkSplitInstanceBindRegions
type VkDeviceGroupRenderPassBeginInfoKHR = VkDeviceGroupRenderPassBeginInfo


pattern VkDeviceGroupRenderPassBeginInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceMask" ::: Word32) -> ("deviceRenderAreaCount" ::: Word32) -> ("pDeviceRenderAreas" ::: Ptr VkRect2D) -> VkDeviceGroupRenderPassBeginInfoKHR
pattern VkDeviceGroupRenderPassBeginInfoKHR vkSType vkNext vkDeviceMask vkDeviceRenderAreaCount vkDeviceRenderAreas = VkDeviceGroupRenderPassBeginInfo vkSType vkNext vkDeviceMask vkDeviceRenderAreaCount vkDeviceRenderAreas
type VkDeviceGroupCommandBufferBeginInfoKHR = VkDeviceGroupCommandBufferBeginInfo


pattern VkDeviceGroupCommandBufferBeginInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceMask" ::: Word32) -> VkDeviceGroupCommandBufferBeginInfoKHR
pattern VkDeviceGroupCommandBufferBeginInfoKHR vkSType vkNext vkDeviceMask = VkDeviceGroupCommandBufferBeginInfo vkSType vkNext vkDeviceMask
type VkDeviceGroupSubmitInfoKHR = VkDeviceGroupSubmitInfo


pattern VkDeviceGroupSubmitInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("waitSemaphoreCount" ::: Word32) -> ("pWaitSemaphoreDeviceIndices" ::: Ptr Word32) -> ("commandBufferCount" ::: Word32) -> ("pCommandBufferDeviceMasks" ::: Ptr Word32) -> ("signalSemaphoreCount" ::: Word32) -> ("pSignalSemaphoreDeviceIndices" ::: Ptr Word32) -> VkDeviceGroupSubmitInfoKHR
pattern VkDeviceGroupSubmitInfoKHR vkSType vkNext vkWaitSemaphoreCount vkWaitSemaphoreDeviceIndices vkCommandBufferCount vkCommandBufferDeviceMasks vkSignalSemaphoreCount vkSignalSemaphoreDeviceIndices = VkDeviceGroupSubmitInfo vkSType vkNext vkWaitSemaphoreCount vkWaitSemaphoreDeviceIndices vkCommandBufferCount vkCommandBufferDeviceMasks vkSignalSemaphoreCount vkSignalSemaphoreDeviceIndices
type VkDeviceGroupBindSparseInfoKHR = VkDeviceGroupBindSparseInfo


pattern VkDeviceGroupBindSparseInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("resourceDeviceIndex" ::: Word32) -> ("memoryDeviceIndex" ::: Word32) -> VkDeviceGroupBindSparseInfoKHR
pattern VkDeviceGroupBindSparseInfoKHR vkSType vkNext vkResourceDeviceIndex vkMemoryDeviceIndex = VkDeviceGroupBindSparseInfo vkSType vkNext vkResourceDeviceIndex vkMemoryDeviceIndex
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR = VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR = VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR = VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR = VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR :: VkMemoryAllocateFlagBits
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR = VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR = VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR = VK_PIPELINE_CREATE_DISPATCH_BASE
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR :: VkDependencyFlagBits
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR = VK_DEPENDENCY_DEVICE_GROUP_BIT
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
