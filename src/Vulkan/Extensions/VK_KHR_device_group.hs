{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_device_group  ( pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR
                                              , pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR
                                              , pattern PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR
                                              , pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR
                                              , pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR
                                              , pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR
                                              , pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
                                              , pattern PIPELINE_CREATE_DISPATCH_BASE_KHR
                                              , pattern DEPENDENCY_DEVICE_GROUP_BIT_KHR
                                              , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR
                                              , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR
                                              , pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
                                              , getDeviceGroupPeerMemoryFeaturesKHR
                                              , cmdSetDeviceMaskKHR
                                              , cmdDispatchBaseKHR
                                              , PeerMemoryFeatureFlagsKHR
                                              , MemoryAllocateFlagsKHR
                                              , PeerMemoryFeatureFlagBitsKHR
                                              , MemoryAllocateFlagBitsKHR
                                              , MemoryAllocateFlagsInfoKHR
                                              , BindBufferMemoryDeviceGroupInfoKHR
                                              , BindImageMemoryDeviceGroupInfoKHR
                                              , DeviceGroupRenderPassBeginInfoKHR
                                              , DeviceGroupCommandBufferBeginInfoKHR
                                              , DeviceGroupSubmitInfoKHR
                                              , DeviceGroupBindSparseInfoKHR
                                              , KHR_DEVICE_GROUP_SPEC_VERSION
                                              , pattern KHR_DEVICE_GROUP_SPEC_VERSION
                                              , KHR_DEVICE_GROUP_EXTENSION_NAME
                                              , pattern KHR_DEVICE_GROUP_EXTENSION_NAME
                                              , SurfaceKHR(..)
                                              , SwapchainKHR(..)
                                              , DeviceGroupPresentCapabilitiesKHR(..)
                                              , ImageSwapchainCreateInfoKHR(..)
                                              , BindImageMemorySwapchainInfoKHR(..)
                                              , AcquireNextImageInfoKHR(..)
                                              , DeviceGroupPresentInfoKHR(..)
                                              , DeviceGroupSwapchainCreateInfoKHR(..)
                                              , getDeviceGroupPresentCapabilitiesKHR
                                              , getDeviceGroupSurfacePresentModesKHR
                                              , acquireNextImage2KHR
                                              , acquireNextImage2KHRSafe
                                              , getPhysicalDevicePresentRectanglesKHR
                                              , DeviceGroupPresentModeFlagBitsKHR(..)
                                              , DeviceGroupPresentModeFlagsKHR
                                              , SwapchainCreateFlagBitsKHR(..)
                                              , SwapchainCreateFlagsKHR
                                              ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (cmdDispatchBase)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (cmdSetDeviceMask)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (getDeviceGroupPeerMemoryFeatures)
import Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindBufferMemoryDeviceGroupInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindImageMemoryDeviceGroupInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupBindSparseInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupCommandBufferBeginInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupRenderPassBeginInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupSubmitInfo)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (MemoryAllocateFlagsInfo)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(DEPENDENCY_DEVICE_GROUP_BIT))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT))
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits(MEMORY_ALLOCATE_DEVICE_MASK_BIT))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(PEER_MEMORY_FEATURE_COPY_DST_BIT))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(PEER_MEMORY_FEATURE_COPY_SRC_BIT))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(PEER_MEMORY_FEATURE_GENERIC_DST_BIT))
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlagBits(PEER_MEMORY_FEATURE_GENERIC_SRC_BIT))
import Vulkan.Core11.Promoted_From_VK_KHR_device_group (pattern PIPELINE_CREATE_DISPATCH_BASE)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO))
import Vulkan.Extensions.VK_KHR_swapchain (acquireNextImage2KHR)
import Vulkan.Extensions.VK_KHR_swapchain (acquireNextImage2KHRSafe)
import Vulkan.Extensions.VK_KHR_swapchain (getDeviceGroupPresentCapabilitiesKHR)
import Vulkan.Extensions.VK_KHR_swapchain (getDeviceGroupSurfacePresentModesKHR)
import Vulkan.Extensions.VK_KHR_swapchain (getPhysicalDevicePresentRectanglesKHR)
import Vulkan.Extensions.VK_KHR_swapchain (AcquireNextImageInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (BindImageMemorySwapchainInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentCapabilitiesKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagsKHR)
import Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupSwapchainCreateInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (ImageSwapchainCreateInfoKHR(..))
import Vulkan.Extensions.Handles (SurfaceKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR = STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO


-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR"
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR = PEER_MEMORY_FEATURE_COPY_SRC_BIT


-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR"
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR = PEER_MEMORY_FEATURE_COPY_DST_BIT


-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR"
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR = PEER_MEMORY_FEATURE_GENERIC_SRC_BIT


-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR"
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR = PEER_MEMORY_FEATURE_GENERIC_DST_BIT


-- No documentation found for TopLevel "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR"
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR = MEMORY_ALLOCATE_DEVICE_MASK_BIT


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR"
pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR = PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT


-- No documentation found for TopLevel "VK_PIPELINE_CREATE_DISPATCH_BASE_KHR"
pattern PIPELINE_CREATE_DISPATCH_BASE_KHR = PIPELINE_CREATE_DISPATCH_BASE


-- No documentation found for TopLevel "VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR"
pattern DEPENDENCY_DEVICE_GROUP_BIT_KHR = DEPENDENCY_DEVICE_GROUP_BIT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR = STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR = STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO


-- No documentation found for TopLevel "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT


-- No documentation found for TopLevel "vkGetDeviceGroupPeerMemoryFeaturesKHR"
getDeviceGroupPeerMemoryFeaturesKHR = getDeviceGroupPeerMemoryFeatures


-- No documentation found for TopLevel "vkCmdSetDeviceMaskKHR"
cmdSetDeviceMaskKHR = cmdSetDeviceMask


-- No documentation found for TopLevel "vkCmdDispatchBaseKHR"
cmdDispatchBaseKHR = cmdDispatchBase


-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagsKHR"
type PeerMemoryFeatureFlagsKHR = PeerMemoryFeatureFlags


-- No documentation found for TopLevel "VkMemoryAllocateFlagsKHR"
type MemoryAllocateFlagsKHR = MemoryAllocateFlags


-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagBitsKHR"
type PeerMemoryFeatureFlagBitsKHR = PeerMemoryFeatureFlagBits


-- No documentation found for TopLevel "VkMemoryAllocateFlagBitsKHR"
type MemoryAllocateFlagBitsKHR = MemoryAllocateFlagBits


-- No documentation found for TopLevel "VkMemoryAllocateFlagsInfoKHR"
type MemoryAllocateFlagsInfoKHR = MemoryAllocateFlagsInfo


-- No documentation found for TopLevel "VkBindBufferMemoryDeviceGroupInfoKHR"
type BindBufferMemoryDeviceGroupInfoKHR = BindBufferMemoryDeviceGroupInfo


-- No documentation found for TopLevel "VkBindImageMemoryDeviceGroupInfoKHR"
type BindImageMemoryDeviceGroupInfoKHR = BindImageMemoryDeviceGroupInfo


-- No documentation found for TopLevel "VkDeviceGroupRenderPassBeginInfoKHR"
type DeviceGroupRenderPassBeginInfoKHR = DeviceGroupRenderPassBeginInfo


-- No documentation found for TopLevel "VkDeviceGroupCommandBufferBeginInfoKHR"
type DeviceGroupCommandBufferBeginInfoKHR = DeviceGroupCommandBufferBeginInfo


-- No documentation found for TopLevel "VkDeviceGroupSubmitInfoKHR"
type DeviceGroupSubmitInfoKHR = DeviceGroupSubmitInfo


-- No documentation found for TopLevel "VkDeviceGroupBindSparseInfoKHR"
type DeviceGroupBindSparseInfoKHR = DeviceGroupBindSparseInfo


type KHR_DEVICE_GROUP_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_SPEC_VERSION"
pattern KHR_DEVICE_GROUP_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEVICE_GROUP_SPEC_VERSION = 4


type KHR_DEVICE_GROUP_EXTENSION_NAME = "VK_KHR_device_group"

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_EXTENSION_NAME"
pattern KHR_DEVICE_GROUP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEVICE_GROUP_EXTENSION_NAME = "VK_KHR_device_group"

