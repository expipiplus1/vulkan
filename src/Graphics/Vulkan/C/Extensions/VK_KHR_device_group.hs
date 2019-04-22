{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( VkBindBufferMemoryDeviceGroupInfoKHR
  , pattern VkBindBufferMemoryDeviceGroupInfoKHR
  , VkBindImageMemoryDeviceGroupInfoKHR
  , pattern VkBindImageMemoryDeviceGroupInfoKHR
  , VkDeviceGroupBindSparseInfoKHR
  , pattern VkDeviceGroupBindSparseInfoKHR
  , VkDeviceGroupCommandBufferBeginInfoKHR
  , pattern VkDeviceGroupCommandBufferBeginInfoKHR
  , VkDeviceGroupRenderPassBeginInfoKHR
  , pattern VkDeviceGroupRenderPassBeginInfoKHR
  , VkDeviceGroupSubmitInfoKHR
  , pattern VkDeviceGroupSubmitInfoKHR
  , VkMemoryAllocateFlagBitsKHR
  , VkMemoryAllocateFlagsInfoKHR
  , pattern VkMemoryAllocateFlagsInfoKHR
  , VkMemoryAllocateFlagsKHR
  , VkPeerMemoryFeatureFlagBitsKHR
  , VkPeerMemoryFeatureFlagsKHR
  , vkCmdDispatchBaseKHR
  , vkCmdSetDeviceMaskKHR
  , vkGetDeviceGroupPeerMemoryFeaturesKHR
  , FN_vkGetDeviceGroupSurfacePresentModes2EXT
  , PFN_vkGetDeviceGroupSurfacePresentModes2EXT
  , vkGetDeviceGroupSurfacePresentModes2EXT
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  , pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME
  , pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , VkDeviceGroupPresentModeFlagBitsKHR(..)
  , VkDeviceGroupPresentModeFlagsKHR
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , vkGetDeviceGroupPresentCapabilitiesKHR
  , vkGetDeviceGroupSurfacePresentModesKHR
  , vkGetPhysicalDevicePresentRectanglesKHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  , VkImageSwapchainCreateInfoKHR(..)
  , VkBindImageMemorySwapchainInfoKHR(..)
  , VkAcquireNextImageInfoKHR(..)
  , VkDeviceGroupPresentInfoKHR(..)
  , VkDeviceGroupSwapchainCreateInfoKHR(..)
  , vkAcquireNextImage2KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkDependencyFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipelineCreateFlagBits(..)
  , VkRect2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkDeviceGroupBindSparseInfo(..)
  , VkDeviceGroupCommandBufferBeginInfo(..)
  , VkDeviceGroupRenderPassBeginInfo(..)
  , VkDeviceGroupSubmitInfo(..)
  , VkMemoryAllocateFlagBits(..)
  , VkMemoryAllocateFlagsInfo(..)
  , VkPeerMemoryFeatureFlagBits(..)
  , VkMemoryAllocateFlags
  , VkPeerMemoryFeatureFlags
  , vkCmdDispatchBase
  , vkCmdSetDeviceMask
  , vkGetDeviceGroupPeerMemoryFeatures
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( VkBindBufferMemoryDeviceGroupInfo(..)
  , VkBindImageMemoryDeviceGroupInfo(..)
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkDeviceGroupPresentModeFlagBitsKHR(..)
  , VkDeviceGroupPresentModeFlagsKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR(..)
  , VkBindImageMemorySwapchainInfoKHR(..)
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , VkDeviceGroupPresentInfoKHR(..)
  , VkDeviceGroupSwapchainCreateInfoKHR(..)
  , VkImageSwapchainCreateInfoKHR(..)
  , vkAcquireNextImage2KHR
  , vkGetDeviceGroupPresentCapabilitiesKHR
  , vkGetDeviceGroupSurfacePresentModesKHR
  , vkGetPhysicalDevicePresentRectanglesKHR
  , pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  )


-- No documentation found for TopLevel "VkBindBufferMemoryDeviceGroupInfoKHR"
type VkBindBufferMemoryDeviceGroupInfoKHR = VkBindBufferMemoryDeviceGroupInfo


-- No documentation found for TopLevel "VkBindBufferMemoryDeviceGroupInfoKHR"
pattern VkBindBufferMemoryDeviceGroupInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceIndexCount" ::: Word32) -> ("pDeviceIndices" ::: Ptr Word32) -> VkBindBufferMemoryDeviceGroupInfoKHR
pattern VkBindBufferMemoryDeviceGroupInfoKHR vkSType vkPNext vkDeviceIndexCount vkPDeviceIndices = VkBindBufferMemoryDeviceGroupInfo vkSType vkPNext vkDeviceIndexCount vkPDeviceIndices

-- No documentation found for TopLevel "VkBindImageMemoryDeviceGroupInfoKHR"
type VkBindImageMemoryDeviceGroupInfoKHR = VkBindImageMemoryDeviceGroupInfo


-- No documentation found for TopLevel "VkBindImageMemoryDeviceGroupInfoKHR"
pattern VkBindImageMemoryDeviceGroupInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceIndexCount" ::: Word32) -> ("pDeviceIndices" ::: Ptr Word32) -> ("splitInstanceBindRegionCount" ::: Word32) -> ("pSplitInstanceBindRegions" ::: Ptr VkRect2D) -> VkBindImageMemoryDeviceGroupInfoKHR
pattern VkBindImageMemoryDeviceGroupInfoKHR vkSType vkPNext vkDeviceIndexCount vkPDeviceIndices vkSplitInstanceBindRegionCount vkPSplitInstanceBindRegions = VkBindImageMemoryDeviceGroupInfo vkSType vkPNext vkDeviceIndexCount vkPDeviceIndices vkSplitInstanceBindRegionCount vkPSplitInstanceBindRegions

-- No documentation found for TopLevel "VkDeviceGroupBindSparseInfoKHR"
type VkDeviceGroupBindSparseInfoKHR = VkDeviceGroupBindSparseInfo


-- No documentation found for TopLevel "VkDeviceGroupBindSparseInfoKHR"
pattern VkDeviceGroupBindSparseInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("resourceDeviceIndex" ::: Word32) -> ("memoryDeviceIndex" ::: Word32) -> VkDeviceGroupBindSparseInfoKHR
pattern VkDeviceGroupBindSparseInfoKHR vkSType vkPNext vkResourceDeviceIndex vkMemoryDeviceIndex = VkDeviceGroupBindSparseInfo vkSType vkPNext vkResourceDeviceIndex vkMemoryDeviceIndex

-- No documentation found for TopLevel "VkDeviceGroupCommandBufferBeginInfoKHR"
type VkDeviceGroupCommandBufferBeginInfoKHR = VkDeviceGroupCommandBufferBeginInfo


-- No documentation found for TopLevel "VkDeviceGroupCommandBufferBeginInfoKHR"
pattern VkDeviceGroupCommandBufferBeginInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceMask" ::: Word32) -> VkDeviceGroupCommandBufferBeginInfoKHR
pattern VkDeviceGroupCommandBufferBeginInfoKHR vkSType vkPNext vkDeviceMask = VkDeviceGroupCommandBufferBeginInfo vkSType vkPNext vkDeviceMask

-- No documentation found for TopLevel "VkDeviceGroupRenderPassBeginInfoKHR"
type VkDeviceGroupRenderPassBeginInfoKHR = VkDeviceGroupRenderPassBeginInfo


-- No documentation found for TopLevel "VkDeviceGroupRenderPassBeginInfoKHR"
pattern VkDeviceGroupRenderPassBeginInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("deviceMask" ::: Word32) -> ("deviceRenderAreaCount" ::: Word32) -> ("pDeviceRenderAreas" ::: Ptr VkRect2D) -> VkDeviceGroupRenderPassBeginInfoKHR
pattern VkDeviceGroupRenderPassBeginInfoKHR vkSType vkPNext vkDeviceMask vkDeviceRenderAreaCount vkPDeviceRenderAreas = VkDeviceGroupRenderPassBeginInfo vkSType vkPNext vkDeviceMask vkDeviceRenderAreaCount vkPDeviceRenderAreas

-- No documentation found for TopLevel "VkDeviceGroupSubmitInfoKHR"
type VkDeviceGroupSubmitInfoKHR = VkDeviceGroupSubmitInfo


-- No documentation found for TopLevel "VkDeviceGroupSubmitInfoKHR"
pattern VkDeviceGroupSubmitInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("waitSemaphoreCount" ::: Word32) -> ("pWaitSemaphoreDeviceIndices" ::: Ptr Word32) -> ("commandBufferCount" ::: Word32) -> ("pCommandBufferDeviceMasks" ::: Ptr Word32) -> ("signalSemaphoreCount" ::: Word32) -> ("pSignalSemaphoreDeviceIndices" ::: Ptr Word32) -> VkDeviceGroupSubmitInfoKHR
pattern VkDeviceGroupSubmitInfoKHR vkSType vkPNext vkWaitSemaphoreCount vkPWaitSemaphoreDeviceIndices vkCommandBufferCount vkPCommandBufferDeviceMasks vkSignalSemaphoreCount vkPSignalSemaphoreDeviceIndices = VkDeviceGroupSubmitInfo vkSType vkPNext vkWaitSemaphoreCount vkPWaitSemaphoreDeviceIndices vkCommandBufferCount vkPCommandBufferDeviceMasks vkSignalSemaphoreCount vkPSignalSemaphoreDeviceIndices

-- No documentation found for TopLevel "VkMemoryAllocateFlagBitsKHR"
type VkMemoryAllocateFlagBitsKHR = VkMemoryAllocateFlagBits

-- No documentation found for TopLevel "VkMemoryAllocateFlagsInfoKHR"
type VkMemoryAllocateFlagsInfoKHR = VkMemoryAllocateFlagsInfo


-- No documentation found for TopLevel "VkMemoryAllocateFlagsInfoKHR"
pattern VkMemoryAllocateFlagsInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("flags" ::: VkMemoryAllocateFlags) -> ("deviceMask" ::: Word32) -> VkMemoryAllocateFlagsInfoKHR
pattern VkMemoryAllocateFlagsInfoKHR vkSType vkPNext vkFlags vkDeviceMask = VkMemoryAllocateFlagsInfo vkSType vkPNext vkFlags vkDeviceMask

-- No documentation found for TopLevel "VkMemoryAllocateFlagsKHR"
type VkMemoryAllocateFlagsKHR = VkMemoryAllocateFlags

-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagBitsKHR"
type VkPeerMemoryFeatureFlagBitsKHR = VkPeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "VkPeerMemoryFeatureFlagsKHR"
type VkPeerMemoryFeatureFlagsKHR = VkPeerMemoryFeatureFlags

-- No documentation found for TopLevel "vkCmdDispatchBaseKHR"
vkCmdDispatchBaseKHR :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
vkCmdDispatchBaseKHR = vkCmdDispatchBase

-- No documentation found for TopLevel "vkCmdSetDeviceMaskKHR"
vkCmdSetDeviceMaskKHR :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()
vkCmdSetDeviceMaskKHR = vkCmdSetDeviceMask

-- No documentation found for TopLevel "vkGetDeviceGroupPeerMemoryFeaturesKHR"
vkGetDeviceGroupPeerMemoryFeaturesKHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()
vkGetDeviceGroupPeerMemoryFeaturesKHR = vkGetDeviceGroupPeerMemoryFeatures

-- | vkGetDeviceGroupSurfacePresentModes2EXT - Query device group present
-- capabilities for a surface
--
-- = Parameters
--
-- -   @device@ is the logical device.
--
-- -   @pSurfaceInfo@ points to an instance of the
--     VkPhysicalDeviceSurfaceInfo2KHR structure, describing the surface
--     and other fixed parameters that would be consumed by
--     vkCreateSwapchainKHR.
--
-- -   @pModes@ is a pointer to a value of type
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagsKHR'
--     that is filled with the supported device group present modes for the
--     surface.
--
-- = Description
--
-- 'vkGetDeviceGroupSurfacePresentModes2EXT' behaves similarly to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetDeviceGroupSurfacePresentModesKHR',
-- with the ability to specify extended inputs via chained input
-- structures.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR'
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceGroupSurfacePresentModes2EXT" vkGetDeviceGroupSurfacePresentModes2EXT :: ("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult
#else
vkGetDeviceGroupSurfacePresentModes2EXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult
vkGetDeviceGroupSurfacePresentModes2EXT deviceCmds = mkVkGetDeviceGroupSurfacePresentModes2EXT (pVkGetDeviceGroupSurfacePresentModes2EXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupSurfacePresentModes2EXT
  :: FunPtr (("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult)
#endif

type FN_vkGetDeviceGroupSurfacePresentModes2EXT = ("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult
type PFN_vkGetDeviceGroupSurfacePresentModes2EXT = FunPtr FN_vkGetDeviceGroupSurfacePresentModes2EXT

-- No documentation found for TopLevel "VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR"
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR :: VkDependencyFlagBits
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT_KHR = VK_DEPENDENCY_DEVICE_GROUP_BIT

-- No documentation found for TopLevel "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_EXTENSION_NAME"
pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_DEVICE_GROUP_EXTENSION_NAME = "VK_KHR_device_group"

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_SPEC_VERSION"
pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DEVICE_GROUP_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR"
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR :: VkMemoryAllocateFlagBits
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT_KHR = VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT

-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR"
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT_KHR = VK_PEER_MEMORY_FEATURE_COPY_DST_BIT

-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR"
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT_KHR = VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT

-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR"
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT_KHR = VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT

-- No documentation found for TopLevel "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR"
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT_KHR = VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT

-- No documentation found for TopLevel "VK_PIPELINE_CREATE_DISPATCH_BASE_KHR"
pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_DISPATCH_BASE_KHR = VK_PIPELINE_CREATE_DISPATCH_BASE

-- No documentation found for TopLevel "VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR"
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR = VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR"
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO_KHR = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO_KHR = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO_KHR = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
