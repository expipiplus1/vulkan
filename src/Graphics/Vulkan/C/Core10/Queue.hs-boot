{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  , VkFence
  , VkPipelineStageFlagBits
  , VkPipelineStageFlags
  , VkQueue
  , VkSemaphore
  , VkSubmitInfo
  , FN_vkDeviceWaitIdle
  , PFN_vkDeviceWaitIdle
  , FN_vkGetDeviceQueue
  , PFN_vkGetDeviceQueue
  , FN_vkQueueSubmit
  , PFN_vkQueueSubmit
  , FN_vkQueueWaitIdle
  , PFN_vkQueueWaitIdle
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
  ( VkDevice
  )


-- | Dummy data to tag the 'Ptr' with
data VkCommandBuffer_T
-- | VkCommandBuffer - Opaque handle to a command buffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX',
-- 'VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkAllocateCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkBeginCommandBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.vkCmdBeginConditionalRenderingEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdBeginDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBeginQueryIndexedEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.vkCmdBeginRenderPass2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBeginTransformFeedbackEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindPipeline',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdBindShadingRateImageNV',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBindTransformFeedbackBuffersEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdCopyAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerBeginEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerEndEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerInsertEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatch',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkCmdDispatchBase',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group.vkCmdDispatchBaseKHR',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDraw',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexed',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count.vkCmdDrawIndexedIndirectCountKHR',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdDrawIndirectByteCountEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count.vkCmdDrawIndirectCountKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectCountNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksNV',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.vkCmdEndConditionalRenderingEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdEndDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdEndQueryIndexedEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndRenderPass',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.vkCmdEndRenderPass2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdEndTransformFeedbackEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCmdInsertDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdNextSubpass',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.vkCmdNextSubpass2KHR',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCmdProcessCommandsNVX',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPushConstants',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCmdReserveSpaceForCommandsNVX',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResolveImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetBlendConstants',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.vkCmdSetCheckpointNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdSetCoarseSampleOrderNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetDepthBias',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetDepthBounds',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkCmdSetDeviceMask',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group.vkCmdSetDeviceMaskKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles.vkCmdSetDiscardRectangleEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive.vkCmdSetExclusiveScissorNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetLineWidth',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.vkCmdSetSampleLocationsEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetScissor',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilReference',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilWriteMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetViewport',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdSetViewportShadingRatePaletteNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling.vkCmdSetViewportWScalingNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdTraceRaysNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdWriteAccelerationStructuresPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker.vkCmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkEndCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkResetCommandBuffer'
type VkCommandBuffer = Ptr VkCommandBuffer_T

-- | Dummy data to tag the 'Ptr' with
data VkFence_T
-- | VkFence - Opaque handle to a fence object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkAcquireNextImageInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkFenceGetFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkFenceGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR',
-- 'Graphics.Vulkan.C.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'vkQueueSubmit',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkRegisterDeviceEventEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkRegisterDisplayEventEXT',
-- 'Graphics.Vulkan.C.Core10.Fence.vkResetFences',
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences'
type VkFence = Ptr VkFence_T

data VkPipelineStageFlagBits

-- | VkPipelineStageFlags - Bitmask of VkPipelineStageFlagBits
--
-- = Description
--
-- 'VkPipelineStageFlags' is a bitmask type for setting a mask of zero or
-- more 'VkPipelineStageFlagBits'.
--
-- = See Also
--
-- 'VkPipelineStageFlagBits',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.VkQueueFamilyCheckpointPropertiesNV',
-- 'VkSubmitInfo', 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassDependency2KHR',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
type VkPipelineStageFlags = VkPipelineStageFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkQueue_T
-- | VkQueue - Opaque handle to a queue object
--
-- = See Also
--
-- 'vkGetDeviceQueue',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.vkGetDeviceQueue2',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints.vkGetQueueCheckpointDataNV',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkQueueBeginDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkQueueEndDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkQueueInsertDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR',
-- 'vkQueueSubmit', 'vkQueueWaitIdle'
type VkQueue = Ptr VkQueue_T

-- | Dummy data to tag the 'Ptr' with
data VkSemaphore_T
-- | VkSemaphore - Opaque handle to a semaphore object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkAcquireNextImageInfoKHR',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkImportSemaphoreWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkSemaphoreGetFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkSemaphoreGetWin32HandleInfoKHR',
-- 'VkSubmitInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkDestroySemaphore'
type VkSemaphore = Ptr VkSemaphore_T

data VkSubmitInfo

type FN_vkDeviceWaitIdle = ("device" ::: VkDevice) -> IO VkResult
type PFN_vkDeviceWaitIdle = FunPtr FN_vkDeviceWaitIdle

type FN_vkGetDeviceQueue = ("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
type PFN_vkGetDeviceQueue = FunPtr FN_vkGetDeviceQueue

type FN_vkQueueSubmit = ("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult
type PFN_vkQueueSubmit = FunPtr FN_vkQueueSubmit

type FN_vkQueueWaitIdle = ("queue" ::: VkQueue) -> IO VkResult
type PFN_vkQueueWaitIdle = FunPtr FN_vkQueueWaitIdle
