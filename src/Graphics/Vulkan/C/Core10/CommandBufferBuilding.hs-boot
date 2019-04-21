{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkBufferCopy
  , VkBufferImageCopy
  , VkBufferMemoryBarrier
  , VkClearAttachment
  , VkClearColorValue
  , VkClearDepthStencilValue
  , VkClearRect
  , VkClearValue
  , VkDispatchIndirectCommand
  , VkDrawIndexedIndirectCommand
  , VkDrawIndirectCommand
  , VkImageBlit
  , VkImageCopy
  , VkImageMemoryBarrier
  , VkImageResolve
  , VkImageSubresourceLayers
  , VkIndexType
  , VkMemoryBarrier
  , VkRenderPassBeginInfo
  , VkStencilFaceFlagBits
  , VkStencilFaceFlags
  , VkSubpassContents
  , FN_vkCmdBeginQuery
  , PFN_vkCmdBeginQuery
  , FN_vkCmdBeginRenderPass
  , PFN_vkCmdBeginRenderPass
  , FN_vkCmdBindDescriptorSets
  , PFN_vkCmdBindDescriptorSets
  , FN_vkCmdBindIndexBuffer
  , PFN_vkCmdBindIndexBuffer
  , FN_vkCmdBindPipeline
  , PFN_vkCmdBindPipeline
  , FN_vkCmdBindVertexBuffers
  , PFN_vkCmdBindVertexBuffers
  , FN_vkCmdBlitImage
  , PFN_vkCmdBlitImage
  , FN_vkCmdClearAttachments
  , PFN_vkCmdClearAttachments
  , FN_vkCmdClearColorImage
  , PFN_vkCmdClearColorImage
  , FN_vkCmdClearDepthStencilImage
  , PFN_vkCmdClearDepthStencilImage
  , FN_vkCmdCopyBuffer
  , PFN_vkCmdCopyBuffer
  , FN_vkCmdCopyBufferToImage
  , PFN_vkCmdCopyBufferToImage
  , FN_vkCmdCopyImage
  , PFN_vkCmdCopyImage
  , FN_vkCmdCopyImageToBuffer
  , PFN_vkCmdCopyImageToBuffer
  , FN_vkCmdCopyQueryPoolResults
  , PFN_vkCmdCopyQueryPoolResults
  , FN_vkCmdDispatch
  , PFN_vkCmdDispatch
  , FN_vkCmdDispatchIndirect
  , PFN_vkCmdDispatchIndirect
  , FN_vkCmdDraw
  , PFN_vkCmdDraw
  , FN_vkCmdDrawIndexed
  , PFN_vkCmdDrawIndexed
  , FN_vkCmdDrawIndexedIndirect
  , PFN_vkCmdDrawIndexedIndirect
  , FN_vkCmdDrawIndirect
  , PFN_vkCmdDrawIndirect
  , FN_vkCmdEndQuery
  , PFN_vkCmdEndQuery
  , FN_vkCmdEndRenderPass
  , PFN_vkCmdEndRenderPass
  , FN_vkCmdExecuteCommands
  , PFN_vkCmdExecuteCommands
  , FN_vkCmdFillBuffer
  , PFN_vkCmdFillBuffer
  , FN_vkCmdNextSubpass
  , PFN_vkCmdNextSubpass
  , FN_vkCmdPipelineBarrier
  , PFN_vkCmdPipelineBarrier
  , FN_vkCmdPushConstants
  , PFN_vkCmdPushConstants
  , FN_vkCmdResetEvent
  , PFN_vkCmdResetEvent
  , FN_vkCmdResetQueryPool
  , PFN_vkCmdResetQueryPool
  , FN_vkCmdResolveImage
  , PFN_vkCmdResolveImage
  , FN_vkCmdSetBlendConstants
  , PFN_vkCmdSetBlendConstants
  , FN_vkCmdSetDepthBias
  , PFN_vkCmdSetDepthBias
  , FN_vkCmdSetDepthBounds
  , PFN_vkCmdSetDepthBounds
  , FN_vkCmdSetEvent
  , PFN_vkCmdSetEvent
  , FN_vkCmdSetLineWidth
  , PFN_vkCmdSetLineWidth
  , FN_vkCmdSetScissor
  , PFN_vkCmdSetScissor
  , FN_vkCmdSetStencilCompareMask
  , PFN_vkCmdSetStencilCompareMask
  , FN_vkCmdSetStencilReference
  , PFN_vkCmdSetStencilReference
  , FN_vkCmdSetStencilWriteMask
  , PFN_vkCmdSetStencilWriteMask
  , FN_vkCmdSetViewport
  , PFN_vkCmdSetViewport
  , FN_vkCmdUpdateBuffer
  , PFN_vkCmdUpdateBuffer
  , FN_vkCmdWaitEvents
  , PFN_vkCmdWaitEvents
  , FN_vkCmdWriteTimestamp
  , PFN_vkCmdWriteTimestamp
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
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
import {-# source #-} Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorSet
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Event
  ( VkEvent
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout
  )
import {-# source #-} Graphics.Vulkan.C.Core10.ImageView
  ( VkImageSubresourceRange
  )
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pass
  ( VkPipelineBindPoint
  , VkDependencyFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkRect2D
  , VkViewport
  , VkPipeline
  , VkPipelineLayout
  )
import {-# source #-} Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Query
  ( VkQueryPool
  , VkQueryResultFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits
  , VkCommandBuffer
  , VkPipelineStageFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Sampler
  ( VkFilter
  )


data VkBufferCopy

data VkBufferImageCopy

data VkBufferMemoryBarrier

data VkClearAttachment

data VkClearColorValue

data VkClearDepthStencilValue

data VkClearRect

data VkClearValue

data VkDispatchIndirectCommand

data VkDrawIndexedIndirectCommand

data VkDrawIndirectCommand

data VkImageBlit

data VkImageCopy

data VkImageMemoryBarrier

data VkImageResolve

data VkImageSubresourceLayers

data VkIndexType

data VkMemoryBarrier

data VkRenderPassBeginInfo

data VkStencilFaceFlagBits

-- | VkStencilFaceFlags - Bitmask of VkStencilFaceFlagBits
--
-- = Description
--
-- 'VkStencilFaceFlags' is a bitmask type for setting a mask of zero or
-- more 'VkStencilFaceFlagBits'.
--
-- = See Also
--
-- 'VkStencilFaceFlagBits', 'vkCmdSetStencilCompareMask',
-- 'vkCmdSetStencilReference', 'vkCmdSetStencilWriteMask'
type VkStencilFaceFlags = VkStencilFaceFlagBits

data VkSubpassContents

type FN_vkCmdBeginQuery = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ()
type PFN_vkCmdBeginQuery = FunPtr FN_vkCmdBeginQuery

type FN_vkCmdBeginRenderPass = ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ()
type PFN_vkCmdBeginRenderPass = FunPtr FN_vkCmdBeginRenderPass

type FN_vkCmdBindDescriptorSets = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ()
type PFN_vkCmdBindDescriptorSets = FunPtr FN_vkCmdBindDescriptorSets

type FN_vkCmdBindIndexBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ()
type PFN_vkCmdBindIndexBuffer = FunPtr FN_vkCmdBindIndexBuffer

type FN_vkCmdBindPipeline = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ()
type PFN_vkCmdBindPipeline = FunPtr FN_vkCmdBindPipeline

type FN_vkCmdBindVertexBuffers = ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkCmdBindVertexBuffers = FunPtr FN_vkCmdBindVertexBuffers

type FN_vkCmdBlitImage = ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ()
type PFN_vkCmdBlitImage = FunPtr FN_vkCmdBlitImage

type FN_vkCmdClearAttachments = ("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ()
type PFN_vkCmdClearAttachments = FunPtr FN_vkCmdClearAttachments

type FN_vkCmdClearColorImage = ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
type PFN_vkCmdClearColorImage = FunPtr FN_vkCmdClearColorImage

type FN_vkCmdClearDepthStencilImage = ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
type PFN_vkCmdClearDepthStencilImage = FunPtr FN_vkCmdClearDepthStencilImage

type FN_vkCmdCopyBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ()
type PFN_vkCmdCopyBuffer = FunPtr FN_vkCmdCopyBuffer

type FN_vkCmdCopyBufferToImage = ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
type PFN_vkCmdCopyBufferToImage = FunPtr FN_vkCmdCopyBufferToImage

type FN_vkCmdCopyImage = ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ()
type PFN_vkCmdCopyImage = FunPtr FN_vkCmdCopyImage

type FN_vkCmdCopyImageToBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
type PFN_vkCmdCopyImageToBuffer = FunPtr FN_vkCmdCopyImageToBuffer

type FN_vkCmdCopyQueryPoolResults = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ()
type PFN_vkCmdCopyQueryPoolResults = FunPtr FN_vkCmdCopyQueryPoolResults

type FN_vkCmdDispatch = ("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
type PFN_vkCmdDispatch = FunPtr FN_vkCmdDispatch

type FN_vkCmdDispatchIndirect = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ()
type PFN_vkCmdDispatchIndirect = FunPtr FN_vkCmdDispatchIndirect

type FN_vkCmdDraw = ("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ()
type PFN_vkCmdDraw = FunPtr FN_vkCmdDraw

type FN_vkCmdDrawIndexed = ("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndexed = FunPtr FN_vkCmdDrawIndexed

type FN_vkCmdDrawIndexedIndirect = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndexedIndirect = FunPtr FN_vkCmdDrawIndexedIndirect

type FN_vkCmdDrawIndirect = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndirect = FunPtr FN_vkCmdDrawIndirect

type FN_vkCmdEndQuery = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
type PFN_vkCmdEndQuery = FunPtr FN_vkCmdEndQuery

type FN_vkCmdEndRenderPass = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdEndRenderPass = FunPtr FN_vkCmdEndRenderPass

type FN_vkCmdExecuteCommands = ("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
type PFN_vkCmdExecuteCommands = FunPtr FN_vkCmdExecuteCommands

type FN_vkCmdFillBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ()
type PFN_vkCmdFillBuffer = FunPtr FN_vkCmdFillBuffer

type FN_vkCmdNextSubpass = ("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ()
type PFN_vkCmdNextSubpass = FunPtr FN_vkCmdNextSubpass

type FN_vkCmdPipelineBarrier = ("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
type PFN_vkCmdPipelineBarrier = FunPtr FN_vkCmdPipelineBarrier

type FN_vkCmdPushConstants = ("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ()
type PFN_vkCmdPushConstants = FunPtr FN_vkCmdPushConstants

type FN_vkCmdResetEvent = ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
type PFN_vkCmdResetEvent = FunPtr FN_vkCmdResetEvent

type FN_vkCmdResetQueryPool = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
type PFN_vkCmdResetQueryPool = FunPtr FN_vkCmdResetQueryPool

type FN_vkCmdResolveImage = ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ()
type PFN_vkCmdResolveImage = FunPtr FN_vkCmdResolveImage

type FN_vkCmdSetBlendConstants = ("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ()
type PFN_vkCmdSetBlendConstants = FunPtr FN_vkCmdSetBlendConstants

type FN_vkCmdSetDepthBias = ("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ()
type PFN_vkCmdSetDepthBias = FunPtr FN_vkCmdSetDepthBias

type FN_vkCmdSetDepthBounds = ("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ()
type PFN_vkCmdSetDepthBounds = FunPtr FN_vkCmdSetDepthBounds

type FN_vkCmdSetEvent = ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
type PFN_vkCmdSetEvent = FunPtr FN_vkCmdSetEvent

type FN_vkCmdSetLineWidth = ("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ()
type PFN_vkCmdSetLineWidth = FunPtr FN_vkCmdSetLineWidth

type FN_vkCmdSetScissor = ("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ()
type PFN_vkCmdSetScissor = FunPtr FN_vkCmdSetScissor

type FN_vkCmdSetStencilCompareMask = ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ()
type PFN_vkCmdSetStencilCompareMask = FunPtr FN_vkCmdSetStencilCompareMask

type FN_vkCmdSetStencilReference = ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ()
type PFN_vkCmdSetStencilReference = FunPtr FN_vkCmdSetStencilReference

type FN_vkCmdSetStencilWriteMask = ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ()
type PFN_vkCmdSetStencilWriteMask = FunPtr FN_vkCmdSetStencilWriteMask

type FN_vkCmdSetViewport = ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ()
type PFN_vkCmdSetViewport = FunPtr FN_vkCmdSetViewport

type FN_vkCmdUpdateBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ()
type PFN_vkCmdUpdateBuffer = FunPtr FN_vkCmdUpdateBuffer

type FN_vkCmdWaitEvents = ("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
type PFN_vkCmdWaitEvents = FunPtr FN_vkCmdWaitEvents

type FN_vkCmdWriteTimestamp = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
type PFN_vkCmdWriteTimestamp = FunPtr FN_vkCmdWriteTimestamp
