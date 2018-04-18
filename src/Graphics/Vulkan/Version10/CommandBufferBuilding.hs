{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}

module Graphics.Vulkan.Version10.CommandBufferBuilding
  ( VkIndexType(..)
  , pattern VK_INDEX_TYPE_UINT16
  , pattern VK_INDEX_TYPE_UINT32
  , VkSubpassContents(..)
  , pattern VK_SUBPASS_CONTENTS_INLINE
  , pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
  , VkStencilFaceFlagBits(..)
  , pattern VK_STENCIL_FACE_FRONT_BIT
  , pattern VK_STENCIL_FACE_BACK_BIT
  , pattern VK_STENCIL_FRONT_AND_BACK
  , vkCmdBindPipeline
  , vkCmdSetViewport
  , vkCmdSetScissor
  , vkCmdSetLineWidth
  , vkCmdSetDepthBias
  , vkCmdSetBlendConstants
  , vkCmdSetDepthBounds
  , vkCmdSetStencilCompareMask
  , vkCmdSetStencilWriteMask
  , vkCmdSetStencilReference
  , vkCmdBindDescriptorSets
  , vkCmdBindIndexBuffer
  , vkCmdBindVertexBuffers
  , vkCmdDraw
  , vkCmdDrawIndexed
  , vkCmdDrawIndirect
  , vkCmdDrawIndexedIndirect
  , vkCmdDispatch
  , vkCmdDispatchIndirect
  , vkCmdCopyBuffer
  , vkCmdCopyImage
  , vkCmdBlitImage
  , vkCmdCopyBufferToImage
  , vkCmdCopyImageToBuffer
  , vkCmdUpdateBuffer
  , vkCmdFillBuffer
  , vkCmdClearColorImage
  , vkCmdClearDepthStencilImage
  , vkCmdClearAttachments
  , vkCmdResolveImage
  , vkCmdSetEvent
  , vkCmdResetEvent
  , vkCmdWaitEvents
  , vkCmdPipelineBarrier
  , vkCmdBeginQuery
  , vkCmdEndQuery
  , vkCmdResetQueryPool
  , vkCmdWriteTimestamp
  , vkCmdCopyQueryPoolResults
  , vkCmdPushConstants
  , vkCmdBeginRenderPass
  , vkCmdNextSubpass
  , vkCmdEndRenderPass
  , vkCmdExecuteCommands
  , VkClearRect(..)
  , VkImageSubresourceLayers(..)
  , VkMemoryBarrier(..)
  , VkBufferMemoryBarrier(..)
  , VkImageMemoryBarrier(..)
  , VkBufferCopy(..)
  , VkImageCopy(..)
  , VkImageBlit(..)
  , VkBufferImageCopy(..)
  , VkImageResolve(..)
  , VkRenderPassBeginInfo(..)
  , VkClearDepthStencilValue(..)
  , VkClearAttachment(..)
  , VkDrawIndirectCommand(..)
  , VkDrawIndexedIndirectCommand(..)
  , VkDispatchIndirectCommand(..)
  , VkClearColorValue(..)
  , VkClearValue(..)
  , VkStencilFaceFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Ptr
  ( castPtr
  , plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Version10.CommandBuffer
  ( VkQueryControlFlagBits(..)
  , VkQueryControlFlags
  )
import Graphics.Vulkan.Version10.Core
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Version10.DescriptorSet
  ( VkDescriptorSet
  )
import Graphics.Vulkan.Version10.DeviceInitialization
  ( VkExtent3D(..)
  , VkDeviceSize
  )
import Graphics.Vulkan.Version10.Event
  ( VkEvent
  )
import Graphics.Vulkan.Version10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.Version10.ImageView
  ( VkImageSubresourceRange(..)
  )
import Graphics.Vulkan.Version10.MemoryManagement
  ( VkImage
  , VkBuffer
  )
import Graphics.Vulkan.Version10.Pass
  ( VkFramebuffer
  , VkAccessFlags
  , VkDependencyFlagBits(..)
  , VkDependencyFlags
  , VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.Version10.Pipeline
  ( VkRenderPass
  , VkShaderStageFlagBits(..)
  , VkPipelineLayout
  , VkRect2D(..)
  , VkViewport(..)
  , VkPipeline
  )
import Graphics.Vulkan.Version10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.Version10.Query
  ( VkQueryResultFlagBits(..)
  , VkQueryResultFlags
  , VkQueryPool
  )
import Graphics.Vulkan.Version10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkPipelineStageFlags
  , VkCommandBuffer
  )
import Graphics.Vulkan.Version10.Sampler
  ( VkFilter(..)
  )
import Graphics.Vulkan.Version10.SparseResourceMemoryManagement
  ( VkOffset3D(..)
  , VkImageAspectFlags
  )


-- ** VkIndexType

-- | 
newtype VkIndexType = VkIndexType Int32
  deriving (Eq, Ord, Storable)

instance Show VkIndexType where
  showsPrec _ VK_INDEX_TYPE_UINT16 = showString "VK_INDEX_TYPE_UINT16"
  showsPrec _ VK_INDEX_TYPE_UINT32 = showString "VK_INDEX_TYPE_UINT32"
  showsPrec p (VkIndexType x) = showParen (p >= 11) (showString "VkIndexType " . showsPrec 11 x)

instance Read VkIndexType where
  readPrec = parens ( choose [ ("VK_INDEX_TYPE_UINT16", pure VK_INDEX_TYPE_UINT16)
                             , ("VK_INDEX_TYPE_UINT32", pure VK_INDEX_TYPE_UINT32)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIndexType")
                        v <- step readPrec
                        pure (VkIndexType v)
                        )
                    )

-- | 
pattern VK_INDEX_TYPE_UINT16 :: VkIndexType
pattern VK_INDEX_TYPE_UINT16 = VkIndexType 0

-- | 
pattern VK_INDEX_TYPE_UINT32 :: VkIndexType
pattern VK_INDEX_TYPE_UINT32 = VkIndexType 1
-- ** VkSubpassContents

-- | 
newtype VkSubpassContents = VkSubpassContents Int32
  deriving (Eq, Ord, Storable)

instance Show VkSubpassContents where
  showsPrec _ VK_SUBPASS_CONTENTS_INLINE = showString "VK_SUBPASS_CONTENTS_INLINE"
  showsPrec _ VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = showString "VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
  showsPrec p (VkSubpassContents x) = showParen (p >= 11) (showString "VkSubpassContents " . showsPrec 11 x)

instance Read VkSubpassContents where
  readPrec = parens ( choose [ ("VK_SUBPASS_CONTENTS_INLINE",                    pure VK_SUBPASS_CONTENTS_INLINE)
                             , ("VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS", pure VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSubpassContents")
                        v <- step readPrec
                        pure (VkSubpassContents v)
                        )
                    )

-- | 
pattern VK_SUBPASS_CONTENTS_INLINE :: VkSubpassContents
pattern VK_SUBPASS_CONTENTS_INLINE = VkSubpassContents 0

-- | 
pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS :: VkSubpassContents
pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = VkSubpassContents 1
-- ** VkStencilFaceFlagBits

-- | 
newtype VkStencilFaceFlagBits = VkStencilFaceFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkStencilFaceFlagBits where
  showsPrec _ VK_STENCIL_FACE_FRONT_BIT = showString "VK_STENCIL_FACE_FRONT_BIT"
  showsPrec _ VK_STENCIL_FACE_BACK_BIT = showString "VK_STENCIL_FACE_BACK_BIT"
  showsPrec _ VK_STENCIL_FRONT_AND_BACK = showString "VK_STENCIL_FRONT_AND_BACK"
  showsPrec p (VkStencilFaceFlagBits x) = showParen (p >= 11) (showString "VkStencilFaceFlagBits " . showsPrec 11 x)

instance Read VkStencilFaceFlagBits where
  readPrec = parens ( choose [ ("VK_STENCIL_FACE_FRONT_BIT", pure VK_STENCIL_FACE_FRONT_BIT)
                             , ("VK_STENCIL_FACE_BACK_BIT",  pure VK_STENCIL_FACE_BACK_BIT)
                             , ("VK_STENCIL_FRONT_AND_BACK", pure VK_STENCIL_FRONT_AND_BACK)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStencilFaceFlagBits")
                        v <- step readPrec
                        pure (VkStencilFaceFlagBits v)
                        )
                    )

-- | Front face
pattern VK_STENCIL_FACE_FRONT_BIT :: VkStencilFaceFlagBits
pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlagBits 0x00000001

-- | Back face
pattern VK_STENCIL_FACE_BACK_BIT :: VkStencilFaceFlagBits
pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlagBits 0x00000002

-- | Front and back faces
pattern VK_STENCIL_FRONT_AND_BACK :: VkStencilFaceFlagBits
pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlagBits 0x00000003
-- | 
foreign import ccall "vkCmdBindPipeline" vkCmdBindPipeline :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ()
-- | 
foreign import ccall "vkCmdSetViewport" vkCmdSetViewport :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ()
-- | 
foreign import ccall "vkCmdSetScissor" vkCmdSetScissor :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ()
-- | 
foreign import ccall "vkCmdSetLineWidth" vkCmdSetLineWidth :: ("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ()
-- | 
foreign import ccall "vkCmdSetDepthBias" vkCmdSetDepthBias :: ("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ()
-- | 
foreign import ccall "vkCmdSetBlendConstants" vkCmdSetBlendConstants :: ("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ()
-- | 
foreign import ccall "vkCmdSetDepthBounds" vkCmdSetDepthBounds :: ("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ()
-- | 
foreign import ccall "vkCmdSetStencilCompareMask" vkCmdSetStencilCompareMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdSetStencilWriteMask" vkCmdSetStencilWriteMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdSetStencilReference" vkCmdSetStencilReference :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdBindDescriptorSets" vkCmdBindDescriptorSets :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ()
-- | 
foreign import ccall "vkCmdBindIndexBuffer" vkCmdBindIndexBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ()
-- | 
foreign import ccall "vkCmdBindVertexBuffers" vkCmdBindVertexBuffers :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ()
-- | 
foreign import ccall "vkCmdDraw" vkCmdDraw :: ("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdDrawIndexed" vkCmdDrawIndexed :: ("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdDrawIndirect" vkCmdDrawIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdDrawIndexedIndirect" vkCmdDrawIndexedIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdDispatch" vkCmdDispatch :: ("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdDispatchIndirect" vkCmdDispatchIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ()
-- | 
foreign import ccall "vkCmdCopyBuffer" vkCmdCopyBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ()
-- | 
foreign import ccall "vkCmdCopyImage" vkCmdCopyImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ()
-- | 
foreign import ccall "vkCmdBlitImage" vkCmdBlitImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ()
-- | 
foreign import ccall "vkCmdCopyBufferToImage" vkCmdCopyBufferToImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
-- | 
foreign import ccall "vkCmdCopyImageToBuffer" vkCmdCopyImageToBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
-- | 
foreign import ccall "vkCmdUpdateBuffer" vkCmdUpdateBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ()
-- | transfer support is only available when VK_KHR_maintenance1 is enabled, as documented in valid usage language in the specification
foreign import ccall "vkCmdFillBuffer" vkCmdFillBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdClearColorImage" vkCmdClearColorImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
-- | 
foreign import ccall "vkCmdClearDepthStencilImage" vkCmdClearDepthStencilImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
-- | 
foreign import ccall "vkCmdClearAttachments" vkCmdClearAttachments :: ("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ()
-- | 
foreign import ccall "vkCmdResolveImage" vkCmdResolveImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ()
-- | 
foreign import ccall "vkCmdSetEvent" vkCmdSetEvent :: ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
-- | 
foreign import ccall "vkCmdResetEvent" vkCmdResetEvent :: ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
-- | 
foreign import ccall "vkCmdWaitEvents" vkCmdWaitEvents :: ("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
-- | 
foreign import ccall "vkCmdPipelineBarrier" vkCmdPipelineBarrier :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
-- | 
foreign import ccall "vkCmdBeginQuery" vkCmdBeginQuery :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ()
-- | 
foreign import ccall "vkCmdEndQuery" vkCmdEndQuery :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdResetQueryPool" vkCmdResetQueryPool :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdWriteTimestamp" vkCmdWriteTimestamp :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdCopyQueryPoolResults" vkCmdCopyQueryPoolResults :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ()
-- | 
foreign import ccall "vkCmdPushConstants" vkCmdPushConstants :: ("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ()
-- | 
foreign import ccall "vkCmdBeginRenderPass" vkCmdBeginRenderPass :: ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ()
-- | 
foreign import ccall "vkCmdNextSubpass" vkCmdNextSubpass :: ("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ()
-- | 
foreign import ccall "vkCmdEndRenderPass" vkCmdEndRenderPass :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()
-- | 
foreign import ccall "vkCmdExecuteCommands" vkCmdExecuteCommands :: ("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
-- | TODO: Struct comments
data VkClearRect = VkClearRect
  { vkRect :: VkRect2D
  , vkBaseArrayLayer :: Word32
  , vkLayerCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkClearRect where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkClearRect <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRect (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 16) (vkBaseArrayLayer (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 20) (vkLayerCount (poked :: VkClearRect))
-- | TODO: Struct comments
data VkImageSubresourceLayers = VkImageSubresourceLayers
  { vkAspectMask :: VkImageAspectFlags
  , vkMipLevel :: Word32
  , vkBaseArrayLayer :: Word32
  , vkLayerCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkImageSubresourceLayers where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkImageSubresourceLayers <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 4) (vkMipLevel (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 8) (vkBaseArrayLayer (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 12) (vkLayerCount (poked :: VkImageSubresourceLayers))
-- | TODO: Struct comments
data VkMemoryBarrier = VkMemoryBarrier
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSrcAccessMask :: VkAccessFlags
  , vkDstAccessMask :: VkAccessFlags
  }
  deriving (Eq, Show)

instance Storable VkMemoryBarrier where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryBarrier <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 16)
                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkMemoryBarrier))
-- | TODO: Struct comments
data VkBufferMemoryBarrier = VkBufferMemoryBarrier
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSrcAccessMask :: VkAccessFlags
  , vkDstAccessMask :: VkAccessFlags
  , vkSrcQueueFamilyIndex :: Word32
  , vkDstQueueFamilyIndex :: Word32
  , vkBuffer :: VkBuffer
  , vkOffset :: VkDeviceSize
  , vkSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBufferMemoryBarrier where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 28)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkSrcQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkDstQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkBuffer (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSize (poked :: VkBufferMemoryBarrier))
-- | TODO: Struct comments
data VkImageMemoryBarrier = VkImageMemoryBarrier
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSrcAccessMask :: VkAccessFlags
  , vkDstAccessMask :: VkAccessFlags
  , vkOldLayout :: VkImageLayout
  , vkNewLayout :: VkImageLayout
  , vkSrcQueueFamilyIndex :: Word32
  , vkDstQueueFamilyIndex :: Word32
  , vkImage :: VkImage
  , vkSubresourceRange :: VkImageSubresourceRange
  }
  deriving (Eq, Show)

instance Storable VkImageMemoryBarrier where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkImageMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 20)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 28)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 36)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkOldLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkNewLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkSrcQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 36) (vkDstQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkImage (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSubresourceRange (poked :: VkImageMemoryBarrier))
-- | TODO: Struct comments
data VkBufferCopy = VkBufferCopy
  { vkSrcOffset :: VkDeviceSize
  , vkDstOffset :: VkDeviceSize
  , vkSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBufferCopy where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferCopy <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 8) (vkDstOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 16) (vkSize (poked :: VkBufferCopy))
-- | TODO: Struct comments
data VkImageCopy = VkImageCopy
  { vkSrcSubresource :: VkImageSubresourceLayers
  , vkSrcOffset :: VkOffset3D
  , vkDstSubresource :: VkImageSubresourceLayers
  , vkDstOffset :: VkOffset3D
  , vkExtent :: VkExtent3D
  }
  deriving (Eq, Show)

instance Storable VkImageCopy where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = VkImageCopy <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 28)
                         <*> peek (ptr `plusPtr` 44)
                         <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 16) (vkSrcOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 28) (vkDstSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 44) (vkDstOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 56) (vkExtent (poked :: VkImageCopy))
-- | TODO: Struct comments
data VkImageBlit = VkImageBlit
  { vkSrcSubresource :: VkImageSubresourceLayers
  , vkSrcOffsets :: Vector 2 VkOffset3D
  , vkDstSubresource :: VkImageSubresourceLayers
  , vkDstOffsets :: Vector 2 VkOffset3D
  }
  deriving (Eq, Show)

instance Storable VkImageBlit where
  sizeOf ~_ = 80
  alignment ~_ = 4
  peek ptr = VkImageBlit <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 40)
                         <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 16) (vkSrcOffsets (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 40) (vkDstSubresource (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 56) (vkDstOffsets (poked :: VkImageBlit))
-- | TODO: Struct comments
data VkBufferImageCopy = VkBufferImageCopy
  { vkBufferOffset :: VkDeviceSize
  , vkBufferRowLength :: Word32
  , vkBufferImageHeight :: Word32
  , vkImageSubresource :: VkImageSubresourceLayers
  , vkImageOffset :: VkOffset3D
  , vkImageExtent :: VkExtent3D
  }
  deriving (Eq, Show)

instance Storable VkBufferImageCopy where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferImageCopy <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 12)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 32)
                               <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBufferOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 8) (vkBufferRowLength (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 12) (vkBufferImageHeight (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 16) (vkImageSubresource (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 32) (vkImageOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 44) (vkImageExtent (poked :: VkBufferImageCopy))
-- | TODO: Struct comments
data VkImageResolve = VkImageResolve
  { vkSrcSubresource :: VkImageSubresourceLayers
  , vkSrcOffset :: VkOffset3D
  , vkDstSubresource :: VkImageSubresourceLayers
  , vkDstOffset :: VkOffset3D
  , vkExtent :: VkExtent3D
  }
  deriving (Eq, Show)

instance Storable VkImageResolve where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = VkImageResolve <$> peek (ptr `plusPtr` 0)
                            <*> peek (ptr `plusPtr` 16)
                            <*> peek (ptr `plusPtr` 28)
                            <*> peek (ptr `plusPtr` 44)
                            <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 16) (vkSrcOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 28) (vkDstSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 44) (vkDstOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 56) (vkExtent (poked :: VkImageResolve))
-- | TODO: Struct comments
data VkRenderPassBeginInfo = VkRenderPassBeginInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkRenderPass :: VkRenderPass
  , vkFramebuffer :: VkFramebuffer
  , vkRenderArea :: VkRect2D
  , vkClearValueCount :: Word32
  , vkClearValues :: Ptr VkClearValue
  }
  deriving (Eq, Show)

instance Storable VkRenderPassBeginInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkRenderPassBeginInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 48)
                                   <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkRenderPass (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkFramebuffer (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 32) (vkRenderArea (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 48) (vkClearValueCount (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 56) (vkClearValues (poked :: VkRenderPassBeginInfo))
-- | TODO: Struct comments
data VkClearDepthStencilValue = VkClearDepthStencilValue
  { vkDepth :: CFloat
  , vkStencil :: Word32
  }
  deriving (Eq, Show)

instance Storable VkClearDepthStencilValue where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkClearDepthStencilValue <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDepth (poked :: VkClearDepthStencilValue))
                *> poke (ptr `plusPtr` 4) (vkStencil (poked :: VkClearDepthStencilValue))
-- | TODO: Struct comments
data VkClearAttachment = VkClearAttachment
  { vkAspectMask :: VkImageAspectFlags
  , vkColorAttachment :: Word32
  , vkClearValue :: VkClearValue
  }
  deriving (Eq, Show)

instance Storable VkClearAttachment where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkClearAttachment <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 4)
                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 4) (vkColorAttachment (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 8) (vkClearValue (poked :: VkClearAttachment))
-- | TODO: Struct comments
data VkDrawIndirectCommand = VkDrawIndirectCommand
  { vkVertexCount :: Word32
  , vkInstanceCount :: Word32
  , vkFirstVertex :: Word32
  , vkFirstInstance :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDrawIndirectCommand where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkDrawIndirectCommand <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkVertexCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstVertex (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkFirstInstance (poked :: VkDrawIndirectCommand))
-- | TODO: Struct comments
data VkDrawIndexedIndirectCommand = VkDrawIndexedIndirectCommand
  { vkIndexCount :: Word32
  , vkInstanceCount :: Word32
  , vkFirstIndex :: Word32
  , vkVertexOffset :: Int32
  , vkFirstInstance :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDrawIndexedIndirectCommand where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkDrawIndexedIndirectCommand <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkIndexCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstIndex (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkVertexOffset (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 16) (vkFirstInstance (poked :: VkDrawIndexedIndirectCommand))
-- | TODO: Struct comments
data VkDispatchIndirectCommand = VkDispatchIndirectCommand
  { vkX :: Word32
  , vkY :: Word32
  , vkZ :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDispatchIndirectCommand where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkDispatchIndirectCommand <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 4)
                                       <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkZ (poked :: VkDispatchIndirectCommand))
-- | TODO: Union comments
data VkClearColorValue
  = VkFloat32 (Vector 4 CFloat)
  | VkInt32 (Vector 4 Int32)
  | VkUint32 (Vector 4 Word32)
  deriving (Eq, Show)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearColorValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek _   = error "peek @VkClearColorValue"
  poke ptr = \case
    VkFloat32 e -> poke (castPtr ptr) e
    VkInt32 e -> poke (castPtr ptr) e
    VkUint32 e -> poke (castPtr ptr) e
-- | TODO: Union comments
data VkClearValue
  = VkColor VkClearColorValue
  | VkDepthStencil VkClearDepthStencilValue
  deriving (Eq, Show)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek _   = error "peek @VkClearValue"
  poke ptr = \case
    VkColor e -> poke (castPtr ptr) e
    VkDepthStencil e -> poke (castPtr ptr) e
type VkStencilFaceFlags = VkStencilFaceFlagBits
