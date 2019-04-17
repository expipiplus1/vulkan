{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language LambdaCase #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkBufferCopy(..)
  , VkBufferImageCopy(..)
  , VkBufferMemoryBarrier(..)
  , VkClearAttachment(..)
  , VkClearColorValue(..)
  , VkClearDepthStencilValue(..)
  , VkClearRect(..)
  , VkClearValue(..)
  , VkDispatchIndirectCommand(..)
  , VkDrawIndexedIndirectCommand(..)
  , VkDrawIndirectCommand(..)
  , VkImageBlit(..)
  , VkImageCopy(..)
  , VkImageMemoryBarrier(..)
  , VkImageResolve(..)
  , VkImageSubresourceLayers(..)
  , VkIndexType(..)
  , pattern VK_INDEX_TYPE_UINT16
  , pattern VK_INDEX_TYPE_UINT32
  , VkMemoryBarrier(..)
  , VkRenderPassBeginInfo(..)
  , VkStencilFaceFlagBits(..)
  , pattern VK_STENCIL_FACE_FRONT_BIT
  , pattern VK_STENCIL_FACE_BACK_BIT
  , pattern VK_STENCIL_FRONT_AND_BACK
  , VkStencilFaceFlags
  , VkSubpassContents(..)
  , pattern VK_SUBPASS_CONTENTS_INLINE
  , pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdBeginQuery
#endif
  , FN_vkCmdBeginQuery
  , PFN_vkCmdBeginQuery
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdBeginRenderPass
#endif
  , FN_vkCmdBeginRenderPass
  , PFN_vkCmdBeginRenderPass
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdBindDescriptorSets
#endif
  , FN_vkCmdBindDescriptorSets
  , PFN_vkCmdBindDescriptorSets
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdBindIndexBuffer
#endif
  , FN_vkCmdBindIndexBuffer
  , PFN_vkCmdBindIndexBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdBindPipeline
#endif
  , FN_vkCmdBindPipeline
  , PFN_vkCmdBindPipeline
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdBindVertexBuffers
#endif
  , FN_vkCmdBindVertexBuffers
  , PFN_vkCmdBindVertexBuffers
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdBlitImage
#endif
  , FN_vkCmdBlitImage
  , PFN_vkCmdBlitImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdClearAttachments
#endif
  , FN_vkCmdClearAttachments
  , PFN_vkCmdClearAttachments
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdClearColorImage
#endif
  , FN_vkCmdClearColorImage
  , PFN_vkCmdClearColorImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdClearDepthStencilImage
#endif
  , FN_vkCmdClearDepthStencilImage
  , PFN_vkCmdClearDepthStencilImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdCopyBuffer
#endif
  , FN_vkCmdCopyBuffer
  , PFN_vkCmdCopyBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdCopyBufferToImage
#endif
  , FN_vkCmdCopyBufferToImage
  , PFN_vkCmdCopyBufferToImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdCopyImage
#endif
  , FN_vkCmdCopyImage
  , PFN_vkCmdCopyImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdCopyImageToBuffer
#endif
  , FN_vkCmdCopyImageToBuffer
  , PFN_vkCmdCopyImageToBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdCopyQueryPoolResults
#endif
  , FN_vkCmdCopyQueryPoolResults
  , PFN_vkCmdCopyQueryPoolResults
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdDispatch
#endif
  , FN_vkCmdDispatch
  , PFN_vkCmdDispatch
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdDispatchIndirect
#endif
  , FN_vkCmdDispatchIndirect
  , PFN_vkCmdDispatchIndirect
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdDraw
#endif
  , FN_vkCmdDraw
  , PFN_vkCmdDraw
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdDrawIndexed
#endif
  , FN_vkCmdDrawIndexed
  , PFN_vkCmdDrawIndexed
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdDrawIndexedIndirect
#endif
  , FN_vkCmdDrawIndexedIndirect
  , PFN_vkCmdDrawIndexedIndirect
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdDrawIndirect
#endif
  , FN_vkCmdDrawIndirect
  , PFN_vkCmdDrawIndirect
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdEndQuery
#endif
  , FN_vkCmdEndQuery
  , PFN_vkCmdEndQuery
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdEndRenderPass
#endif
  , FN_vkCmdEndRenderPass
  , PFN_vkCmdEndRenderPass
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdExecuteCommands
#endif
  , FN_vkCmdExecuteCommands
  , PFN_vkCmdExecuteCommands
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdFillBuffer
#endif
  , FN_vkCmdFillBuffer
  , PFN_vkCmdFillBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdNextSubpass
#endif
  , FN_vkCmdNextSubpass
  , PFN_vkCmdNextSubpass
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdPipelineBarrier
#endif
  , FN_vkCmdPipelineBarrier
  , PFN_vkCmdPipelineBarrier
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdPushConstants
#endif
  , FN_vkCmdPushConstants
  , PFN_vkCmdPushConstants
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdResetEvent
#endif
  , FN_vkCmdResetEvent
  , PFN_vkCmdResetEvent
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdResetQueryPool
#endif
  , FN_vkCmdResetQueryPool
  , PFN_vkCmdResetQueryPool
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdResolveImage
#endif
  , FN_vkCmdResolveImage
  , PFN_vkCmdResolveImage
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetBlendConstants
#endif
  , FN_vkCmdSetBlendConstants
  , PFN_vkCmdSetBlendConstants
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetDepthBias
#endif
  , FN_vkCmdSetDepthBias
  , PFN_vkCmdSetDepthBias
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetDepthBounds
#endif
  , FN_vkCmdSetDepthBounds
  , PFN_vkCmdSetDepthBounds
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetEvent
#endif
  , FN_vkCmdSetEvent
  , PFN_vkCmdSetEvent
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetLineWidth
#endif
  , FN_vkCmdSetLineWidth
  , PFN_vkCmdSetLineWidth
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetScissor
#endif
  , FN_vkCmdSetScissor
  , PFN_vkCmdSetScissor
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetStencilCompareMask
#endif
  , FN_vkCmdSetStencilCompareMask
  , PFN_vkCmdSetStencilCompareMask
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetStencilReference
#endif
  , FN_vkCmdSetStencilReference
  , PFN_vkCmdSetStencilReference
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetStencilWriteMask
#endif
  , FN_vkCmdSetStencilWriteMask
  , PFN_vkCmdSetStencilWriteMask
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdSetViewport
#endif
  , FN_vkCmdSetViewport
  , PFN_vkCmdSetViewport
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdUpdateBuffer
#endif
  , FN_vkCmdUpdateBuffer
  , PFN_vkCmdUpdateBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdWaitEvents
#endif
  , FN_vkCmdWaitEvents
  , PFN_vkCmdWaitEvents
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCmdWriteTimestamp
#endif
  , FN_vkCmdWriteTimestamp
  , PFN_vkCmdWriteTimestamp
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
  ( FunPtr
  , Ptr
  , castPtr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
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


import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkQueryControlFlags
  )

#if defined(EXPOSE_CORE10_COMMANDS)
import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkQueryControlFlagBits(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorSet
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkExtent3D(..)
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.Event
  ( VkEvent
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageSubresourceRange(..)
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkPipelineBindPoint(..)
  , VkAccessFlags
  , VkDependencyFlags
  , VkFramebuffer
  )

#if defined(EXPOSE_CORE10_COMMANDS)
import Graphics.Vulkan.C.Core10.Pass
  ( VkDependencyFlagBits(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkRect2D(..)
  , VkViewport(..)
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
  )

#if defined(EXPOSE_CORE10_COMMANDS)
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkShaderStageFlagBits(..)
  )
#endif
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryPool
  , VkQueryResultFlags
  )

#if defined(EXPOSE_CORE10_COMMANDS)
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryResultFlagBits(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  , VkPipelineStageFlags
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkFilter(..)
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkOffset3D(..)
  , VkImageAspectFlags
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkBufferCopy"
data VkBufferCopy = VkBufferCopy
  { -- No documentation found for Nested "VkBufferCopy" "srcOffset"
  vkSrcOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferCopy" "dstOffset"
  vkDstOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferCopy" "size"
  vkSize :: VkDeviceSize
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

instance Zero VkBufferCopy where
  zero = VkBufferCopy zero
                      zero
                      zero
-- No documentation found for TopLevel "VkBufferImageCopy"
data VkBufferImageCopy = VkBufferImageCopy
  { -- No documentation found for Nested "VkBufferImageCopy" "bufferOffset"
  vkBufferOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferImageCopy" "bufferRowLength"
  vkBufferRowLength :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy" "bufferImageHeight"
  vkBufferImageHeight :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy" "imageSubresource"
  vkImageSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkBufferImageCopy" "imageOffset"
  vkImageOffset :: VkOffset3D
  , -- No documentation found for Nested "VkBufferImageCopy" "imageExtent"
  vkImageExtent :: VkExtent3D
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

instance Zero VkBufferImageCopy where
  zero = VkBufferImageCopy zero
                           zero
                           zero
                           zero
                           zero
                           zero
-- No documentation found for TopLevel "VkBufferMemoryBarrier"
data VkBufferMemoryBarrier = VkBufferMemoryBarrier
  { -- No documentation found for Nested "VkBufferMemoryBarrier" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "srcAccessMask"
  vkSrcAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "dstAccessMask"
  vkDstAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "srcQueueFamilyIndex"
  vkSrcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "dstQueueFamilyIndex"
  vkDstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "buffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "offset"
  vkOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "size"
  vkSize :: VkDeviceSize
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkSrcQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkDstQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkBuffer (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSize (poked :: VkBufferMemoryBarrier))

instance Zero VkBufferMemoryBarrier where
  zero = VkBufferMemoryBarrier zero
                               zero
                               zero
                               zero
                               zero
                               zero
                               zero
                               zero
                               zero
-- No documentation found for TopLevel "VkClearAttachment"
data VkClearAttachment = VkClearAttachment
  { -- No documentation found for Nested "VkClearAttachment" "aspectMask"
  vkAspectMask :: VkImageAspectFlags
  , -- No documentation found for Nested "VkClearAttachment" "colorAttachment"
  vkColorAttachment :: Word32
  , -- No documentation found for Nested "VkClearAttachment" "clearValue"
  vkClearValue :: VkClearValue
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

instance Zero VkClearAttachment where
  zero = VkClearAttachment zero
                           zero
                           zero
-- No documentation found for TopLevel "VkClearColorValue"
data VkClearColorValue
  = -- No documentation found for Nested "VkClearColorValue" "VkFloat32"
  VkFloat32 (Vector 4 CFloat)
  | -- No documentation found for Nested "VkClearColorValue" "VkInt32"
  VkInt32 (Vector 4 Int32)
  | -- No documentation found for Nested "VkClearColorValue" "VkUint32"
  VkUint32 (Vector 4 Word32)
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

instance Zero VkClearColorValue where
  zero = VkFloat32 zero
-- No documentation found for TopLevel "VkClearDepthStencilValue"
data VkClearDepthStencilValue = VkClearDepthStencilValue
  { -- No documentation found for Nested "VkClearDepthStencilValue" "depth"
  vkDepth :: CFloat
  , -- No documentation found for Nested "VkClearDepthStencilValue" "stencil"
  vkStencil :: Word32
  }
  deriving (Eq, Show)

instance Storable VkClearDepthStencilValue where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkClearDepthStencilValue <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDepth (poked :: VkClearDepthStencilValue))
                *> poke (ptr `plusPtr` 4) (vkStencil (poked :: VkClearDepthStencilValue))

instance Zero VkClearDepthStencilValue where
  zero = VkClearDepthStencilValue zero
                                  zero
-- No documentation found for TopLevel "VkClearRect"
data VkClearRect = VkClearRect
  { -- No documentation found for Nested "VkClearRect" "rect"
  vkRect :: VkRect2D
  , -- No documentation found for Nested "VkClearRect" "baseArrayLayer"
  vkBaseArrayLayer :: Word32
  , -- No documentation found for Nested "VkClearRect" "layerCount"
  vkLayerCount :: Word32
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

instance Zero VkClearRect where
  zero = VkClearRect zero
                     zero
                     zero
-- No documentation found for TopLevel "VkClearValue"
data VkClearValue
  = -- No documentation found for Nested "VkClearValue" "VkColor"
  VkColor VkClearColorValue
  | -- No documentation found for Nested "VkClearValue" "VkDepthStencil"
  VkDepthStencil VkClearDepthStencilValue
  deriving (Eq, Show)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek _   = error "peek @VkClearValue"
  poke ptr = \case
    VkColor e -> poke (castPtr ptr) e
    VkDepthStencil e -> poke (castPtr ptr) e

instance Zero VkClearValue where
  zero = VkColor zero
-- No documentation found for TopLevel "VkDispatchIndirectCommand"
data VkDispatchIndirectCommand = VkDispatchIndirectCommand
  { -- No documentation found for Nested "VkDispatchIndirectCommand" "x"
  vkX :: Word32
  , -- No documentation found for Nested "VkDispatchIndirectCommand" "y"
  vkY :: Word32
  , -- No documentation found for Nested "VkDispatchIndirectCommand" "z"
  vkZ :: Word32
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

instance Zero VkDispatchIndirectCommand where
  zero = VkDispatchIndirectCommand zero
                                   zero
                                   zero
-- No documentation found for TopLevel "VkDrawIndexedIndirectCommand"
data VkDrawIndexedIndirectCommand = VkDrawIndexedIndirectCommand
  { -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "indexCount"
  vkIndexCount :: Word32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "instanceCount"
  vkInstanceCount :: Word32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "firstIndex"
  vkFirstIndex :: Word32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "vertexOffset"
  vkVertexOffset :: Int32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "firstInstance"
  vkFirstInstance :: Word32
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

instance Zero VkDrawIndexedIndirectCommand where
  zero = VkDrawIndexedIndirectCommand zero
                                      zero
                                      zero
                                      zero
                                      zero
-- No documentation found for TopLevel "VkDrawIndirectCommand"
data VkDrawIndirectCommand = VkDrawIndirectCommand
  { -- No documentation found for Nested "VkDrawIndirectCommand" "vertexCount"
  vkVertexCount :: Word32
  , -- No documentation found for Nested "VkDrawIndirectCommand" "instanceCount"
  vkInstanceCount :: Word32
  , -- No documentation found for Nested "VkDrawIndirectCommand" "firstVertex"
  vkFirstVertex :: Word32
  , -- No documentation found for Nested "VkDrawIndirectCommand" "firstInstance"
  vkFirstInstance :: Word32
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

instance Zero VkDrawIndirectCommand where
  zero = VkDrawIndirectCommand zero
                               zero
                               zero
                               zero
-- No documentation found for TopLevel "VkImageBlit"
data VkImageBlit = VkImageBlit
  { -- No documentation found for Nested "VkImageBlit" "srcSubresource"
  vkSrcSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageBlit" "srcOffsets"
  vkSrcOffsets :: Vector 2 VkOffset3D
  , -- No documentation found for Nested "VkImageBlit" "dstSubresource"
  vkDstSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageBlit" "dstOffsets"
  vkDstOffsets :: Vector 2 VkOffset3D
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

instance Zero VkImageBlit where
  zero = VkImageBlit zero
                     zero
                     zero
                     zero
-- No documentation found for TopLevel "VkImageCopy"
data VkImageCopy = VkImageCopy
  { -- No documentation found for Nested "VkImageCopy" "srcSubresource"
  vkSrcSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy" "srcOffset"
  vkSrcOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageCopy" "dstSubresource"
  vkDstSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy" "dstOffset"
  vkDstOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageCopy" "extent"
  vkExtent :: VkExtent3D
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

instance Zero VkImageCopy where
  zero = VkImageCopy zero
                     zero
                     zero
                     zero
                     zero
-- No documentation found for TopLevel "VkImageMemoryBarrier"
data VkImageMemoryBarrier = VkImageMemoryBarrier
  { -- No documentation found for Nested "VkImageMemoryBarrier" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageMemoryBarrier" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageMemoryBarrier" "srcAccessMask"
  vkSrcAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkImageMemoryBarrier" "dstAccessMask"
  vkDstAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkImageMemoryBarrier" "oldLayout"
  vkOldLayout :: VkImageLayout
  , -- No documentation found for Nested "VkImageMemoryBarrier" "newLayout"
  vkNewLayout :: VkImageLayout
  , -- No documentation found for Nested "VkImageMemoryBarrier" "srcQueueFamilyIndex"
  vkSrcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkImageMemoryBarrier" "dstQueueFamilyIndex"
  vkDstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkImageMemoryBarrier" "image"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkImageMemoryBarrier" "subresourceRange"
  vkSubresourceRange :: VkImageSubresourceRange
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkOldLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkNewLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkSrcQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 36) (vkDstQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkImage (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSubresourceRange (poked :: VkImageMemoryBarrier))

instance Zero VkImageMemoryBarrier where
  zero = VkImageMemoryBarrier zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
-- No documentation found for TopLevel "VkImageResolve"
data VkImageResolve = VkImageResolve
  { -- No documentation found for Nested "VkImageResolve" "srcSubresource"
  vkSrcSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve" "srcOffset"
  vkSrcOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageResolve" "dstSubresource"
  vkDstSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve" "dstOffset"
  vkDstOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageResolve" "extent"
  vkExtent :: VkExtent3D
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

instance Zero VkImageResolve where
  zero = VkImageResolve zero
                        zero
                        zero
                        zero
                        zero
-- No documentation found for TopLevel "VkImageSubresourceLayers"
data VkImageSubresourceLayers = VkImageSubresourceLayers
  { -- No documentation found for Nested "VkImageSubresourceLayers" "aspectMask"
  vkAspectMask :: VkImageAspectFlags
  , -- No documentation found for Nested "VkImageSubresourceLayers" "mipLevel"
  vkMipLevel :: Word32
  , -- No documentation found for Nested "VkImageSubresourceLayers" "baseArrayLayer"
  vkBaseArrayLayer :: Word32
  , -- No documentation found for Nested "VkImageSubresourceLayers" "layerCount"
  vkLayerCount :: Word32
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

instance Zero VkImageSubresourceLayers where
  zero = VkImageSubresourceLayers zero
                                  zero
                                  zero
                                  zero
-- ** VkIndexType

-- No documentation found for TopLevel "VkIndexType"
newtype VkIndexType = VkIndexType Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkIndexType where
  showsPrec _ VK_INDEX_TYPE_UINT16 = showString "VK_INDEX_TYPE_UINT16"
  showsPrec _ VK_INDEX_TYPE_UINT32 = showString "VK_INDEX_TYPE_UINT32"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkIndexType 1000165000) = showString "VK_INDEX_TYPE_NONE_NV"
  showsPrec p (VkIndexType x) = showParen (p >= 11) (showString "VkIndexType " . showsPrec 11 x)

instance Read VkIndexType where
  readPrec = parens ( choose [ ("VK_INDEX_TYPE_UINT16", pure VK_INDEX_TYPE_UINT16)
                             , ("VK_INDEX_TYPE_UINT32", pure VK_INDEX_TYPE_UINT32)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_INDEX_TYPE_NONE_NV", pure (VkIndexType 1000165000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIndexType")
                        v <- step readPrec
                        pure (VkIndexType v)
                        )
                    )

-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_UINT16"
pattern VK_INDEX_TYPE_UINT16 :: VkIndexType
pattern VK_INDEX_TYPE_UINT16 = VkIndexType 0

-- No documentation found for Nested "VkIndexType" "VK_INDEX_TYPE_UINT32"
pattern VK_INDEX_TYPE_UINT32 :: VkIndexType
pattern VK_INDEX_TYPE_UINT32 = VkIndexType 1
-- No documentation found for TopLevel "VkMemoryBarrier"
data VkMemoryBarrier = VkMemoryBarrier
  { -- No documentation found for Nested "VkMemoryBarrier" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryBarrier" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryBarrier" "srcAccessMask"
  vkSrcAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkMemoryBarrier" "dstAccessMask"
  vkDstAccessMask :: VkAccessFlags
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkMemoryBarrier))

instance Zero VkMemoryBarrier where
  zero = VkMemoryBarrier zero
                         zero
                         zero
                         zero
-- No documentation found for TopLevel "VkRenderPassBeginInfo"
data VkRenderPassBeginInfo = VkRenderPassBeginInfo
  { -- No documentation found for Nested "VkRenderPassBeginInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "renderPass"
  vkRenderPass :: VkRenderPass
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "framebuffer"
  vkFramebuffer :: VkFramebuffer
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "renderArea"
  vkRenderArea :: VkRect2D
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "clearValueCount"
  vkClearValueCount :: Word32
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "pClearValues"
  vkPClearValues :: Ptr VkClearValue
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkRenderPass (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkFramebuffer (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 32) (vkRenderArea (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 48) (vkClearValueCount (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 56) (vkPClearValues (poked :: VkRenderPassBeginInfo))

instance Zero VkRenderPassBeginInfo where
  zero = VkRenderPassBeginInfo zero
                               zero
                               zero
                               zero
                               zero
                               zero
                               zero
-- ** VkStencilFaceFlagBits

-- No documentation found for TopLevel "VkStencilFaceFlagBits"
newtype VkStencilFaceFlagBits = VkStencilFaceFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkStencilFaceFlagBits" "VK_STENCIL_FACE_FRONT_BIT"
pattern VK_STENCIL_FACE_FRONT_BIT :: VkStencilFaceFlagBits
pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlagBits 0x00000001

-- No documentation found for Nested "VkStencilFaceFlagBits" "VK_STENCIL_FACE_BACK_BIT"
pattern VK_STENCIL_FACE_BACK_BIT :: VkStencilFaceFlagBits
pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlagBits 0x00000002

-- No documentation found for Nested "VkStencilFaceFlagBits" "VK_STENCIL_FRONT_AND_BACK"
pattern VK_STENCIL_FRONT_AND_BACK :: VkStencilFaceFlagBits
pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlagBits 0x00000003
-- No documentation found for TopLevel "VkStencilFaceFlags"
type VkStencilFaceFlags = VkStencilFaceFlagBits
-- ** VkSubpassContents

-- No documentation found for TopLevel "VkSubpassContents"
newtype VkSubpassContents = VkSubpassContents Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for Nested "VkSubpassContents" "VK_SUBPASS_CONTENTS_INLINE"
pattern VK_SUBPASS_CONTENTS_INLINE :: VkSubpassContents
pattern VK_SUBPASS_CONTENTS_INLINE = VkSubpassContents 0

-- No documentation found for Nested "VkSubpassContents" "VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS :: VkSubpassContents
pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = VkSubpassContents 1
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdBeginQuery"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginQuery" vkCmdBeginQuery :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ()

#endif
type FN_vkCmdBeginQuery = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ()
type PFN_vkCmdBeginQuery = FunPtr FN_vkCmdBeginQuery
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdBeginRenderPass"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginRenderPass" vkCmdBeginRenderPass :: ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ()

#endif
type FN_vkCmdBeginRenderPass = ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ()
type PFN_vkCmdBeginRenderPass = FunPtr FN_vkCmdBeginRenderPass
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdBindDescriptorSets"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindDescriptorSets" vkCmdBindDescriptorSets :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ()

#endif
type FN_vkCmdBindDescriptorSets = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ()
type PFN_vkCmdBindDescriptorSets = FunPtr FN_vkCmdBindDescriptorSets
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdBindIndexBuffer"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindIndexBuffer" vkCmdBindIndexBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ()

#endif
type FN_vkCmdBindIndexBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ()
type PFN_vkCmdBindIndexBuffer = FunPtr FN_vkCmdBindIndexBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdBindPipeline"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindPipeline" vkCmdBindPipeline :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ()

#endif
type FN_vkCmdBindPipeline = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ()
type PFN_vkCmdBindPipeline = FunPtr FN_vkCmdBindPipeline
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdBindVertexBuffers"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindVertexBuffers" vkCmdBindVertexBuffers :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ()

#endif
type FN_vkCmdBindVertexBuffers = ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkCmdBindVertexBuffers = FunPtr FN_vkCmdBindVertexBuffers
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdBlitImage"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBlitImage" vkCmdBlitImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ()

#endif
type FN_vkCmdBlitImage = ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ()
type PFN_vkCmdBlitImage = FunPtr FN_vkCmdBlitImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdClearAttachments"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdClearAttachments" vkCmdClearAttachments :: ("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ()

#endif
type FN_vkCmdClearAttachments = ("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ()
type PFN_vkCmdClearAttachments = FunPtr FN_vkCmdClearAttachments
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdClearColorImage"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdClearColorImage" vkCmdClearColorImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()

#endif
type FN_vkCmdClearColorImage = ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
type PFN_vkCmdClearColorImage = FunPtr FN_vkCmdClearColorImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdClearDepthStencilImage"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdClearDepthStencilImage" vkCmdClearDepthStencilImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()

#endif
type FN_vkCmdClearDepthStencilImage = ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
type PFN_vkCmdClearDepthStencilImage = FunPtr FN_vkCmdClearDepthStencilImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdCopyBuffer"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyBuffer" vkCmdCopyBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ()

#endif
type FN_vkCmdCopyBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ()
type PFN_vkCmdCopyBuffer = FunPtr FN_vkCmdCopyBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdCopyBufferToImage"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyBufferToImage" vkCmdCopyBufferToImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()

#endif
type FN_vkCmdCopyBufferToImage = ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
type PFN_vkCmdCopyBufferToImage = FunPtr FN_vkCmdCopyBufferToImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdCopyImage"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyImage" vkCmdCopyImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ()

#endif
type FN_vkCmdCopyImage = ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ()
type PFN_vkCmdCopyImage = FunPtr FN_vkCmdCopyImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdCopyImageToBuffer"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyImageToBuffer" vkCmdCopyImageToBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()

#endif
type FN_vkCmdCopyImageToBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
type PFN_vkCmdCopyImageToBuffer = FunPtr FN_vkCmdCopyImageToBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdCopyQueryPoolResults"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyQueryPoolResults" vkCmdCopyQueryPoolResults :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ()

#endif
type FN_vkCmdCopyQueryPoolResults = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ()
type PFN_vkCmdCopyQueryPoolResults = FunPtr FN_vkCmdCopyQueryPoolResults
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdDispatch"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDispatch" vkCmdDispatch :: ("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()

#endif
type FN_vkCmdDispatch = ("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
type PFN_vkCmdDispatch = FunPtr FN_vkCmdDispatch
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdDispatchIndirect"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDispatchIndirect" vkCmdDispatchIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ()

#endif
type FN_vkCmdDispatchIndirect = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ()
type PFN_vkCmdDispatchIndirect = FunPtr FN_vkCmdDispatchIndirect
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdDraw"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDraw" vkCmdDraw :: ("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ()

#endif
type FN_vkCmdDraw = ("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ()
type PFN_vkCmdDraw = FunPtr FN_vkCmdDraw
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdDrawIndexed"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndexed" vkCmdDrawIndexed :: ("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawIndexed = ("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndexed = FunPtr FN_vkCmdDrawIndexed
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdDrawIndexedIndirect"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndexedIndirect" vkCmdDrawIndexedIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawIndexedIndirect = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndexedIndirect = FunPtr FN_vkCmdDrawIndexedIndirect
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdDrawIndirect"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndirect" vkCmdDrawIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawIndirect = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndirect = FunPtr FN_vkCmdDrawIndirect
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdEndQuery"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndQuery" vkCmdEndQuery :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()

#endif
type FN_vkCmdEndQuery = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
type PFN_vkCmdEndQuery = FunPtr FN_vkCmdEndQuery
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdEndRenderPass"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndRenderPass" vkCmdEndRenderPass :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()

#endif
type FN_vkCmdEndRenderPass = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdEndRenderPass = FunPtr FN_vkCmdEndRenderPass
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdExecuteCommands"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdExecuteCommands" vkCmdExecuteCommands :: ("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()

#endif
type FN_vkCmdExecuteCommands = ("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
type PFN_vkCmdExecuteCommands = FunPtr FN_vkCmdExecuteCommands
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdFillBuffer"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdFillBuffer" vkCmdFillBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ()

#endif
type FN_vkCmdFillBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ()
type PFN_vkCmdFillBuffer = FunPtr FN_vkCmdFillBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdNextSubpass"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdNextSubpass" vkCmdNextSubpass :: ("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ()

#endif
type FN_vkCmdNextSubpass = ("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ()
type PFN_vkCmdNextSubpass = FunPtr FN_vkCmdNextSubpass
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdPipelineBarrier"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdPipelineBarrier" vkCmdPipelineBarrier :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()

#endif
type FN_vkCmdPipelineBarrier = ("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
type PFN_vkCmdPipelineBarrier = FunPtr FN_vkCmdPipelineBarrier
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdPushConstants"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdPushConstants" vkCmdPushConstants :: ("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ()

#endif
type FN_vkCmdPushConstants = ("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ()
type PFN_vkCmdPushConstants = FunPtr FN_vkCmdPushConstants
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdResetEvent"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdResetEvent" vkCmdResetEvent :: ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()

#endif
type FN_vkCmdResetEvent = ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
type PFN_vkCmdResetEvent = FunPtr FN_vkCmdResetEvent
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdResetQueryPool"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdResetQueryPool" vkCmdResetQueryPool :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()

#endif
type FN_vkCmdResetQueryPool = ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
type PFN_vkCmdResetQueryPool = FunPtr FN_vkCmdResetQueryPool
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdResolveImage"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdResolveImage" vkCmdResolveImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ()

#endif
type FN_vkCmdResolveImage = ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ()
type PFN_vkCmdResolveImage = FunPtr FN_vkCmdResolveImage
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetBlendConstants"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetBlendConstants" vkCmdSetBlendConstants :: ("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ()

#endif
type FN_vkCmdSetBlendConstants = ("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ()
type PFN_vkCmdSetBlendConstants = FunPtr FN_vkCmdSetBlendConstants
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetDepthBias"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetDepthBias" vkCmdSetDepthBias :: ("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ()

#endif
type FN_vkCmdSetDepthBias = ("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ()
type PFN_vkCmdSetDepthBias = FunPtr FN_vkCmdSetDepthBias
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetDepthBounds"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetDepthBounds" vkCmdSetDepthBounds :: ("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ()

#endif
type FN_vkCmdSetDepthBounds = ("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ()
type PFN_vkCmdSetDepthBounds = FunPtr FN_vkCmdSetDepthBounds
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetEvent"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetEvent" vkCmdSetEvent :: ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()

#endif
type FN_vkCmdSetEvent = ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
type PFN_vkCmdSetEvent = FunPtr FN_vkCmdSetEvent
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetLineWidth"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetLineWidth" vkCmdSetLineWidth :: ("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ()

#endif
type FN_vkCmdSetLineWidth = ("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ()
type PFN_vkCmdSetLineWidth = FunPtr FN_vkCmdSetLineWidth
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetScissor"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetScissor" vkCmdSetScissor :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ()

#endif
type FN_vkCmdSetScissor = ("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ()
type PFN_vkCmdSetScissor = FunPtr FN_vkCmdSetScissor
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetStencilCompareMask"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetStencilCompareMask" vkCmdSetStencilCompareMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ()

#endif
type FN_vkCmdSetStencilCompareMask = ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ()
type PFN_vkCmdSetStencilCompareMask = FunPtr FN_vkCmdSetStencilCompareMask
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetStencilReference"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetStencilReference" vkCmdSetStencilReference :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ()

#endif
type FN_vkCmdSetStencilReference = ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ()
type PFN_vkCmdSetStencilReference = FunPtr FN_vkCmdSetStencilReference
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetStencilWriteMask"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetStencilWriteMask" vkCmdSetStencilWriteMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ()

#endif
type FN_vkCmdSetStencilWriteMask = ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ()
type PFN_vkCmdSetStencilWriteMask = FunPtr FN_vkCmdSetStencilWriteMask
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetViewport"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetViewport" vkCmdSetViewport :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ()

#endif
type FN_vkCmdSetViewport = ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ()
type PFN_vkCmdSetViewport = FunPtr FN_vkCmdSetViewport
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdUpdateBuffer"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdUpdateBuffer" vkCmdUpdateBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ()

#endif
type FN_vkCmdUpdateBuffer = ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ()
type PFN_vkCmdUpdateBuffer = FunPtr FN_vkCmdUpdateBuffer
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdWaitEvents"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdWaitEvents" vkCmdWaitEvents :: ("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()

#endif
type FN_vkCmdWaitEvents = ("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
type PFN_vkCmdWaitEvents = FunPtr FN_vkCmdWaitEvents
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkCmdWriteTimestamp"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdWriteTimestamp" vkCmdWriteTimestamp :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()

#endif
type FN_vkCmdWriteTimestamp = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
type PFN_vkCmdWriteTimestamp = FunPtr FN_vkCmdWriteTimestamp
