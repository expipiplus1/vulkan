{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.CommandBufferBuilding
  ( BufferCopy(..)
  , BufferImageCopy(..)
#if defined(VK_USE_PLATFORM_GGP)
  , BufferMemoryBarrier(..)
#endif
  , ClearAttachment(..)
  , ClearColorValue(..)
  , ClearDepthStencilValue(..)
  , ClearRect(..)
  , ClearValue(..)
  , DispatchIndirectCommand(..)
  , DrawIndexedIndirectCommand(..)
  , DrawIndirectCommand(..)
  , ImageBlit(..)
  , ImageCopy(..)
#if defined(VK_USE_PLATFORM_GGP)
  , ImageMemoryBarrier(..)
#endif
  , ImageResolve(..)
  , ImageSubresourceLayers(..)
  , IndexType
  , pattern INDEX_TYPE_UINT16
  , pattern INDEX_TYPE_UINT32
  , pattern INDEX_TYPE_NONE_NV
#if defined(VK_USE_PLATFORM_GGP)
  , MemoryBarrier(..)
  , RenderPassBeginInfo(..)
#endif
  , StencilFaceFlagBits
  , pattern STENCIL_FACE_FRONT_BIT
  , pattern STENCIL_FACE_BACK_BIT
  , pattern STENCIL_FRONT_AND_BACK
  , StencilFaceFlags
  , SubpassContents
  , pattern SUBPASS_CONTENTS_INLINE
  , pattern SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
  , cmdBeginQuery
  , cmdBeginRenderPass
  , cmdBindDescriptorSets
  , cmdBindIndexBuffer
  , cmdBindPipeline
  , cmdBindVertexBuffers
  , cmdBlitImage
  , cmdClearAttachments
  , cmdClearColorImage
  , cmdClearDepthStencilImage
  , cmdCopyBuffer
  , cmdCopyBufferToImage
  , cmdCopyImage
  , cmdCopyImageToBuffer
  , cmdCopyQueryPoolResults
  , cmdDispatch
  , cmdDispatchIndirect
  , cmdDraw
  , cmdDrawIndexed
  , cmdDrawIndexedIndirect
  , cmdDrawIndirect
  , cmdEndQuery
  , cmdEndRenderPass
  , cmdExecuteCommands
  , cmdFillBuffer
  , cmdNextSubpass
  , cmdPipelineBarrier
  , cmdPushConstants
  , cmdResetEvent
  , cmdResetQueryPool
  , cmdResolveImage
  , cmdSetBlendConstants
  , cmdSetDepthBias
  , cmdSetDepthBounds
  , cmdSetEvent
  , cmdSetLineWidth
  , cmdSetScissor
  , cmdSetStencilCompareMask
  , cmdSetStencilReference
  , cmdSetStencilWriteMask
  , cmdSetViewport
  , cmdUpdateBuffer
  , cmdWaitEvents
  , cmdWriteTimestamp
  ) where

import Data.Function
  ( (&)
  )
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( head
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( Storable
  , pokeElemOff
  , sizeOf
  )


import Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkIndexType(..)
  , VkStencilFaceFlagBits(..)
  , VkSubpassContents(..)
  , vkCmdBeginQuery
  , vkCmdBeginRenderPass
  , vkCmdBindDescriptorSets
  , vkCmdBindIndexBuffer
  , vkCmdBindPipeline
  , vkCmdBindVertexBuffers
  , vkCmdBlitImage
  , vkCmdClearAttachments
  , vkCmdClearColorImage
  , vkCmdClearDepthStencilImage
  , vkCmdCopyBuffer
  , vkCmdCopyBufferToImage
  , vkCmdCopyImage
  , vkCmdCopyImageToBuffer
  , vkCmdCopyQueryPoolResults
  , vkCmdDispatch
  , vkCmdDispatchIndirect
  , vkCmdDraw
  , vkCmdDrawIndexed
  , vkCmdDrawIndexedIndirect
  , vkCmdDrawIndirect
  , vkCmdEndQuery
  , vkCmdEndRenderPass
  , vkCmdExecuteCommands
  , vkCmdFillBuffer
  , vkCmdNextSubpass
  , vkCmdPipelineBarrier
  , vkCmdPushConstants
  , vkCmdResetEvent
  , vkCmdResetQueryPool
  , vkCmdResolveImage
  , vkCmdSetBlendConstants
  , vkCmdSetDepthBias
  , vkCmdSetDepthBounds
  , vkCmdSetEvent
  , vkCmdSetLineWidth
  , vkCmdSetScissor
  , vkCmdSetStencilCompareMask
  , vkCmdSetStencilReference
  , vkCmdSetStencilWriteMask
  , vkCmdSetViewport
  , vkCmdUpdateBuffer
  , vkCmdWaitEvents
  , vkCmdWriteTimestamp
  , pattern VK_INDEX_TYPE_UINT16
  , pattern VK_INDEX_TYPE_UINT32
  , pattern VK_STENCIL_FACE_BACK_BIT
  , pattern VK_STENCIL_FACE_FRONT_BIT
  , pattern VK_STENCIL_FRONT_AND_BACK
  , pattern VK_SUBPASS_CONTENTS_INLINE
  , pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_INDEX_TYPE_NONE_NV
  )
import Graphics.Vulkan.Core10.CommandBuffer
  ( QueryControlFlags
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Extent3D(..)
  , DeviceSize
  )
import Graphics.Vulkan.Core10.Event
  ( Event
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageSubresourceRange(..)
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
import Graphics.Vulkan.Core10.Pass
  ( DependencyFlags
  , PipelineBindPoint
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pass
  ( Framebuffer
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pass
  ( AccessFlags
  )
#endif
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , Viewport(..)
  , Pipeline
  , PipelineLayout
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( RenderPass
  )
#endif
import Graphics.Vulkan.Core10.PipelineLayout
  ( ShaderStageFlags
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPool
  , QueryResultFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , PipelineStageFlagBits
  , PipelineStageFlags
  )
import Graphics.Vulkan.Core10.Sampler
  ( Filter
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( Offset3D(..)
  , ImageAspectFlags
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif



-- No documentation found for TopLevel "VkBufferCopy"
data BufferCopy = BufferCopy
  { -- No documentation found for Nested "BufferCopy" "srcOffset"
  srcOffset :: DeviceSize
  , -- No documentation found for Nested "BufferCopy" "dstOffset"
  dstOffset :: DeviceSize
  , -- No documentation found for Nested "BufferCopy" "size"
  size :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero BufferCopy where
  zero = BufferCopy zero
                    zero
                    zero



-- No documentation found for TopLevel "VkBufferImageCopy"
data BufferImageCopy = BufferImageCopy
  { -- No documentation found for Nested "BufferImageCopy" "bufferOffset"
  bufferOffset :: DeviceSize
  , -- No documentation found for Nested "BufferImageCopy" "bufferRowLength"
  bufferRowLength :: Word32
  , -- No documentation found for Nested "BufferImageCopy" "bufferImageHeight"
  bufferImageHeight :: Word32
  , -- No documentation found for Nested "BufferImageCopy" "imageSubresource"
  imageSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "BufferImageCopy" "imageOffset"
  imageOffset :: Offset3D
  , -- No documentation found for Nested "BufferImageCopy" "imageExtent"
  imageExtent :: Extent3D
  }
  deriving (Show, Eq)

instance Zero BufferImageCopy where
  zero = BufferImageCopy zero
                         zero
                         zero
                         zero
                         zero
                         zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBufferMemoryBarrier"
data BufferMemoryBarrier = BufferMemoryBarrier
  { -- No documentation found for Nested "BufferMemoryBarrier" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferMemoryBarrier" "srcAccessMask"
  srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "BufferMemoryBarrier" "dstAccessMask"
  dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "BufferMemoryBarrier" "srcQueueFamilyIndex"
  srcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "BufferMemoryBarrier" "dstQueueFamilyIndex"
  dstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "BufferMemoryBarrier" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "BufferMemoryBarrier" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "BufferMemoryBarrier" "size"
  size :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero BufferMemoryBarrier where
  zero = BufferMemoryBarrier Nothing
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero

#endif


-- No documentation found for TopLevel "VkClearAttachment"
data ClearAttachment = ClearAttachment
  { -- No documentation found for Nested "ClearAttachment" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ClearAttachment" "colorAttachment"
  colorAttachment :: Word32
  , -- No documentation found for Nested "ClearAttachment" "clearValue"
  clearValue :: ClearValue
  }
  deriving (Show, Eq)

instance Zero ClearAttachment where
  zero = ClearAttachment zero
                         zero
                         zero



-- No documentation found for TopLevel "VkClearColorValue"
data ClearColorValue
  = Float32 (Float, Float, Float, Float)
  | Int32 (Int32, Int32, Int32, Int32)
  | Uint32 (Word32, Word32, Word32, Word32)
  deriving (Show, Eq)

instance Zero ClearColorValue where
  zero = Float32 (zero, zero, zero, zero)



-- No documentation found for TopLevel "VkClearDepthStencilValue"
data ClearDepthStencilValue = ClearDepthStencilValue
  { -- No documentation found for Nested "ClearDepthStencilValue" "depth"
  depth :: Float
  , -- No documentation found for Nested "ClearDepthStencilValue" "stencil"
  stencil :: Word32
  }
  deriving (Show, Eq)

instance Zero ClearDepthStencilValue where
  zero = ClearDepthStencilValue zero
                                zero



-- No documentation found for TopLevel "VkClearRect"
data ClearRect = ClearRect
  { -- No documentation found for Nested "ClearRect" "rect"
  rect :: Rect2D
  , -- No documentation found for Nested "ClearRect" "baseArrayLayer"
  baseArrayLayer :: Word32
  , -- No documentation found for Nested "ClearRect" "layerCount"
  layerCount :: Word32
  }
  deriving (Show, Eq)

instance Zero ClearRect where
  zero = ClearRect zero
                   zero
                   zero



-- No documentation found for TopLevel "VkClearValue"
data ClearValue
  = Color ClearColorValue
  | DepthStencil ClearDepthStencilValue
  deriving (Show, Eq)

instance Zero ClearValue where
  zero = Color zero



-- No documentation found for TopLevel "VkDispatchIndirectCommand"
data DispatchIndirectCommand = DispatchIndirectCommand
  { -- No documentation found for Nested "DispatchIndirectCommand" "x"
  x :: Word32
  , -- No documentation found for Nested "DispatchIndirectCommand" "y"
  y :: Word32
  , -- No documentation found for Nested "DispatchIndirectCommand" "z"
  z :: Word32
  }
  deriving (Show, Eq)

instance Zero DispatchIndirectCommand where
  zero = DispatchIndirectCommand zero
                                 zero
                                 zero



-- No documentation found for TopLevel "VkDrawIndexedIndirectCommand"
data DrawIndexedIndirectCommand = DrawIndexedIndirectCommand
  { -- No documentation found for Nested "DrawIndexedIndirectCommand" "indexCount"
  indexCount :: Word32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "instanceCount"
  instanceCount :: Word32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "firstIndex"
  firstIndex :: Word32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "vertexOffset"
  vertexOffset :: Int32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "firstInstance"
  firstInstance :: Word32
  }
  deriving (Show, Eq)

instance Zero DrawIndexedIndirectCommand where
  zero = DrawIndexedIndirectCommand zero
                                    zero
                                    zero
                                    zero
                                    zero



-- No documentation found for TopLevel "VkDrawIndirectCommand"
data DrawIndirectCommand = DrawIndirectCommand
  { -- No documentation found for Nested "DrawIndirectCommand" "vertexCount"
  vertexCount :: Word32
  , -- No documentation found for Nested "DrawIndirectCommand" "instanceCount"
  instanceCount :: Word32
  , -- No documentation found for Nested "DrawIndirectCommand" "firstVertex"
  firstVertex :: Word32
  , -- No documentation found for Nested "DrawIndirectCommand" "firstInstance"
  firstInstance :: Word32
  }
  deriving (Show, Eq)

instance Zero DrawIndirectCommand where
  zero = DrawIndirectCommand zero
                             zero
                             zero
                             zero



-- No documentation found for TopLevel "VkImageBlit"
data ImageBlit = ImageBlit
  { -- No documentation found for Nested "ImageBlit" "srcSubresource"
  srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageBlit" "srcOffsets"
  srcOffsets :: (Offset3D, Offset3D)
  , -- No documentation found for Nested "ImageBlit" "dstSubresource"
  dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageBlit" "dstOffsets"
  dstOffsets :: (Offset3D, Offset3D)
  }
  deriving (Show, Eq)

instance Zero ImageBlit where
  zero = ImageBlit zero
                   (zero, zero)
                   zero
                   (zero, zero)



-- No documentation found for TopLevel "VkImageCopy"
data ImageCopy = ImageCopy
  { -- No documentation found for Nested "ImageCopy" "srcSubresource"
  srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageCopy" "srcOffset"
  srcOffset :: Offset3D
  , -- No documentation found for Nested "ImageCopy" "dstSubresource"
  dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageCopy" "dstOffset"
  dstOffset :: Offset3D
  , -- No documentation found for Nested "ImageCopy" "extent"
  extent :: Extent3D
  }
  deriving (Show, Eq)

instance Zero ImageCopy where
  zero = ImageCopy zero
                   zero
                   zero
                   zero
                   zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageMemoryBarrier"
data ImageMemoryBarrier = ImageMemoryBarrier
  { -- No documentation found for Nested "ImageMemoryBarrier" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageMemoryBarrier" "srcAccessMask"
  srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "ImageMemoryBarrier" "dstAccessMask"
  dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "ImageMemoryBarrier" "oldLayout"
  oldLayout :: ImageLayout
  , -- No documentation found for Nested "ImageMemoryBarrier" "newLayout"
  newLayout :: ImageLayout
  , -- No documentation found for Nested "ImageMemoryBarrier" "srcQueueFamilyIndex"
  srcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "ImageMemoryBarrier" "dstQueueFamilyIndex"
  dstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "ImageMemoryBarrier" "image"
  image :: Image
  , -- No documentation found for Nested "ImageMemoryBarrier" "subresourceRange"
  subresourceRange :: ImageSubresourceRange
  }
  deriving (Show, Eq)

instance Zero ImageMemoryBarrier where
  zero = ImageMemoryBarrier Nothing
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero

#endif


-- No documentation found for TopLevel "VkImageResolve"
data ImageResolve = ImageResolve
  { -- No documentation found for Nested "ImageResolve" "srcSubresource"
  srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageResolve" "srcOffset"
  srcOffset :: Offset3D
  , -- No documentation found for Nested "ImageResolve" "dstSubresource"
  dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageResolve" "dstOffset"
  dstOffset :: Offset3D
  , -- No documentation found for Nested "ImageResolve" "extent"
  extent :: Extent3D
  }
  deriving (Show, Eq)

instance Zero ImageResolve where
  zero = ImageResolve zero
                      zero
                      zero
                      zero
                      zero



-- No documentation found for TopLevel "VkImageSubresourceLayers"
data ImageSubresourceLayers = ImageSubresourceLayers
  { -- No documentation found for Nested "ImageSubresourceLayers" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ImageSubresourceLayers" "mipLevel"
  mipLevel :: Word32
  , -- No documentation found for Nested "ImageSubresourceLayers" "baseArrayLayer"
  baseArrayLayer :: Word32
  , -- No documentation found for Nested "ImageSubresourceLayers" "layerCount"
  layerCount :: Word32
  }
  deriving (Show, Eq)

instance Zero ImageSubresourceLayers where
  zero = ImageSubresourceLayers zero
                                zero
                                zero
                                zero


-- No documentation found for TopLevel "IndexType"
type IndexType = VkIndexType


{-# complete INDEX_TYPE_UINT16, INDEX_TYPE_UINT32, INDEX_TYPE_NONE_NV :: IndexType #-}


-- No documentation found for Nested "IndexType" "INDEX_TYPE_UINT16"
pattern INDEX_TYPE_UINT16 :: (a ~ IndexType) => a
pattern INDEX_TYPE_UINT16 = VK_INDEX_TYPE_UINT16


-- No documentation found for Nested "IndexType" "INDEX_TYPE_UINT32"
pattern INDEX_TYPE_UINT32 :: (a ~ IndexType) => a
pattern INDEX_TYPE_UINT32 = VK_INDEX_TYPE_UINT32


-- No documentation found for Nested "IndexType" "INDEX_TYPE_NONE_NV"
pattern INDEX_TYPE_NONE_NV :: (a ~ IndexType) => a
pattern INDEX_TYPE_NONE_NV = VK_INDEX_TYPE_NONE_NV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryBarrier"
data MemoryBarrier = MemoryBarrier
  { -- No documentation found for Nested "MemoryBarrier" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryBarrier" "srcAccessMask"
  srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "MemoryBarrier" "dstAccessMask"
  dstAccessMask :: AccessFlags
  }
  deriving (Show, Eq)

instance Zero MemoryBarrier where
  zero = MemoryBarrier Nothing
                       zero
                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkRenderPassBeginInfo"
data RenderPassBeginInfo = RenderPassBeginInfo
  { -- No documentation found for Nested "RenderPassBeginInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassBeginInfo" "renderPass"
  renderPass :: RenderPass
  , -- No documentation found for Nested "RenderPassBeginInfo" "framebuffer"
  framebuffer :: Framebuffer
  , -- No documentation found for Nested "RenderPassBeginInfo" "renderArea"
  renderArea :: Rect2D
  , -- No documentation found for Nested "RenderPassBeginInfo" "pClearValues"
  clearValues :: Vector ClearValue
  }
  deriving (Show, Eq)

instance Zero RenderPassBeginInfo where
  zero = RenderPassBeginInfo Nothing
                             zero
                             zero
                             zero
                             mempty

#endif

-- No documentation found for TopLevel "StencilFaceFlagBits"
type StencilFaceFlagBits = VkStencilFaceFlagBits


{-# complete STENCIL_FACE_FRONT_BIT, STENCIL_FACE_BACK_BIT, STENCIL_FRONT_AND_BACK :: StencilFaceFlagBits #-}


-- No documentation found for Nested "StencilFaceFlagBits" "STENCIL_FACE_FRONT_BIT"
pattern STENCIL_FACE_FRONT_BIT :: (a ~ StencilFaceFlagBits) => a
pattern STENCIL_FACE_FRONT_BIT = VK_STENCIL_FACE_FRONT_BIT


-- No documentation found for Nested "StencilFaceFlagBits" "STENCIL_FACE_BACK_BIT"
pattern STENCIL_FACE_BACK_BIT :: (a ~ StencilFaceFlagBits) => a
pattern STENCIL_FACE_BACK_BIT = VK_STENCIL_FACE_BACK_BIT


-- No documentation found for Nested "StencilFaceFlagBits" "STENCIL_FRONT_AND_BACK"
pattern STENCIL_FRONT_AND_BACK :: (a ~ StencilFaceFlagBits) => a
pattern STENCIL_FRONT_AND_BACK = VK_STENCIL_FRONT_AND_BACK

-- No documentation found for TopLevel "StencilFaceFlags"
type StencilFaceFlags = StencilFaceFlagBits

-- No documentation found for TopLevel "SubpassContents"
type SubpassContents = VkSubpassContents


{-# complete SUBPASS_CONTENTS_INLINE, SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS :: SubpassContents #-}


-- No documentation found for Nested "SubpassContents" "SUBPASS_CONTENTS_INLINE"
pattern SUBPASS_CONTENTS_INLINE :: (a ~ SubpassContents) => a
pattern SUBPASS_CONTENTS_INLINE = VK_SUBPASS_CONTENTS_INLINE


-- No documentation found for Nested "SubpassContents" "SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
pattern SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS :: (a ~ SubpassContents) => a
pattern SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS


-- No documentation found for TopLevel "vkCmdBeginQuery"
cmdBeginQuery :: CommandBuffer ->  QueryPool ->  Word32 ->  QueryControlFlags ->  IO ()
cmdBeginQuery = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdBeginRenderPass"
cmdBeginRenderPass :: CommandBuffer ->  RenderPassBeginInfo ->  SubpassContents ->  IO ()
cmdBeginRenderPass = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdBindDescriptorSets"
cmdBindDescriptorSets :: CommandBuffer ->  PipelineBindPoint ->  PipelineLayout ->  Word32 ->  Vector DescriptorSet ->  Vector Word32 ->  IO ()
cmdBindDescriptorSets = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdBindIndexBuffer"
cmdBindIndexBuffer :: CommandBuffer ->  Buffer ->  DeviceSize ->  IndexType ->  IO ()
cmdBindIndexBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdBindPipeline"
cmdBindPipeline :: CommandBuffer ->  PipelineBindPoint ->  Pipeline ->  IO ()
cmdBindPipeline = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdBindVertexBuffers"
cmdBindVertexBuffers :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Vector DeviceSize ->  IO ()
cmdBindVertexBuffers = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdBlitImage"
cmdBlitImage :: CommandBuffer ->  Image ->  ImageLayout ->  Image ->  ImageLayout ->  Vector ImageBlit ->  Filter ->  IO ()
cmdBlitImage = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdClearAttachments"
cmdClearAttachments :: CommandBuffer ->  Vector ClearAttachment ->  Vector ClearRect ->  IO ()
cmdClearAttachments = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdClearColorImage"
cmdClearColorImage :: CommandBuffer ->  Image ->  ImageLayout ->  ClearColorValue ->  Vector ImageSubresourceRange ->  IO ()
cmdClearColorImage = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdClearDepthStencilImage"
cmdClearDepthStencilImage :: CommandBuffer ->  Image ->  ImageLayout ->  ClearDepthStencilValue ->  Vector ImageSubresourceRange ->  IO ()
cmdClearDepthStencilImage = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdCopyBuffer"
cmdCopyBuffer :: CommandBuffer ->  Buffer ->  Buffer ->  Vector BufferCopy ->  IO ()
cmdCopyBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdCopyBufferToImage"
cmdCopyBufferToImage :: CommandBuffer ->  Buffer ->  Image ->  ImageLayout ->  Vector BufferImageCopy ->  IO ()
cmdCopyBufferToImage = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdCopyImage"
cmdCopyImage :: CommandBuffer ->  Image ->  ImageLayout ->  Image ->  ImageLayout ->  Vector ImageCopy ->  IO ()
cmdCopyImage = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdCopyImageToBuffer"
cmdCopyImageToBuffer :: CommandBuffer ->  Image ->  ImageLayout ->  Buffer ->  Vector BufferImageCopy ->  IO ()
cmdCopyImageToBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdCopyQueryPoolResults"
cmdCopyQueryPoolResults :: CommandBuffer ->  QueryPool ->  Word32 ->  Word32 ->  Buffer ->  DeviceSize ->  DeviceSize ->  QueryResultFlags ->  IO ()
cmdCopyQueryPoolResults = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDispatch"
cmdDispatch :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDispatch = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDispatchIndirect"
cmdDispatchIndirect :: CommandBuffer ->  Buffer ->  DeviceSize ->  IO ()
cmdDispatchIndirect = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDraw"
cmdDraw :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDraw = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDrawIndexed"
cmdDrawIndexed :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Int32 ->  Word32 ->  IO ()
cmdDrawIndexed = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDrawIndexedIndirect"
cmdDrawIndexedIndirect :: CommandBuffer ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndexedIndirect = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDrawIndirect"
cmdDrawIndirect :: CommandBuffer ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirect = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdEndQuery"
cmdEndQuery :: CommandBuffer ->  QueryPool ->  Word32 ->  IO ()
cmdEndQuery = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdEndRenderPass"
cmdEndRenderPass :: CommandBuffer ->  IO ()
cmdEndRenderPass = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdExecuteCommands"
cmdExecuteCommands :: CommandBuffer ->  Vector CommandBuffer ->  IO ()
cmdExecuteCommands = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdFillBuffer"
cmdFillBuffer :: CommandBuffer ->  Buffer ->  DeviceSize ->  DeviceSize ->  Word32 ->  IO ()
cmdFillBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdNextSubpass"
cmdNextSubpass :: CommandBuffer ->  SubpassContents ->  IO ()
cmdNextSubpass = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdPipelineBarrier"
cmdPipelineBarrier :: CommandBuffer ->  PipelineStageFlags ->  PipelineStageFlags ->  DependencyFlags ->  Vector MemoryBarrier ->  Vector BufferMemoryBarrier ->  Vector ImageMemoryBarrier ->  IO ()
cmdPipelineBarrier = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdPushConstants"
cmdPushConstants :: (Storable a) => CommandBuffer ->  PipelineLayout ->  ShaderStageFlags ->  Word32 ->  Vector a ->  IO ()
cmdPushConstants = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdResetEvent"
cmdResetEvent :: CommandBuffer ->  Event ->  PipelineStageFlags ->  IO ()
cmdResetEvent = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdResetQueryPool"
cmdResetQueryPool :: CommandBuffer ->  QueryPool ->  Word32 ->  Word32 ->  IO ()
cmdResetQueryPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdResolveImage"
cmdResolveImage :: CommandBuffer ->  Image ->  ImageLayout ->  Image ->  ImageLayout ->  Vector ImageResolve ->  IO ()
cmdResolveImage = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetBlendConstants"
cmdSetBlendConstants :: CommandBuffer ->  (CFloat, CFloat, CFloat, CFloat) ->  IO ()
cmdSetBlendConstants = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetDepthBias"
cmdSetDepthBias :: CommandBuffer ->  CFloat ->  CFloat ->  CFloat ->  IO ()
cmdSetDepthBias = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetDepthBounds"
cmdSetDepthBounds :: CommandBuffer ->  CFloat ->  CFloat ->  IO ()
cmdSetDepthBounds = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetEvent"
cmdSetEvent :: CommandBuffer ->  Event ->  PipelineStageFlags ->  IO ()
cmdSetEvent = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetLineWidth"
cmdSetLineWidth :: CommandBuffer ->  CFloat ->  IO ()
cmdSetLineWidth = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetScissor"
cmdSetScissor :: CommandBuffer ->  Word32 ->  Vector Rect2D ->  IO ()
cmdSetScissor = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetStencilCompareMask"
cmdSetStencilCompareMask :: CommandBuffer ->  StencilFaceFlags ->  Word32 ->  IO ()
cmdSetStencilCompareMask = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetStencilReference"
cmdSetStencilReference :: CommandBuffer ->  StencilFaceFlags ->  Word32 ->  IO ()
cmdSetStencilReference = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetStencilWriteMask"
cmdSetStencilWriteMask :: CommandBuffer ->  StencilFaceFlags ->  Word32 ->  IO ()
cmdSetStencilWriteMask = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetViewport"
cmdSetViewport :: CommandBuffer ->  Word32 ->  Vector Viewport ->  IO ()
cmdSetViewport = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdUpdateBuffer"
cmdUpdateBuffer :: (Storable a) => CommandBuffer ->  Buffer ->  DeviceSize ->  Vector a ->  IO ()
cmdUpdateBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdWaitEvents"
cmdWaitEvents :: CommandBuffer ->  Vector Event ->  PipelineStageFlags ->  PipelineStageFlags ->  Vector MemoryBarrier ->  Vector BufferMemoryBarrier ->  Vector ImageMemoryBarrier ->  IO ()
cmdWaitEvents = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdWriteTimestamp"
cmdWriteTimestamp :: CommandBuffer ->  PipelineStageFlagBits ->  QueryPool ->  Word32 ->  IO ()
cmdWriteTimestamp = undefined {- {wrapped (pretty cName) :: Doc ()} -}
