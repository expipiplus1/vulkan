{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.CommandBufferBuilding
  ( withCStructBufferCopy
  , fromCStructBufferCopy
  , BufferCopy(..)
  , withCStructBufferImageCopy
  , fromCStructBufferImageCopy
  , BufferImageCopy(..)
  , withCStructBufferMemoryBarrier
  , fromCStructBufferMemoryBarrier
  , BufferMemoryBarrier(..)
  , withCStructClearAttachment
  , ClearAttachment(..)
  , withCStructClearColorValue
  , ClearColorValue(..)
  , withCStructClearDepthStencilValue
  , fromCStructClearDepthStencilValue
  , ClearDepthStencilValue(..)
  , withCStructClearRect
  , fromCStructClearRect
  , ClearRect(..)
  , withCStructClearValue
  , ClearValue(..)
  , withCStructDispatchIndirectCommand
  , fromCStructDispatchIndirectCommand
  , DispatchIndirectCommand(..)
  , withCStructDrawIndexedIndirectCommand
  , fromCStructDrawIndexedIndirectCommand
  , DrawIndexedIndirectCommand(..)
  , withCStructDrawIndirectCommand
  , fromCStructDrawIndirectCommand
  , DrawIndirectCommand(..)
  , withCStructImageBlit
  , fromCStructImageBlit
  , ImageBlit(..)
  , withCStructImageCopy
  , fromCStructImageCopy
  , ImageCopy(..)
  , withCStructImageMemoryBarrier
  , fromCStructImageMemoryBarrier
  , ImageMemoryBarrier(..)
  , withCStructImageResolve
  , fromCStructImageResolve
  , ImageResolve(..)
  , withCStructImageSubresourceLayers
  , fromCStructImageSubresourceLayers
  , ImageSubresourceLayers(..)
  , IndexType
  , withCStructMemoryBarrier
  , fromCStructMemoryBarrier
  , MemoryBarrier(..)
  , withCStructRenderPassBeginInfo
  , RenderPassBeginInfo(..)
  , StencilFaceFlagBits
  , StencilFaceFlags
  , SubpassContents
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
  ( empty
  , head
  , length
  )
import Data.Vector.Generic.Sized
  ( convert
  , fromTuple
  )
import qualified Data.Vector.Storable.Sized
  ( unsafeIndex
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
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( Storable
  , pokeElemOff
  , sizeOf
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdBeginQuery
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
  )


import Graphics.Vulkan.C.Core10.CommandBufferBuilding
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
  , VkMemoryBarrier(..)
  , VkRenderPassBeginInfo(..)
  , VkStencilFaceFlagBits(..)
  , VkSubpassContents(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
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
  , fromCStructExtent3D
  , withCStructExtent3D
  )
import Graphics.Vulkan.Core10.Event
  ( Event
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageSubresourceRange(..)
  , fromCStructImageSubresourceRange
  , withCStructImageSubresourceRange
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
import Graphics.Vulkan.Core10.Pass
  ( AccessFlags
  , DependencyFlags
  , Framebuffer
  , PipelineBindPoint
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , Viewport(..)
  , Pipeline
  , PipelineLayout
  , RenderPass
  , fromCStructRect2D
  , withCStructRect2D
  , withCStructViewport
  )
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
  , fromCStructOffset3D
  , withCStructOffset3D
  )
import Graphics.Vulkan.Marshal.Utils
  ( withSizedArray
  , withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "BufferCopy"
data BufferCopy = BufferCopy
  { -- No documentation found for Nested "BufferCopy" "srcOffset"
  vkSrcOffset :: DeviceSize
  , -- No documentation found for Nested "BufferCopy" "dstOffset"
  vkDstOffset :: DeviceSize
  , -- No documentation found for Nested "BufferCopy" "size"
  vkSize :: DeviceSize
  }
  deriving (Show, Eq)
withCStructBufferCopy :: BufferCopy -> (VkBufferCopy -> IO a) -> IO a
withCStructBufferCopy from cont = cont (VkBufferCopy (vkSrcOffset (from :: BufferCopy)) (vkDstOffset (from :: BufferCopy)) (vkSize (from :: BufferCopy)))
fromCStructBufferCopy :: VkBufferCopy -> IO BufferCopy
fromCStructBufferCopy c = BufferCopy <$> pure (vkSrcOffset (c :: VkBufferCopy))
                                     <*> pure (vkDstOffset (c :: VkBufferCopy))
                                     <*> pure (vkSize (c :: VkBufferCopy))
instance Zero BufferCopy where
  zero = BufferCopy zero
                    zero
                    zero
-- No documentation found for TopLevel "BufferImageCopy"
data BufferImageCopy = BufferImageCopy
  { -- No documentation found for Nested "BufferImageCopy" "bufferOffset"
  vkBufferOffset :: DeviceSize
  , -- No documentation found for Nested "BufferImageCopy" "bufferRowLength"
  vkBufferRowLength :: Word32
  , -- No documentation found for Nested "BufferImageCopy" "bufferImageHeight"
  vkBufferImageHeight :: Word32
  , -- No documentation found for Nested "BufferImageCopy" "imageSubresource"
  vkImageSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "BufferImageCopy" "imageOffset"
  vkImageOffset :: Offset3D
  , -- No documentation found for Nested "BufferImageCopy" "imageExtent"
  vkImageExtent :: Extent3D
  }
  deriving (Show, Eq)
withCStructBufferImageCopy :: BufferImageCopy -> (VkBufferImageCopy -> IO a) -> IO a
withCStructBufferImageCopy from cont = withCStructExtent3D (vkImageExtent (from :: BufferImageCopy)) (\imageExtent -> withCStructOffset3D (vkImageOffset (from :: BufferImageCopy)) (\imageOffset -> withCStructImageSubresourceLayers (vkImageSubresource (from :: BufferImageCopy)) (\imageSubresource -> cont (VkBufferImageCopy (vkBufferOffset (from :: BufferImageCopy)) (vkBufferRowLength (from :: BufferImageCopy)) (vkBufferImageHeight (from :: BufferImageCopy)) imageSubresource imageOffset imageExtent))))
fromCStructBufferImageCopy :: VkBufferImageCopy -> IO BufferImageCopy
fromCStructBufferImageCopy c = BufferImageCopy <$> pure (vkBufferOffset (c :: VkBufferImageCopy))
                                               <*> pure (vkBufferRowLength (c :: VkBufferImageCopy))
                                               <*> pure (vkBufferImageHeight (c :: VkBufferImageCopy))
                                               <*> (fromCStructImageSubresourceLayers (vkImageSubresource (c :: VkBufferImageCopy)))
                                               <*> (fromCStructOffset3D (vkImageOffset (c :: VkBufferImageCopy)))
                                               <*> (fromCStructExtent3D (vkImageExtent (c :: VkBufferImageCopy)))
instance Zero BufferImageCopy where
  zero = BufferImageCopy zero
                         zero
                         zero
                         zero
                         zero
                         zero
-- No documentation found for TopLevel "BufferMemoryBarrier"
data BufferMemoryBarrier = BufferMemoryBarrier
  { -- Univalued Member elided
  -- No documentation found for Nested "BufferMemoryBarrier" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferMemoryBarrier" "srcAccessMask"
  vkSrcAccessMask :: AccessFlags
  , -- No documentation found for Nested "BufferMemoryBarrier" "dstAccessMask"
  vkDstAccessMask :: AccessFlags
  , -- No documentation found for Nested "BufferMemoryBarrier" "srcQueueFamilyIndex"
  vkSrcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "BufferMemoryBarrier" "dstQueueFamilyIndex"
  vkDstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "BufferMemoryBarrier" "buffer"
  vkBuffer :: Buffer
  , -- No documentation found for Nested "BufferMemoryBarrier" "offset"
  vkOffset :: DeviceSize
  , -- No documentation found for Nested "BufferMemoryBarrier" "size"
  vkSize :: DeviceSize
  }
  deriving (Show, Eq)
withCStructBufferMemoryBarrier :: BufferMemoryBarrier -> (VkBufferMemoryBarrier -> IO a) -> IO a
withCStructBufferMemoryBarrier from cont = maybeWith withSomeVkStruct (vkPNext (from :: BufferMemoryBarrier)) (\pPNext -> cont (VkBufferMemoryBarrier VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER pPNext (vkSrcAccessMask (from :: BufferMemoryBarrier)) (vkDstAccessMask (from :: BufferMemoryBarrier)) (vkSrcQueueFamilyIndex (from :: BufferMemoryBarrier)) (vkDstQueueFamilyIndex (from :: BufferMemoryBarrier)) (vkBuffer (from :: BufferMemoryBarrier)) (vkOffset (from :: BufferMemoryBarrier)) (vkSize (from :: BufferMemoryBarrier))))
fromCStructBufferMemoryBarrier :: VkBufferMemoryBarrier -> IO BufferMemoryBarrier
fromCStructBufferMemoryBarrier c = BufferMemoryBarrier <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferMemoryBarrier)))
                                                       <*> pure (vkSrcAccessMask (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkDstAccessMask (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkSrcQueueFamilyIndex (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkDstQueueFamilyIndex (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkBuffer (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkOffset (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkSize (c :: VkBufferMemoryBarrier))
instance Zero BufferMemoryBarrier where
  zero = BufferMemoryBarrier Nothing
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero
-- No documentation found for TopLevel "ClearAttachment"
data ClearAttachment = ClearAttachment
  { -- No documentation found for Nested "ClearAttachment" "aspectMask"
  vkAspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ClearAttachment" "colorAttachment"
  vkColorAttachment :: Word32
  , -- No documentation found for Nested "ClearAttachment" "clearValue"
  vkClearValue :: ClearValue
  }
  deriving (Show, Eq)
withCStructClearAttachment :: ClearAttachment -> (VkClearAttachment -> IO a) -> IO a
withCStructClearAttachment from cont = withCStructClearValue (vkClearValue (from :: ClearAttachment)) (\clearValue -> cont (VkClearAttachment (vkAspectMask (from :: ClearAttachment)) (vkColorAttachment (from :: ClearAttachment)) clearValue))
-- No fromCStruct function for types containing unions
instance Zero ClearAttachment where
  zero = ClearAttachment zero
                         zero
                         zero
-- No documentation found for TopLevel "ClearColorValue"
data ClearColorValue
  = Float32 (CFloat, CFloat, CFloat, CFloat)
  | Int32 (Int32, Int32, Int32, Int32)
  | Uint32 (Word32, Word32, Word32, Word32)
  deriving (Show, Eq)
withCStructClearColorValue :: ClearColorValue -> (VkClearColorValue -> IO a) -> IO a
withCStructClearColorValue from cont = case from of
  Float32 x -> cont (VkFloat32 (fromTuple x))
  Int32 x -> cont (VkInt32 (fromTuple x))
  Uint32 x -> cont (VkUint32 (fromTuple x))
-- No FromCStruct function for sum types
instance Zero ClearColorValue where
  zero = Float32 (zero, zero, zero, zero)
-- No documentation found for TopLevel "ClearDepthStencilValue"
data ClearDepthStencilValue = ClearDepthStencilValue
  { -- No documentation found for Nested "ClearDepthStencilValue" "depth"
  vkDepth :: CFloat
  , -- No documentation found for Nested "ClearDepthStencilValue" "stencil"
  vkStencil :: Word32
  }
  deriving (Show, Eq)
withCStructClearDepthStencilValue :: ClearDepthStencilValue -> (VkClearDepthStencilValue -> IO a) -> IO a
withCStructClearDepthStencilValue from cont = cont (VkClearDepthStencilValue (vkDepth (from :: ClearDepthStencilValue)) (vkStencil (from :: ClearDepthStencilValue)))
fromCStructClearDepthStencilValue :: VkClearDepthStencilValue -> IO ClearDepthStencilValue
fromCStructClearDepthStencilValue c = ClearDepthStencilValue <$> pure (vkDepth (c :: VkClearDepthStencilValue))
                                                             <*> pure (vkStencil (c :: VkClearDepthStencilValue))
instance Zero ClearDepthStencilValue where
  zero = ClearDepthStencilValue zero
                                zero
-- No documentation found for TopLevel "ClearRect"
data ClearRect = ClearRect
  { -- No documentation found for Nested "ClearRect" "rect"
  vkRect :: Rect2D
  , -- No documentation found for Nested "ClearRect" "baseArrayLayer"
  vkBaseArrayLayer :: Word32
  , -- No documentation found for Nested "ClearRect" "layerCount"
  vkLayerCount :: Word32
  }
  deriving (Show, Eq)
withCStructClearRect :: ClearRect -> (VkClearRect -> IO a) -> IO a
withCStructClearRect from cont = withCStructRect2D (vkRect (from :: ClearRect)) (\rect -> cont (VkClearRect rect (vkBaseArrayLayer (from :: ClearRect)) (vkLayerCount (from :: ClearRect))))
fromCStructClearRect :: VkClearRect -> IO ClearRect
fromCStructClearRect c = ClearRect <$> (fromCStructRect2D (vkRect (c :: VkClearRect)))
                                   <*> pure (vkBaseArrayLayer (c :: VkClearRect))
                                   <*> pure (vkLayerCount (c :: VkClearRect))
instance Zero ClearRect where
  zero = ClearRect zero
                   zero
                   zero
-- No documentation found for TopLevel "ClearValue"
data ClearValue
  = Color ClearColorValue
  | DepthStencil ClearDepthStencilValue
  deriving (Show, Eq)
withCStructClearValue :: ClearValue -> (VkClearValue -> IO a) -> IO a
withCStructClearValue from cont = case from of
  Color x -> withCStructClearColorValue x (cont . VkColor)
  DepthStencil x -> withCStructClearDepthStencilValue x (cont . VkDepthStencil)
-- No FromCStruct function for sum types
instance Zero ClearValue where
  zero = Color zero
-- No documentation found for TopLevel "DispatchIndirectCommand"
data DispatchIndirectCommand = DispatchIndirectCommand
  { -- No documentation found for Nested "DispatchIndirectCommand" "x"
  vkX :: Word32
  , -- No documentation found for Nested "DispatchIndirectCommand" "y"
  vkY :: Word32
  , -- No documentation found for Nested "DispatchIndirectCommand" "z"
  vkZ :: Word32
  }
  deriving (Show, Eq)
withCStructDispatchIndirectCommand :: DispatchIndirectCommand -> (VkDispatchIndirectCommand -> IO a) -> IO a
withCStructDispatchIndirectCommand from cont = cont (VkDispatchIndirectCommand (vkX (from :: DispatchIndirectCommand)) (vkY (from :: DispatchIndirectCommand)) (vkZ (from :: DispatchIndirectCommand)))
fromCStructDispatchIndirectCommand :: VkDispatchIndirectCommand -> IO DispatchIndirectCommand
fromCStructDispatchIndirectCommand c = DispatchIndirectCommand <$> pure (vkX (c :: VkDispatchIndirectCommand))
                                                               <*> pure (vkY (c :: VkDispatchIndirectCommand))
                                                               <*> pure (vkZ (c :: VkDispatchIndirectCommand))
instance Zero DispatchIndirectCommand where
  zero = DispatchIndirectCommand zero
                                 zero
                                 zero
-- No documentation found for TopLevel "DrawIndexedIndirectCommand"
data DrawIndexedIndirectCommand = DrawIndexedIndirectCommand
  { -- No documentation found for Nested "DrawIndexedIndirectCommand" "indexCount"
  vkIndexCount :: Word32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "instanceCount"
  vkInstanceCount :: Word32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "firstIndex"
  vkFirstIndex :: Word32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "vertexOffset"
  vkVertexOffset :: Int32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "firstInstance"
  vkFirstInstance :: Word32
  }
  deriving (Show, Eq)
withCStructDrawIndexedIndirectCommand :: DrawIndexedIndirectCommand -> (VkDrawIndexedIndirectCommand -> IO a) -> IO a
withCStructDrawIndexedIndirectCommand from cont = cont (VkDrawIndexedIndirectCommand (vkIndexCount (from :: DrawIndexedIndirectCommand)) (vkInstanceCount (from :: DrawIndexedIndirectCommand)) (vkFirstIndex (from :: DrawIndexedIndirectCommand)) (vkVertexOffset (from :: DrawIndexedIndirectCommand)) (vkFirstInstance (from :: DrawIndexedIndirectCommand)))
fromCStructDrawIndexedIndirectCommand :: VkDrawIndexedIndirectCommand -> IO DrawIndexedIndirectCommand
fromCStructDrawIndexedIndirectCommand c = DrawIndexedIndirectCommand <$> pure (vkIndexCount (c :: VkDrawIndexedIndirectCommand))
                                                                     <*> pure (vkInstanceCount (c :: VkDrawIndexedIndirectCommand))
                                                                     <*> pure (vkFirstIndex (c :: VkDrawIndexedIndirectCommand))
                                                                     <*> pure (vkVertexOffset (c :: VkDrawIndexedIndirectCommand))
                                                                     <*> pure (vkFirstInstance (c :: VkDrawIndexedIndirectCommand))
instance Zero DrawIndexedIndirectCommand where
  zero = DrawIndexedIndirectCommand zero
                                    zero
                                    zero
                                    zero
                                    zero
-- No documentation found for TopLevel "DrawIndirectCommand"
data DrawIndirectCommand = DrawIndirectCommand
  { -- No documentation found for Nested "DrawIndirectCommand" "vertexCount"
  vkVertexCount :: Word32
  , -- No documentation found for Nested "DrawIndirectCommand" "instanceCount"
  vkInstanceCount :: Word32
  , -- No documentation found for Nested "DrawIndirectCommand" "firstVertex"
  vkFirstVertex :: Word32
  , -- No documentation found for Nested "DrawIndirectCommand" "firstInstance"
  vkFirstInstance :: Word32
  }
  deriving (Show, Eq)
withCStructDrawIndirectCommand :: DrawIndirectCommand -> (VkDrawIndirectCommand -> IO a) -> IO a
withCStructDrawIndirectCommand from cont = cont (VkDrawIndirectCommand (vkVertexCount (from :: DrawIndirectCommand)) (vkInstanceCount (from :: DrawIndirectCommand)) (vkFirstVertex (from :: DrawIndirectCommand)) (vkFirstInstance (from :: DrawIndirectCommand)))
fromCStructDrawIndirectCommand :: VkDrawIndirectCommand -> IO DrawIndirectCommand
fromCStructDrawIndirectCommand c = DrawIndirectCommand <$> pure (vkVertexCount (c :: VkDrawIndirectCommand))
                                                       <*> pure (vkInstanceCount (c :: VkDrawIndirectCommand))
                                                       <*> pure (vkFirstVertex (c :: VkDrawIndirectCommand))
                                                       <*> pure (vkFirstInstance (c :: VkDrawIndirectCommand))
instance Zero DrawIndirectCommand where
  zero = DrawIndirectCommand zero
                             zero
                             zero
                             zero
-- No documentation found for TopLevel "ImageBlit"
data ImageBlit = ImageBlit
  { -- No documentation found for Nested "ImageBlit" "srcSubresource"
  vkSrcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageBlit" "srcOffsets"
  vkSrcOffsets :: (Offset3D, Offset3D)
  , -- No documentation found for Nested "ImageBlit" "dstSubresource"
  vkDstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageBlit" "dstOffsets"
  vkDstOffsets :: (Offset3D, Offset3D)
  }
  deriving (Show, Eq)
withCStructImageBlit :: ImageBlit -> (VkImageBlit -> IO a) -> IO a
withCStructImageBlit from cont = withSizedArray withCStructOffset3D (fromTuple (vkDstOffsets (from :: ImageBlit))) (\dstOffsets -> withCStructImageSubresourceLayers (vkDstSubresource (from :: ImageBlit)) (\dstSubresource -> withSizedArray withCStructOffset3D (fromTuple (vkSrcOffsets (from :: ImageBlit))) (\srcOffsets -> withCStructImageSubresourceLayers (vkSrcSubresource (from :: ImageBlit)) (\srcSubresource -> cont (VkImageBlit srcSubresource (Data.Vector.Generic.Sized.convert srcOffsets) dstSubresource (Data.Vector.Generic.Sized.convert dstOffsets))))))
fromCStructImageBlit :: VkImageBlit -> IO ImageBlit
fromCStructImageBlit c = ImageBlit <$> (fromCStructImageSubresourceLayers (vkSrcSubresource (c :: VkImageBlit)))
                                   <*> (let x = (vkSrcOffsets (c :: VkImageBlit)) in (, ) <$> fromCStructOffset3D (Data.Vector.Storable.Sized.unsafeIndex x 0)
                                                                                          <*> fromCStructOffset3D (Data.Vector.Storable.Sized.unsafeIndex x 1))
                                   <*> (fromCStructImageSubresourceLayers (vkDstSubresource (c :: VkImageBlit)))
                                   <*> (let x = (vkDstOffsets (c :: VkImageBlit)) in (, ) <$> fromCStructOffset3D (Data.Vector.Storable.Sized.unsafeIndex x 0)
                                                                                          <*> fromCStructOffset3D (Data.Vector.Storable.Sized.unsafeIndex x 1))
instance Zero ImageBlit where
  zero = ImageBlit zero
                   (zero, zero)
                   zero
                   (zero, zero)
-- No documentation found for TopLevel "ImageCopy"
data ImageCopy = ImageCopy
  { -- No documentation found for Nested "ImageCopy" "srcSubresource"
  vkSrcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageCopy" "srcOffset"
  vkSrcOffset :: Offset3D
  , -- No documentation found for Nested "ImageCopy" "dstSubresource"
  vkDstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageCopy" "dstOffset"
  vkDstOffset :: Offset3D
  , -- No documentation found for Nested "ImageCopy" "extent"
  vkExtent :: Extent3D
  }
  deriving (Show, Eq)
withCStructImageCopy :: ImageCopy -> (VkImageCopy -> IO a) -> IO a
withCStructImageCopy from cont = withCStructExtent3D (vkExtent (from :: ImageCopy)) (\extent -> withCStructOffset3D (vkDstOffset (from :: ImageCopy)) (\dstOffset -> withCStructImageSubresourceLayers (vkDstSubresource (from :: ImageCopy)) (\dstSubresource -> withCStructOffset3D (vkSrcOffset (from :: ImageCopy)) (\srcOffset -> withCStructImageSubresourceLayers (vkSrcSubresource (from :: ImageCopy)) (\srcSubresource -> cont (VkImageCopy srcSubresource srcOffset dstSubresource dstOffset extent))))))
fromCStructImageCopy :: VkImageCopy -> IO ImageCopy
fromCStructImageCopy c = ImageCopy <$> (fromCStructImageSubresourceLayers (vkSrcSubresource (c :: VkImageCopy)))
                                   <*> (fromCStructOffset3D (vkSrcOffset (c :: VkImageCopy)))
                                   <*> (fromCStructImageSubresourceLayers (vkDstSubresource (c :: VkImageCopy)))
                                   <*> (fromCStructOffset3D (vkDstOffset (c :: VkImageCopy)))
                                   <*> (fromCStructExtent3D (vkExtent (c :: VkImageCopy)))
instance Zero ImageCopy where
  zero = ImageCopy zero
                   zero
                   zero
                   zero
                   zero
-- No documentation found for TopLevel "ImageMemoryBarrier"
data ImageMemoryBarrier = ImageMemoryBarrier
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageMemoryBarrier" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageMemoryBarrier" "srcAccessMask"
  vkSrcAccessMask :: AccessFlags
  , -- No documentation found for Nested "ImageMemoryBarrier" "dstAccessMask"
  vkDstAccessMask :: AccessFlags
  , -- No documentation found for Nested "ImageMemoryBarrier" "oldLayout"
  vkOldLayout :: ImageLayout
  , -- No documentation found for Nested "ImageMemoryBarrier" "newLayout"
  vkNewLayout :: ImageLayout
  , -- No documentation found for Nested "ImageMemoryBarrier" "srcQueueFamilyIndex"
  vkSrcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "ImageMemoryBarrier" "dstQueueFamilyIndex"
  vkDstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "ImageMemoryBarrier" "image"
  vkImage :: Image
  , -- No documentation found for Nested "ImageMemoryBarrier" "subresourceRange"
  vkSubresourceRange :: ImageSubresourceRange
  }
  deriving (Show, Eq)
withCStructImageMemoryBarrier :: ImageMemoryBarrier -> (VkImageMemoryBarrier -> IO a) -> IO a
withCStructImageMemoryBarrier from cont = withCStructImageSubresourceRange (vkSubresourceRange (from :: ImageMemoryBarrier)) (\subresourceRange -> maybeWith withSomeVkStruct (vkPNext (from :: ImageMemoryBarrier)) (\pPNext -> cont (VkImageMemoryBarrier VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER pPNext (vkSrcAccessMask (from :: ImageMemoryBarrier)) (vkDstAccessMask (from :: ImageMemoryBarrier)) (vkOldLayout (from :: ImageMemoryBarrier)) (vkNewLayout (from :: ImageMemoryBarrier)) (vkSrcQueueFamilyIndex (from :: ImageMemoryBarrier)) (vkDstQueueFamilyIndex (from :: ImageMemoryBarrier)) (vkImage (from :: ImageMemoryBarrier)) subresourceRange)))
fromCStructImageMemoryBarrier :: VkImageMemoryBarrier -> IO ImageMemoryBarrier
fromCStructImageMemoryBarrier c = ImageMemoryBarrier <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageMemoryBarrier)))
                                                     <*> pure (vkSrcAccessMask (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkDstAccessMask (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkOldLayout (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkNewLayout (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkSrcQueueFamilyIndex (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkDstQueueFamilyIndex (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkImage (c :: VkImageMemoryBarrier))
                                                     <*> (fromCStructImageSubresourceRange (vkSubresourceRange (c :: VkImageMemoryBarrier)))
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
-- No documentation found for TopLevel "ImageResolve"
data ImageResolve = ImageResolve
  { -- No documentation found for Nested "ImageResolve" "srcSubresource"
  vkSrcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageResolve" "srcOffset"
  vkSrcOffset :: Offset3D
  , -- No documentation found for Nested "ImageResolve" "dstSubresource"
  vkDstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageResolve" "dstOffset"
  vkDstOffset :: Offset3D
  , -- No documentation found for Nested "ImageResolve" "extent"
  vkExtent :: Extent3D
  }
  deriving (Show, Eq)
withCStructImageResolve :: ImageResolve -> (VkImageResolve -> IO a) -> IO a
withCStructImageResolve from cont = withCStructExtent3D (vkExtent (from :: ImageResolve)) (\extent -> withCStructOffset3D (vkDstOffset (from :: ImageResolve)) (\dstOffset -> withCStructImageSubresourceLayers (vkDstSubresource (from :: ImageResolve)) (\dstSubresource -> withCStructOffset3D (vkSrcOffset (from :: ImageResolve)) (\srcOffset -> withCStructImageSubresourceLayers (vkSrcSubresource (from :: ImageResolve)) (\srcSubresource -> cont (VkImageResolve srcSubresource srcOffset dstSubresource dstOffset extent))))))
fromCStructImageResolve :: VkImageResolve -> IO ImageResolve
fromCStructImageResolve c = ImageResolve <$> (fromCStructImageSubresourceLayers (vkSrcSubresource (c :: VkImageResolve)))
                                         <*> (fromCStructOffset3D (vkSrcOffset (c :: VkImageResolve)))
                                         <*> (fromCStructImageSubresourceLayers (vkDstSubresource (c :: VkImageResolve)))
                                         <*> (fromCStructOffset3D (vkDstOffset (c :: VkImageResolve)))
                                         <*> (fromCStructExtent3D (vkExtent (c :: VkImageResolve)))
instance Zero ImageResolve where
  zero = ImageResolve zero
                      zero
                      zero
                      zero
                      zero
-- No documentation found for TopLevel "ImageSubresourceLayers"
data ImageSubresourceLayers = ImageSubresourceLayers
  { -- No documentation found for Nested "ImageSubresourceLayers" "aspectMask"
  vkAspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ImageSubresourceLayers" "mipLevel"
  vkMipLevel :: Word32
  , -- No documentation found for Nested "ImageSubresourceLayers" "baseArrayLayer"
  vkBaseArrayLayer :: Word32
  , -- No documentation found for Nested "ImageSubresourceLayers" "layerCount"
  vkLayerCount :: Word32
  }
  deriving (Show, Eq)
withCStructImageSubresourceLayers :: ImageSubresourceLayers -> (VkImageSubresourceLayers -> IO a) -> IO a
withCStructImageSubresourceLayers from cont = cont (VkImageSubresourceLayers (vkAspectMask (from :: ImageSubresourceLayers)) (vkMipLevel (from :: ImageSubresourceLayers)) (vkBaseArrayLayer (from :: ImageSubresourceLayers)) (vkLayerCount (from :: ImageSubresourceLayers)))
fromCStructImageSubresourceLayers :: VkImageSubresourceLayers -> IO ImageSubresourceLayers
fromCStructImageSubresourceLayers c = ImageSubresourceLayers <$> pure (vkAspectMask (c :: VkImageSubresourceLayers))
                                                             <*> pure (vkMipLevel (c :: VkImageSubresourceLayers))
                                                             <*> pure (vkBaseArrayLayer (c :: VkImageSubresourceLayers))
                                                             <*> pure (vkLayerCount (c :: VkImageSubresourceLayers))
instance Zero ImageSubresourceLayers where
  zero = ImageSubresourceLayers zero
                                zero
                                zero
                                zero
-- No documentation found for TopLevel "IndexType"
type IndexType = VkIndexType
-- No documentation found for TopLevel "MemoryBarrier"
data MemoryBarrier = MemoryBarrier
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryBarrier" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryBarrier" "srcAccessMask"
  vkSrcAccessMask :: AccessFlags
  , -- No documentation found for Nested "MemoryBarrier" "dstAccessMask"
  vkDstAccessMask :: AccessFlags
  }
  deriving (Show, Eq)
withCStructMemoryBarrier :: MemoryBarrier -> (VkMemoryBarrier -> IO a) -> IO a
withCStructMemoryBarrier from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryBarrier)) (\pPNext -> cont (VkMemoryBarrier VK_STRUCTURE_TYPE_MEMORY_BARRIER pPNext (vkSrcAccessMask (from :: MemoryBarrier)) (vkDstAccessMask (from :: MemoryBarrier))))
fromCStructMemoryBarrier :: VkMemoryBarrier -> IO MemoryBarrier
fromCStructMemoryBarrier c = MemoryBarrier <$> -- Univalued Member elided
                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryBarrier)))
                                           <*> pure (vkSrcAccessMask (c :: VkMemoryBarrier))
                                           <*> pure (vkDstAccessMask (c :: VkMemoryBarrier))
instance Zero MemoryBarrier where
  zero = MemoryBarrier Nothing
                       zero
                       zero
-- No documentation found for TopLevel "RenderPassBeginInfo"
data RenderPassBeginInfo = RenderPassBeginInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "RenderPassBeginInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassBeginInfo" "renderPass"
  vkRenderPass :: RenderPass
  , -- No documentation found for Nested "RenderPassBeginInfo" "framebuffer"
  vkFramebuffer :: Framebuffer
  , -- No documentation found for Nested "RenderPassBeginInfo" "renderArea"
  vkRenderArea :: Rect2D
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassBeginInfo" "pClearValues"
  vkPClearValues :: Vector ClearValue
  }
  deriving (Show, Eq)
withCStructRenderPassBeginInfo :: RenderPassBeginInfo -> (VkRenderPassBeginInfo -> IO a) -> IO a
withCStructRenderPassBeginInfo from cont = withVec withCStructClearValue (vkPClearValues (from :: RenderPassBeginInfo)) (\pClearValues -> withCStructRect2D (vkRenderArea (from :: RenderPassBeginInfo)) (\renderArea -> maybeWith withSomeVkStruct (vkPNext (from :: RenderPassBeginInfo)) (\pPNext -> cont (VkRenderPassBeginInfo VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO pPNext (vkRenderPass (from :: RenderPassBeginInfo)) (vkFramebuffer (from :: RenderPassBeginInfo)) renderArea (fromIntegral (Data.Vector.length (vkPClearValues (from :: RenderPassBeginInfo)))) pClearValues))))
-- No fromCStruct function for types containing unions
instance Zero RenderPassBeginInfo where
  zero = RenderPassBeginInfo Nothing
                             zero
                             zero
                             zero
                             Data.Vector.empty
-- No documentation found for TopLevel "StencilFaceFlagBits"
type StencilFaceFlagBits = VkStencilFaceFlagBits
-- No documentation found for TopLevel "StencilFaceFlags"
type StencilFaceFlags = StencilFaceFlagBits
-- No documentation found for TopLevel "SubpassContents"
type SubpassContents = VkSubpassContents

-- | Wrapper for 'vkCmdBeginQuery'
cmdBeginQuery :: CommandBuffer ->  QueryPool ->  Word32 ->  QueryControlFlags ->  IO ()
cmdBeginQuery = \(CommandBuffer commandBuffer commandTable) -> \queryPool -> \query -> \flags -> Graphics.Vulkan.C.Dynamic.cmdBeginQuery commandTable commandBuffer queryPool query flags *> (pure ())

-- | Wrapper for 'vkCmdBeginRenderPass'
cmdBeginRenderPass :: CommandBuffer ->  RenderPassBeginInfo ->  SubpassContents ->  IO ()
cmdBeginRenderPass = \(CommandBuffer commandBuffer commandTable) -> \renderPassBegin -> \contents -> (\a -> withCStructRenderPassBeginInfo a . flip with) renderPassBegin (\pRenderPassBegin -> Graphics.Vulkan.C.Dynamic.cmdBeginRenderPass commandTable commandBuffer pRenderPassBegin contents *> (pure ()))

-- | Wrapper for 'vkCmdBindDescriptorSets'
cmdBindDescriptorSets :: CommandBuffer ->  PipelineBindPoint ->  PipelineLayout ->  Word32 ->  Vector DescriptorSet ->  Vector Word32 ->  IO ()
cmdBindDescriptorSets = \(CommandBuffer commandBuffer commandTable) -> \pipelineBindPoint -> \layout -> \firstSet -> \descriptorSets -> \dynamicOffsets -> withVec (&) dynamicOffsets (\pDynamicOffsets -> withVec (&) descriptorSets (\pDescriptorSets -> Graphics.Vulkan.C.Dynamic.cmdBindDescriptorSets commandTable commandBuffer pipelineBindPoint layout firstSet (fromIntegral $ Data.Vector.length descriptorSets) pDescriptorSets (fromIntegral $ Data.Vector.length dynamicOffsets) pDynamicOffsets *> (pure ())))

-- | Wrapper for 'vkCmdBindIndexBuffer'
cmdBindIndexBuffer :: CommandBuffer ->  Buffer ->  DeviceSize ->  IndexType ->  IO ()
cmdBindIndexBuffer = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> \indexType -> Graphics.Vulkan.C.Dynamic.cmdBindIndexBuffer commandTable commandBuffer buffer offset indexType *> (pure ())

-- | Wrapper for 'vkCmdBindPipeline'
cmdBindPipeline :: CommandBuffer ->  PipelineBindPoint ->  Pipeline ->  IO ()
cmdBindPipeline = \(CommandBuffer commandBuffer commandTable) -> \pipelineBindPoint -> \pipeline -> Graphics.Vulkan.C.Dynamic.cmdBindPipeline commandTable commandBuffer pipelineBindPoint pipeline *> (pure ())

-- | Wrapper for 'vkCmdBindVertexBuffers'
cmdBindVertexBuffers :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Vector DeviceSize ->  IO ()
cmdBindVertexBuffers = \(CommandBuffer commandBuffer commandTable) -> \firstBinding -> \buffers -> \offsets -> withVec (&) offsets (\pOffsets -> withVec (&) buffers (\pBuffers -> Graphics.Vulkan.C.Dynamic.cmdBindVertexBuffers commandTable commandBuffer firstBinding (fromIntegral $ Data.Vector.length buffers `min` Data.Vector.length offsets) pBuffers pOffsets *> (pure ())))

-- | Wrapper for 'vkCmdBlitImage'
cmdBlitImage :: CommandBuffer ->  Image ->  ImageLayout ->  Image ->  ImageLayout ->  Vector ImageBlit ->  Filter ->  IO ()
cmdBlitImage = \(CommandBuffer commandBuffer commandTable) -> \srcImage -> \srcImageLayout -> \dstImage -> \dstImageLayout -> \regions -> \filter' -> withVec withCStructImageBlit regions (\pRegions -> Graphics.Vulkan.C.Dynamic.cmdBlitImage commandTable commandBuffer srcImage srcImageLayout dstImage dstImageLayout (fromIntegral $ Data.Vector.length regions) pRegions filter' *> (pure ()))

-- | Wrapper for 'vkCmdClearAttachments'
cmdClearAttachments :: CommandBuffer ->  Vector ClearAttachment ->  Vector ClearRect ->  IO ()
cmdClearAttachments = \(CommandBuffer commandBuffer commandTable) -> \attachments -> \rects -> withVec withCStructClearRect rects (\pRects -> withVec withCStructClearAttachment attachments (\pAttachments -> Graphics.Vulkan.C.Dynamic.cmdClearAttachments commandTable commandBuffer (fromIntegral $ Data.Vector.length attachments) pAttachments (fromIntegral $ Data.Vector.length rects) pRects *> (pure ())))

-- | Wrapper for 'vkCmdClearColorImage'
cmdClearColorImage :: CommandBuffer ->  Image ->  ImageLayout ->  ClearColorValue ->  Vector ImageSubresourceRange ->  IO ()
cmdClearColorImage = \(CommandBuffer commandBuffer commandTable) -> \image -> \imageLayout -> \color -> \ranges -> withVec withCStructImageSubresourceRange ranges (\pRanges -> (\a -> withCStructClearColorValue a . flip with) color (\pColor -> Graphics.Vulkan.C.Dynamic.cmdClearColorImage commandTable commandBuffer image imageLayout pColor (fromIntegral $ Data.Vector.length ranges) pRanges *> (pure ())))

-- | Wrapper for 'vkCmdClearDepthStencilImage'
cmdClearDepthStencilImage :: CommandBuffer ->  Image ->  ImageLayout ->  ClearDepthStencilValue ->  Vector ImageSubresourceRange ->  IO ()
cmdClearDepthStencilImage = \(CommandBuffer commandBuffer commandTable) -> \image -> \imageLayout -> \depthStencil -> \ranges -> withVec withCStructImageSubresourceRange ranges (\pRanges -> (\a -> withCStructClearDepthStencilValue a . flip with) depthStencil (\pDepthStencil -> Graphics.Vulkan.C.Dynamic.cmdClearDepthStencilImage commandTable commandBuffer image imageLayout pDepthStencil (fromIntegral $ Data.Vector.length ranges) pRanges *> (pure ())))

-- | Wrapper for 'vkCmdCopyBuffer'
cmdCopyBuffer :: CommandBuffer ->  Buffer ->  Buffer ->  Vector BufferCopy ->  IO ()
cmdCopyBuffer = \(CommandBuffer commandBuffer commandTable) -> \srcBuffer -> \dstBuffer -> \regions -> withVec withCStructBufferCopy regions (\pRegions -> Graphics.Vulkan.C.Dynamic.cmdCopyBuffer commandTable commandBuffer srcBuffer dstBuffer (fromIntegral $ Data.Vector.length regions) pRegions *> (pure ()))

-- | Wrapper for 'vkCmdCopyBufferToImage'
cmdCopyBufferToImage :: CommandBuffer ->  Buffer ->  Image ->  ImageLayout ->  Vector BufferImageCopy ->  IO ()
cmdCopyBufferToImage = \(CommandBuffer commandBuffer commandTable) -> \srcBuffer -> \dstImage -> \dstImageLayout -> \regions -> withVec withCStructBufferImageCopy regions (\pRegions -> Graphics.Vulkan.C.Dynamic.cmdCopyBufferToImage commandTable commandBuffer srcBuffer dstImage dstImageLayout (fromIntegral $ Data.Vector.length regions) pRegions *> (pure ()))

-- | Wrapper for 'vkCmdCopyImage'
cmdCopyImage :: CommandBuffer ->  Image ->  ImageLayout ->  Image ->  ImageLayout ->  Vector ImageCopy ->  IO ()
cmdCopyImage = \(CommandBuffer commandBuffer commandTable) -> \srcImage -> \srcImageLayout -> \dstImage -> \dstImageLayout -> \regions -> withVec withCStructImageCopy regions (\pRegions -> Graphics.Vulkan.C.Dynamic.cmdCopyImage commandTable commandBuffer srcImage srcImageLayout dstImage dstImageLayout (fromIntegral $ Data.Vector.length regions) pRegions *> (pure ()))

-- | Wrapper for 'vkCmdCopyImageToBuffer'
cmdCopyImageToBuffer :: CommandBuffer ->  Image ->  ImageLayout ->  Buffer ->  Vector BufferImageCopy ->  IO ()
cmdCopyImageToBuffer = \(CommandBuffer commandBuffer commandTable) -> \srcImage -> \srcImageLayout -> \dstBuffer -> \regions -> withVec withCStructBufferImageCopy regions (\pRegions -> Graphics.Vulkan.C.Dynamic.cmdCopyImageToBuffer commandTable commandBuffer srcImage srcImageLayout dstBuffer (fromIntegral $ Data.Vector.length regions) pRegions *> (pure ()))

-- | Wrapper for 'vkCmdCopyQueryPoolResults'
cmdCopyQueryPoolResults :: CommandBuffer ->  QueryPool ->  Word32 ->  Word32 ->  Buffer ->  DeviceSize ->  DeviceSize ->  QueryResultFlags ->  IO ()
cmdCopyQueryPoolResults = \(CommandBuffer commandBuffer commandTable) -> \queryPool -> \firstQuery -> \queryCount -> \dstBuffer -> \dstOffset -> \stride -> \flags -> Graphics.Vulkan.C.Dynamic.cmdCopyQueryPoolResults commandTable commandBuffer queryPool firstQuery queryCount dstBuffer dstOffset stride flags *> (pure ())

-- | Wrapper for 'vkCmdDispatch'
cmdDispatch :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDispatch = \(CommandBuffer commandBuffer commandTable) -> \groupCountX -> \groupCountY -> \groupCountZ -> Graphics.Vulkan.C.Dynamic.cmdDispatch commandTable commandBuffer groupCountX groupCountY groupCountZ *> (pure ())

-- | Wrapper for 'vkCmdDispatchIndirect'
cmdDispatchIndirect :: CommandBuffer ->  Buffer ->  DeviceSize ->  IO ()
cmdDispatchIndirect = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> Graphics.Vulkan.C.Dynamic.cmdDispatchIndirect commandTable commandBuffer buffer offset *> (pure ())

-- | Wrapper for 'vkCmdDraw'
cmdDraw :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDraw = \(CommandBuffer commandBuffer commandTable) -> \vertexCount -> \instanceCount -> \firstVertex -> \firstInstance -> Graphics.Vulkan.C.Dynamic.cmdDraw commandTable commandBuffer vertexCount instanceCount firstVertex firstInstance *> (pure ())

-- | Wrapper for 'vkCmdDrawIndexed'
cmdDrawIndexed :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Int32 ->  Word32 ->  IO ()
cmdDrawIndexed = \(CommandBuffer commandBuffer commandTable) -> \indexCount -> \instanceCount -> \firstIndex -> \vertexOffset -> \firstInstance -> Graphics.Vulkan.C.Dynamic.cmdDrawIndexed commandTable commandBuffer indexCount instanceCount firstIndex vertexOffset firstInstance *> (pure ())

-- | Wrapper for 'vkCmdDrawIndexedIndirect'
cmdDrawIndexedIndirect :: CommandBuffer ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndexedIndirect = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> \drawCount -> \stride -> Graphics.Vulkan.C.Dynamic.cmdDrawIndexedIndirect commandTable commandBuffer buffer offset drawCount stride *> (pure ())

-- | Wrapper for 'vkCmdDrawIndirect'
cmdDrawIndirect :: CommandBuffer ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirect = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> \drawCount -> \stride -> Graphics.Vulkan.C.Dynamic.cmdDrawIndirect commandTable commandBuffer buffer offset drawCount stride *> (pure ())

-- | Wrapper for 'vkCmdEndQuery'
cmdEndQuery :: CommandBuffer ->  QueryPool ->  Word32 ->  IO ()
cmdEndQuery = \(CommandBuffer commandBuffer commandTable) -> \queryPool -> \query -> Graphics.Vulkan.C.Dynamic.cmdEndQuery commandTable commandBuffer queryPool query *> (pure ())

-- | Wrapper for 'vkCmdEndRenderPass'
cmdEndRenderPass :: CommandBuffer ->  IO ()
cmdEndRenderPass = \(CommandBuffer commandBuffer commandTable) -> Graphics.Vulkan.C.Dynamic.cmdEndRenderPass commandTable commandBuffer *> (pure ())

-- | Wrapper for 'vkCmdExecuteCommands'
cmdExecuteCommands :: CommandBuffer ->  Vector CommandBuffer ->  IO ()
cmdExecuteCommands = \(CommandBuffer commandBuffer commandTable) -> \commandBuffers -> withVec ((&) . commandBufferHandle) commandBuffers (\pCommandBuffers -> Graphics.Vulkan.C.Dynamic.cmdExecuteCommands commandTable commandBuffer (fromIntegral $ Data.Vector.length commandBuffers) pCommandBuffers *> (pure ()))

-- | Wrapper for 'vkCmdFillBuffer'
cmdFillBuffer :: CommandBuffer ->  Buffer ->  DeviceSize ->  DeviceSize ->  Word32 ->  IO ()
cmdFillBuffer = \(CommandBuffer commandBuffer commandTable) -> \dstBuffer -> \dstOffset -> \size -> \data' -> Graphics.Vulkan.C.Dynamic.cmdFillBuffer commandTable commandBuffer dstBuffer dstOffset size data' *> (pure ())

-- | Wrapper for 'vkCmdNextSubpass'
cmdNextSubpass :: CommandBuffer ->  SubpassContents ->  IO ()
cmdNextSubpass = \(CommandBuffer commandBuffer commandTable) -> \contents -> Graphics.Vulkan.C.Dynamic.cmdNextSubpass commandTable commandBuffer contents *> (pure ())

-- | Wrapper for 'vkCmdPipelineBarrier'
cmdPipelineBarrier :: CommandBuffer ->  PipelineStageFlags ->  PipelineStageFlags ->  DependencyFlags ->  Vector MemoryBarrier ->  Vector BufferMemoryBarrier ->  Vector ImageMemoryBarrier ->  IO ()
cmdPipelineBarrier = \(CommandBuffer commandBuffer commandTable) -> \srcStageMask -> \dstStageMask -> \dependencyFlags -> \memoryBarriers -> \bufferMemoryBarriers -> \imageMemoryBarriers -> withVec withCStructImageMemoryBarrier imageMemoryBarriers (\pImageMemoryBarriers -> withVec withCStructBufferMemoryBarrier bufferMemoryBarriers (\pBufferMemoryBarriers -> withVec withCStructMemoryBarrier memoryBarriers (\pMemoryBarriers -> Graphics.Vulkan.C.Dynamic.cmdPipelineBarrier commandTable commandBuffer srcStageMask dstStageMask dependencyFlags (fromIntegral $ Data.Vector.length memoryBarriers) pMemoryBarriers (fromIntegral $ Data.Vector.length bufferMemoryBarriers) pBufferMemoryBarriers (fromIntegral $ Data.Vector.length imageMemoryBarriers) pImageMemoryBarriers *> (pure ()))))

-- | Wrapper for 'vkCmdPushConstants'
cmdPushConstants :: (Storable a) => CommandBuffer ->  PipelineLayout ->  ShaderStageFlags ->  Word32 ->  Vector a ->  IO ()
cmdPushConstants = \(CommandBuffer commandBuffer commandTable) -> \layout -> \stageFlags -> \offset -> \values -> withVec (&) values (\pValues -> Graphics.Vulkan.C.Dynamic.cmdPushConstants commandTable commandBuffer layout stageFlags offset (fromIntegral $ sizeOf (Data.Vector.head values) * Data.Vector.length values) (castPtr pValues) *> (pure ()))

-- | Wrapper for 'vkCmdResetEvent'
cmdResetEvent :: CommandBuffer ->  Event ->  PipelineStageFlags ->  IO ()
cmdResetEvent = \(CommandBuffer commandBuffer commandTable) -> \event -> \stageMask -> Graphics.Vulkan.C.Dynamic.cmdResetEvent commandTable commandBuffer event stageMask *> (pure ())

-- | Wrapper for 'vkCmdResetQueryPool'
cmdResetQueryPool :: CommandBuffer ->  QueryPool ->  Word32 ->  Word32 ->  IO ()
cmdResetQueryPool = \(CommandBuffer commandBuffer commandTable) -> \queryPool -> \firstQuery -> \queryCount -> Graphics.Vulkan.C.Dynamic.cmdResetQueryPool commandTable commandBuffer queryPool firstQuery queryCount *> (pure ())

-- | Wrapper for 'vkCmdResolveImage'
cmdResolveImage :: CommandBuffer ->  Image ->  ImageLayout ->  Image ->  ImageLayout ->  Vector ImageResolve ->  IO ()
cmdResolveImage = \(CommandBuffer commandBuffer commandTable) -> \srcImage -> \srcImageLayout -> \dstImage -> \dstImageLayout -> \regions -> withVec withCStructImageResolve regions (\pRegions -> Graphics.Vulkan.C.Dynamic.cmdResolveImage commandTable commandBuffer srcImage srcImageLayout dstImage dstImageLayout (fromIntegral $ Data.Vector.length regions) pRegions *> (pure ()))

-- | Wrapper for 'vkCmdSetBlendConstants'
cmdSetBlendConstants :: CommandBuffer ->  (CFloat, CFloat, CFloat, CFloat) ->  IO ()
cmdSetBlendConstants = \(CommandBuffer commandBuffer commandTable) -> \(blendConstants0, blendConstants1, blendConstants2, blendConstants3) -> allocaArray 4 (\pBlendConstants -> pokeElemOff pBlendConstants 0 blendConstants0*> pokeElemOff pBlendConstants 1 blendConstants1*> pokeElemOff pBlendConstants 2 blendConstants2*> pokeElemOff pBlendConstants 3 blendConstants3 *> Graphics.Vulkan.C.Dynamic.cmdSetBlendConstants commandTable commandBuffer pBlendConstants *> (pure ()))

-- | Wrapper for 'vkCmdSetDepthBias'
cmdSetDepthBias :: CommandBuffer ->  CFloat ->  CFloat ->  CFloat ->  IO ()
cmdSetDepthBias = \(CommandBuffer commandBuffer commandTable) -> \depthBiasConstantFactor -> \depthBiasClamp -> \depthBiasSlopeFactor -> Graphics.Vulkan.C.Dynamic.cmdSetDepthBias commandTable commandBuffer depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor *> (pure ())

-- | Wrapper for 'vkCmdSetDepthBounds'
cmdSetDepthBounds :: CommandBuffer ->  CFloat ->  CFloat ->  IO ()
cmdSetDepthBounds = \(CommandBuffer commandBuffer commandTable) -> \minDepthBounds -> \maxDepthBounds -> Graphics.Vulkan.C.Dynamic.cmdSetDepthBounds commandTable commandBuffer minDepthBounds maxDepthBounds *> (pure ())

-- | Wrapper for 'vkCmdSetEvent'
cmdSetEvent :: CommandBuffer ->  Event ->  PipelineStageFlags ->  IO ()
cmdSetEvent = \(CommandBuffer commandBuffer commandTable) -> \event -> \stageMask -> Graphics.Vulkan.C.Dynamic.cmdSetEvent commandTable commandBuffer event stageMask *> (pure ())

-- | Wrapper for 'vkCmdSetLineWidth'
cmdSetLineWidth :: CommandBuffer ->  CFloat ->  IO ()
cmdSetLineWidth = \(CommandBuffer commandBuffer commandTable) -> \lineWidth -> Graphics.Vulkan.C.Dynamic.cmdSetLineWidth commandTable commandBuffer lineWidth *> (pure ())

-- | Wrapper for 'vkCmdSetScissor'
cmdSetScissor :: CommandBuffer ->  Word32 ->  Vector Rect2D ->  IO ()
cmdSetScissor = \(CommandBuffer commandBuffer commandTable) -> \firstScissor -> \scissors -> withVec withCStructRect2D scissors (\pScissors -> Graphics.Vulkan.C.Dynamic.cmdSetScissor commandTable commandBuffer firstScissor (fromIntegral $ Data.Vector.length scissors) pScissors *> (pure ()))

-- | Wrapper for 'vkCmdSetStencilCompareMask'
cmdSetStencilCompareMask :: CommandBuffer ->  StencilFaceFlags ->  Word32 ->  IO ()
cmdSetStencilCompareMask = \(CommandBuffer commandBuffer commandTable) -> \faceMask -> \compareMask -> Graphics.Vulkan.C.Dynamic.cmdSetStencilCompareMask commandTable commandBuffer faceMask compareMask *> (pure ())

-- | Wrapper for 'vkCmdSetStencilReference'
cmdSetStencilReference :: CommandBuffer ->  StencilFaceFlags ->  Word32 ->  IO ()
cmdSetStencilReference = \(CommandBuffer commandBuffer commandTable) -> \faceMask -> \reference -> Graphics.Vulkan.C.Dynamic.cmdSetStencilReference commandTable commandBuffer faceMask reference *> (pure ())

-- | Wrapper for 'vkCmdSetStencilWriteMask'
cmdSetStencilWriteMask :: CommandBuffer ->  StencilFaceFlags ->  Word32 ->  IO ()
cmdSetStencilWriteMask = \(CommandBuffer commandBuffer commandTable) -> \faceMask -> \writeMask -> Graphics.Vulkan.C.Dynamic.cmdSetStencilWriteMask commandTable commandBuffer faceMask writeMask *> (pure ())

-- | Wrapper for 'vkCmdSetViewport'
cmdSetViewport :: CommandBuffer ->  Word32 ->  Vector Viewport ->  IO ()
cmdSetViewport = \(CommandBuffer commandBuffer commandTable) -> \firstViewport -> \viewports -> withVec withCStructViewport viewports (\pViewports -> Graphics.Vulkan.C.Dynamic.cmdSetViewport commandTable commandBuffer firstViewport (fromIntegral $ Data.Vector.length viewports) pViewports *> (pure ()))

-- | Wrapper for 'vkCmdUpdateBuffer'
cmdUpdateBuffer :: (Storable a) => CommandBuffer ->  Buffer ->  DeviceSize ->  Vector a ->  IO ()
cmdUpdateBuffer = \(CommandBuffer commandBuffer commandTable) -> \dstBuffer -> \dstOffset -> \data' -> withVec (&) data' (\pData -> Graphics.Vulkan.C.Dynamic.cmdUpdateBuffer commandTable commandBuffer dstBuffer dstOffset (fromIntegral $ sizeOf (Data.Vector.head data') * Data.Vector.length data') (castPtr pData) *> (pure ()))

-- | Wrapper for 'vkCmdWaitEvents'
cmdWaitEvents :: CommandBuffer ->  Vector Event ->  PipelineStageFlags ->  PipelineStageFlags ->  Vector MemoryBarrier ->  Vector BufferMemoryBarrier ->  Vector ImageMemoryBarrier ->  IO ()
cmdWaitEvents = \(CommandBuffer commandBuffer commandTable) -> \events -> \srcStageMask -> \dstStageMask -> \memoryBarriers -> \bufferMemoryBarriers -> \imageMemoryBarriers -> withVec withCStructImageMemoryBarrier imageMemoryBarriers (\pImageMemoryBarriers -> withVec withCStructBufferMemoryBarrier bufferMemoryBarriers (\pBufferMemoryBarriers -> withVec withCStructMemoryBarrier memoryBarriers (\pMemoryBarriers -> withVec (&) events (\pEvents -> Graphics.Vulkan.C.Dynamic.cmdWaitEvents commandTable commandBuffer (fromIntegral $ Data.Vector.length events) pEvents srcStageMask dstStageMask (fromIntegral $ Data.Vector.length memoryBarriers) pMemoryBarriers (fromIntegral $ Data.Vector.length bufferMemoryBarriers) pBufferMemoryBarriers (fromIntegral $ Data.Vector.length imageMemoryBarriers) pImageMemoryBarriers *> (pure ())))))

-- | Wrapper for 'vkCmdWriteTimestamp'
cmdWriteTimestamp :: CommandBuffer ->  PipelineStageFlagBits ->  QueryPool ->  Word32 ->  IO ()
cmdWriteTimestamp = \(CommandBuffer commandBuffer commandTable) -> \pipelineStage -> \queryPool -> \query -> Graphics.Vulkan.C.Dynamic.cmdWriteTimestamp commandTable commandBuffer pipelineStage queryPool query *> (pure ())
