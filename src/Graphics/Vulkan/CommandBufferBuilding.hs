{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.CommandBufferBuilding where

import Data.Vector.Storable.Sized( Vector(..)
                                 )
import Graphics.Vulkan.Buffer( Buffer(..)
                             )
import Graphics.Vulkan.Pass( RenderPass(..)
                           , DependencyFlags(..)
                           , Framebuffer(..)
                           )
import Text.Read.Lex( Lexeme(Ident)
                    )
import Graphics.Vulkan.Event( Event(..)
                            )
import GHC.Read( expectP
               , choose
               )
import Graphics.Vulkan.Pipeline( Pipeline(..)
                               , PipelineBindPoint(..)
                               , PipelineStageFlags(..)
                               )
import Data.Word( Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , castPtr
                  , plusPtr
                  )
import Graphics.Vulkan.DescriptorSet( DescriptorSet(..)
                                    )
import Graphics.Vulkan.CommandBuffer( CommandBuffer(..)
                                    )
import Data.Int( Int32(..)
               , Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.PipelineLayout( PipelineLayout(..)
                                     )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Shader( ShaderStageFlags(..)
                             )
import Graphics.Vulkan.Sampler( Filter(..)
                              )
import Graphics.Vulkan.Image( ImageAspectFlags(..)
                            , Image(..)
                            , ImageLayout(..)
                            , ImageSubresourceRange(..)
                            )
import Graphics.Vulkan.Query( QueryPool(..)
                            , QueryControlFlags(..)
                            , QueryResultFlags(..)
                            )
import Graphics.Vulkan.OtherTypes( BufferMemoryBarrier(..)
                                 , ImageMemoryBarrier(..)
                                 , MemoryBarrier(..)
                                 )
import Graphics.Vulkan.Core( Offset3D(..)
                           , StructureType(..)
                           , Viewport(..)
                           , Rect2D(..)
                           , Extent3D(..)
                           , DeviceSize(..)
                           , Flags(..)
                           )
import Foreign.C.Types( CFloat(..)
                      )

-- ** cmdPushConstants
foreign import ccall "vkCmdPushConstants" cmdPushConstants ::
  CommandBuffer ->
  PipelineLayout ->
    ShaderStageFlags -> Word32 -> Word32 -> Ptr Void -> IO ()

-- ** cmdSetStencilWriteMask
foreign import ccall "vkCmdSetStencilWriteMask" cmdSetStencilWriteMask ::
  CommandBuffer -> StencilFaceFlags -> Word32 -> IO ()

-- ** cmdBindIndexBuffer
foreign import ccall "vkCmdBindIndexBuffer" cmdBindIndexBuffer ::
  CommandBuffer -> Buffer -> DeviceSize -> IndexType -> IO ()

-- ** cmdResetQueryPool
foreign import ccall "vkCmdResetQueryPool" cmdResetQueryPool ::
  CommandBuffer -> QueryPool -> Word32 -> Word32 -> IO ()

-- ** cmdResolveImage
foreign import ccall "vkCmdResolveImage" cmdResolveImage ::
  CommandBuffer ->
  Image ->
    ImageLayout ->
      Image -> ImageLayout -> Word32 -> Ptr ImageResolve -> IO ()

-- ** cmdBindPipeline
foreign import ccall "vkCmdBindPipeline" cmdBindPipeline ::
  CommandBuffer -> PipelineBindPoint -> Pipeline -> IO ()

-- ** cmdBindVertexBuffers
foreign import ccall "vkCmdBindVertexBuffers" cmdBindVertexBuffers ::
  CommandBuffer ->
  Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- ** cmdDraw
foreign import ccall "vkCmdDraw" cmdDraw ::
  CommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()


data ImageCopy =
  ImageCopy{ srcSubresource :: ImageSubresourceLayers 
           , srcOffset :: Offset3D 
           , dstSubresource :: ImageSubresourceLayers 
           , dstOffset :: Offset3D 
           , extent :: Extent3D 
           }
  deriving (Eq)

instance Storable ImageCopy where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = ImageCopy <$> peek (ptr `plusPtr` 0)
                       <*> peek (ptr `plusPtr` 16)
                       <*> peek (ptr `plusPtr` 28)
                       <*> peek (ptr `plusPtr` 44)
                       <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcSubresource (poked :: ImageCopy))
                *> poke (ptr `plusPtr` 16) (srcOffset (poked :: ImageCopy))
                *> poke (ptr `plusPtr` 28) (dstSubresource (poked :: ImageCopy))
                *> poke (ptr `plusPtr` 44) (dstOffset (poked :: ImageCopy))
                *> poke (ptr `plusPtr` 56) (extent (poked :: ImageCopy))


-- ** cmdNextSubpass
foreign import ccall "vkCmdNextSubpass" cmdNextSubpass ::
  CommandBuffer -> SubpassContents -> IO ()

-- ** cmdEndQuery
foreign import ccall "vkCmdEndQuery" cmdEndQuery ::
  CommandBuffer -> QueryPool -> Word32 -> IO ()

-- ** cmdSetScissor
foreign import ccall "vkCmdSetScissor" cmdSetScissor ::
  CommandBuffer -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- ** cmdSetEvent
foreign import ccall "vkCmdSetEvent" cmdSetEvent ::
  CommandBuffer -> Event -> PipelineStageFlags -> IO ()

-- ** cmdCopyImageToBuffer
foreign import ccall "vkCmdCopyImageToBuffer" cmdCopyImageToBuffer ::
  CommandBuffer ->
  Image ->
    ImageLayout -> Buffer -> Word32 -> Ptr BufferImageCopy -> IO ()

-- ** cmdDispatchIndirect
foreign import ccall "vkCmdDispatchIndirect" cmdDispatchIndirect ::
  CommandBuffer -> Buffer -> DeviceSize -> IO ()

-- ** cmdBeginQuery
foreign import ccall "vkCmdBeginQuery" cmdBeginQuery ::
  CommandBuffer -> QueryPool -> Word32 -> QueryControlFlags -> IO ()

-- ** cmdEndRenderPass
foreign import ccall "vkCmdEndRenderPass" cmdEndRenderPass ::
  CommandBuffer -> IO ()

-- ** cmdFillBuffer
foreign import ccall "vkCmdFillBuffer" cmdFillBuffer ::
  CommandBuffer ->
  Buffer -> DeviceSize -> DeviceSize -> Word32 -> IO ()


data ClearRect =
  ClearRect{ rect :: Rect2D 
           , baseArrayLayer :: Word32 
           , layerCount :: Word32 
           }
  deriving (Eq)

instance Storable ClearRect where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = ClearRect <$> peek (ptr `plusPtr` 0)
                       <*> peek (ptr `plusPtr` 16)
                       <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (rect (poked :: ClearRect))
                *> poke (ptr `plusPtr` 16) (baseArrayLayer (poked :: ClearRect))
                *> poke (ptr `plusPtr` 20) (layerCount (poked :: ClearRect))


-- ** cmdWaitEvents
foreign import ccall "vkCmdWaitEvents" cmdWaitEvents ::
  CommandBuffer ->
  Word32 ->
    Ptr Event ->
      PipelineStageFlags ->
        PipelineStageFlags ->
          Word32 ->
            Ptr MemoryBarrier ->
              Word32 ->
                Ptr BufferMemoryBarrier ->
                  Word32 -> Ptr ImageMemoryBarrier -> IO ()

-- ** cmdClearColorImage
foreign import ccall "vkCmdClearColorImage" cmdClearColorImage ::
  CommandBuffer ->
  Image ->
    ImageLayout ->
      Ptr ClearColorValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()

-- ** IndexType

newtype IndexType = IndexType Int32
  deriving (Eq, Storable)

instance Show IndexType where
  showsPrec _ VK_INDEX_TYPE_UINT16 = showString "VK_INDEX_TYPE_UINT16"
  showsPrec _ VK_INDEX_TYPE_UINT32 = showString "VK_INDEX_TYPE_UINT32"
  showsPrec p (IndexType x) = showParen (p >= 11) (showString "IndexType " . showsPrec 11 x)

instance Read IndexType where
  readPrec = parens ( choose [ ("VK_INDEX_TYPE_UINT16", pure VK_INDEX_TYPE_UINT16)
                             , ("VK_INDEX_TYPE_UINT32", pure VK_INDEX_TYPE_UINT32)
                             ] +++
                      prec 10 (do
                        expectP (Ident "IndexType")
                        v <- step readPrec
                        pure (IndexType v)
                        )
                    )


pattern VK_INDEX_TYPE_UINT16 = IndexType 0

pattern VK_INDEX_TYPE_UINT32 = IndexType 1


data BufferImageCopy =
  BufferImageCopy{ bufferOffset :: DeviceSize 
                 , bufferRowLength :: Word32 
                 , bufferImageHeight :: Word32 
                 , imageSubresource :: ImageSubresourceLayers 
                 , imageOffset :: Offset3D 
                 , imageExtent :: Extent3D 
                 }
  deriving (Eq)

instance Storable BufferImageCopy where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = BufferImageCopy <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 12)
                             <*> peek (ptr `plusPtr` 16)
                             <*> peek (ptr `plusPtr` 32)
                             <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (bufferOffset (poked :: BufferImageCopy))
                *> poke (ptr `plusPtr` 8) (bufferRowLength (poked :: BufferImageCopy))
                *> poke (ptr `plusPtr` 12) (bufferImageHeight (poked :: BufferImageCopy))
                *> poke (ptr `plusPtr` 16) (imageSubresource (poked :: BufferImageCopy))
                *> poke (ptr `plusPtr` 32) (imageOffset (poked :: BufferImageCopy))
                *> poke (ptr `plusPtr` 44) (imageExtent (poked :: BufferImageCopy))


-- ** cmdSetDepthBounds
foreign import ccall "vkCmdSetDepthBounds" cmdSetDepthBounds ::
  CommandBuffer -> CFloat -> CFloat -> IO ()

-- ** cmdCopyBufferToImage
foreign import ccall "vkCmdCopyBufferToImage" cmdCopyBufferToImage ::
  CommandBuffer ->
  Buffer ->
    Image -> ImageLayout -> Word32 -> Ptr BufferImageCopy -> IO ()

-- ** cmdDrawIndexedIndirect
foreign import ccall "vkCmdDrawIndexedIndirect" cmdDrawIndexedIndirect ::
  CommandBuffer -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- ** cmdUpdateBuffer
foreign import ccall "vkCmdUpdateBuffer" cmdUpdateBuffer ::
  CommandBuffer ->
  Buffer -> DeviceSize -> DeviceSize -> Ptr Word32 -> IO ()

-- ** cmdCopyImage
foreign import ccall "vkCmdCopyImage" cmdCopyImage ::
  CommandBuffer ->
  Image ->
    ImageLayout ->
      Image -> ImageLayout -> Word32 -> Ptr ImageCopy -> IO ()

-- ** cmdWriteTimestamp
foreign import ccall "vkCmdWriteTimestamp" cmdWriteTimestamp ::
  CommandBuffer -> PipelineStageFlags -> QueryPool -> Word32 -> IO ()


data ImageSubresourceLayers =
  ImageSubresourceLayers{ aspectMask :: ImageAspectFlags 
                        , mipLevel :: Word32 
                        , baseArrayLayer :: Word32 
                        , layerCount :: Word32 
                        }
  deriving (Eq)

instance Storable ImageSubresourceLayers where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = ImageSubresourceLayers <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 4)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: ImageSubresourceLayers))
                *> poke (ptr `plusPtr` 4) (mipLevel (poked :: ImageSubresourceLayers))
                *> poke (ptr `plusPtr` 8) (baseArrayLayer (poked :: ImageSubresourceLayers))
                *> poke (ptr `plusPtr` 12) (layerCount (poked :: ImageSubresourceLayers))


-- ** cmdDrawIndexed
foreign import ccall "vkCmdDrawIndexed" cmdDrawIndexed ::
  CommandBuffer ->
  Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()

-- ** cmdSetDepthBias
foreign import ccall "vkCmdSetDepthBias" cmdSetDepthBias ::
  CommandBuffer -> CFloat -> CFloat -> CFloat -> IO ()

-- ** cmdDrawIndirect
foreign import ccall "vkCmdDrawIndirect" cmdDrawIndirect ::
  CommandBuffer -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()


data ClearDepthStencilValue =
  ClearDepthStencilValue{ depth :: CFloat 
                        , stencil :: Word32 
                        }
  deriving (Eq)

instance Storable ClearDepthStencilValue where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = ClearDepthStencilValue <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (depth (poked :: ClearDepthStencilValue))
                *> poke (ptr `plusPtr` 4) (stencil (poked :: ClearDepthStencilValue))



data BufferCopy =
  BufferCopy{ srcOffset :: DeviceSize 
            , dstOffset :: DeviceSize 
            , size :: DeviceSize 
            }
  deriving (Eq)

instance Storable BufferCopy where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = BufferCopy <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 8)
                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcOffset (poked :: BufferCopy))
                *> poke (ptr `plusPtr` 8) (dstOffset (poked :: BufferCopy))
                *> poke (ptr `plusPtr` 16) (size (poked :: BufferCopy))


-- ** cmdClearAttachments
foreign import ccall "vkCmdClearAttachments" cmdClearAttachments ::
  CommandBuffer ->
  Word32 -> Ptr ClearAttachment -> Word32 -> Ptr ClearRect -> IO ()

-- ** cmdSetViewport
foreign import ccall "vkCmdSetViewport" cmdSetViewport ::
  CommandBuffer -> Word32 -> Word32 -> Ptr Viewport -> IO ()

-- ** cmdCopyBuffer
foreign import ccall "vkCmdCopyBuffer" cmdCopyBuffer ::
  CommandBuffer ->
  Buffer -> Buffer -> Word32 -> Ptr BufferCopy -> IO ()

-- ** cmdBindDescriptorSets
foreign import ccall "vkCmdBindDescriptorSets" cmdBindDescriptorSets ::
  CommandBuffer ->
  PipelineBindPoint ->
    PipelineLayout ->
      Word32 ->
        Word32 -> Ptr DescriptorSet -> Word32 -> Ptr Word32 -> IO ()

-- ** cmdSetLineWidth
foreign import ccall "vkCmdSetLineWidth" cmdSetLineWidth ::
  CommandBuffer -> CFloat -> IO ()

-- ** cmdExecuteCommands
foreign import ccall "vkCmdExecuteCommands" cmdExecuteCommands ::
  CommandBuffer -> Word32 -> Ptr CommandBuffer -> IO ()


data RenderPassBeginInfo =
  RenderPassBeginInfo{ sType :: StructureType 
                     , pNext :: Ptr Void 
                     , renderPass :: RenderPass 
                     , framebuffer :: Framebuffer 
                     , renderArea :: Rect2D 
                     , clearValueCount :: Word32 
                     , pClearValues :: Ptr ClearValue 
                     }
  deriving (Eq)

instance Storable RenderPassBeginInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = RenderPassBeginInfo <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 48)
                                 <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: RenderPassBeginInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: RenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (renderPass (poked :: RenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (framebuffer (poked :: RenderPassBeginInfo))
                *> poke (ptr `plusPtr` 32) (renderArea (poked :: RenderPassBeginInfo))
                *> poke (ptr `plusPtr` 48) (clearValueCount (poked :: RenderPassBeginInfo))
                *> poke (ptr `plusPtr` 56) (pClearValues (poked :: RenderPassBeginInfo))


-- ** cmdSetStencilCompareMask
foreign import ccall "vkCmdSetStencilCompareMask" cmdSetStencilCompareMask ::
  CommandBuffer -> StencilFaceFlags -> Word32 -> IO ()


data ImageBlit =
  ImageBlit{ srcSubresource :: ImageSubresourceLayers 
           , srcOffsets :: Vector 2 Offset3D 
           , dstSubresource :: ImageSubresourceLayers 
           , dstOffsets :: Vector 2 Offset3D 
           }
  deriving (Eq)

instance Storable ImageBlit where
  sizeOf ~_ = 80
  alignment ~_ = 4
  peek ptr = ImageBlit <$> peek (ptr `plusPtr` 0)
                       <*> peek (ptr `plusPtr` 16)
                       <*> peek (ptr `plusPtr` 40)
                       <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcSubresource (poked :: ImageBlit))
                *> poke (ptr `plusPtr` 16) (srcOffsets (poked :: ImageBlit))
                *> poke (ptr `plusPtr` 40) (dstSubresource (poked :: ImageBlit))
                *> poke (ptr `plusPtr` 56) (dstOffsets (poked :: ImageBlit))



data ClearAttachment =
  ClearAttachment{ aspectMask :: ImageAspectFlags 
                 , colorAttachment :: Word32 
                 , clearValue :: ClearValue 
                 }
  deriving (Eq)

instance Storable ClearAttachment where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = ClearAttachment <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 4)
                             <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: ClearAttachment))
                *> poke (ptr `plusPtr` 4) (colorAttachment (poked :: ClearAttachment))
                *> poke (ptr `plusPtr` 8) (clearValue (poked :: ClearAttachment))


-- | // Union allowing specification of color or depth and stencil values. Actual value selected is based on attachment being cleared.
data ClearValue = Color ClearColorValue 
                | DepthStencil ClearDepthStencilValue 
  deriving (Eq)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable ClearValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ~_ = error "peek@ClearValue"
  poke ptr poked = case poked of
                     Color e -> poke (castPtr ptr) e
                     DepthStencil e -> poke (castPtr ptr) e


-- ** StencilFaceFlags

newtype StencilFaceFlags = StencilFaceFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show StencilFaceFlags where
  showsPrec _ VK_STENCIL_FACE_FRONT_BIT = showString "VK_STENCIL_FACE_FRONT_BIT"
  showsPrec _ VK_STENCIL_FACE_BACK_BIT = showString "VK_STENCIL_FACE_BACK_BIT"
  showsPrec _ VK_STENCIL_FRONT_AND_BACK = showString "VK_STENCIL_FRONT_AND_BACK"
  showsPrec p (StencilFaceFlags x) = showParen (p >= 11) (showString "StencilFaceFlags " . showsPrec 11 x)

instance Read StencilFaceFlags where
  readPrec = parens ( choose [ ("VK_STENCIL_FACE_FRONT_BIT", pure VK_STENCIL_FACE_FRONT_BIT)
                             , ("VK_STENCIL_FACE_BACK_BIT", pure VK_STENCIL_FACE_BACK_BIT)
                             , ("VK_STENCIL_FRONT_AND_BACK", pure VK_STENCIL_FRONT_AND_BACK)
                             ] +++
                      prec 10 (do
                        expectP (Ident "StencilFaceFlags")
                        v <- step readPrec
                        pure (StencilFaceFlags v)
                        )
                    )

-- | Front face
pattern VK_STENCIL_FACE_FRONT_BIT = StencilFaceFlags 0x1
-- | Back face
pattern VK_STENCIL_FACE_BACK_BIT = StencilFaceFlags 0x2
-- | Front and back faces
pattern VK_STENCIL_FRONT_AND_BACK = StencilFaceFlags 0x3

-- | // Union allowing specification of floating point, integer, or unsigned integer color data. Actual value selected is based on image/attachment being cleared.
data ClearColorValue = Float32 (Vector 4 CFloat) 
                     | Int32 (Vector 4 Int32) 
                     | Uint32 (Vector 4 Word32) 
  deriving (Eq)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable ClearColorValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ~_ = error "peek@ClearColorValue"
  poke ptr poked = case poked of
                     Float32 e -> poke (castPtr ptr) e
                     Int32 e -> poke (castPtr ptr) e
                     Uint32 e -> poke (castPtr ptr) e


-- ** SubpassContents

newtype SubpassContents = SubpassContents Int32
  deriving (Eq, Storable)

instance Show SubpassContents where
  showsPrec _ VK_SUBPASS_CONTENTS_INLINE = showString "VK_SUBPASS_CONTENTS_INLINE"
  showsPrec _ VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = showString "VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
  showsPrec p (SubpassContents x) = showParen (p >= 11) (showString "SubpassContents " . showsPrec 11 x)

instance Read SubpassContents where
  readPrec = parens ( choose [ ("VK_SUBPASS_CONTENTS_INLINE", pure VK_SUBPASS_CONTENTS_INLINE)
                             , ("VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS", pure VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SubpassContents")
                        v <- step readPrec
                        pure (SubpassContents v)
                        )
                    )


pattern VK_SUBPASS_CONTENTS_INLINE = SubpassContents 0

pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = SubpassContents 1

-- ** cmdCopyQueryPoolResults
foreign import ccall "vkCmdCopyQueryPoolResults" cmdCopyQueryPoolResults ::
  CommandBuffer ->
  QueryPool ->
    Word32 ->
      Word32 ->
        Buffer -> DeviceSize -> DeviceSize -> QueryResultFlags -> IO ()

-- ** cmdBlitImage
foreign import ccall "vkCmdBlitImage" cmdBlitImage ::
  CommandBuffer ->
  Image ->
    ImageLayout ->
      Image -> ImageLayout -> Word32 -> Ptr ImageBlit -> Filter -> IO ()

-- ** cmdSetBlendConstants
foreign import ccall "vkCmdSetBlendConstants" cmdSetBlendConstants ::
  CommandBuffer -> Ptr CFloat -> IO ()

-- ** cmdClearDepthStencilImage
foreign import ccall "vkCmdClearDepthStencilImage" cmdClearDepthStencilImage ::
  CommandBuffer ->
  Image ->
    ImageLayout ->
      Ptr ClearDepthStencilValue ->
        Word32 -> Ptr ImageSubresourceRange -> IO ()


data ImageResolve =
  ImageResolve{ srcSubresource :: ImageSubresourceLayers 
              , srcOffset :: Offset3D 
              , dstSubresource :: ImageSubresourceLayers 
              , dstOffset :: Offset3D 
              , extent :: Extent3D 
              }
  deriving (Eq)

instance Storable ImageResolve where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = ImageResolve <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 16)
                          <*> peek (ptr `plusPtr` 28)
                          <*> peek (ptr `plusPtr` 44)
                          <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcSubresource (poked :: ImageResolve))
                *> poke (ptr `plusPtr` 16) (srcOffset (poked :: ImageResolve))
                *> poke (ptr `plusPtr` 28) (dstSubresource (poked :: ImageResolve))
                *> poke (ptr `plusPtr` 44) (dstOffset (poked :: ImageResolve))
                *> poke (ptr `plusPtr` 56) (extent (poked :: ImageResolve))


-- ** cmdDispatch
foreign import ccall "vkCmdDispatch" cmdDispatch ::
  CommandBuffer -> Word32 -> Word32 -> Word32 -> IO ()

-- ** cmdSetStencilReference
foreign import ccall "vkCmdSetStencilReference" cmdSetStencilReference ::
  CommandBuffer -> StencilFaceFlags -> Word32 -> IO ()

-- ** cmdPipelineBarrier
foreign import ccall "vkCmdPipelineBarrier" cmdPipelineBarrier ::
  CommandBuffer ->
  PipelineStageFlags ->
    PipelineStageFlags ->
      DependencyFlags ->
        Word32 ->
          Ptr MemoryBarrier ->
            Word32 ->
              Ptr BufferMemoryBarrier ->
                Word32 -> Ptr ImageMemoryBarrier -> IO ()

-- ** cmdBeginRenderPass
foreign import ccall "vkCmdBeginRenderPass" cmdBeginRenderPass ::
  CommandBuffer ->
  Ptr RenderPassBeginInfo -> SubpassContents -> IO ()

-- ** cmdResetEvent
foreign import ccall "vkCmdResetEvent" cmdResetEvent ::
  CommandBuffer -> Event -> PipelineStageFlags -> IO ()

