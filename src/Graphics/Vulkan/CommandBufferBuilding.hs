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
                           , Framebuffer(..)
                           , VkDependencyFlags(..)
                           )
import Text.Read.Lex( Lexeme(Ident)
                    )
import Graphics.Vulkan.Event( Event(..)
                            )
import GHC.Read( expectP
               , choose
               )
import Graphics.Vulkan.Pipeline( Pipeline(..)
                               , VkPipelineBindPoint(..)
                               , VkPipelineStageFlags(..)
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
import Graphics.Vulkan.Shader( VkShaderStageFlags(..)
                             )
import Graphics.Vulkan.Sampler( VkFilter(..)
                              )
import Graphics.Vulkan.Image( Image(..)
                            , VkImageAspectFlags(..)
                            , ImageSubresourceRange(..)
                            , VkImageLayout(..)
                            )
import Graphics.Vulkan.Query( VkQueryResultFlags(..)
                            , QueryPool(..)
                            , VkQueryControlFlags(..)
                            )
import Graphics.Vulkan.OtherTypes( BufferMemoryBarrier(..)
                                 , ImageMemoryBarrier(..)
                                 , MemoryBarrier(..)
                                 )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , Offset3D(..)
                           , VkFlags(..)
                           , Viewport(..)
                           , Rect2D(..)
                           , Extent3D(..)
                           , VkDeviceSize(..)
                           )
import Foreign.C.Types( CFloat(..)
                      )

-- ** vkCmdPushConstants
foreign import ccall "vkCmdPushConstants" vkCmdPushConstants ::
  CommandBuffer ->
  PipelineLayout ->
    VkShaderStageFlags -> Word32 -> Word32 -> Ptr Void -> IO ()

-- ** vkCmdSetStencilWriteMask
foreign import ccall "vkCmdSetStencilWriteMask" vkCmdSetStencilWriteMask ::
  CommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()

-- ** vkCmdBindIndexBuffer
foreign import ccall "vkCmdBindIndexBuffer" vkCmdBindIndexBuffer ::
  CommandBuffer -> Buffer -> VkDeviceSize -> VkIndexType -> IO ()

-- ** vkCmdResetQueryPool
foreign import ccall "vkCmdResetQueryPool" vkCmdResetQueryPool ::
  CommandBuffer -> QueryPool -> Word32 -> Word32 -> IO ()

-- ** vkCmdResolveImage
foreign import ccall "vkCmdResolveImage" vkCmdResolveImage ::
  CommandBuffer ->
  Image ->
    VkImageLayout ->
      Image -> VkImageLayout -> Word32 -> Ptr ImageResolve -> IO ()

-- ** vkCmdBindPipeline
foreign import ccall "vkCmdBindPipeline" vkCmdBindPipeline ::
  CommandBuffer -> VkPipelineBindPoint -> Pipeline -> IO ()

-- ** vkCmdBindVertexBuffers
foreign import ccall "vkCmdBindVertexBuffers" vkCmdBindVertexBuffers ::
  CommandBuffer ->
  Word32 -> Word32 -> Ptr Buffer -> Ptr VkDeviceSize -> IO ()

-- ** vkCmdDraw
foreign import ccall "vkCmdDraw" vkCmdDraw ::
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


-- ** vkCmdNextSubpass
foreign import ccall "vkCmdNextSubpass" vkCmdNextSubpass ::
  CommandBuffer -> VkSubpassContents -> IO ()

-- ** vkCmdEndQuery
foreign import ccall "vkCmdEndQuery" vkCmdEndQuery ::
  CommandBuffer -> QueryPool -> Word32 -> IO ()

-- ** vkCmdSetScissor
foreign import ccall "vkCmdSetScissor" vkCmdSetScissor ::
  CommandBuffer -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- ** vkCmdSetEvent
foreign import ccall "vkCmdSetEvent" vkCmdSetEvent ::
  CommandBuffer -> Event -> VkPipelineStageFlags -> IO ()

-- ** vkCmdCopyImageToBuffer
foreign import ccall "vkCmdCopyImageToBuffer" vkCmdCopyImageToBuffer ::
  CommandBuffer ->
  Image ->
    VkImageLayout -> Buffer -> Word32 -> Ptr BufferImageCopy -> IO ()

-- ** vkCmdDispatchIndirect
foreign import ccall "vkCmdDispatchIndirect" vkCmdDispatchIndirect ::
  CommandBuffer -> Buffer -> VkDeviceSize -> IO ()

-- ** vkCmdBeginQuery
foreign import ccall "vkCmdBeginQuery" vkCmdBeginQuery ::
  CommandBuffer ->
  QueryPool -> Word32 -> VkQueryControlFlags -> IO ()

-- ** vkCmdEndRenderPass
foreign import ccall "vkCmdEndRenderPass" vkCmdEndRenderPass ::
  CommandBuffer -> IO ()

-- ** vkCmdFillBuffer
foreign import ccall "vkCmdFillBuffer" vkCmdFillBuffer ::
  CommandBuffer ->
  Buffer -> VkDeviceSize -> VkDeviceSize -> Word32 -> IO ()


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


-- ** vkCmdWaitEvents
foreign import ccall "vkCmdWaitEvents" vkCmdWaitEvents ::
  CommandBuffer ->
  Word32 ->
    Ptr Event ->
      VkPipelineStageFlags ->
        VkPipelineStageFlags ->
          Word32 ->
            Ptr MemoryBarrier ->
              Word32 ->
                Ptr BufferMemoryBarrier ->
                  Word32 -> Ptr ImageMemoryBarrier -> IO ()

-- ** vkCmdClearColorImage
foreign import ccall "vkCmdClearColorImage" vkCmdClearColorImage ::
  CommandBuffer ->
  Image ->
    VkImageLayout ->
      Ptr ClearColorValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()

-- ** VkIndexType

newtype VkIndexType = VkIndexType Int32
  deriving (Eq, Storable)

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


pattern VK_INDEX_TYPE_UINT16 = VkIndexType 0

pattern VK_INDEX_TYPE_UINT32 = VkIndexType 1


data BufferImageCopy =
  BufferImageCopy{ bufferOffset :: VkDeviceSize 
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


-- ** vkCmdSetDepthBounds
foreign import ccall "vkCmdSetDepthBounds" vkCmdSetDepthBounds ::
  CommandBuffer -> CFloat -> CFloat -> IO ()

-- ** vkCmdCopyBufferToImage
foreign import ccall "vkCmdCopyBufferToImage" vkCmdCopyBufferToImage ::
  CommandBuffer ->
  Buffer ->
    Image -> VkImageLayout -> Word32 -> Ptr BufferImageCopy -> IO ()

-- ** vkCmdDrawIndexedIndirect
foreign import ccall "vkCmdDrawIndexedIndirect" vkCmdDrawIndexedIndirect ::
  CommandBuffer ->
  Buffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()

-- ** vkCmdUpdateBuffer
foreign import ccall "vkCmdUpdateBuffer" vkCmdUpdateBuffer ::
  CommandBuffer ->
  Buffer -> VkDeviceSize -> VkDeviceSize -> Ptr Word32 -> IO ()

-- ** vkCmdCopyImage
foreign import ccall "vkCmdCopyImage" vkCmdCopyImage ::
  CommandBuffer ->
  Image ->
    VkImageLayout ->
      Image -> VkImageLayout -> Word32 -> Ptr ImageCopy -> IO ()

-- ** vkCmdWriteTimestamp
foreign import ccall "vkCmdWriteTimestamp" vkCmdWriteTimestamp ::
  CommandBuffer ->
  VkPipelineStageFlags -> QueryPool -> Word32 -> IO ()


data ImageSubresourceLayers =
  ImageSubresourceLayers{ aspectMask :: VkImageAspectFlags 
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


-- ** vkCmdDrawIndexed
foreign import ccall "vkCmdDrawIndexed" vkCmdDrawIndexed ::
  CommandBuffer ->
  Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()

-- ** vkCmdSetDepthBias
foreign import ccall "vkCmdSetDepthBias" vkCmdSetDepthBias ::
  CommandBuffer -> CFloat -> CFloat -> CFloat -> IO ()

-- ** vkCmdDrawIndirect
foreign import ccall "vkCmdDrawIndirect" vkCmdDrawIndirect ::
  CommandBuffer ->
  Buffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()


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
  BufferCopy{ srcOffset :: VkDeviceSize 
            , dstOffset :: VkDeviceSize 
            , size :: VkDeviceSize 
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


-- ** vkCmdClearAttachments
foreign import ccall "vkCmdClearAttachments" vkCmdClearAttachments ::
  CommandBuffer ->
  Word32 -> Ptr ClearAttachment -> Word32 -> Ptr ClearRect -> IO ()

-- ** vkCmdSetViewport
foreign import ccall "vkCmdSetViewport" vkCmdSetViewport ::
  CommandBuffer -> Word32 -> Word32 -> Ptr Viewport -> IO ()

-- ** vkCmdCopyBuffer
foreign import ccall "vkCmdCopyBuffer" vkCmdCopyBuffer ::
  CommandBuffer ->
  Buffer -> Buffer -> Word32 -> Ptr BufferCopy -> IO ()

-- ** vkCmdBindDescriptorSets
foreign import ccall "vkCmdBindDescriptorSets" vkCmdBindDescriptorSets ::
  CommandBuffer ->
  VkPipelineBindPoint ->
    PipelineLayout ->
      Word32 ->
        Word32 -> Ptr DescriptorSet -> Word32 -> Ptr Word32 -> IO ()

-- ** vkCmdSetLineWidth
foreign import ccall "vkCmdSetLineWidth" vkCmdSetLineWidth ::
  CommandBuffer -> CFloat -> IO ()

-- ** vkCmdExecuteCommands
foreign import ccall "vkCmdExecuteCommands" vkCmdExecuteCommands ::
  CommandBuffer -> Word32 -> Ptr CommandBuffer -> IO ()


data RenderPassBeginInfo =
  RenderPassBeginInfo{ sType :: VkStructureType 
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


-- ** vkCmdSetStencilCompareMask
foreign import ccall "vkCmdSetStencilCompareMask" vkCmdSetStencilCompareMask ::
  CommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()


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
  ClearAttachment{ aspectMask :: VkImageAspectFlags 
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


-- ** VkStencilFaceFlags

newtype VkStencilFaceFlags = VkStencilFaceFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkStencilFaceFlags where
  showsPrec _ VK_STENCIL_FACE_FRONT_BIT = showString "VK_STENCIL_FACE_FRONT_BIT"
  showsPrec _ VK_STENCIL_FACE_BACK_BIT = showString "VK_STENCIL_FACE_BACK_BIT"
  showsPrec _ VK_STENCIL_FRONT_AND_BACK = showString "VK_STENCIL_FRONT_AND_BACK"
  showsPrec p (VkStencilFaceFlags x) = showParen (p >= 11) (showString "VkStencilFaceFlags " . showsPrec 11 x)

instance Read VkStencilFaceFlags where
  readPrec = parens ( choose [ ("VK_STENCIL_FACE_FRONT_BIT", pure VK_STENCIL_FACE_FRONT_BIT)
                             , ("VK_STENCIL_FACE_BACK_BIT", pure VK_STENCIL_FACE_BACK_BIT)
                             , ("VK_STENCIL_FRONT_AND_BACK", pure VK_STENCIL_FRONT_AND_BACK)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStencilFaceFlags")
                        v <- step readPrec
                        pure (VkStencilFaceFlags v)
                        )
                    )

-- | Front face
pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlags 0x1
-- | Back face
pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlags 0x2
-- | Front and back faces
pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlags 0x3

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


-- ** VkSubpassContents

newtype VkSubpassContents = VkSubpassContents Int32
  deriving (Eq, Storable)

instance Show VkSubpassContents where
  showsPrec _ VK_SUBPASS_CONTENTS_INLINE = showString "VK_SUBPASS_CONTENTS_INLINE"
  showsPrec _ VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = showString "VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
  showsPrec p (VkSubpassContents x) = showParen (p >= 11) (showString "VkSubpassContents " . showsPrec 11 x)

instance Read VkSubpassContents where
  readPrec = parens ( choose [ ("VK_SUBPASS_CONTENTS_INLINE", pure VK_SUBPASS_CONTENTS_INLINE)
                             , ("VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS", pure VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSubpassContents")
                        v <- step readPrec
                        pure (VkSubpassContents v)
                        )
                    )


pattern VK_SUBPASS_CONTENTS_INLINE = VkSubpassContents 0

pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = VkSubpassContents 1

-- ** vkCmdCopyQueryPoolResults
foreign import ccall "vkCmdCopyQueryPoolResults" vkCmdCopyQueryPoolResults ::
  CommandBuffer ->
  QueryPool ->
    Word32 ->
      Word32 ->
        Buffer ->
          VkDeviceSize -> VkDeviceSize -> VkQueryResultFlags -> IO ()

-- ** vkCmdBlitImage
foreign import ccall "vkCmdBlitImage" vkCmdBlitImage ::
  CommandBuffer ->
  Image ->
    VkImageLayout ->
      Image ->
        VkImageLayout -> Word32 -> Ptr ImageBlit -> VkFilter -> IO ()

-- ** vkCmdSetBlendConstants
foreign import ccall "vkCmdSetBlendConstants" vkCmdSetBlendConstants ::
  CommandBuffer -> Ptr CFloat -> IO ()

-- ** vkCmdClearDepthStencilImage
foreign import ccall "vkCmdClearDepthStencilImage" vkCmdClearDepthStencilImage ::
  CommandBuffer ->
  Image ->
    VkImageLayout ->
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


-- ** vkCmdDispatch
foreign import ccall "vkCmdDispatch" vkCmdDispatch ::
  CommandBuffer -> Word32 -> Word32 -> Word32 -> IO ()

-- ** vkCmdSetStencilReference
foreign import ccall "vkCmdSetStencilReference" vkCmdSetStencilReference ::
  CommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()

-- ** vkCmdPipelineBarrier
foreign import ccall "vkCmdPipelineBarrier" vkCmdPipelineBarrier ::
  CommandBuffer ->
  VkPipelineStageFlags ->
    VkPipelineStageFlags ->
      VkDependencyFlags ->
        Word32 ->
          Ptr MemoryBarrier ->
            Word32 ->
              Ptr BufferMemoryBarrier ->
                Word32 -> Ptr ImageMemoryBarrier -> IO ()

-- ** vkCmdBeginRenderPass
foreign import ccall "vkCmdBeginRenderPass" vkCmdBeginRenderPass ::
  CommandBuffer ->
  Ptr RenderPassBeginInfo -> VkSubpassContents -> IO ()

-- ** vkCmdResetEvent
foreign import ccall "vkCmdResetEvent" vkCmdResetEvent ::
  CommandBuffer -> Event -> VkPipelineStageFlags -> IO ()

