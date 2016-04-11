{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.CommandBufferBuilding where

import Data.Vector.Storable.Sized( Vector
                                 )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , castPtr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Foreign.C.Types( CFloat
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
      Image -> VkImageLayout -> Word32 -> Ptr VkImageResolve -> IO ()

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


data VkImageCopy =
  VkImageCopy{ srcSubresource :: VkImageSubresourceLayers 
             , srcOffset :: VkOffset3D 
             , dstSubresource :: VkImageSubresourceLayers 
             , dstOffset :: VkOffset3D 
             , extent :: VkExtent3D 
             }
  deriving (Eq)

instance Storable VkImageCopy where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = VkImageCopy <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 28)
                         <*> peek (ptr `plusPtr` 44)
                         <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 16) (srcOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 28) (dstSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 44) (dstOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 56) (extent (poked :: VkImageCopy))


-- ** vkCmdNextSubpass
foreign import ccall "vkCmdNextSubpass" vkCmdNextSubpass ::
  CommandBuffer -> VkSubpassContents -> IO ()

-- ** vkCmdEndQuery
foreign import ccall "vkCmdEndQuery" vkCmdEndQuery ::
  CommandBuffer -> QueryPool -> Word32 -> IO ()

-- ** vkCmdSetScissor
foreign import ccall "vkCmdSetScissor" vkCmdSetScissor ::
  CommandBuffer -> Word32 -> Word32 -> Ptr VkRect2D -> IO ()

-- ** vkCmdSetEvent
foreign import ccall "vkCmdSetEvent" vkCmdSetEvent ::
  CommandBuffer -> Event -> VkPipelineStageFlags -> IO ()

-- ** vkCmdCopyImageToBuffer
foreign import ccall "vkCmdCopyImageToBuffer" vkCmdCopyImageToBuffer ::
  CommandBuffer ->
  Image ->
    VkImageLayout -> Buffer -> Word32 -> Ptr VkBufferImageCopy -> IO ()

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


data VkClearRect =
  VkClearRect{ rect :: VkRect2D 
             , baseArrayLayer :: Word32 
             , layerCount :: Word32 
             }
  deriving (Eq)

instance Storable VkClearRect where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkClearRect <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (rect (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 16) (baseArrayLayer (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 20) (layerCount (poked :: VkClearRect))


-- ** vkCmdWaitEvents
foreign import ccall "vkCmdWaitEvents" vkCmdWaitEvents ::
  CommandBuffer ->
  Word32 ->
    Ptr Event ->
      VkPipelineStageFlags ->
        VkPipelineStageFlags ->
          Word32 ->
            Ptr VkMemoryBarrier ->
              Word32 ->
                Ptr VkBufferMemoryBarrier ->
                  Word32 -> Ptr VkImageMemoryBarrier -> IO ()

-- ** vkCmdClearColorImage
foreign import ccall "vkCmdClearColorImage" vkCmdClearColorImage ::
  CommandBuffer ->
  Image ->
    VkImageLayout ->
      Ptr VkClearColorValue ->
        Word32 -> Ptr VkImageSubresourceRange -> IO ()

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


data VkBufferImageCopy =
  VkBufferImageCopy{ bufferOffset :: VkDeviceSize 
                   , bufferRowLength :: Word32 
                   , bufferImageHeight :: Word32 
                   , imageSubresource :: VkImageSubresourceLayers 
                   , imageOffset :: VkOffset3D 
                   , imageExtent :: VkExtent3D 
                   }
  deriving (Eq)

instance Storable VkBufferImageCopy where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferImageCopy <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 12)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 32)
                               <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (bufferOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 8) (bufferRowLength (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 12) (bufferImageHeight (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 16) (imageSubresource (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 32) (imageOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 44) (imageExtent (poked :: VkBufferImageCopy))


-- ** vkCmdSetDepthBounds
foreign import ccall "vkCmdSetDepthBounds" vkCmdSetDepthBounds ::
  CommandBuffer -> CFloat -> CFloat -> IO ()

-- ** vkCmdCopyBufferToImage
foreign import ccall "vkCmdCopyBufferToImage" vkCmdCopyBufferToImage ::
  CommandBuffer ->
  Buffer ->
    Image -> VkImageLayout -> Word32 -> Ptr VkBufferImageCopy -> IO ()

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
      Image -> VkImageLayout -> Word32 -> Ptr VkImageCopy -> IO ()

-- ** vkCmdWriteTimestamp
foreign import ccall "vkCmdWriteTimestamp" vkCmdWriteTimestamp ::
  CommandBuffer ->
  VkPipelineStageFlagBits -> QueryPool -> Word32 -> IO ()


data VkImageSubresourceLayers =
  VkImageSubresourceLayers{ aspectMask :: VkImageAspectFlags 
                          , mipLevel :: Word32 
                          , baseArrayLayer :: Word32 
                          , layerCount :: Word32 
                          }
  deriving (Eq)

instance Storable VkImageSubresourceLayers where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkImageSubresourceLayers <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 4) (mipLevel (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 8) (baseArrayLayer (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 12) (layerCount (poked :: VkImageSubresourceLayers))


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


data VkClearDepthStencilValue =
  VkClearDepthStencilValue{ depth :: CFloat 
                          , stencil :: Word32 
                          }
  deriving (Eq)

instance Storable VkClearDepthStencilValue where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkClearDepthStencilValue <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (depth (poked :: VkClearDepthStencilValue))
                *> poke (ptr `plusPtr` 4) (stencil (poked :: VkClearDepthStencilValue))



data VkBufferCopy =
  VkBufferCopy{ srcOffset :: VkDeviceSize 
              , dstOffset :: VkDeviceSize 
              , size :: VkDeviceSize 
              }
  deriving (Eq)

instance Storable VkBufferCopy where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferCopy <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 8) (dstOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 16) (size (poked :: VkBufferCopy))


-- ** vkCmdClearAttachments
foreign import ccall "vkCmdClearAttachments" vkCmdClearAttachments ::
  CommandBuffer ->
  Word32 ->
    Ptr VkClearAttachment -> Word32 -> Ptr VkClearRect -> IO ()

-- ** vkCmdSetViewport
foreign import ccall "vkCmdSetViewport" vkCmdSetViewport ::
  CommandBuffer -> Word32 -> Word32 -> Ptr VkViewport -> IO ()

-- ** vkCmdCopyBuffer
foreign import ccall "vkCmdCopyBuffer" vkCmdCopyBuffer ::
  CommandBuffer ->
  Buffer -> Buffer -> Word32 -> Ptr VkBufferCopy -> IO ()

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


data VkRenderPassBeginInfo =
  VkRenderPassBeginInfo{ sType :: VkStructureType 
                       , pNext :: Ptr Void 
                       , renderPass :: RenderPass 
                       , framebuffer :: Framebuffer 
                       , renderArea :: VkRect2D 
                       , clearValueCount :: Word32 
                       , pClearValues :: Ptr VkClearValue 
                       }
  deriving (Eq)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (renderPass (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (framebuffer (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 32) (renderArea (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 48) (clearValueCount (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 56) (pClearValues (poked :: VkRenderPassBeginInfo))


-- ** vkCmdSetStencilCompareMask
foreign import ccall "vkCmdSetStencilCompareMask" vkCmdSetStencilCompareMask ::
  CommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()


data VkImageBlit =
  VkImageBlit{ srcSubresource :: VkImageSubresourceLayers 
             , srcOffsets :: Vector 2 VkOffset3D 
             , dstSubresource :: VkImageSubresourceLayers 
             , dstOffsets :: Vector 2 VkOffset3D 
             }
  deriving (Eq)

instance Storable VkImageBlit where
  sizeOf ~_ = 80
  alignment ~_ = 4
  peek ptr = VkImageBlit <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 40)
                         <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcSubresource (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 16) (srcOffsets (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 40) (dstSubresource (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 56) (dstOffsets (poked :: VkImageBlit))



data VkClearAttachment =
  VkClearAttachment{ aspectMask :: VkImageAspectFlags 
                   , colorAttachment :: Word32 
                   , clearValue :: VkClearValue 
                   }
  deriving (Eq)

instance Storable VkClearAttachment where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkClearAttachment <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 4)
                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (aspectMask (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 4) (colorAttachment (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 8) (clearValue (poked :: VkClearAttachment))


-- | // Union allowing specification of color or depth and stencil values. Actual value selected is based on attachment being cleared.
data VkClearValue = Color VkClearColorValue 
                  | DepthStencil VkClearDepthStencilValue 
  deriving (Eq)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ~_ = error "peek@VkClearValue"
  poke ptr poked = case poked of
                     Color e -> poke (castPtr ptr) e
                     DepthStencil e -> poke (castPtr ptr) e


-- ** VkStencilFaceFlags

newtype VkStencilFaceFlagBits = VkStencilFaceFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkStencilFaceFlagBits
type VkStencilFaceFlags = VkStencilFaceFlagBits

instance Show VkStencilFaceFlagBits where
  showsPrec _ VK_STENCIL_FACE_FRONT_BIT = showString "VK_STENCIL_FACE_FRONT_BIT"
  showsPrec _ VK_STENCIL_FACE_BACK_BIT = showString "VK_STENCIL_FACE_BACK_BIT"
  showsPrec _ VK_STENCIL_FRONT_AND_BACK = showString "VK_STENCIL_FRONT_AND_BACK"
  showsPrec p (VkStencilFaceFlagBits x) = showParen (p >= 11) (showString "VkStencilFaceFlagBits " . showsPrec 11 x)

instance Read VkStencilFaceFlagBits where
  readPrec = parens ( choose [ ("VK_STENCIL_FACE_FRONT_BIT", pure VK_STENCIL_FACE_FRONT_BIT)
                             , ("VK_STENCIL_FACE_BACK_BIT", pure VK_STENCIL_FACE_BACK_BIT)
                             , ("VK_STENCIL_FRONT_AND_BACK", pure VK_STENCIL_FRONT_AND_BACK)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStencilFaceFlagBits")
                        v <- step readPrec
                        pure (VkStencilFaceFlagBits v)
                        )
                    )

-- | Front face
pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlagBits 0x1
-- | Back face
pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlagBits 0x2
-- | Front and back faces
pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlagBits 0x3

-- | // Union allowing specification of floating point, integer, or unsigned integer color data. Actual value selected is based on image/attachment being cleared.
data VkClearColorValue = Float32 (Vector 4 CFloat) 
                       | Int32 (Vector 4 Int32) 
                       | Uint32 (Vector 4 Word32) 
  deriving (Eq)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearColorValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ~_ = error "peek@VkClearColorValue"
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
        VkImageLayout -> Word32 -> Ptr VkImageBlit -> VkFilter -> IO ()

-- ** vkCmdSetBlendConstants
foreign import ccall "vkCmdSetBlendConstants" vkCmdSetBlendConstants ::
  CommandBuffer -> Ptr CFloat -> IO ()

-- ** vkCmdClearDepthStencilImage
foreign import ccall "vkCmdClearDepthStencilImage" vkCmdClearDepthStencilImage ::
  CommandBuffer ->
  Image ->
    VkImageLayout ->
      Ptr VkClearDepthStencilValue ->
        Word32 -> Ptr VkImageSubresourceRange -> IO ()


data VkImageResolve =
  VkImageResolve{ srcSubresource :: VkImageSubresourceLayers 
                , srcOffset :: VkOffset3D 
                , dstSubresource :: VkImageSubresourceLayers 
                , dstOffset :: VkOffset3D 
                , extent :: VkExtent3D 
                }
  deriving (Eq)

instance Storable VkImageResolve where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = VkImageResolve <$> peek (ptr `plusPtr` 0)
                            <*> peek (ptr `plusPtr` 16)
                            <*> peek (ptr `plusPtr` 28)
                            <*> peek (ptr `plusPtr` 44)
                            <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 16) (srcOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 28) (dstSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 44) (dstOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 56) (extent (poked :: VkImageResolve))


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
          Ptr VkMemoryBarrier ->
            Word32 ->
              Ptr VkBufferMemoryBarrier ->
                Word32 -> Ptr VkImageMemoryBarrier -> IO ()

-- ** vkCmdBeginRenderPass
foreign import ccall "vkCmdBeginRenderPass" vkCmdBeginRenderPass ::
  CommandBuffer ->
  Ptr VkRenderPassBeginInfo -> VkSubpassContents -> IO ()

-- ** vkCmdResetEvent
foreign import ccall "vkCmdResetEvent" vkCmdResetEvent ::
  CommandBuffer -> Event -> VkPipelineStageFlags -> IO ()

