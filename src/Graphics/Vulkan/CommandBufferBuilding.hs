{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.CommandBufferBuilding where
import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import Graphics.Vulkan.Pass( VkDependencyFlagBits(..)
                           , VkFramebuffer(..)
                           , VkRenderPass(..)
                           , VkAccessFlags(..)
                           , VkDependencyFlags(..)
                           , VkAccessFlagBits(..)
                           )
import Text.Read.Lex( Lexeme(Ident)
                    )
import Graphics.Vulkan.Event( VkEvent(..)
                            )
import GHC.Read( expectP
               , choose
               )
import Graphics.Vulkan.Pipeline( VkPipelineStageFlagBits(..)
                               , VkPipelineStageFlags(..)
                               , VkPipeline(..)
                               , VkPipelineBindPoint(..)
                               )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , castPtr
                  , plusPtr
                  )
import Graphics.Vulkan.DescriptorSet( VkDescriptorSet(..)
                                    )
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
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
import Graphics.Vulkan.PipelineLayout( VkPipelineLayout(..)
                                     )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Shader( VkShaderStageFlagBits(..)
                             , VkShaderStageFlags(..)
                             )
import Graphics.Vulkan.Sampler( VkFilter(..)
                              )
import Graphics.Vulkan.Image( VkImage(..)
                            , VkImageLayout(..)
                            , VkImageAspectFlagBits(..)
                            , VkImageSubresourceRange(..)
                            , VkImageAspectFlags(..)
                            )
import Graphics.Vulkan.Query( VkQueryResultFlagBits(..)
                            , VkQueryControlFlagBits(..)
                            , VkQueryControlFlags(..)
                            , VkQueryPool(..)
                            , VkQueryResultFlags(..)
                            )
import Graphics.Vulkan.OtherTypes( VkImageMemoryBarrier(..)
                                 , VkMemoryBarrier(..)
                                 , VkBufferMemoryBarrier(..)
                                 )
import Graphics.Vulkan.Core( VkExtent3D(..)
                           , VkDeviceSize(..)
                           , VkExtent2D(..)
                           , VkFlags(..)
                           , VkOffset2D(..)
                           , VkOffset3D(..)
                           , VkRect2D(..)
                           , VkViewport(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CFloat
                      , CFloat(..)
                      )
-- ** vkCmdPushConstants
foreign import ccall "vkCmdPushConstants" vkCmdPushConstants :: 
  VkCommandBuffer ->
  VkPipelineLayout ->
    VkShaderStageFlags -> Word32 -> Word32 -> Ptr Void -> IO ()

-- ** vkCmdSetStencilWriteMask
foreign import ccall "vkCmdSetStencilWriteMask" vkCmdSetStencilWriteMask :: 
  VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()

-- ** vkCmdBindIndexBuffer
foreign import ccall "vkCmdBindIndexBuffer" vkCmdBindIndexBuffer :: 
  VkCommandBuffer -> VkBuffer -> VkDeviceSize -> VkIndexType -> IO ()

-- ** vkCmdResetQueryPool
foreign import ccall "vkCmdResetQueryPool" vkCmdResetQueryPool :: 
  VkCommandBuffer -> VkQueryPool -> Word32 -> Word32 -> IO ()

-- ** vkCmdResolveImage
foreign import ccall "vkCmdResolveImage" vkCmdResolveImage :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      VkImage -> VkImageLayout -> Word32 -> Ptr VkImageResolve -> IO ()

-- ** vkCmdBindPipeline
foreign import ccall "vkCmdBindPipeline" vkCmdBindPipeline :: 
  VkCommandBuffer -> VkPipelineBindPoint -> VkPipeline -> IO ()

-- ** vkCmdBindVertexBuffers
foreign import ccall "vkCmdBindVertexBuffers" vkCmdBindVertexBuffers :: 
  VkCommandBuffer ->
  Word32 -> Word32 -> Ptr VkBuffer -> Ptr VkDeviceSize -> IO ()

-- ** vkCmdDraw
foreign import ccall "vkCmdDraw" vkCmdDraw :: 
  VkCommandBuffer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()


data VkImageCopy =
  VkImageCopy{ vkSrcSubresource :: VkImageSubresourceLayers 
             , vkSrcOffset :: VkOffset3D 
             , vkDstSubresource :: VkImageSubresourceLayers 
             , vkDstOffset :: VkOffset3D 
             , vkExtent :: VkExtent3D 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 16) (vkSrcOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 28) (vkDstSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 44) (vkDstOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 56) (vkExtent (poked :: VkImageCopy))


-- ** vkCmdNextSubpass
foreign import ccall "vkCmdNextSubpass" vkCmdNextSubpass :: 
  VkCommandBuffer -> VkSubpassContents -> IO ()

-- ** vkCmdEndQuery
foreign import ccall "vkCmdEndQuery" vkCmdEndQuery :: 
  VkCommandBuffer -> VkQueryPool -> Word32 -> IO ()

-- ** vkCmdSetScissor
foreign import ccall "vkCmdSetScissor" vkCmdSetScissor :: 
  VkCommandBuffer -> Word32 -> Word32 -> Ptr VkRect2D -> IO ()

-- ** vkCmdSetEvent
foreign import ccall "vkCmdSetEvent" vkCmdSetEvent :: 
  VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ()

-- ** vkCmdCopyImageToBuffer
foreign import ccall "vkCmdCopyImageToBuffer" vkCmdCopyImageToBuffer :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      VkBuffer -> Word32 -> Ptr VkBufferImageCopy -> IO ()

-- ** vkCmdDispatchIndirect
foreign import ccall "vkCmdDispatchIndirect" vkCmdDispatchIndirect :: 
  VkCommandBuffer -> VkBuffer -> VkDeviceSize -> IO ()

-- ** vkCmdBeginQuery
foreign import ccall "vkCmdBeginQuery" vkCmdBeginQuery :: 
  VkCommandBuffer ->
  VkQueryPool -> Word32 -> VkQueryControlFlags -> IO ()

-- ** vkCmdEndRenderPass
foreign import ccall "vkCmdEndRenderPass" vkCmdEndRenderPass :: 
  VkCommandBuffer -> IO ()

-- ** vkCmdFillBuffer
foreign import ccall "vkCmdFillBuffer" vkCmdFillBuffer :: 
  VkCommandBuffer ->
  VkBuffer -> VkDeviceSize -> VkDeviceSize -> Word32 -> IO ()


data VkClearRect =
  VkClearRect{ vkRect :: VkRect2D 
             , vkBaseArrayLayer :: Word32 
             , vkLayerCount :: Word32 
             }
  deriving (Eq)

instance Storable VkClearRect where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkClearRect <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRect (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 16) (vkBaseArrayLayer (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 20) (vkLayerCount (poked :: VkClearRect))


-- ** vkCmdWaitEvents
foreign import ccall "vkCmdWaitEvents" vkCmdWaitEvents :: 
  VkCommandBuffer ->
  Word32 ->
    Ptr VkEvent ->
      VkPipelineStageFlags ->
        VkPipelineStageFlags ->
          Word32 ->
            Ptr VkMemoryBarrier ->
              Word32 ->
                Ptr VkBufferMemoryBarrier ->
                  Word32 -> Ptr VkImageMemoryBarrier -> IO ()

-- ** vkCmdClearColorImage
foreign import ccall "vkCmdClearColorImage" vkCmdClearColorImage :: 
  VkCommandBuffer ->
  VkImage ->
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
  VkBufferImageCopy{ vkBufferOffset :: VkDeviceSize 
                   , vkBufferRowLength :: Word32 
                   , vkBufferImageHeight :: Word32 
                   , vkImageSubresource :: VkImageSubresourceLayers 
                   , vkImageOffset :: VkOffset3D 
                   , vkImageExtent :: VkExtent3D 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBufferOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 8) (vkBufferRowLength (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 12) (vkBufferImageHeight (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 16) (vkImageSubresource (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 32) (vkImageOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 44) (vkImageExtent (poked :: VkBufferImageCopy))


-- ** vkCmdSetDepthBounds
foreign import ccall "vkCmdSetDepthBounds" vkCmdSetDepthBounds :: 
  VkCommandBuffer -> CFloat -> CFloat -> IO ()

-- ** vkCmdCopyBufferToImage
foreign import ccall "vkCmdCopyBufferToImage" vkCmdCopyBufferToImage :: 
  VkCommandBuffer ->
  VkBuffer ->
    VkImage ->
      VkImageLayout -> Word32 -> Ptr VkBufferImageCopy -> IO ()

-- ** vkCmdDrawIndexedIndirect
foreign import ccall "vkCmdDrawIndexedIndirect" vkCmdDrawIndexedIndirect :: 
  VkCommandBuffer ->
  VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()

-- ** vkCmdUpdateBuffer
foreign import ccall "vkCmdUpdateBuffer" vkCmdUpdateBuffer :: 
  VkCommandBuffer ->
  VkBuffer -> VkDeviceSize -> VkDeviceSize -> Ptr Word32 -> IO ()

-- ** vkCmdCopyImage
foreign import ccall "vkCmdCopyImage" vkCmdCopyImage :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      VkImage -> VkImageLayout -> Word32 -> Ptr VkImageCopy -> IO ()

-- ** vkCmdWriteTimestamp
foreign import ccall "vkCmdWriteTimestamp" vkCmdWriteTimestamp :: 
  VkCommandBuffer ->
  VkPipelineStageFlagBits -> VkQueryPool -> Word32 -> IO ()


data VkImageSubresourceLayers =
  VkImageSubresourceLayers{ vkAspectMask :: VkImageAspectFlags 
                          , vkMipLevel :: Word32 
                          , vkBaseArrayLayer :: Word32 
                          , vkLayerCount :: Word32 
                          }
  deriving (Eq)

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


-- ** vkCmdDrawIndexed
foreign import ccall "vkCmdDrawIndexed" vkCmdDrawIndexed :: 
  VkCommandBuffer ->
  Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()

-- ** vkCmdSetDepthBias
foreign import ccall "vkCmdSetDepthBias" vkCmdSetDepthBias :: 
  VkCommandBuffer -> CFloat -> CFloat -> CFloat -> IO ()

-- ** vkCmdDrawIndirect
foreign import ccall "vkCmdDrawIndirect" vkCmdDrawIndirect :: 
  VkCommandBuffer ->
  VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()


data VkClearDepthStencilValue =
  VkClearDepthStencilValue{ vkDepth :: CFloat 
                          , vkStencil :: Word32 
                          }
  deriving (Eq)

instance Storable VkClearDepthStencilValue where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkClearDepthStencilValue <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDepth (poked :: VkClearDepthStencilValue))
                *> poke (ptr `plusPtr` 4) (vkStencil (poked :: VkClearDepthStencilValue))



data VkBufferCopy =
  VkBufferCopy{ vkSrcOffset :: VkDeviceSize 
              , vkDstOffset :: VkDeviceSize 
              , vkSize :: VkDeviceSize 
              }
  deriving (Eq)

instance Storable VkBufferCopy where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferCopy <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 8) (vkDstOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 16) (vkSize (poked :: VkBufferCopy))


-- ** vkCmdClearAttachments
foreign import ccall "vkCmdClearAttachments" vkCmdClearAttachments :: 
  VkCommandBuffer ->
  Word32 ->
    Ptr VkClearAttachment -> Word32 -> Ptr VkClearRect -> IO ()

-- ** vkCmdSetViewport
foreign import ccall "vkCmdSetViewport" vkCmdSetViewport :: 
  VkCommandBuffer -> Word32 -> Word32 -> Ptr VkViewport -> IO ()

-- ** vkCmdCopyBuffer
foreign import ccall "vkCmdCopyBuffer" vkCmdCopyBuffer :: 
  VkCommandBuffer ->
  VkBuffer -> VkBuffer -> Word32 -> Ptr VkBufferCopy -> IO ()

-- ** vkCmdBindDescriptorSets
foreign import ccall "vkCmdBindDescriptorSets" vkCmdBindDescriptorSets :: 
  VkCommandBuffer ->
  VkPipelineBindPoint ->
    VkPipelineLayout ->
      Word32 ->
        Word32 -> Ptr VkDescriptorSet -> Word32 -> Ptr Word32 -> IO ()

-- ** vkCmdSetLineWidth
foreign import ccall "vkCmdSetLineWidth" vkCmdSetLineWidth :: 
  VkCommandBuffer -> CFloat -> IO ()

-- ** vkCmdExecuteCommands
foreign import ccall "vkCmdExecuteCommands" vkCmdExecuteCommands :: 
  VkCommandBuffer -> Word32 -> Ptr VkCommandBuffer -> IO ()


data VkRenderPassBeginInfo =
  VkRenderPassBeginInfo{ vkSType :: VkStructureType 
                       , vkPNext :: Ptr Void 
                       , vkRenderPass :: VkRenderPass 
                       , vkFramebuffer :: VkFramebuffer 
                       , vkRenderArea :: VkRect2D 
                       , vkClearValueCount :: Word32 
                       , vkPClearValues :: Ptr VkClearValue 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkRenderPass (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkFramebuffer (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 32) (vkRenderArea (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 48) (vkClearValueCount (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 56) (vkPClearValues (poked :: VkRenderPassBeginInfo))


-- ** vkCmdSetStencilCompareMask
foreign import ccall "vkCmdSetStencilCompareMask" vkCmdSetStencilCompareMask :: 
  VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()


data VkImageBlit =
  VkImageBlit{ vkSrcSubresource :: VkImageSubresourceLayers 
             , vkSrcOffsets :: Vector 2 VkOffset3D 
             , vkDstSubresource :: VkImageSubresourceLayers 
             , vkDstOffsets :: Vector 2 VkOffset3D 
             }
  deriving (Eq)

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



data VkClearAttachment =
  VkClearAttachment{ vkAspectMask :: VkImageAspectFlags 
                   , vkColorAttachment :: Word32 
                   , vkClearValue :: VkClearValue 
                   }
  deriving (Eq)

instance Storable VkClearAttachment where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkClearAttachment <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 4)
                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 4) (vkColorAttachment (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 8) (vkClearValue (poked :: VkClearAttachment))


-- | // Union allowing specification of color or depth and stencil values. Actual value selected is based on attachment being cleared.
data VkClearValue = VkColor VkClearColorValue 
                  | VkDepthStencil VkClearDepthStencilValue 
  deriving (Eq)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ~_ = error "peek@VkClearValue"
  poke ptr poked = case poked of
                     VkColor e -> poke (castPtr ptr) e
                     VkDepthStencil e -> poke (castPtr ptr) e


-- ** VkStencilFaceFlags

newtype VkStencilFaceFlagBits = VkStencilFaceFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for VkStencilFaceFlagBits
type VkStencilFaceFlags = VkStencilFaceFlagBits
-- | Front face
pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlagBits 0x1
-- | Back face
pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlagBits 0x2
-- | Front and back faces
pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlagBits 0x3

-- | // Union allowing specification of floating point, integer, or unsigned integer color data. Actual value selected is based on image/attachment being cleared.
data VkClearColorValue = VkFloat (Vector 4 CFloat) 
                       | VkInt (Vector 4 Int32) 
                       | VkUint (Vector 4 Word32) 
  deriving (Eq)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearColorValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ~_ = error "peek@VkClearColorValue"
  poke ptr poked = case poked of
                     VkFloat e -> poke (castPtr ptr) e
                     VkInt e -> poke (castPtr ptr) e
                     VkUint e -> poke (castPtr ptr) e


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
  VkCommandBuffer ->
  VkQueryPool ->
    Word32 ->
      Word32 ->
        VkBuffer ->
          VkDeviceSize -> VkDeviceSize -> VkQueryResultFlags -> IO ()

-- ** vkCmdBlitImage
foreign import ccall "vkCmdBlitImage" vkCmdBlitImage :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      VkImage ->
        VkImageLayout -> Word32 -> Ptr VkImageBlit -> VkFilter -> IO ()

-- ** vkCmdSetBlendConstants
foreign import ccall "vkCmdSetBlendConstants" vkCmdSetBlendConstants :: 
  VkCommandBuffer -> Ptr CFloat -> IO ()

-- ** vkCmdClearDepthStencilImage
foreign import ccall "vkCmdClearDepthStencilImage" vkCmdClearDepthStencilImage :: 
  VkCommandBuffer ->
  VkImage ->
    VkImageLayout ->
      Ptr VkClearDepthStencilValue ->
        Word32 -> Ptr VkImageSubresourceRange -> IO ()


data VkImageResolve =
  VkImageResolve{ vkSrcSubresource :: VkImageSubresourceLayers 
                , vkSrcOffset :: VkOffset3D 
                , vkDstSubresource :: VkImageSubresourceLayers 
                , vkDstOffset :: VkOffset3D 
                , vkExtent :: VkExtent3D 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 16) (vkSrcOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 28) (vkDstSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 44) (vkDstOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 56) (vkExtent (poked :: VkImageResolve))


-- ** vkCmdDispatch
foreign import ccall "vkCmdDispatch" vkCmdDispatch :: 
  VkCommandBuffer -> Word32 -> Word32 -> Word32 -> IO ()

-- ** vkCmdSetStencilReference
foreign import ccall "vkCmdSetStencilReference" vkCmdSetStencilReference :: 
  VkCommandBuffer -> VkStencilFaceFlags -> Word32 -> IO ()

-- ** vkCmdPipelineBarrier
foreign import ccall "vkCmdPipelineBarrier" vkCmdPipelineBarrier :: 
  VkCommandBuffer ->
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
  VkCommandBuffer ->
  Ptr VkRenderPassBeginInfo -> VkSubpassContents -> IO ()

-- ** vkCmdResetEvent
foreign import ccall "vkCmdResetEvent" vkCmdResetEvent :: 
  VkCommandBuffer -> VkEvent -> VkPipelineStageFlags -> IO ()

