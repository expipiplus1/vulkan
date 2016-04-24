{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Pass where

import Graphics.Vulkan.Device( Device(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Graphics.Vulkan.Pipeline( PipelineBindPoint(..)
                               , PipelineStageFlags(..)
                               )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( SampleCountFlags(..)
                              )
import Graphics.Vulkan.Image( ImageLayout(..)
                            )
import Graphics.Vulkan.ImageView( ImageView(..)
                                )
import Graphics.Vulkan.Core( StructureType(..)
                           , Format(..)
                           , Result(..)
                           , Flags(..)
                           , Extent2D(..)
                           )


data SubpassDependency =
  SubpassDependency{ srcSubpass :: Word32 
                   , dstSubpass :: Word32 
                   , srcStageMask :: PipelineStageFlags 
                   , dstStageMask :: PipelineStageFlags 
                   , srcAccessMask :: AccessFlags 
                   , dstAccessMask :: AccessFlags 
                   , dependencyFlags :: DependencyFlags 
                   }
  deriving (Eq, Ord)

instance Storable SubpassDependency where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek ptr = SubpassDependency <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 4)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 12)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 20)
                               <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcSubpass (poked :: SubpassDependency))
                *> poke (ptr `plusPtr` 4) (dstSubpass (poked :: SubpassDependency))
                *> poke (ptr `plusPtr` 8) (srcStageMask (poked :: SubpassDependency))
                *> poke (ptr `plusPtr` 12) (dstStageMask (poked :: SubpassDependency))
                *> poke (ptr `plusPtr` 16) (srcAccessMask (poked :: SubpassDependency))
                *> poke (ptr `plusPtr` 20) (dstAccessMask (poked :: SubpassDependency))
                *> poke (ptr `plusPtr` 24) (dependencyFlags (poked :: SubpassDependency))


-- ** SubpassDescriptionFlags
-- | Opaque flag
newtype SubpassDescriptionFlags = SubpassDescriptionFlags Flags
  deriving (Eq, Ord, Storable)

newtype Framebuffer = Framebuffer Word64
  deriving (Eq, Ord, Storable)

-- ** AttachmentDescriptionFlags

newtype AttachmentDescriptionFlags = AttachmentDescriptionFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show AttachmentDescriptionFlags where
  showsPrec _ AttachmentDescriptionMayAliasBit = showString "AttachmentDescriptionMayAliasBit"
  
  showsPrec p (AttachmentDescriptionFlags x) = showParen (p >= 11) (showString "AttachmentDescriptionFlags " . showsPrec 11 x)

instance Read AttachmentDescriptionFlags where
  readPrec = parens ( choose [ ("AttachmentDescriptionMayAliasBit", pure AttachmentDescriptionMayAliasBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "AttachmentDescriptionFlags")
                        v <- step readPrec
                        pure (AttachmentDescriptionFlags v)
                        )
                    )

-- | The attachment may alias physical memory of another attachment in the same render pass
pattern AttachmentDescriptionMayAliasBit = AttachmentDescriptionFlags 0x1


-- ** DependencyFlags

newtype DependencyFlags = DependencyFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show DependencyFlags where
  showsPrec _ DependencyByRegionBit = showString "DependencyByRegionBit"
  
  showsPrec p (DependencyFlags x) = showParen (p >= 11) (showString "DependencyFlags " . showsPrec 11 x)

instance Read DependencyFlags where
  readPrec = parens ( choose [ ("DependencyByRegionBit", pure DependencyByRegionBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DependencyFlags")
                        v <- step readPrec
                        pure (DependencyFlags v)
                        )
                    )

-- | Dependency is per pixel region 
pattern DependencyByRegionBit = DependencyFlags 0x1


-- ** destroyRenderPass
foreign import ccall "vkDestroyRenderPass" destroyRenderPass ::
  Device -> RenderPass -> Ptr AllocationCallbacks -> IO ()

-- ** createFramebuffer
foreign import ccall "vkCreateFramebuffer" createFramebuffer ::
  Device ->
  Ptr FramebufferCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Framebuffer -> IO Result


data FramebufferCreateInfo =
  FramebufferCreateInfo{ sType :: StructureType 
                       , pNext :: Ptr Void 
                       , flags :: FramebufferCreateFlags 
                       , renderPass :: RenderPass 
                       , attachmentCount :: Word32 
                       , pAttachments :: Ptr ImageView 
                       , width :: Word32 
                       , height :: Word32 
                       , layers :: Word32 
                       }
  deriving (Eq, Ord)

instance Storable FramebufferCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = FramebufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 48)
                                   <*> peek (ptr `plusPtr` 52)
                                   <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: FramebufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: FramebufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: FramebufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (renderPass (poked :: FramebufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (attachmentCount (poked :: FramebufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (pAttachments (poked :: FramebufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (width (poked :: FramebufferCreateInfo))
                *> poke (ptr `plusPtr` 52) (height (poked :: FramebufferCreateInfo))
                *> poke (ptr `plusPtr` 56) (layers (poked :: FramebufferCreateInfo))


-- ** getRenderAreaGranularity
foreign import ccall "vkGetRenderAreaGranularity" getRenderAreaGranularity ::
  Device -> RenderPass -> Ptr Extent2D -> IO ()

-- ** AttachmentLoadOp

newtype AttachmentLoadOp = AttachmentLoadOp Int32
  deriving (Eq, Ord, Storable)

instance Show AttachmentLoadOp where
  showsPrec _ AttachmentLoadOpLoad = showString "AttachmentLoadOpLoad"
  showsPrec _ AttachmentLoadOpClear = showString "AttachmentLoadOpClear"
  showsPrec _ AttachmentLoadOpDontCare = showString "AttachmentLoadOpDontCare"
  showsPrec p (AttachmentLoadOp x) = showParen (p >= 11) (showString "AttachmentLoadOp " . showsPrec 11 x)

instance Read AttachmentLoadOp where
  readPrec = parens ( choose [ ("AttachmentLoadOpLoad", pure AttachmentLoadOpLoad)
                             , ("AttachmentLoadOpClear", pure AttachmentLoadOpClear)
                             , ("AttachmentLoadOpDontCare", pure AttachmentLoadOpDontCare)
                             ] +++
                      prec 10 (do
                        expectP (Ident "AttachmentLoadOp")
                        v <- step readPrec
                        pure (AttachmentLoadOp v)
                        )
                    )


pattern AttachmentLoadOpLoad = AttachmentLoadOp 0

pattern AttachmentLoadOpClear = AttachmentLoadOp 1

pattern AttachmentLoadOpDontCare = AttachmentLoadOp 2

-- ** AttachmentStoreOp

newtype AttachmentStoreOp = AttachmentStoreOp Int32
  deriving (Eq, Ord, Storable)

instance Show AttachmentStoreOp where
  showsPrec _ AttachmentStoreOpStore = showString "AttachmentStoreOpStore"
  showsPrec _ AttachmentStoreOpDontCare = showString "AttachmentStoreOpDontCare"
  showsPrec p (AttachmentStoreOp x) = showParen (p >= 11) (showString "AttachmentStoreOp " . showsPrec 11 x)

instance Read AttachmentStoreOp where
  readPrec = parens ( choose [ ("AttachmentStoreOpStore", pure AttachmentStoreOpStore)
                             , ("AttachmentStoreOpDontCare", pure AttachmentStoreOpDontCare)
                             ] +++
                      prec 10 (do
                        expectP (Ident "AttachmentStoreOp")
                        v <- step readPrec
                        pure (AttachmentStoreOp v)
                        )
                    )


pattern AttachmentStoreOpStore = AttachmentStoreOp 0

pattern AttachmentStoreOpDontCare = AttachmentStoreOp 1

-- ** AccessFlags

newtype AccessFlags = AccessFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show AccessFlags where
  showsPrec _ AccessIndirectCommandReadBit = showString "AccessIndirectCommandReadBit"
  showsPrec _ AccessIndexReadBit = showString "AccessIndexReadBit"
  showsPrec _ AccessVertexAttributeReadBit = showString "AccessVertexAttributeReadBit"
  showsPrec _ AccessUniformReadBit = showString "AccessUniformReadBit"
  showsPrec _ AccessInputAttachmentReadBit = showString "AccessInputAttachmentReadBit"
  showsPrec _ AccessShaderReadBit = showString "AccessShaderReadBit"
  showsPrec _ AccessShaderWriteBit = showString "AccessShaderWriteBit"
  showsPrec _ AccessColorAttachmentReadBit = showString "AccessColorAttachmentReadBit"
  showsPrec _ AccessColorAttachmentWriteBit = showString "AccessColorAttachmentWriteBit"
  showsPrec _ AccessDepthStencilAttachmentReadBit = showString "AccessDepthStencilAttachmentReadBit"
  showsPrec _ AccessDepthStencilAttachmentWriteBit = showString "AccessDepthStencilAttachmentWriteBit"
  showsPrec _ AccessTransferReadBit = showString "AccessTransferReadBit"
  showsPrec _ AccessTransferWriteBit = showString "AccessTransferWriteBit"
  showsPrec _ AccessHostReadBit = showString "AccessHostReadBit"
  showsPrec _ AccessHostWriteBit = showString "AccessHostWriteBit"
  showsPrec _ AccessMemoryReadBit = showString "AccessMemoryReadBit"
  showsPrec _ AccessMemoryWriteBit = showString "AccessMemoryWriteBit"
  
  showsPrec p (AccessFlags x) = showParen (p >= 11) (showString "AccessFlags " . showsPrec 11 x)

instance Read AccessFlags where
  readPrec = parens ( choose [ ("AccessIndirectCommandReadBit", pure AccessIndirectCommandReadBit)
                             , ("AccessIndexReadBit", pure AccessIndexReadBit)
                             , ("AccessVertexAttributeReadBit", pure AccessVertexAttributeReadBit)
                             , ("AccessUniformReadBit", pure AccessUniformReadBit)
                             , ("AccessInputAttachmentReadBit", pure AccessInputAttachmentReadBit)
                             , ("AccessShaderReadBit", pure AccessShaderReadBit)
                             , ("AccessShaderWriteBit", pure AccessShaderWriteBit)
                             , ("AccessColorAttachmentReadBit", pure AccessColorAttachmentReadBit)
                             , ("AccessColorAttachmentWriteBit", pure AccessColorAttachmentWriteBit)
                             , ("AccessDepthStencilAttachmentReadBit", pure AccessDepthStencilAttachmentReadBit)
                             , ("AccessDepthStencilAttachmentWriteBit", pure AccessDepthStencilAttachmentWriteBit)
                             , ("AccessTransferReadBit", pure AccessTransferReadBit)
                             , ("AccessTransferWriteBit", pure AccessTransferWriteBit)
                             , ("AccessHostReadBit", pure AccessHostReadBit)
                             , ("AccessHostWriteBit", pure AccessHostWriteBit)
                             , ("AccessMemoryReadBit", pure AccessMemoryReadBit)
                             , ("AccessMemoryWriteBit", pure AccessMemoryWriteBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "AccessFlags")
                        v <- step readPrec
                        pure (AccessFlags v)
                        )
                    )

-- | Controls coherency of indirect command reads
pattern AccessIndirectCommandReadBit = AccessFlags 0x1
-- | Controls coherency of index reads
pattern AccessIndexReadBit = AccessFlags 0x2
-- | Controls coherency of vertex attribute reads
pattern AccessVertexAttributeReadBit = AccessFlags 0x4
-- | Controls coherency of uniform buffer reads
pattern AccessUniformReadBit = AccessFlags 0x8
-- | Controls coherency of input attachment reads
pattern AccessInputAttachmentReadBit = AccessFlags 0x10
-- | Controls coherency of shader reads
pattern AccessShaderReadBit = AccessFlags 0x20
-- | Controls coherency of shader writes
pattern AccessShaderWriteBit = AccessFlags 0x40
-- | Controls coherency of color attachment reads
pattern AccessColorAttachmentReadBit = AccessFlags 0x80
-- | Controls coherency of color attachment writes
pattern AccessColorAttachmentWriteBit = AccessFlags 0x100
-- | Controls coherency of depth/stencil attachment reads
pattern AccessDepthStencilAttachmentReadBit = AccessFlags 0x200
-- | Controls coherency of depth/stencil attachment writes
pattern AccessDepthStencilAttachmentWriteBit = AccessFlags 0x400
-- | Controls coherency of transfer reads
pattern AccessTransferReadBit = AccessFlags 0x800
-- | Controls coherency of transfer writes
pattern AccessTransferWriteBit = AccessFlags 0x1000
-- | Controls coherency of host reads
pattern AccessHostReadBit = AccessFlags 0x2000
-- | Controls coherency of host writes
pattern AccessHostWriteBit = AccessFlags 0x4000
-- | Controls coherency of memory reads
pattern AccessMemoryReadBit = AccessFlags 0x8000
-- | Controls coherency of memory writes
pattern AccessMemoryWriteBit = AccessFlags 0x10000


newtype RenderPass = RenderPass Word64
  deriving (Eq, Ord, Storable)

-- ** destroyFramebuffer
foreign import ccall "vkDestroyFramebuffer" destroyFramebuffer ::
  Device -> Framebuffer -> Ptr AllocationCallbacks -> IO ()


data AttachmentReference =
  AttachmentReference{ attachment :: Word32 
                     , layout :: ImageLayout 
                     }
  deriving (Eq, Ord)

instance Storable AttachmentReference where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = AttachmentReference <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (attachment (poked :: AttachmentReference))
                *> poke (ptr `plusPtr` 4) (layout (poked :: AttachmentReference))


-- ** RenderPassCreateFlags
-- | Opaque flag
newtype RenderPassCreateFlags = RenderPassCreateFlags Flags
  deriving (Eq, Ord, Storable)


data AttachmentDescription =
  AttachmentDescription{ flags :: AttachmentDescriptionFlags 
                       , format :: Format 
                       , samples :: SampleCountFlags 
                       , loadOp :: AttachmentLoadOp 
                       , storeOp :: AttachmentStoreOp 
                       , stencilLoadOp :: AttachmentLoadOp 
                       , stencilStoreOp :: AttachmentStoreOp 
                       , initialLayout :: ImageLayout 
                       , finalLayout :: ImageLayout 
                       }
  deriving (Eq, Ord)

instance Storable AttachmentDescription where
  sizeOf ~_ = 36
  alignment ~_ = 4
  peek ptr = AttachmentDescription <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 12)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 28)
                                   <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (flags (poked :: AttachmentDescription))
                *> poke (ptr `plusPtr` 4) (format (poked :: AttachmentDescription))
                *> poke (ptr `plusPtr` 8) (samples (poked :: AttachmentDescription))
                *> poke (ptr `plusPtr` 12) (loadOp (poked :: AttachmentDescription))
                *> poke (ptr `plusPtr` 16) (storeOp (poked :: AttachmentDescription))
                *> poke (ptr `plusPtr` 20) (stencilLoadOp (poked :: AttachmentDescription))
                *> poke (ptr `plusPtr` 24) (stencilStoreOp (poked :: AttachmentDescription))
                *> poke (ptr `plusPtr` 28) (initialLayout (poked :: AttachmentDescription))
                *> poke (ptr `plusPtr` 32) (finalLayout (poked :: AttachmentDescription))



data SubpassDescription =
  SubpassDescription{ flags :: SubpassDescriptionFlags 
                    , pipelineBindPoint :: PipelineBindPoint 
                    , inputAttachmentCount :: Word32 
                    , pInputAttachments :: Ptr AttachmentReference 
                    , colorAttachmentCount :: Word32 
                    , pColorAttachments :: Ptr AttachmentReference 
                    , pResolveAttachments :: Ptr AttachmentReference 
                    , pDepthStencilAttachment :: Ptr AttachmentReference 
                    , preserveAttachmentCount :: Word32 
                    , pPreserveAttachments :: Ptr Word32 
                    }
  deriving (Eq, Ord)

instance Storable SubpassDescription where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = SubpassDescription <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
                                <*> peek (ptr `plusPtr` 56)
                                <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (flags (poked :: SubpassDescription))
                *> poke (ptr `plusPtr` 4) (pipelineBindPoint (poked :: SubpassDescription))
                *> poke (ptr `plusPtr` 8) (inputAttachmentCount (poked :: SubpassDescription))
                *> poke (ptr `plusPtr` 16) (pInputAttachments (poked :: SubpassDescription))
                *> poke (ptr `plusPtr` 24) (colorAttachmentCount (poked :: SubpassDescription))
                *> poke (ptr `plusPtr` 32) (pColorAttachments (poked :: SubpassDescription))
                *> poke (ptr `plusPtr` 40) (pResolveAttachments (poked :: SubpassDescription))
                *> poke (ptr `plusPtr` 48) (pDepthStencilAttachment (poked :: SubpassDescription))
                *> poke (ptr `plusPtr` 56) (preserveAttachmentCount (poked :: SubpassDescription))
                *> poke (ptr `plusPtr` 64) (pPreserveAttachments (poked :: SubpassDescription))


-- ** createRenderPass
foreign import ccall "vkCreateRenderPass" createRenderPass ::
  Device ->
  Ptr RenderPassCreateInfo ->
    Ptr AllocationCallbacks -> Ptr RenderPass -> IO Result


data RenderPassCreateInfo =
  RenderPassCreateInfo{ sType :: StructureType 
                      , pNext :: Ptr Void 
                      , flags :: RenderPassCreateFlags 
                      , attachmentCount :: Word32 
                      , pAttachments :: Ptr AttachmentDescription 
                      , subpassCount :: Word32 
                      , pSubpasses :: Ptr SubpassDescription 
                      , dependencyCount :: Word32 
                      , pDependencies :: Ptr SubpassDependency 
                      }
  deriving (Eq, Ord)

instance Storable RenderPassCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = RenderPassCreateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 20)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: RenderPassCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: RenderPassCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: RenderPassCreateInfo))
                *> poke (ptr `plusPtr` 20) (attachmentCount (poked :: RenderPassCreateInfo))
                *> poke (ptr `plusPtr` 24) (pAttachments (poked :: RenderPassCreateInfo))
                *> poke (ptr `plusPtr` 32) (subpassCount (poked :: RenderPassCreateInfo))
                *> poke (ptr `plusPtr` 40) (pSubpasses (poked :: RenderPassCreateInfo))
                *> poke (ptr `plusPtr` 48) (dependencyCount (poked :: RenderPassCreateInfo))
                *> poke (ptr `plusPtr` 56) (pDependencies (poked :: RenderPassCreateInfo))


-- ** FramebufferCreateFlags
-- | Opaque flag
newtype FramebufferCreateFlags = FramebufferCreateFlags Flags
  deriving (Eq, Ord, Storable)

