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
import Graphics.Vulkan.Pipeline( VkPipelineBindPoint(..)
                               , VkPipelineStageFlags(..)
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
import Graphics.Vulkan.Sampler( VkSampleCountFlags(..)
                              )
import Graphics.Vulkan.Image( VkImageLayout(..)
                            )
import Graphics.Vulkan.ImageView( ImageView(..)
                                )
import Graphics.Vulkan.Core( VkStructureType(..)
                           , VkFormat(..)
                           , VkFlags(..)
                           , VkResult(..)
                           , Extent2D(..)
                           )


data SubpassDependency =
  SubpassDependency{ srcSubpass :: Word32 
                   , dstSubpass :: Word32 
                   , srcStageMask :: VkPipelineStageFlags 
                   , dstStageMask :: VkPipelineStageFlags 
                   , srcAccessMask :: VkAccessFlags 
                   , dstAccessMask :: VkAccessFlags 
                   , dependencyFlags :: VkDependencyFlags 
                   }
  deriving (Eq)

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


-- ** VkSubpassDescriptionFlags
-- | Opaque flag
newtype VkSubpassDescriptionFlags = VkSubpassDescriptionFlags VkFlags
  deriving (Eq, Storable)

newtype Framebuffer = Framebuffer Word64
  deriving (Eq, Storable)

-- ** VkAttachmentDescriptionFlags

newtype VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkAttachmentDescriptionFlags where
  showsPrec _ VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = showString "VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT"
  
  showsPrec p (VkAttachmentDescriptionFlags x) = showParen (p >= 11) (showString "VkAttachmentDescriptionFlags " . showsPrec 11 x)

instance Read VkAttachmentDescriptionFlags where
  readPrec = parens ( choose [ ("VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT", pure VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAttachmentDescriptionFlags")
                        v <- step readPrec
                        pure (VkAttachmentDescriptionFlags v)
                        )
                    )

-- | The attachment may alias physical memory of another attachment in the same render pass
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VkAttachmentDescriptionFlags 0x1


-- ** VkDependencyFlags

newtype VkDependencyFlags = VkDependencyFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkDependencyFlags where
  showsPrec _ VK_DEPENDENCY_BY_REGION_BIT = showString "VK_DEPENDENCY_BY_REGION_BIT"
  
  showsPrec p (VkDependencyFlags x) = showParen (p >= 11) (showString "VkDependencyFlags " . showsPrec 11 x)

instance Read VkDependencyFlags where
  readPrec = parens ( choose [ ("VK_DEPENDENCY_BY_REGION_BIT", pure VK_DEPENDENCY_BY_REGION_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDependencyFlags")
                        v <- step readPrec
                        pure (VkDependencyFlags v)
                        )
                    )

-- | Dependency is per pixel region 
pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlags 0x1


-- ** vkDestroyRenderPass
foreign import ccall "vkDestroyRenderPass" vkDestroyRenderPass ::
  Device -> RenderPass -> Ptr AllocationCallbacks -> IO ()

-- ** vkCreateFramebuffer
foreign import ccall "vkCreateFramebuffer" vkCreateFramebuffer ::
  Device ->
  Ptr FramebufferCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Framebuffer -> IO VkResult


data FramebufferCreateInfo =
  FramebufferCreateInfo{ sType :: VkStructureType 
                       , pNext :: Ptr Void 
                       , flags :: VkFramebufferCreateFlags 
                       , renderPass :: RenderPass 
                       , attachmentCount :: Word32 
                       , pAttachments :: Ptr ImageView 
                       , width :: Word32 
                       , height :: Word32 
                       , layers :: Word32 
                       }
  deriving (Eq)

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


-- ** vkGetRenderAreaGranularity
foreign import ccall "vkGetRenderAreaGranularity" vkGetRenderAreaGranularity ::
  Device -> RenderPass -> Ptr Extent2D -> IO ()

-- ** VkAttachmentLoadOp

newtype VkAttachmentLoadOp = VkAttachmentLoadOp Int32
  deriving (Eq, Storable)

instance Show VkAttachmentLoadOp where
  showsPrec _ VK_ATTACHMENT_LOAD_OP_LOAD = showString "VK_ATTACHMENT_LOAD_OP_LOAD"
  showsPrec _ VK_ATTACHMENT_LOAD_OP_CLEAR = showString "VK_ATTACHMENT_LOAD_OP_CLEAR"
  showsPrec _ VK_ATTACHMENT_LOAD_OP_DONT_CARE = showString "VK_ATTACHMENT_LOAD_OP_DONT_CARE"
  showsPrec p (VkAttachmentLoadOp x) = showParen (p >= 11) (showString "VkAttachmentLoadOp " . showsPrec 11 x)

instance Read VkAttachmentLoadOp where
  readPrec = parens ( choose [ ("VK_ATTACHMENT_LOAD_OP_LOAD", pure VK_ATTACHMENT_LOAD_OP_LOAD)
                             , ("VK_ATTACHMENT_LOAD_OP_CLEAR", pure VK_ATTACHMENT_LOAD_OP_CLEAR)
                             , ("VK_ATTACHMENT_LOAD_OP_DONT_CARE", pure VK_ATTACHMENT_LOAD_OP_DONT_CARE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAttachmentLoadOp")
                        v <- step readPrec
                        pure (VkAttachmentLoadOp v)
                        )
                    )


pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0

pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1

pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2

-- ** VkAttachmentStoreOp

newtype VkAttachmentStoreOp = VkAttachmentStoreOp Int32
  deriving (Eq, Storable)

instance Show VkAttachmentStoreOp where
  showsPrec _ VK_ATTACHMENT_STORE_OP_STORE = showString "VK_ATTACHMENT_STORE_OP_STORE"
  showsPrec _ VK_ATTACHMENT_STORE_OP_DONT_CARE = showString "VK_ATTACHMENT_STORE_OP_DONT_CARE"
  showsPrec p (VkAttachmentStoreOp x) = showParen (p >= 11) (showString "VkAttachmentStoreOp " . showsPrec 11 x)

instance Read VkAttachmentStoreOp where
  readPrec = parens ( choose [ ("VK_ATTACHMENT_STORE_OP_STORE", pure VK_ATTACHMENT_STORE_OP_STORE)
                             , ("VK_ATTACHMENT_STORE_OP_DONT_CARE", pure VK_ATTACHMENT_STORE_OP_DONT_CARE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAttachmentStoreOp")
                        v <- step readPrec
                        pure (VkAttachmentStoreOp v)
                        )
                    )


pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0

pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1

-- ** VkAccessFlags

newtype VkAccessFlags = VkAccessFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkAccessFlags where
  showsPrec _ VK_ACCESS_INDIRECT_COMMAND_READ_BIT = showString "VK_ACCESS_INDIRECT_COMMAND_READ_BIT"
  showsPrec _ VK_ACCESS_INDEX_READ_BIT = showString "VK_ACCESS_INDEX_READ_BIT"
  showsPrec _ VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = showString "VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT"
  showsPrec _ VK_ACCESS_UNIFORM_READ_BIT = showString "VK_ACCESS_UNIFORM_READ_BIT"
  showsPrec _ VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = showString "VK_ACCESS_INPUT_ATTACHMENT_READ_BIT"
  showsPrec _ VK_ACCESS_SHADER_READ_BIT = showString "VK_ACCESS_SHADER_READ_BIT"
  showsPrec _ VK_ACCESS_SHADER_WRITE_BIT = showString "VK_ACCESS_SHADER_WRITE_BIT"
  showsPrec _ VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = showString "VK_ACCESS_COLOR_ATTACHMENT_READ_BIT"
  showsPrec _ VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = showString "VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT"
  showsPrec _ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = showString "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT"
  showsPrec _ VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = showString "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT"
  showsPrec _ VK_ACCESS_TRANSFER_READ_BIT = showString "VK_ACCESS_TRANSFER_READ_BIT"
  showsPrec _ VK_ACCESS_TRANSFER_WRITE_BIT = showString "VK_ACCESS_TRANSFER_WRITE_BIT"
  showsPrec _ VK_ACCESS_HOST_READ_BIT = showString "VK_ACCESS_HOST_READ_BIT"
  showsPrec _ VK_ACCESS_HOST_WRITE_BIT = showString "VK_ACCESS_HOST_WRITE_BIT"
  showsPrec _ VK_ACCESS_MEMORY_READ_BIT = showString "VK_ACCESS_MEMORY_READ_BIT"
  showsPrec _ VK_ACCESS_MEMORY_WRITE_BIT = showString "VK_ACCESS_MEMORY_WRITE_BIT"
  
  showsPrec p (VkAccessFlags x) = showParen (p >= 11) (showString "VkAccessFlags " . showsPrec 11 x)

instance Read VkAccessFlags where
  readPrec = parens ( choose [ ("VK_ACCESS_INDIRECT_COMMAND_READ_BIT", pure VK_ACCESS_INDIRECT_COMMAND_READ_BIT)
                             , ("VK_ACCESS_INDEX_READ_BIT", pure VK_ACCESS_INDEX_READ_BIT)
                             , ("VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT", pure VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT)
                             , ("VK_ACCESS_UNIFORM_READ_BIT", pure VK_ACCESS_UNIFORM_READ_BIT)
                             , ("VK_ACCESS_INPUT_ATTACHMENT_READ_BIT", pure VK_ACCESS_INPUT_ATTACHMENT_READ_BIT)
                             , ("VK_ACCESS_SHADER_READ_BIT", pure VK_ACCESS_SHADER_READ_BIT)
                             , ("VK_ACCESS_SHADER_WRITE_BIT", pure VK_ACCESS_SHADER_WRITE_BIT)
                             , ("VK_ACCESS_COLOR_ATTACHMENT_READ_BIT", pure VK_ACCESS_COLOR_ATTACHMENT_READ_BIT)
                             , ("VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT", pure VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                             , ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT", pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT)
                             , ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT", pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)
                             , ("VK_ACCESS_TRANSFER_READ_BIT", pure VK_ACCESS_TRANSFER_READ_BIT)
                             , ("VK_ACCESS_TRANSFER_WRITE_BIT", pure VK_ACCESS_TRANSFER_WRITE_BIT)
                             , ("VK_ACCESS_HOST_READ_BIT", pure VK_ACCESS_HOST_READ_BIT)
                             , ("VK_ACCESS_HOST_WRITE_BIT", pure VK_ACCESS_HOST_WRITE_BIT)
                             , ("VK_ACCESS_MEMORY_READ_BIT", pure VK_ACCESS_MEMORY_READ_BIT)
                             , ("VK_ACCESS_MEMORY_WRITE_BIT", pure VK_ACCESS_MEMORY_WRITE_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAccessFlags")
                        v <- step readPrec
                        pure (VkAccessFlags v)
                        )
                    )

-- | Controls coherency of indirect command reads
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessFlags 0x1
-- | Controls coherency of index reads
pattern VK_ACCESS_INDEX_READ_BIT = VkAccessFlags 0x2
-- | Controls coherency of vertex attribute reads
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessFlags 0x4
-- | Controls coherency of uniform buffer reads
pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessFlags 0x8
-- | Controls coherency of input attachment reads
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessFlags 0x10
-- | Controls coherency of shader reads
pattern VK_ACCESS_SHADER_READ_BIT = VkAccessFlags 0x20
-- | Controls coherency of shader writes
pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessFlags 0x40
-- | Controls coherency of color attachment reads
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessFlags 0x80
-- | Controls coherency of color attachment writes
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessFlags 0x100
-- | Controls coherency of depth/stencil attachment reads
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = VkAccessFlags 0x200
-- | Controls coherency of depth/stencil attachment writes
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = VkAccessFlags 0x400
-- | Controls coherency of transfer reads
pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessFlags 0x800
-- | Controls coherency of transfer writes
pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessFlags 0x1000
-- | Controls coherency of host reads
pattern VK_ACCESS_HOST_READ_BIT = VkAccessFlags 0x2000
-- | Controls coherency of host writes
pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessFlags 0x4000
-- | Controls coherency of memory reads
pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessFlags 0x8000
-- | Controls coherency of memory writes
pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessFlags 0x10000


newtype RenderPass = RenderPass Word64
  deriving (Eq, Storable)

-- ** vkDestroyFramebuffer
foreign import ccall "vkDestroyFramebuffer" vkDestroyFramebuffer ::
  Device -> Framebuffer -> Ptr AllocationCallbacks -> IO ()


data AttachmentReference =
  AttachmentReference{ attachment :: Word32 
                     , layout :: VkImageLayout 
                     }
  deriving (Eq)

instance Storable AttachmentReference where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = AttachmentReference <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (attachment (poked :: AttachmentReference))
                *> poke (ptr `plusPtr` 4) (layout (poked :: AttachmentReference))


-- ** VkRenderPassCreateFlags
-- | Opaque flag
newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags VkFlags
  deriving (Eq, Storable)


data AttachmentDescription =
  AttachmentDescription{ flags :: VkAttachmentDescriptionFlags 
                       , format :: VkFormat 
                       , samples :: VkSampleCountFlags 
                       , loadOp :: VkAttachmentLoadOp 
                       , storeOp :: VkAttachmentStoreOp 
                       , stencilLoadOp :: VkAttachmentLoadOp 
                       , stencilStoreOp :: VkAttachmentStoreOp 
                       , initialLayout :: VkImageLayout 
                       , finalLayout :: VkImageLayout 
                       }
  deriving (Eq)

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
  SubpassDescription{ flags :: VkSubpassDescriptionFlags 
                    , pipelineBindPoint :: VkPipelineBindPoint 
                    , inputAttachmentCount :: Word32 
                    , pInputAttachments :: Ptr AttachmentReference 
                    , colorAttachmentCount :: Word32 
                    , pColorAttachments :: Ptr AttachmentReference 
                    , pResolveAttachments :: Ptr AttachmentReference 
                    , pDepthStencilAttachment :: Ptr AttachmentReference 
                    , preserveAttachmentCount :: Word32 
                    , pPreserveAttachments :: Ptr Word32 
                    }
  deriving (Eq)

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


-- ** vkCreateRenderPass
foreign import ccall "vkCreateRenderPass" vkCreateRenderPass ::
  Device ->
  Ptr RenderPassCreateInfo ->
    Ptr AllocationCallbacks -> Ptr RenderPass -> IO VkResult


data RenderPassCreateInfo =
  RenderPassCreateInfo{ sType :: VkStructureType 
                      , pNext :: Ptr Void 
                      , flags :: VkRenderPassCreateFlags 
                      , attachmentCount :: Word32 
                      , pAttachments :: Ptr AttachmentDescription 
                      , subpassCount :: Word32 
                      , pSubpasses :: Ptr SubpassDescription 
                      , dependencyCount :: Word32 
                      , pDependencies :: Ptr SubpassDependency 
                      }
  deriving (Eq)

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


-- ** VkFramebufferCreateFlags
-- | Opaque flag
newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags VkFlags
  deriving (Eq, Storable)

