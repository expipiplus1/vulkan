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
import Graphics.Vulkan.Pipeline( VkPipelineStageFlagBits(..)
                               , VkPipelineStageFlags(..)
                               , VkPipelineBindPoint(..)
                               )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
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
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Sampler( VkSampleCountFlagBits(..)
                              )
import Graphics.Vulkan.Image( VkImageLayout(..)
                            )
import Graphics.Vulkan.ImageView( ImageView(..)
                                )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkExtent2D(..)
                           , VkFlags(..)
                           , VkFormat(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize(..)
                      )


data VkSubpassDependency =
  VkSubpassDependency{ srcSubpass :: Word32 
                     , dstSubpass :: Word32 
                     , srcStageMask :: VkPipelineStageFlags 
                     , dstStageMask :: VkPipelineStageFlags 
                     , srcAccessMask :: VkAccessFlags 
                     , dstAccessMask :: VkAccessFlags 
                     , dependencyFlags :: VkDependencyFlags 
                     }
  deriving (Eq)

instance Storable VkSubpassDependency where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek ptr = VkSubpassDependency <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 12)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 20)
                                 <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (srcSubpass (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 4) (dstSubpass (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 8) (srcStageMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 12) (dstStageMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 16) (srcAccessMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 20) (dstAccessMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 24) (dependencyFlags (poked :: VkSubpassDependency))


-- ** VkSubpassDescriptionFlags
-- | Opaque flag
newtype VkSubpassDescriptionFlags = VkSubpassDescriptionFlags VkFlags
  deriving (Eq, Storable)

newtype Framebuffer = Framebuffer Word64
  deriving (Eq, Storable)

-- ** VkAttachmentDescriptionFlags

newtype VkAttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkAttachmentDescriptionFlagBits
type VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlagBits

instance Show VkAttachmentDescriptionFlagBits where
  showsPrec _ VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = showString "VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT"
  
  showsPrec p (VkAttachmentDescriptionFlagBits x) = showParen (p >= 11) (showString "VkAttachmentDescriptionFlagBits " . showsPrec 11 x)

instance Read VkAttachmentDescriptionFlagBits where
  readPrec = parens ( choose [ ("VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT", pure VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAttachmentDescriptionFlagBits")
                        v <- step readPrec
                        pure (VkAttachmentDescriptionFlagBits v)
                        )
                    )

-- | The attachment may alias physical memory of another attachment in the same render pass
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VkAttachmentDescriptionFlagBits 0x1


-- ** VkDependencyFlags

newtype VkDependencyFlagBits = VkDependencyFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkDependencyFlagBits
type VkDependencyFlags = VkDependencyFlagBits

instance Show VkDependencyFlagBits where
  showsPrec _ VK_DEPENDENCY_BY_REGION_BIT = showString "VK_DEPENDENCY_BY_REGION_BIT"
  
  showsPrec p (VkDependencyFlagBits x) = showParen (p >= 11) (showString "VkDependencyFlagBits " . showsPrec 11 x)

instance Read VkDependencyFlagBits where
  readPrec = parens ( choose [ ("VK_DEPENDENCY_BY_REGION_BIT", pure VK_DEPENDENCY_BY_REGION_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDependencyFlagBits")
                        v <- step readPrec
                        pure (VkDependencyFlagBits v)
                        )
                    )

-- | Dependency is per pixel region 
pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlagBits 0x1


-- ** vkDestroyRenderPass
foreign import ccall "vkDestroyRenderPass" vkDestroyRenderPass ::
  Device -> RenderPass -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateFramebuffer
foreign import ccall "vkCreateFramebuffer" vkCreateFramebuffer ::
  Device ->
  Ptr VkFramebufferCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr Framebuffer -> IO VkResult


data VkFramebufferCreateInfo =
  VkFramebufferCreateInfo{ sType :: VkStructureType 
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

instance Storable VkFramebufferCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkFramebufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 40)
                                     <*> peek (ptr `plusPtr` 48)
                                     <*> peek (ptr `plusPtr` 52)
                                     <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (renderPass (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (attachmentCount (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (pAttachments (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (width (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 52) (height (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 56) (layers (poked :: VkFramebufferCreateInfo))


-- ** vkGetRenderAreaGranularity
foreign import ccall "vkGetRenderAreaGranularity" vkGetRenderAreaGranularity ::
  Device -> RenderPass -> Ptr VkExtent2D -> IO ()

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

newtype VkAccessFlagBits = VkAccessFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkAccessFlagBits
type VkAccessFlags = VkAccessFlagBits

instance Show VkAccessFlagBits where
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
  
  showsPrec p (VkAccessFlagBits x) = showParen (p >= 11) (showString "VkAccessFlagBits " . showsPrec 11 x)

instance Read VkAccessFlagBits where
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
                        expectP (Ident "VkAccessFlagBits")
                        v <- step readPrec
                        pure (VkAccessFlagBits v)
                        )
                    )

-- | Controls coherency of indirect command reads
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessFlagBits 0x1
-- | Controls coherency of index reads
pattern VK_ACCESS_INDEX_READ_BIT = VkAccessFlagBits 0x2
-- | Controls coherency of vertex attribute reads
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessFlagBits 0x4
-- | Controls coherency of uniform buffer reads
pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessFlagBits 0x8
-- | Controls coherency of input attachment reads
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x10
-- | Controls coherency of shader reads
pattern VK_ACCESS_SHADER_READ_BIT = VkAccessFlagBits 0x20
-- | Controls coherency of shader writes
pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessFlagBits 0x40
-- | Controls coherency of color attachment reads
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x80
-- | Controls coherency of color attachment writes
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x100
-- | Controls coherency of depth/stencil attachment reads
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x200
-- | Controls coherency of depth/stencil attachment writes
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x400
-- | Controls coherency of transfer reads
pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessFlagBits 0x800
-- | Controls coherency of transfer writes
pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessFlagBits 0x1000
-- | Controls coherency of host reads
pattern VK_ACCESS_HOST_READ_BIT = VkAccessFlagBits 0x2000
-- | Controls coherency of host writes
pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessFlagBits 0x4000
-- | Controls coherency of memory reads
pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessFlagBits 0x8000
-- | Controls coherency of memory writes
pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessFlagBits 0x10000


newtype RenderPass = RenderPass Word64
  deriving (Eq, Storable)

-- ** vkDestroyFramebuffer
foreign import ccall "vkDestroyFramebuffer" vkDestroyFramebuffer ::
  Device -> Framebuffer -> Ptr VkAllocationCallbacks -> IO ()


data VkAttachmentReference =
  VkAttachmentReference{ attachment :: Word32 
                       , layout :: VkImageLayout 
                       }
  deriving (Eq)

instance Storable VkAttachmentReference where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkAttachmentReference <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (attachment (poked :: VkAttachmentReference))
                *> poke (ptr `plusPtr` 4) (layout (poked :: VkAttachmentReference))


-- ** VkRenderPassCreateFlags
-- | Opaque flag
newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags VkFlags
  deriving (Eq, Storable)


data VkAttachmentDescription =
  VkAttachmentDescription{ flags :: VkAttachmentDescriptionFlags 
                         , format :: VkFormat 
                         , samples :: VkSampleCountFlagBits 
                         , loadOp :: VkAttachmentLoadOp 
                         , storeOp :: VkAttachmentStoreOp 
                         , stencilLoadOp :: VkAttachmentLoadOp 
                         , stencilStoreOp :: VkAttachmentStoreOp 
                         , initialLayout :: VkImageLayout 
                         , finalLayout :: VkImageLayout 
                         }
  deriving (Eq)

instance Storable VkAttachmentDescription where
  sizeOf ~_ = 36
  alignment ~_ = 4
  peek ptr = VkAttachmentDescription <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 28)
                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (flags (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 4) (format (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 8) (samples (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 12) (loadOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 16) (storeOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 20) (stencilLoadOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 24) (stencilStoreOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 28) (initialLayout (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 32) (finalLayout (poked :: VkAttachmentDescription))



data VkSubpassDescription =
  VkSubpassDescription{ flags :: VkSubpassDescriptionFlags 
                      , pipelineBindPoint :: VkPipelineBindPoint 
                      , inputAttachmentCount :: Word32 
                      , pInputAttachments :: Ptr VkAttachmentReference 
                      , colorAttachmentCount :: Word32 
                      , pColorAttachments :: Ptr VkAttachmentReference 
                      , pResolveAttachments :: Ptr VkAttachmentReference 
                      , pDepthStencilAttachment :: Ptr VkAttachmentReference 
                      , preserveAttachmentCount :: Word32 
                      , pPreserveAttachments :: Ptr Word32 
                      }
  deriving (Eq)

instance Storable VkSubpassDescription where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkSubpassDescription <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
                                  <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (flags (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 4) (pipelineBindPoint (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 8) (inputAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 16) (pInputAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 24) (colorAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 32) (pColorAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 40) (pResolveAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 48) (pDepthStencilAttachment (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 56) (preserveAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 64) (pPreserveAttachments (poked :: VkSubpassDescription))


-- ** vkCreateRenderPass
foreign import ccall "vkCreateRenderPass" vkCreateRenderPass ::
  Device ->
  Ptr VkRenderPassCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr RenderPass -> IO VkResult


data VkRenderPassCreateInfo =
  VkRenderPassCreateInfo{ sType :: VkStructureType 
                        , pNext :: Ptr Void 
                        , flags :: VkRenderPassCreateFlags 
                        , attachmentCount :: Word32 
                        , pAttachments :: Ptr VkAttachmentDescription 
                        , subpassCount :: Word32 
                        , pSubpasses :: Ptr VkSubpassDescription 
                        , dependencyCount :: Word32 
                        , pDependencies :: Ptr VkSubpassDependency 
                        }
  deriving (Eq)

instance Storable VkRenderPassCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkRenderPassCreateInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 20)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
                                    <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 20) (attachmentCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 24) (pAttachments (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 32) (subpassCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 40) (pSubpasses (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 48) (dependencyCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 56) (pDependencies (poked :: VkRenderPassCreateInfo))


-- ** VkFramebufferCreateFlags
-- | Opaque flag
newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags VkFlags
  deriving (Eq, Storable)

