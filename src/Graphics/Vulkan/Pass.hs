{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Pass where
import Graphics.Vulkan.Device( VkDevice(..)
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
import Graphics.Vulkan.ImageView( VkImageView(..)
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
  VkSubpassDependency{ vkSrcSubpass :: Word32 
                     , vkDstSubpass :: Word32 
                     , vkSrcStageMask :: VkPipelineStageFlags 
                     , vkDstStageMask :: VkPipelineStageFlags 
                     , vkSrcAccessMask :: VkAccessFlags 
                     , vkDstAccessMask :: VkAccessFlags 
                     , vkDependencyFlags :: VkDependencyFlags 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubpass (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 4) (vkDstSubpass (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 8) (vkSrcStageMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 12) (vkDstStageMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkSubpassDependency))
                *> poke (ptr `plusPtr` 24) (vkDependencyFlags (poked :: VkSubpassDependency))


-- ** VkSubpassDescriptionFlags
-- | Opaque flag
newtype VkSubpassDescriptionFlags = VkSubpassDescriptionFlags VkFlags
  deriving (Eq, Storable)

newtype VkFramebuffer = VkFramebuffer Word64
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
  VkDevice -> VkRenderPass -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateFramebuffer
foreign import ccall "vkCreateFramebuffer" vkCreateFramebuffer :: 
  VkDevice ->
  Ptr VkFramebufferCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkFramebuffer -> IO VkResult


data VkFramebufferCreateInfo =
  VkFramebufferCreateInfo{ vkSType :: VkStructureType 
                         , vkPNext :: Ptr Void 
                         , vkFlags :: VkFramebufferCreateFlags 
                         , vkRenderPass :: VkRenderPass 
                         , vkAttachmentCount :: Word32 
                         , vkPAttachments :: Ptr VkImageView 
                         , vkWidth :: Word32 
                         , vkHeight :: Word32 
                         , vkLayers :: Word32 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkRenderPass (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkAttachmentCount (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPAttachments (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkWidth (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 52) (vkHeight (poked :: VkFramebufferCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkLayers (poked :: VkFramebufferCreateInfo))


-- ** vkGetRenderAreaGranularity
foreign import ccall "vkGetRenderAreaGranularity" vkGetRenderAreaGranularity :: 
  VkDevice -> VkRenderPass -> Ptr VkExtent2D -> IO ()

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


newtype VkRenderPass = VkRenderPass Word64
  deriving (Eq, Storable)

-- ** vkDestroyFramebuffer
foreign import ccall "vkDestroyFramebuffer" vkDestroyFramebuffer :: 
  VkDevice -> VkFramebuffer -> Ptr VkAllocationCallbacks -> IO ()


data VkAttachmentReference =
  VkAttachmentReference{ vkAttachment :: Word32 
                       , vkLayout :: VkImageLayout 
                       }
  deriving (Eq)

instance Storable VkAttachmentReference where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkAttachmentReference <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAttachment (poked :: VkAttachmentReference))
                *> poke (ptr `plusPtr` 4) (vkLayout (poked :: VkAttachmentReference))


-- ** VkRenderPassCreateFlags
-- | Opaque flag
newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags VkFlags
  deriving (Eq, Storable)


data VkAttachmentDescription =
  VkAttachmentDescription{ vkFlags :: VkAttachmentDescriptionFlags 
                         , vkFormat :: VkFormat 
                         , vkSamples :: VkSampleCountFlagBits 
                         , vkLoadOp :: VkAttachmentLoadOp 
                         , vkStoreOp :: VkAttachmentStoreOp 
                         , vkStencilLoadOp :: VkAttachmentLoadOp 
                         , vkStencilStoreOp :: VkAttachmentStoreOp 
                         , vkInitialLayout :: VkImageLayout 
                         , vkFinalLayout :: VkImageLayout 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFlags (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 4) (vkFormat (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 8) (vkSamples (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 12) (vkLoadOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 16) (vkStoreOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 20) (vkStencilLoadOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 24) (vkStencilStoreOp (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 28) (vkInitialLayout (poked :: VkAttachmentDescription))
                *> poke (ptr `plusPtr` 32) (vkFinalLayout (poked :: VkAttachmentDescription))



data VkSubpassDescription =
  VkSubpassDescription{ vkFlags :: VkSubpassDescriptionFlags 
                      , vkPipelineBindPoint :: VkPipelineBindPoint 
                      , vkInputAttachmentCount :: Word32 
                      , vkPInputAttachments :: Ptr VkAttachmentReference 
                      , vkColorAttachmentCount :: Word32 
                      , vkPColorAttachments :: Ptr VkAttachmentReference 
                      , vkPResolveAttachments :: Ptr VkAttachmentReference 
                      , vkPDepthStencilAttachment :: Ptr VkAttachmentReference 
                      , vkPreserveAttachmentCount :: Word32 
                      , vkPPreserveAttachments :: Ptr Word32 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFlags (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 4) (vkPipelineBindPoint (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 8) (vkInputAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 16) (vkPInputAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 24) (vkColorAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 32) (vkPColorAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 40) (vkPResolveAttachments (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 48) (vkPDepthStencilAttachment (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 56) (vkPreserveAttachmentCount (poked :: VkSubpassDescription))
                *> poke (ptr `plusPtr` 64) (vkPPreserveAttachments (poked :: VkSubpassDescription))


-- ** vkCreateRenderPass
foreign import ccall "vkCreateRenderPass" vkCreateRenderPass :: 
  VkDevice ->
  Ptr VkRenderPassCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkRenderPass -> IO VkResult


data VkRenderPassCreateInfo =
  VkRenderPassCreateInfo{ vkSType :: VkStructureType 
                        , vkPNext :: Ptr Void 
                        , vkFlags :: VkRenderPassCreateFlags 
                        , vkAttachmentCount :: Word32 
                        , vkPAttachments :: Ptr VkAttachmentDescription 
                        , vkSubpassCount :: Word32 
                        , vkPSubpasses :: Ptr VkSubpassDescription 
                        , vkDependencyCount :: Word32 
                        , vkPDependencies :: Ptr VkSubpassDependency 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkAttachmentCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPAttachments (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkSubpassCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPSubpasses (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkDependencyCount (poked :: VkRenderPassCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPDependencies (poked :: VkRenderPassCreateInfo))


-- ** VkFramebufferCreateFlags
-- | Opaque flag
newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags VkFlags
  deriving (Eq, Storable)

