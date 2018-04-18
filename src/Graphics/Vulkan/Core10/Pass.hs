{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Pass
  ( VkAttachmentLoadOp(..)
  , pattern VK_ATTACHMENT_LOAD_OP_LOAD
  , pattern VK_ATTACHMENT_LOAD_OP_CLEAR
  , pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE
  , VkAttachmentStoreOp(..)
  , pattern VK_ATTACHMENT_STORE_OP_STORE
  , pattern VK_ATTACHMENT_STORE_OP_DONT_CARE
  , VkPipelineBindPoint(..)
  , pattern VK_PIPELINE_BIND_POINT_GRAPHICS
  , pattern VK_PIPELINE_BIND_POINT_COMPUTE
  , VkFramebufferCreateFlags(..)
  , VkRenderPassCreateFlags(..)
  , VkAccessFlagBits(..)
  , pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT
  , pattern VK_ACCESS_INDEX_READ_BIT
  , pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT
  , pattern VK_ACCESS_UNIFORM_READ_BIT
  , pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
  , pattern VK_ACCESS_SHADER_READ_BIT
  , pattern VK_ACCESS_SHADER_WRITE_BIT
  , pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
  , pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
  , pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
  , pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
  , pattern VK_ACCESS_TRANSFER_READ_BIT
  , pattern VK_ACCESS_TRANSFER_WRITE_BIT
  , pattern VK_ACCESS_HOST_READ_BIT
  , pattern VK_ACCESS_HOST_WRITE_BIT
  , pattern VK_ACCESS_MEMORY_READ_BIT
  , pattern VK_ACCESS_MEMORY_WRITE_BIT
  , VkSubpassDescriptionFlagBits(..)
  , VkAttachmentDescriptionFlagBits(..)
  , pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
  , VkDependencyFlagBits(..)
  , pattern VK_DEPENDENCY_BY_REGION_BIT
  , VkFramebuffer
  , vkCreateFramebuffer
  , vkDestroyFramebuffer
  , vkCreateRenderPass
  , vkDestroyRenderPass
  , vkGetRenderAreaGranularity
  , VkAttachmentDescription(..)
  , VkAttachmentReference(..)
  , VkSubpassDescription(..)
  , VkSubpassDependency(..)
  , VkRenderPassCreateInfo(..)
  , VkFramebufferCreateInfo(..)
  , VkAccessFlags
  , VkSubpassDescriptionFlags
  , VkAttachmentDescriptionFlags
  , VkDependencyFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkFormat(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkSampleCountFlagBits(..)
  , VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.Core10.ImageView
  ( VkImageView
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkExtent2D(..)
  , VkRenderPass
  )
import Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlags
  )


-- ** VkAttachmentLoadOp

-- | 
newtype VkAttachmentLoadOp = VkAttachmentLoadOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkAttachmentLoadOp where
  showsPrec _ VK_ATTACHMENT_LOAD_OP_LOAD = showString "VK_ATTACHMENT_LOAD_OP_LOAD"
  showsPrec _ VK_ATTACHMENT_LOAD_OP_CLEAR = showString "VK_ATTACHMENT_LOAD_OP_CLEAR"
  showsPrec _ VK_ATTACHMENT_LOAD_OP_DONT_CARE = showString "VK_ATTACHMENT_LOAD_OP_DONT_CARE"
  showsPrec p (VkAttachmentLoadOp x) = showParen (p >= 11) (showString "VkAttachmentLoadOp " . showsPrec 11 x)

instance Read VkAttachmentLoadOp where
  readPrec = parens ( choose [ ("VK_ATTACHMENT_LOAD_OP_LOAD",      pure VK_ATTACHMENT_LOAD_OP_LOAD)
                             , ("VK_ATTACHMENT_LOAD_OP_CLEAR",     pure VK_ATTACHMENT_LOAD_OP_CLEAR)
                             , ("VK_ATTACHMENT_LOAD_OP_DONT_CARE", pure VK_ATTACHMENT_LOAD_OP_DONT_CARE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAttachmentLoadOp")
                        v <- step readPrec
                        pure (VkAttachmentLoadOp v)
                        )
                    )

-- | 
pattern VK_ATTACHMENT_LOAD_OP_LOAD :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0

-- | 
pattern VK_ATTACHMENT_LOAD_OP_CLEAR :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1

-- | 
pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2
-- ** VkAttachmentStoreOp

-- | 
newtype VkAttachmentStoreOp = VkAttachmentStoreOp Int32
  deriving (Eq, Ord, Storable)

instance Show VkAttachmentStoreOp where
  showsPrec _ VK_ATTACHMENT_STORE_OP_STORE = showString "VK_ATTACHMENT_STORE_OP_STORE"
  showsPrec _ VK_ATTACHMENT_STORE_OP_DONT_CARE = showString "VK_ATTACHMENT_STORE_OP_DONT_CARE"
  showsPrec p (VkAttachmentStoreOp x) = showParen (p >= 11) (showString "VkAttachmentStoreOp " . showsPrec 11 x)

instance Read VkAttachmentStoreOp where
  readPrec = parens ( choose [ ("VK_ATTACHMENT_STORE_OP_STORE",     pure VK_ATTACHMENT_STORE_OP_STORE)
                             , ("VK_ATTACHMENT_STORE_OP_DONT_CARE", pure VK_ATTACHMENT_STORE_OP_DONT_CARE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAttachmentStoreOp")
                        v <- step readPrec
                        pure (VkAttachmentStoreOp v)
                        )
                    )

-- | 
pattern VK_ATTACHMENT_STORE_OP_STORE :: VkAttachmentStoreOp
pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0

-- | 
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE :: VkAttachmentStoreOp
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1
-- ** VkPipelineBindPoint

-- | 
newtype VkPipelineBindPoint = VkPipelineBindPoint Int32
  deriving (Eq, Ord, Storable)

instance Show VkPipelineBindPoint where
  showsPrec _ VK_PIPELINE_BIND_POINT_GRAPHICS = showString "VK_PIPELINE_BIND_POINT_GRAPHICS"
  showsPrec _ VK_PIPELINE_BIND_POINT_COMPUTE = showString "VK_PIPELINE_BIND_POINT_COMPUTE"
  showsPrec p (VkPipelineBindPoint x) = showParen (p >= 11) (showString "VkPipelineBindPoint " . showsPrec 11 x)

instance Read VkPipelineBindPoint where
  readPrec = parens ( choose [ ("VK_PIPELINE_BIND_POINT_GRAPHICS", pure VK_PIPELINE_BIND_POINT_GRAPHICS)
                             , ("VK_PIPELINE_BIND_POINT_COMPUTE",  pure VK_PIPELINE_BIND_POINT_COMPUTE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineBindPoint")
                        v <- step readPrec
                        pure (VkPipelineBindPoint v)
                        )
                    )

-- | 
pattern VK_PIPELINE_BIND_POINT_GRAPHICS :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

-- | 
pattern VK_PIPELINE_BIND_POINT_COMPUTE :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1
-- ** VkFramebufferCreateFlags

-- | 
newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkFramebufferCreateFlags where
  
  showsPrec p (VkFramebufferCreateFlags x) = showParen (p >= 11) (showString "VkFramebufferCreateFlags " . showsPrec 11 x)

instance Read VkFramebufferCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFramebufferCreateFlags")
                        v <- step readPrec
                        pure (VkFramebufferCreateFlags v)
                        )
                    )


-- ** VkRenderPassCreateFlags

-- | 
newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkRenderPassCreateFlags where
  
  showsPrec p (VkRenderPassCreateFlags x) = showParen (p >= 11) (showString "VkRenderPassCreateFlags " . showsPrec 11 x)

instance Read VkRenderPassCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkRenderPassCreateFlags")
                        v <- step readPrec
                        pure (VkRenderPassCreateFlags v)
                        )
                    )


-- ** VkAccessFlagBits

-- | 
newtype VkAccessFlagBits = VkAccessFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkAccessFlagBits 0x00020000) = showString "VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX"
  showsPrec _ (VkAccessFlagBits 0x00040000) = showString "VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX"
  showsPrec _ (VkAccessFlagBits 0x00080000) = showString "VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT"
  showsPrec p (VkAccessFlagBits x) = showParen (p >= 11) (showString "VkAccessFlagBits " . showsPrec 11 x)

instance Read VkAccessFlagBits where
  readPrec = parens ( choose [ ("VK_ACCESS_INDIRECT_COMMAND_READ_BIT",          pure VK_ACCESS_INDIRECT_COMMAND_READ_BIT)
                             , ("VK_ACCESS_INDEX_READ_BIT",                     pure VK_ACCESS_INDEX_READ_BIT)
                             , ("VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT",          pure VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT)
                             , ("VK_ACCESS_UNIFORM_READ_BIT",                   pure VK_ACCESS_UNIFORM_READ_BIT)
                             , ("VK_ACCESS_INPUT_ATTACHMENT_READ_BIT",          pure VK_ACCESS_INPUT_ATTACHMENT_READ_BIT)
                             , ("VK_ACCESS_SHADER_READ_BIT",                    pure VK_ACCESS_SHADER_READ_BIT)
                             , ("VK_ACCESS_SHADER_WRITE_BIT",                   pure VK_ACCESS_SHADER_WRITE_BIT)
                             , ("VK_ACCESS_COLOR_ATTACHMENT_READ_BIT",          pure VK_ACCESS_COLOR_ATTACHMENT_READ_BIT)
                             , ("VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT",         pure VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                             , ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT",  pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT)
                             , ("VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT", pure VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)
                             , ("VK_ACCESS_TRANSFER_READ_BIT",                  pure VK_ACCESS_TRANSFER_READ_BIT)
                             , ("VK_ACCESS_TRANSFER_WRITE_BIT",                 pure VK_ACCESS_TRANSFER_WRITE_BIT)
                             , ("VK_ACCESS_HOST_READ_BIT",                      pure VK_ACCESS_HOST_READ_BIT)
                             , ("VK_ACCESS_HOST_WRITE_BIT",                     pure VK_ACCESS_HOST_WRITE_BIT)
                             , ("VK_ACCESS_MEMORY_READ_BIT",                    pure VK_ACCESS_MEMORY_READ_BIT)
                             , ("VK_ACCESS_MEMORY_WRITE_BIT",                   pure VK_ACCESS_MEMORY_WRITE_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX",              pure (VkAccessFlagBits 0x00020000))
                             , ("VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX",             pure (VkAccessFlagBits 0x00040000))
                             , ("VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT", pure (VkAccessFlagBits 0x00080000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAccessFlagBits")
                        v <- step readPrec
                        pure (VkAccessFlagBits v)
                        )
                    )

-- | Controls coherency of indirect command reads
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessFlagBits 0x00000001

-- | Controls coherency of index reads
pattern VK_ACCESS_INDEX_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_INDEX_READ_BIT = VkAccessFlagBits 0x00000002

-- | Controls coherency of vertex attribute reads
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessFlagBits 0x00000004

-- | Controls coherency of uniform buffer reads
pattern VK_ACCESS_UNIFORM_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessFlagBits 0x00000008

-- | Controls coherency of input attachment reads
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x00000010

-- | Controls coherency of shader reads
pattern VK_ACCESS_SHADER_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_SHADER_READ_BIT = VkAccessFlagBits 0x00000020

-- | Controls coherency of shader writes
pattern VK_ACCESS_SHADER_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessFlagBits 0x00000040

-- | Controls coherency of color attachment reads
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x00000080

-- | Controls coherency of color attachment writes
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x00000100

-- | Controls coherency of depth/stencil attachment reads
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x00000200

-- | Controls coherency of depth/stencil attachment writes
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x00000400

-- | Controls coherency of transfer reads
pattern VK_ACCESS_TRANSFER_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessFlagBits 0x00000800

-- | Controls coherency of transfer writes
pattern VK_ACCESS_TRANSFER_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessFlagBits 0x00001000

-- | Controls coherency of host reads
pattern VK_ACCESS_HOST_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_HOST_READ_BIT = VkAccessFlagBits 0x00002000

-- | Controls coherency of host writes
pattern VK_ACCESS_HOST_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessFlagBits 0x00004000

-- | Controls coherency of memory reads
pattern VK_ACCESS_MEMORY_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessFlagBits 0x00008000

-- | Controls coherency of memory writes
pattern VK_ACCESS_MEMORY_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessFlagBits 0x00010000
-- ** VkSubpassDescriptionFlagBits

-- | 
newtype VkSubpassDescriptionFlagBits = VkSubpassDescriptionFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSubpassDescriptionFlagBits where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000001) = showString "VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX"
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000002) = showString "VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX"
  showsPrec p (VkSubpassDescriptionFlagBits x) = showParen (p >= 11) (showString "VkSubpassDescriptionFlagBits " . showsPrec 11 x)

instance Read VkSubpassDescriptionFlagBits where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX",      pure (VkSubpassDescriptionFlagBits 0x00000001))
                             , ("VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX", pure (VkSubpassDescriptionFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSubpassDescriptionFlagBits")
                        v <- step readPrec
                        pure (VkSubpassDescriptionFlagBits v)
                        )
                    )


-- ** VkAttachmentDescriptionFlagBits

-- | 
newtype VkAttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT :: VkAttachmentDescriptionFlagBits
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VkAttachmentDescriptionFlagBits 0x00000001
-- ** VkDependencyFlagBits

-- | 
newtype VkDependencyFlagBits = VkDependencyFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDependencyFlagBits where
  showsPrec _ VK_DEPENDENCY_BY_REGION_BIT = showString "VK_DEPENDENCY_BY_REGION_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDependencyFlagBits 0x00000004) = showString "VK_DEPENDENCY_DEVICE_GROUP_BIT"
  showsPrec _ (VkDependencyFlagBits 0x00000002) = showString "VK_DEPENDENCY_VIEW_LOCAL_BIT"
  showsPrec p (VkDependencyFlagBits x) = showParen (p >= 11) (showString "VkDependencyFlagBits " . showsPrec 11 x)

instance Read VkDependencyFlagBits where
  readPrec = parens ( choose [ ("VK_DEPENDENCY_BY_REGION_BIT", pure VK_DEPENDENCY_BY_REGION_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DEPENDENCY_DEVICE_GROUP_BIT", pure (VkDependencyFlagBits 0x00000004))
                             , ("VK_DEPENDENCY_VIEW_LOCAL_BIT",   pure (VkDependencyFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDependencyFlagBits")
                        v <- step readPrec
                        pure (VkDependencyFlagBits v)
                        )
                    )

-- | Dependency is per pixel region 
pattern VK_DEPENDENCY_BY_REGION_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlagBits 0x00000001
-- |
data VkFramebuffer_T
type VkFramebuffer = Ptr VkFramebuffer_T
-- | 
foreign import ccall "vkCreateFramebuffer" vkCreateFramebuffer :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult
-- | 
foreign import ccall "vkDestroyFramebuffer" vkDestroyFramebuffer :: ("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkCreateRenderPass" vkCreateRenderPass :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
-- | 
foreign import ccall "vkDestroyRenderPass" vkDestroyRenderPass :: ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkGetRenderAreaGranularity" vkGetRenderAreaGranularity :: ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()
-- | TODO: Struct comments
data VkAttachmentDescription = VkAttachmentDescription
  { vkFlags :: VkAttachmentDescriptionFlags
  , vkFormat :: VkFormat
  , vkSamples :: VkSampleCountFlagBits
  , vkLoadOp :: VkAttachmentLoadOp
  , vkStoreOp :: VkAttachmentStoreOp
  , vkStencilLoadOp :: VkAttachmentLoadOp
  , vkStencilStoreOp :: VkAttachmentStoreOp
  , vkInitialLayout :: VkImageLayout
  , vkFinalLayout :: VkImageLayout
  }
  deriving (Eq, Show)

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
-- | TODO: Struct comments
data VkAttachmentReference = VkAttachmentReference
  { vkAttachment :: Word32
  , vkLayout :: VkImageLayout
  }
  deriving (Eq, Show)

instance Storable VkAttachmentReference where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkAttachmentReference <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAttachment (poked :: VkAttachmentReference))
                *> poke (ptr `plusPtr` 4) (vkLayout (poked :: VkAttachmentReference))
-- | TODO: Struct comments
data VkSubpassDescription = VkSubpassDescription
  { vkFlags :: VkSubpassDescriptionFlags
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
  deriving (Eq, Show)

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
-- | TODO: Struct comments
data VkSubpassDependency = VkSubpassDependency
  { vkSrcSubpass :: Word32
  , vkDstSubpass :: Word32
  , vkSrcStageMask :: VkPipelineStageFlags
  , vkDstStageMask :: VkPipelineStageFlags
  , vkSrcAccessMask :: VkAccessFlags
  , vkDstAccessMask :: VkAccessFlags
  , vkDependencyFlags :: VkDependencyFlags
  }
  deriving (Eq, Show)

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
-- | TODO: Struct comments
data VkRenderPassCreateInfo = VkRenderPassCreateInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkRenderPassCreateFlags
  , vkAttachmentCount :: Word32
  , vkPAttachments :: Ptr VkAttachmentDescription
  , vkSubpassCount :: Word32
  , vkPSubpasses :: Ptr VkSubpassDescription
  , vkDependencyCount :: Word32
  , vkPDependencies :: Ptr VkSubpassDependency
  }
  deriving (Eq, Show)

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
-- | TODO: Struct comments
data VkFramebufferCreateInfo = VkFramebufferCreateInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkFlags :: VkFramebufferCreateFlags
  , vkRenderPass :: VkRenderPass
  , vkAttachmentCount :: Word32
  , vkPAttachments :: Ptr VkImageView
  , vkWidth :: Word32
  , vkHeight :: Word32
  , vkLayers :: Word32
  }
  deriving (Eq, Show)

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
type VkAccessFlags = VkAccessFlagBits
type VkSubpassDescriptionFlags = VkSubpassDescriptionFlagBits
type VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlagBits
type VkDependencyFlags = VkDependencyFlagBits
