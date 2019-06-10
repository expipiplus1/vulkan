{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
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
  , VkAccessFlags
  , VkAttachmentDescription(..)
  , VkAttachmentDescriptionFlagBits(..)
  , pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT
  , VkAttachmentDescriptionFlags
  , VkAttachmentLoadOp(..)
  , pattern VK_ATTACHMENT_LOAD_OP_LOAD
  , pattern VK_ATTACHMENT_LOAD_OP_CLEAR
  , pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE
  , VkAttachmentReference(..)
  , VkAttachmentStoreOp(..)
  , pattern VK_ATTACHMENT_STORE_OP_STORE
  , pattern VK_ATTACHMENT_STORE_OP_DONT_CARE
  , VkDependencyFlagBits(..)
  , pattern VK_DEPENDENCY_BY_REGION_BIT
  , VkDependencyFlags
  , VkFramebuffer
  , VkFramebufferCreateFlags(..)
  , VkFramebufferCreateInfo(..)
  , VkPipelineBindPoint(..)
  , pattern VK_PIPELINE_BIND_POINT_GRAPHICS
  , pattern VK_PIPELINE_BIND_POINT_COMPUTE
  , VkRenderPassCreateFlags(..)
  , VkRenderPassCreateInfo(..)
  , VkSubpassDependency(..)
  , VkSubpassDescription(..)
  , VkSubpassDescriptionFlagBits(..)
  , VkSubpassDescriptionFlags
  , FN_vkCreateFramebuffer
  , PFN_vkCreateFramebuffer
  , vkCreateFramebuffer
  , FN_vkCreateRenderPass
  , PFN_vkCreateRenderPass
  , vkCreateRenderPass
  , FN_vkDestroyFramebuffer
  , PFN_vkDestroyFramebuffer
  , vkDestroyFramebuffer
  , FN_vkDestroyRenderPass
  , PFN_vkDestroyRenderPass
  , vkDestroyRenderPass
  , FN_vkGetRenderAreaGranularity
  , PFN_vkGetRenderAreaGranularity
  , vkGetRenderAreaGranularity
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
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
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


import Graphics.Vulkan.C.Core10.Core
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkSampleCountFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageView
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D(..)
  , VkRenderPass
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlags
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkAccessFlagBits

-- No documentation found for TopLevel "VkAccessFlagBits"
newtype VkAccessFlagBits = VkAccessFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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
  showsPrec _ (VkAccessFlagBits 0x40000000) = showString "VK_ACCESS_RESERVED_30_BIT_KHR"
  showsPrec _ (VkAccessFlagBits 0x80000000) = showString "VK_ACCESS_RESERVED_31_BIT_KHR"
  showsPrec _ (VkAccessFlagBits 0x10000000) = showString "VK_ACCESS_RESERVED_28_BIT_KHR"
  showsPrec _ (VkAccessFlagBits 0x20000000) = showString "VK_ACCESS_RESERVED_29_BIT_KHR"
  showsPrec _ (VkAccessFlagBits 0x02000000) = showString "VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x04000000) = showString "VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x08000000) = showString "VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x00100000) = showString "VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x00020000) = showString "VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX"
  showsPrec _ (VkAccessFlagBits 0x00040000) = showString "VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX"
  showsPrec _ (VkAccessFlagBits 0x00080000) = showString "VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT"
  showsPrec _ (VkAccessFlagBits 0x00800000) = showString "VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV"
  showsPrec _ (VkAccessFlagBits 0x00200000) = showString "VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV"
  showsPrec _ (VkAccessFlagBits 0x00400000) = showString "VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV"
  showsPrec _ (VkAccessFlagBits 0x01000000) = showString "VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT"
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
                               ("VK_ACCESS_RESERVED_30_BIT_KHR",                       pure (VkAccessFlagBits 0x40000000))
                             , ("VK_ACCESS_RESERVED_31_BIT_KHR",                       pure (VkAccessFlagBits 0x80000000))
                             , ("VK_ACCESS_RESERVED_28_BIT_KHR",                       pure (VkAccessFlagBits 0x10000000))
                             , ("VK_ACCESS_RESERVED_29_BIT_KHR",                       pure (VkAccessFlagBits 0x20000000))
                             , ("VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT",          pure (VkAccessFlagBits 0x02000000))
                             , ("VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT",   pure (VkAccessFlagBits 0x04000000))
                             , ("VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT",  pure (VkAccessFlagBits 0x08000000))
                             , ("VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT",        pure (VkAccessFlagBits 0x00100000))
                             , ("VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX",              pure (VkAccessFlagBits 0x00020000))
                             , ("VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX",             pure (VkAccessFlagBits 0x00040000))
                             , ("VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT", pure (VkAccessFlagBits 0x00080000))
                             , ("VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV",            pure (VkAccessFlagBits 0x00800000))
                             , ("VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV",        pure (VkAccessFlagBits 0x00200000))
                             , ("VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV",       pure (VkAccessFlagBits 0x00400000))
                             , ("VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT",         pure (VkAccessFlagBits 0x01000000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkAccessFlagBits")
                        v <- step readPrec
                        pure (VkAccessFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_INDIRECT_COMMAND_READ_BIT"
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_INDIRECT_COMMAND_READ_BIT = VkAccessFlagBits 0x00000001

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_INDEX_READ_BIT"
pattern VK_ACCESS_INDEX_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_INDEX_READ_BIT = VkAccessFlagBits 0x00000002

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT"
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT = VkAccessFlagBits 0x00000004

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_UNIFORM_READ_BIT"
pattern VK_ACCESS_UNIFORM_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_UNIFORM_READ_BIT = VkAccessFlagBits 0x00000008

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_INPUT_ATTACHMENT_READ_BIT"
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_INPUT_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x00000010

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_SHADER_READ_BIT"
pattern VK_ACCESS_SHADER_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_SHADER_READ_BIT = VkAccessFlagBits 0x00000020

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_SHADER_WRITE_BIT"
pattern VK_ACCESS_SHADER_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_SHADER_WRITE_BIT = VkAccessFlagBits 0x00000040

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COLOR_ATTACHMENT_READ_BIT"
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x00000080

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT"
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x00000100

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT"
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = VkAccessFlagBits 0x00000200

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT"
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = VkAccessFlagBits 0x00000400

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFER_READ_BIT"
pattern VK_ACCESS_TRANSFER_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFER_READ_BIT = VkAccessFlagBits 0x00000800

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_TRANSFER_WRITE_BIT"
pattern VK_ACCESS_TRANSFER_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_TRANSFER_WRITE_BIT = VkAccessFlagBits 0x00001000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_HOST_READ_BIT"
pattern VK_ACCESS_HOST_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_HOST_READ_BIT = VkAccessFlagBits 0x00002000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_HOST_WRITE_BIT"
pattern VK_ACCESS_HOST_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_HOST_WRITE_BIT = VkAccessFlagBits 0x00004000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_MEMORY_READ_BIT"
pattern VK_ACCESS_MEMORY_READ_BIT :: VkAccessFlagBits
pattern VK_ACCESS_MEMORY_READ_BIT = VkAccessFlagBits 0x00008000

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_MEMORY_WRITE_BIT"
pattern VK_ACCESS_MEMORY_WRITE_BIT :: VkAccessFlagBits
pattern VK_ACCESS_MEMORY_WRITE_BIT = VkAccessFlagBits 0x00010000

-- No documentation found for TopLevel "VkAccessFlags"
type VkAccessFlags = VkAccessFlagBits

-- No documentation found for TopLevel "VkAttachmentDescription"
data VkAttachmentDescription = VkAttachmentDescription
  { -- No documentation found for Nested "VkAttachmentDescription" "flags"
  vkFlags :: VkAttachmentDescriptionFlags
  , -- No documentation found for Nested "VkAttachmentDescription" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkAttachmentDescription" "samples"
  vkSamples :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkAttachmentDescription" "loadOp"
  vkLoadOp :: VkAttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription" "storeOp"
  vkStoreOp :: VkAttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription" "stencilLoadOp"
  vkStencilLoadOp :: VkAttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription" "stencilStoreOp"
  vkStencilStoreOp :: VkAttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription" "initialLayout"
  vkInitialLayout :: VkImageLayout
  , -- No documentation found for Nested "VkAttachmentDescription" "finalLayout"
  vkFinalLayout :: VkImageLayout
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

instance Zero VkAttachmentDescription where
  zero = VkAttachmentDescription zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

-- ** VkAttachmentDescriptionFlagBits

-- No documentation found for TopLevel "VkAttachmentDescriptionFlagBits"
newtype VkAttachmentDescriptionFlagBits = VkAttachmentDescriptionFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkAttachmentDescriptionFlagBits" "VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT"
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT :: VkAttachmentDescriptionFlagBits
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VkAttachmentDescriptionFlagBits 0x00000001

-- No documentation found for TopLevel "VkAttachmentDescriptionFlags"
type VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlagBits

-- ** VkAttachmentLoadOp

-- No documentation found for TopLevel "VkAttachmentLoadOp"
newtype VkAttachmentLoadOp = VkAttachmentLoadOp Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for Nested "VkAttachmentLoadOp" "VK_ATTACHMENT_LOAD_OP_LOAD"
pattern VK_ATTACHMENT_LOAD_OP_LOAD :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0

-- No documentation found for Nested "VkAttachmentLoadOp" "VK_ATTACHMENT_LOAD_OP_CLEAR"
pattern VK_ATTACHMENT_LOAD_OP_CLEAR :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1

-- No documentation found for Nested "VkAttachmentLoadOp" "VK_ATTACHMENT_LOAD_OP_DONT_CARE"
pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2

-- No documentation found for TopLevel "VkAttachmentReference"
data VkAttachmentReference = VkAttachmentReference
  { -- No documentation found for Nested "VkAttachmentReference" "attachment"
  vkAttachment :: Word32
  , -- No documentation found for Nested "VkAttachmentReference" "layout"
  vkLayout :: VkImageLayout
  }
  deriving (Eq, Show)

instance Storable VkAttachmentReference where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkAttachmentReference <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAttachment (poked :: VkAttachmentReference))
                *> poke (ptr `plusPtr` 4) (vkLayout (poked :: VkAttachmentReference))

instance Zero VkAttachmentReference where
  zero = VkAttachmentReference zero
                               zero

-- ** VkAttachmentStoreOp

-- No documentation found for TopLevel "VkAttachmentStoreOp"
newtype VkAttachmentStoreOp = VkAttachmentStoreOp Int32
  deriving (Eq, Ord, Storable, Zero)

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

-- No documentation found for Nested "VkAttachmentStoreOp" "VK_ATTACHMENT_STORE_OP_STORE"
pattern VK_ATTACHMENT_STORE_OP_STORE :: VkAttachmentStoreOp
pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0

-- No documentation found for Nested "VkAttachmentStoreOp" "VK_ATTACHMENT_STORE_OP_DONT_CARE"
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE :: VkAttachmentStoreOp
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1

-- ** VkDependencyFlagBits

-- No documentation found for TopLevel "VkDependencyFlagBits"
newtype VkDependencyFlagBits = VkDependencyFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkDependencyFlagBits" "VK_DEPENDENCY_BY_REGION_BIT"
pattern VK_DEPENDENCY_BY_REGION_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlagBits 0x00000001

-- No documentation found for TopLevel "VkDependencyFlags"
type VkDependencyFlags = VkDependencyFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkFramebuffer_T
-- No documentation found for TopLevel "VkFramebuffer"
type VkFramebuffer = Ptr VkFramebuffer_T

-- ** VkFramebufferCreateFlags

-- No documentation found for TopLevel "VkFramebufferCreateFlags"
newtype VkFramebufferCreateFlags = VkFramebufferCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- No documentation found for TopLevel "VkFramebufferCreateInfo"
data VkFramebufferCreateInfo = VkFramebufferCreateInfo
  { -- No documentation found for Nested "VkFramebufferCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "flags"
  vkFlags :: VkFramebufferCreateFlags
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "renderPass"
  vkRenderPass :: VkRenderPass
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "attachmentCount"
  vkAttachmentCount :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "pAttachments"
  vkPAttachments :: Ptr VkImageView
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "width"
  vkWidth :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "height"
  vkHeight :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "layers"
  vkLayers :: Word32
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

instance Zero VkFramebufferCreateInfo where
  zero = VkFramebufferCreateInfo VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

-- ** VkPipelineBindPoint

-- No documentation found for TopLevel "VkPipelineBindPoint"
newtype VkPipelineBindPoint = VkPipelineBindPoint Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkPipelineBindPoint where
  showsPrec _ VK_PIPELINE_BIND_POINT_GRAPHICS = showString "VK_PIPELINE_BIND_POINT_GRAPHICS"
  showsPrec _ VK_PIPELINE_BIND_POINT_COMPUTE = showString "VK_PIPELINE_BIND_POINT_COMPUTE"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkPipelineBindPoint 1000165000) = showString "VK_PIPELINE_BIND_POINT_RAY_TRACING_NV"
  showsPrec p (VkPipelineBindPoint x) = showParen (p >= 11) (showString "VkPipelineBindPoint " . showsPrec 11 x)

instance Read VkPipelineBindPoint where
  readPrec = parens ( choose [ ("VK_PIPELINE_BIND_POINT_GRAPHICS", pure VK_PIPELINE_BIND_POINT_GRAPHICS)
                             , ("VK_PIPELINE_BIND_POINT_COMPUTE",  pure VK_PIPELINE_BIND_POINT_COMPUTE)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_PIPELINE_BIND_POINT_RAY_TRACING_NV", pure (VkPipelineBindPoint 1000165000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineBindPoint")
                        v <- step readPrec
                        pure (VkPipelineBindPoint v)
                        )
                    )

-- No documentation found for Nested "VkPipelineBindPoint" "VK_PIPELINE_BIND_POINT_GRAPHICS"
pattern VK_PIPELINE_BIND_POINT_GRAPHICS :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

-- No documentation found for Nested "VkPipelineBindPoint" "VK_PIPELINE_BIND_POINT_COMPUTE"
pattern VK_PIPELINE_BIND_POINT_COMPUTE :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1

-- ** VkRenderPassCreateFlags

-- No documentation found for TopLevel "VkRenderPassCreateFlags"
newtype VkRenderPassCreateFlags = VkRenderPassCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- No documentation found for TopLevel "VkRenderPassCreateInfo"
data VkRenderPassCreateInfo = VkRenderPassCreateInfo
  { -- No documentation found for Nested "VkRenderPassCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "flags"
  vkFlags :: VkRenderPassCreateFlags
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "attachmentCount"
  vkAttachmentCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "pAttachments"
  vkPAttachments :: Ptr VkAttachmentDescription
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "subpassCount"
  vkSubpassCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "pSubpasses"
  vkPSubpasses :: Ptr VkSubpassDescription
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "dependencyCount"
  vkDependencyCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "pDependencies"
  vkPDependencies :: Ptr VkSubpassDependency
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

instance Zero VkRenderPassCreateInfo where
  zero = VkRenderPassCreateInfo VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero

-- No documentation found for TopLevel "VkSubpassDependency"
data VkSubpassDependency = VkSubpassDependency
  { -- No documentation found for Nested "VkSubpassDependency" "srcSubpass"
  vkSrcSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency" "dstSubpass"
  vkDstSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency" "srcStageMask"
  vkSrcStageMask :: VkPipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency" "dstStageMask"
  vkDstStageMask :: VkPipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency" "srcAccessMask"
  vkSrcAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkSubpassDependency" "dstAccessMask"
  vkDstAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkSubpassDependency" "dependencyFlags"
  vkDependencyFlags :: VkDependencyFlags
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

instance Zero VkSubpassDependency where
  zero = VkSubpassDependency zero
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero

-- No documentation found for TopLevel "VkSubpassDescription"
data VkSubpassDescription = VkSubpassDescription
  { -- No documentation found for Nested "VkSubpassDescription" "flags"
  vkFlags :: VkSubpassDescriptionFlags
  , -- No documentation found for Nested "VkSubpassDescription" "pipelineBindPoint"
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- No documentation found for Nested "VkSubpassDescription" "inputAttachmentCount"
  vkInputAttachmentCount :: Word32
  , -- No documentation found for Nested "VkSubpassDescription" "pInputAttachments"
  vkPInputAttachments :: Ptr VkAttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "colorAttachmentCount"
  vkColorAttachmentCount :: Word32
  , -- No documentation found for Nested "VkSubpassDescription" "pColorAttachments"
  vkPColorAttachments :: Ptr VkAttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "pResolveAttachments"
  vkPResolveAttachments :: Ptr VkAttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "pDepthStencilAttachment"
  vkPDepthStencilAttachment :: Ptr VkAttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "preserveAttachmentCount"
  vkPreserveAttachmentCount :: Word32
  , -- No documentation found for Nested "VkSubpassDescription" "pPreserveAttachments"
  vkPPreserveAttachments :: Ptr Word32
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

instance Zero VkSubpassDescription where
  zero = VkSubpassDescription zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero

-- ** VkSubpassDescriptionFlagBits

-- No documentation found for TopLevel "VkSubpassDescriptionFlagBits"
newtype VkSubpassDescriptionFlagBits = VkSubpassDescriptionFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkSubpassDescriptionFlagBits where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000001) = showString "VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX"
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000002) = showString "VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX"
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000004) = showString "VK_SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM"
  showsPrec _ (VkSubpassDescriptionFlagBits 0x00000008) = showString "VK_SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM"
  showsPrec p (VkSubpassDescriptionFlagBits x) = showParen (p >= 11) (showString "VkSubpassDescriptionFlagBits " . showsPrec 11 x)

instance Read VkSubpassDescriptionFlagBits where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX",      pure (VkSubpassDescriptionFlagBits 0x00000001))
                             , ("VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX", pure (VkSubpassDescriptionFlagBits 0x00000002))
                             , ("VK_SUBPASS_DESCRIPTION_RESERVED_2_BIT_QCOM",              pure (VkSubpassDescriptionFlagBits 0x00000004))
                             , ("VK_SUBPASS_DESCRIPTION_RESERVED_3_BIT_QCOM",              pure (VkSubpassDescriptionFlagBits 0x00000008))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSubpassDescriptionFlagBits")
                        v <- step readPrec
                        pure (VkSubpassDescriptionFlagBits v)
                        )
                    )



-- No documentation found for TopLevel "VkSubpassDescriptionFlags"
type VkSubpassDescriptionFlags = VkSubpassDescriptionFlagBits

-- No documentation found for TopLevel "vkCreateFramebuffer"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateFramebuffer" vkCreateFramebuffer :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult
#else
vkCreateFramebuffer :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult
vkCreateFramebuffer deviceCmds = mkVkCreateFramebuffer (pVkCreateFramebuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateFramebuffer
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult)
#endif

type FN_vkCreateFramebuffer = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult
type PFN_vkCreateFramebuffer = FunPtr FN_vkCreateFramebuffer

-- No documentation found for TopLevel "vkCreateRenderPass"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateRenderPass" vkCreateRenderPass :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
#else
vkCreateRenderPass :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
vkCreateRenderPass deviceCmds = mkVkCreateRenderPass (pVkCreateRenderPass deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRenderPass
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult)
#endif

type FN_vkCreateRenderPass = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
type PFN_vkCreateRenderPass = FunPtr FN_vkCreateRenderPass

-- No documentation found for TopLevel "vkDestroyFramebuffer"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyFramebuffer" vkDestroyFramebuffer :: ("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyFramebuffer :: DeviceCmds -> ("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyFramebuffer deviceCmds = mkVkDestroyFramebuffer (pVkDestroyFramebuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyFramebuffer
  :: FunPtr (("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyFramebuffer = ("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyFramebuffer = FunPtr FN_vkDestroyFramebuffer

-- No documentation found for TopLevel "vkDestroyRenderPass"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyRenderPass" vkDestroyRenderPass :: ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyRenderPass :: DeviceCmds -> ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyRenderPass deviceCmds = mkVkDestroyRenderPass (pVkDestroyRenderPass deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyRenderPass
  :: FunPtr (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyRenderPass = ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyRenderPass = FunPtr FN_vkDestroyRenderPass

-- No documentation found for TopLevel "vkGetRenderAreaGranularity"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetRenderAreaGranularity" vkGetRenderAreaGranularity :: ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()
#else
vkGetRenderAreaGranularity :: DeviceCmds -> ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()
vkGetRenderAreaGranularity deviceCmds = mkVkGetRenderAreaGranularity (pVkGetRenderAreaGranularity deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRenderAreaGranularity
  :: FunPtr (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()) -> (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ())
#endif

type FN_vkGetRenderAreaGranularity = ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()
type PFN_vkGetRenderAreaGranularity = FunPtr FN_vkGetRenderAreaGranularity
