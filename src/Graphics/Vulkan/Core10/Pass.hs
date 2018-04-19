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

-- | VkAttachmentLoadOp - Specify how contents of an attachment are treated
-- at the beginning of a subpass
--
-- = See Also
-- #_see_also#
--
-- 'VkAttachmentDescription'
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

-- | @VK_ATTACHMENT_LOAD_OP_LOAD@ specifies that the previous contents of the
-- image within the render area will be preserved. For attachments with a
-- depth\/stencil format, this uses the access type
-- @VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT@. For attachments with a
-- color format, this uses the access type
-- @VK_ACCESS_COLOR_ATTACHMENT_READ_BIT@.
pattern VK_ATTACHMENT_LOAD_OP_LOAD :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_LOAD = VkAttachmentLoadOp 0

-- | @VK_ATTACHMENT_LOAD_OP_CLEAR@ specifies that the contents within the
-- render area will be cleared to a uniform value, which is specified when
-- a render pass instance is begun. For attachments with a depth\/stencil
-- format, this uses the access type
-- @VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT@. For attachments with a
-- color format, this uses the access type
-- @VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT@.
pattern VK_ATTACHMENT_LOAD_OP_CLEAR :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_CLEAR = VkAttachmentLoadOp 1

-- | @VK_ATTACHMENT_LOAD_OP_DONT_CARE@ specifies that the previous contents
-- within the area need not be preserved; the contents of the attachment
-- will be undefined inside the render area. For attachments with a
-- depth\/stencil format, this uses the access type
-- @VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT@. For attachments with a
-- color format, this uses the access type
-- @VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT@.
pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE :: VkAttachmentLoadOp
pattern VK_ATTACHMENT_LOAD_OP_DONT_CARE = VkAttachmentLoadOp 2
-- ** VkAttachmentStoreOp

-- | VkAttachmentStoreOp - Specify how contents of an attachment are treated
-- at the end of a subpass
--
-- = See Also
-- #_see_also#
--
-- 'VkAttachmentDescription'
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

-- | @VK_ATTACHMENT_STORE_OP_STORE@ specifies the contents generated during
-- the render pass and within the render area are written to memory. For
-- attachments with a depth\/stencil format, this uses the access type
-- @VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT@. For attachments with a
-- color format, this uses the access type
-- @VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT@.
pattern VK_ATTACHMENT_STORE_OP_STORE :: VkAttachmentStoreOp
pattern VK_ATTACHMENT_STORE_OP_STORE = VkAttachmentStoreOp 0

-- | @VK_ATTACHMENT_STORE_OP_DONT_CARE@ specifies the contents within the
-- render area are not needed after rendering, and /may/ be discarded; the
-- contents of the attachment will be undefined inside the render area. For
-- attachments with a depth\/stencil format, this uses the access type
-- @VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT@. For attachments with a
-- color format, this uses the access type
-- @VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT@.
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE :: VkAttachmentStoreOp
pattern VK_ATTACHMENT_STORE_OP_DONT_CARE = VkAttachmentStoreOp 1
-- ** VkPipelineBindPoint

-- | VkPipelineBindPoint - Specify the bind point of a pipeline object to a
-- command buffer
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutCreateInfoNVX',
-- 'VkSubpassDescription',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindPipeline',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetKHR'
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

-- | @VK_PIPELINE_BIND_POINT_GRAPHICS@ specifies binding as a graphics
-- pipeline.
pattern VK_PIPELINE_BIND_POINT_GRAPHICS :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_GRAPHICS = VkPipelineBindPoint 0

-- | @VK_PIPELINE_BIND_POINT_COMPUTE@ specifies binding as a compute
-- pipeline.
pattern VK_PIPELINE_BIND_POINT_COMPUTE :: VkPipelineBindPoint
pattern VK_PIPELINE_BIND_POINT_COMPUTE = VkPipelineBindPoint 1
-- ** VkFramebufferCreateFlags

-- | VkFramebufferCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkFramebufferCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkFramebufferCreateInfo'
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

-- | VkRenderPassCreateFlags - Reserved for future use
--
-- = Description
-- #_description#
--
-- @VkRenderPassCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
-- #_see_also#
--
-- 'VkRenderPassCreateInfo'
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

-- | VkAccessFlagBits - Bitmask specifying memory access types that will
-- participate in a memory dependency
--
-- = Description
-- #_description#
--
-- -   @VK_ACCESS_INDIRECT_COMMAND_READ_BIT@ specifies read access to an
--     indirect command structure read as part of an indirect drawing or
--     dispatch command.
--
-- -   @VK_ACCESS_INDEX_READ_BIT@ specifies read access to an index buffer
--     as part of an indexed drawing command, bound by
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'.
--
-- -   @VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT@ specifies read access to a
--     vertex buffer as part of a drawing command, bound by
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers'.
--
-- -   @VK_ACCESS_UNIFORM_READ_BIT@ specifies read access to a
--     <{html_spec_relative}#descriptorsets-uniformbuffer uniform buffer>.
--
-- -   @VK_ACCESS_INPUT_ATTACHMENT_READ_BIT@ specifies read access to an
--     <{html_spec_relative}#renderpass input attachment> within a render
--     pass during fragment shading.
--
-- -   @VK_ACCESS_SHADER_READ_BIT@ specifies read access to a
--     <{html_spec_relative}#descriptorsets-storagebuffer storage buffer>,
--     <{html_spec_relative}#descriptorsets-uniformtexelbuffer uniform texel buffer>,
--     <{html_spec_relative}#descriptorsets-storagetexelbuffer storage texel buffer>,
--     <{html_spec_relative}#descriptorsets-sampledimage sampled image>, or
--     <{html_spec_relative}#descriptorsets-storageimage storage image>.
--
-- -   @VK_ACCESS_SHADER_WRITE_BIT@ specifies write access to a
--     <{html_spec_relative}#descriptorsets-storagebuffer storage buffer>,
--     <{html_spec_relative}#descriptorsets-storagetexelbuffer storage texel buffer>,
--     or <{html_spec_relative}#descriptorsets-storageimage storage image>.
--
-- -   @VK_ACCESS_COLOR_ATTACHMENT_READ_BIT@ specifies read access to a
--     <{html_spec_relative}#renderpass color attachment>, such as via
--     <{html_spec_relative}#framebuffer-blending blending>,
--     <{html_spec_relative}#framebuffer-logicop logic operations>, or via
--     certain
--     <{html_spec_relative}#renderpass-load-store-ops subpass load operations>.
--
-- -   @VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT@ specifies write access to a
--     <{html_spec_relative}#renderpass color or resolve attachment> during
--     a <{html_spec_relative}#renderpass render pass> or via certain
--     <{html_spec_relative}#renderpass-load-store-ops subpass load and store operations>.
--
-- -   @VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT@ specifies read access
--     to a <{html_spec_relative}#renderpass depth\/stencil attachment>,
--     via
--     <{html_spec_relative}#fragops-ds-state depth or stencil operations>
--     or via certain
--     <{html_spec_relative}#renderpass-load-store-ops subpass load operations>.
--
-- -   @VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT@ specifies write
--     access to a
--     <{html_spec_relative}#renderpass depth\/stencil attachment>, via
--     <{html_spec_relative}#fragops-ds-state depth or stencil operations>
--     or via certain
--     <{html_spec_relative}#renderpass-load-store-ops subpass load and store operations>.
--
-- -   @VK_ACCESS_TRANSFER_READ_BIT@ specifies read access to an image or
--     buffer in a <{html_spec_relative}#copies copy> operation.
--
-- -   @VK_ACCESS_TRANSFER_WRITE_BIT@ specifies write access to an image or
--     buffer in a <{html_spec_relative}#clears clear> or
--     <{html_spec_relative}#copies copy> operation.
--
-- -   @VK_ACCESS_HOST_READ_BIT@ specifies read access by a host operation.
--     Accesses of this type are not performed through a resource, but
--     directly on memory.
--
-- -   @VK_ACCESS_HOST_WRITE_BIT@ specifies write access by a host
--     operation. Accesses of this type are not performed through a
--     resource, but directly on memory.
--
-- -   @VK_ACCESS_MEMORY_READ_BIT@ specifies read access via non-specific
--     entities. These entities include the Vulkan device and host, but
--     /may/ also include entities external to the Vulkan device or
--     otherwise not part of the core Vulkan pipeline. When included in a
--     destination access mask, makes all available writes visible to all
--     future read accesses on entities known to the Vulkan device.
--
-- -   @VK_ACCESS_MEMORY_WRITE_BIT@ specifies write access via non-specific
--     entities. These entities include the Vulkan device and host, but
--     /may/ also include entities external to the Vulkan device or
--     otherwise not part of the core Vulkan pipeline. When included in a
--     source access mask, all writes that are performed by entities known
--     to the Vulkan device are made available. When included in a
--     destination access mask, makes all available writes visible to all
--     future write accesses on entities known to the Vulkan device.
--
-- Certain access types are only performed by a subset of pipeline stages.
-- Any synchronization command that takes both stage masks and access masks
-- uses both to define the
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scopes>
-- - only the specified access types performed by the specified stages are
-- included in the access scope. An application /must/ not specify an
-- access flag in a synchronization command if it does not include a
-- pipeline stage in the corresponding stage mask that is able to perform
-- accesses of that type. The following table lists, for each access flag,
-- which pipeline stages /can/ perform that type of access.
--
-- > +-----------------------------------+-----------------------------------+
-- > | Access flag                       | Supported pipeline stages         |
-- > +===================================+===================================+
-- > | @VK_ACCESS_INDIRECT_COMMAND_READ_ | @VK_PIPELINE_STAGE_DRAW_INDIRECT_ |
-- > | BIT@                              | BIT@                              |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_INDEX_READ_BIT@        | @VK_PIPELINE_STAGE_VERTEX_INPUT_B |
-- > |                                   | IT@                               |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_VERTEX_ATTRIBUTE_READ_ | @VK_PIPELINE_STAGE_VERTEX_INPUT_B |
-- > | BIT@                              | IT@                               |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_UNIFORM_READ_BIT@      | @VK_PIPELINE_STAGE_VERTEX_SHADER_ |
-- > |                                   | BIT@,                             |
-- > |                                   | @VK_PIPELINE_STAGE_TESSELLATION_C |
-- > |                                   | ONTROL_SHADER_BIT@,               |
-- > |                                   | @VK_PIPELINE_STAGE_TESSELLATION_E |
-- > |                                   | VALUATION_SHADER_BIT@,            |
-- > |                                   | @VK_PIPELINE_STAGE_GEOMETRY_SHADE |
-- > |                                   | R_BIT@,                           |
-- > |                                   | @VK_PIPELINE_STAGE_FRAGMENT_SHADE |
-- > |                                   | R_BIT@,                           |
-- > |                                   | or                                |
-- > |                                   | @VK_PIPELINE_STAGE_COMPUTE_SHADER |
-- > |                                   | _BIT@                             |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_INPUT_ATTACHMENT_READ_ | @VK_PIPELINE_STAGE_FRAGMENT_SHADE |
-- > | BIT@                              | R_BIT@                            |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_SHADER_READ_BIT@       | @VK_PIPELINE_STAGE_VERTEX_SHADER_ |
-- > |                                   | BIT@,                             |
-- > |                                   | @VK_PIPELINE_STAGE_TESSELLATION_C |
-- > |                                   | ONTROL_SHADER_BIT@,               |
-- > |                                   | @VK_PIPELINE_STAGE_TESSELLATION_E |
-- > |                                   | VALUATION_SHADER_BIT@,            |
-- > |                                   | @VK_PIPELINE_STAGE_GEOMETRY_SHADE |
-- > |                                   | R_BIT@,                           |
-- > |                                   | @VK_PIPELINE_STAGE_FRAGMENT_SHADE |
-- > |                                   | R_BIT@,                           |
-- > |                                   | or                                |
-- > |                                   | @VK_PIPELINE_STAGE_COMPUTE_SHADER |
-- > |                                   | _BIT@                             |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_SHADER_WRITE_BIT@      | @VK_PIPELINE_STAGE_VERTEX_SHADER_ |
-- > |                                   | BIT@,                             |
-- > |                                   | @VK_PIPELINE_STAGE_TESSELLATION_C |
-- > |                                   | ONTROL_SHADER_BIT@,               |
-- > |                                   | @VK_PIPELINE_STAGE_TESSELLATION_E |
-- > |                                   | VALUATION_SHADER_BIT@,            |
-- > |                                   | @VK_PIPELINE_STAGE_GEOMETRY_SHADE |
-- > |                                   | R_BIT@,                           |
-- > |                                   | @VK_PIPELINE_STAGE_FRAGMENT_SHADE |
-- > |                                   | R_BIT@,                           |
-- > |                                   | or                                |
-- > |                                   | @VK_PIPELINE_STAGE_COMPUTE_SHADER |
-- > |                                   | _BIT@                             |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_COLOR_ATTACHMENT_READ_ | @VK_PIPELINE_STAGE_COLOR_ATTACHME |
-- > | BIT@                              | NT_OUTPUT_BIT@                    |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_COLOR_ATTACHMENT_WRITE | @VK_PIPELINE_STAGE_COLOR_ATTACHME |
-- > | _BIT@                             | NT_OUTPUT_BIT@                    |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_DEPTH_STENCIL_ATTACHME | @VK_PIPELINE_STAGE_EARLY_FRAGMENT |
-- > | NT_READ_BIT@                      | _TESTS_BIT@,                      |
-- > |                                   | or                                |
-- > |                                   | @VK_PIPELINE_STAGE_LATE_FRAGMENT_ |
-- > |                                   | TESTS_BIT@                        |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_DEPTH_STENCIL_ATTACHME | @VK_PIPELINE_STAGE_EARLY_FRAGMENT |
-- > | NT_WRITE_BIT@                     | _TESTS_BIT@,                      |
-- > |                                   | or                                |
-- > |                                   | @VK_PIPELINE_STAGE_LATE_FRAGMENT_ |
-- > |                                   | TESTS_BIT@                        |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_TRANSFER_READ_BIT@     | @VK_PIPELINE_STAGE_TRANSFER_BIT@  |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_TRANSFER_WRITE_BIT@    | @VK_PIPELINE_STAGE_TRANSFER_BIT@  |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_HOST_READ_BIT@         | @VK_PIPELINE_STAGE_HOST_BIT@      |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_HOST_WRITE_BIT@        | @VK_PIPELINE_STAGE_HOST_BIT@      |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_MEMORY_READ_BIT@       | N\/A                              |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_ACCESS_MEMORY_WRITE_BIT@      | N\/A                              |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Supported access types
--
-- If a memory object does not have the
-- @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@ property, then
-- 'Graphics.Vulkan.Core10.Memory.vkFlushMappedMemoryRanges' /must/ be
-- called in order to guarantee that writes to the memory object from the
-- host are made visible to the @VK_ACCESS_HOST_WRITE_BIT@
-- <{html_spec_relative}#synchronization-access-types access type>, where
-- it /can/ be further made available to the device by
-- <{html_spec_relative}#synchronization synchronization commands>.
-- Similarly,
-- 'Graphics.Vulkan.Core10.Memory.vkInvalidateMappedMemoryRanges' /must/ be
-- called to guarantee that writes which are visible to the
-- @VK_ACCESS_HOST_READ_BIT@
-- <{html_spec_relative}#synchronization-access-types access type> are made
-- visible to host operations.
--
-- If the memory object does have the
-- @VK_MEMORY_PROPERTY_HOST_COHERENT_BIT@ property flag, writes to the
-- memory object from the host are automatically made visible to the
-- @VK_ACCESS_HOST_WRITE_BIT@
-- <{html_spec_relative}#synchronization-access-types access type>.
-- Similarly, writes made visible to the @VK_ACCESS_HOST_READ_BIT@
-- <{html_spec_relative}#synchronization-access-types access type> are
-- automatically made visible to the host.
--
-- __Note__
--
-- The 'Graphics.Vulkan.Core10.Queue.vkQueueSubmit' command
-- <{html_spec_relative}#synchronization-submission-host-writes automatically guarantees that host writes flushed to VK_ACCESS_HOST_WRITE_BIT are made available>
-- if they were flushed before the command executed, so in most cases an
-- explicit memory barrier is not needed for this case. In the few
-- circumstances where a submit does not occur between the host write and
-- the device read access, writes /can/ be made available by using an
-- explicit memory barrier.
--
-- = See Also
-- #_see_also#
--
-- 'VkAccessFlags'
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
-- ** VkSubpassDescriptionFlagBits

-- | VkSubpassDescriptionFlagBits - Bitmask specifying usage of a subpass
--
-- = Description
-- #_description#
--
-- __Note__
--
-- All bits for this type are defined by extensions, and none of those
-- extensions are enabled in this build of the specification.
--
-- = See Also
-- #_see_also#
--
-- 'VkSubpassDescriptionFlags'
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

-- | VkAttachmentDescriptionFlagBits - Bitmask specifying additional
-- properties of an attachment
--
-- = See Also
-- #_see_also#
--
-- 'VkAttachmentDescriptionFlags'
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

-- | @VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT@ specifies that the attachment
-- aliases the same device memory as other attachments.
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT :: VkAttachmentDescriptionFlagBits
pattern VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT = VkAttachmentDescriptionFlagBits 0x00000001
-- ** VkDependencyFlagBits

-- | VkDependencyFlagBits - Bitmask specifying how execution and memory
-- dependencies are formed
--
-- = See Also
-- #_see_also#
--
-- 'VkDependencyFlags'
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

-- | @VK_DEPENDENCY_BY_REGION_BIT@ specifies that dependencies will be
-- <{html_spec_relative}#synchronization-framebuffer-regions framebuffer-local>.
pattern VK_DEPENDENCY_BY_REGION_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_BY_REGION_BIT = VkDependencyFlagBits 0x00000001
-- | Dummy data to tag the 'Ptr' with
data VkFramebuffer_T
-- | VkFramebuffer - Opaque handle to a framebuffer object
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'vkCreateFramebuffer', 'vkDestroyFramebuffer'
type VkFramebuffer = Ptr VkFramebuffer_T
-- | vkCreateFramebuffer - Create a new framebuffer object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that creates the framebuffer.
--
-- -   @pCreateInfo@ points to a 'VkFramebufferCreateInfo' structure which
--     describes additional information about framebuffer creation.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pFramebuffer@ points to a @VkFramebuffer@ handle in which the
--     resulting framebuffer object is returned.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkFramebufferCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pFramebuffer@ /must/ be a valid pointer to a @VkFramebuffer@ handle
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkFramebuffer',
-- 'VkFramebufferCreateInfo'
foreign import ccall "vkCreateFramebuffer" vkCreateFramebuffer :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult
-- | vkDestroyFramebuffer - Destroy a framebuffer object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that destroys the framebuffer.
--
-- -   @framebuffer@ is the handle of the framebuffer to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @framebuffer@ /must/ have
--     completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @framebuffer@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @framebuffer@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @framebuffer@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @framebuffer@
--     /must/ be a valid @VkFramebuffer@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @framebuffer@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @framebuffer@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkFramebuffer'
foreign import ccall "vkDestroyFramebuffer" vkDestroyFramebuffer :: ("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkCreateRenderPass - Create a new render pass object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that creates the render pass.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkRenderPassCreateInfo' structure that describes the parameters of
--     the render pass.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- -   @pRenderPass@ points to a @VkRenderPass@ handle in which the
--     resulting render pass object is returned.
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkRenderPassCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pRenderPass@ /must/ be a valid pointer to a @VkRenderPass@ handle
--
-- == Return Codes
--
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRenderPass', 'VkRenderPassCreateInfo'
foreign import ccall "vkCreateRenderPass" vkCreateRenderPass :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
-- | vkDestroyRenderPass - Destroy a render pass object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that destroys the render pass.
--
-- -   @renderPass@ is the handle of the render pass to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <{html_spec_relative}#memory-allocation Memory Allocation> chapter.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @renderPass@ /must/ have
--     completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @renderPass@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @renderPass@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @renderPass@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @renderPass@
--     /must/ be a valid @VkRenderPass@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @renderPass@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @renderPass@ /must/ be externally synchronized
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRenderPass'
foreign import ccall "vkDestroyRenderPass" vkDestroyRenderPass :: ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkGetRenderAreaGranularity - Returns the granularity for optimal render
-- area
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that owns the render pass.
--
-- -   @renderPass@ is a handle to a render pass.
--
-- -   @pGranularity@ points to a
--     'Graphics.Vulkan.Core10.Pipeline.VkExtent2D' structure in which the
--     granularity is returned.
--
-- = Description
-- #_description#
--
-- The conditions leading to an optimal @renderArea@ are:
--
-- -   the @offset.x@ member in @renderArea@ is a multiple of the @width@
--     member of the returned 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D'
--     (the horizontal granularity).
--
-- -   the @offset.y@ member in @renderArea@ is a multiple of the @height@
--     of the returned 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D' (the
--     vertical granularity).
--
-- -   either the @offset.width@ member in @renderArea@ is a multiple of
--     the horizontal granularity or @offset.x@+@offset.width@ is equal to
--     the @width@ of the @framebuffer@ in the
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'.
--
-- -   either the @offset.height@ member in @renderArea@ is a multiple of
--     the vertical granularity or @offset.y@+@offset.height@ is equal to
--     the @height@ of the @framebuffer@ in the
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'.
--
-- Subpass dependencies are not affected by the render area, and apply to
-- the entire image subresources attached to the framebuffer as specified
-- in the description of
-- <{html_spec_relative}#renderpass-layout-transitions automatic layout transitions>.
-- Similarly, pipeline barriers are valid even if their effect extends
-- outside the render area.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @renderPass@ /must/ be a valid @VkRenderPass@ handle
--
-- -   @pGranularity@ /must/ be a valid pointer to a @VkExtent2D@ structure
--
-- -   @renderPass@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Pipeline.VkExtent2D',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRenderPass'
foreign import ccall "vkGetRenderAreaGranularity" vkGetRenderAreaGranularity :: ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()
-- | VkAttachmentDescription - Structure specifying an attachment description
--
-- = Description
-- #_description#
--
-- If the attachment uses a color format, then @loadOp@ and @storeOp@ are
-- used, and @stencilLoadOp@ and @stencilStoreOp@ are ignored. If the
-- format has depth and\/or stencil components, @loadOp@ and @storeOp@
-- apply only to the depth data, while @stencilLoadOp@ and @stencilStoreOp@
-- define how the stencil data is handled. @loadOp@ and @stencilLoadOp@
-- define the /load operations/ that execute as part of the first subpass
-- that uses the attachment. @storeOp@ and @stencilStoreOp@ define the
-- /store operations/ that execute as part of the last subpass that uses
-- the attachment.
--
-- The load operation for each sample in an attachment happens-before any
-- recorded command which accesses the sample in the first subpass where
-- the attachment is used. Load operations for attachments with a
-- depth\/stencil format execute in the
-- @VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT@ pipeline stage. Load
-- operations for attachments with a color format execute in the
-- @VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT@ pipeline stage.
--
-- The store operation for each sample in an attachment happens-after any
-- recorded command which accesses the sample in the last subpass where the
-- attachment is used. Store operations for attachments with a
-- depth\/stencil format execute in the
-- @VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT@ pipeline stage. Store
-- operations for attachments with a color format execute in the
-- @VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT@ pipeline stage.
--
-- If an attachment is not used by any subpass, then @loadOp@, @storeOp@,
-- @stencilStoreOp@, and @stencilLoadOp@ are ignored, and the attachment’s
-- memory contents will not be modified by execution of a render pass
-- instance.
--
-- During a render pass instance, input\/color attachments with color
-- formats that have a component size of 8, 16, or 32 bits /must/ be
-- represented in the attachment’s format throughout the instance.
-- Attachments with other floating- or fixed-point color formats, or with
-- depth components /may/ be represented in a format with a precision
-- higher than the attachment format, but /must/ be represented with the
-- same range. When such a component is loaded via the @loadOp@, it will be
-- converted into an implementation-dependent format used by the render
-- pass. Such components /must/ be converted from the render pass format,
-- to the format of the attachment, before they are resolved or stored at
-- the end of a render pass instance via @storeOp@. Conversions occur as
-- described in
-- <{html_spec_relative}#fundamentals-numerics Numeric Representation and Computation>
-- and
-- <{html_spec_relative}#fundamentals-fixedconv Fixed-Point Data Conversions>.
--
-- If @flags@ includes @VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT@, then the
-- attachment is treated as if it shares physical memory with another
-- attachment in the same render pass. This information limits the ability
-- of the implementation to reorder certain operations (like layout
-- transitions and the @loadOp@) such that it is not improperly reordered
-- against other uses of the same physical memory via a different
-- attachment. This is described in more detail below.
--
-- == Valid Usage
--
-- -   @finalLayout@ /must/ not be @VK_IMAGE_LAYOUT_UNDEFINED@ or
--     @VK_IMAGE_LAYOUT_PREINITIALIZED@
--
-- == Valid Usage (Implicit)
--
-- -   @flags@ /must/ be a valid combination of
--     'VkAttachmentDescriptionFlagBits' values
--
-- -   @format@ /must/ be a valid 'Graphics.Vulkan.Core10.Core.VkFormat'
--     value
--
-- -   @samples@ /must/ be a valid
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
--     value
--
-- -   @loadOp@ /must/ be a valid 'VkAttachmentLoadOp' value
--
-- -   @storeOp@ /must/ be a valid 'VkAttachmentStoreOp' value
--
-- -   @stencilLoadOp@ /must/ be a valid 'VkAttachmentLoadOp' value
--
-- -   @stencilStoreOp@ /must/ be a valid 'VkAttachmentStoreOp' value
--
-- -   @initialLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @finalLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- = See Also
-- #_see_also#
--
-- 'VkAttachmentDescriptionFlags', 'VkAttachmentLoadOp',
-- 'VkAttachmentStoreOp', 'Graphics.Vulkan.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout', 'VkRenderPassCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkSampleCountFlagBits'
data VkAttachmentDescription = VkAttachmentDescription
  { -- No documentation found for Nested "VkAttachmentDescription" "vkFlags"
  vkFlags :: VkAttachmentDescriptionFlags
  , -- No documentation found for Nested "VkAttachmentDescription" "vkFormat"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkAttachmentDescription" "vkSamples"
  vkSamples :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkAttachmentDescription" "vkLoadOp"
  vkLoadOp :: VkAttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription" "vkStoreOp"
  vkStoreOp :: VkAttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription" "vkStencilLoadOp"
  vkStencilLoadOp :: VkAttachmentLoadOp
  , -- No documentation found for Nested "VkAttachmentDescription" "vkStencilStoreOp"
  vkStencilStoreOp :: VkAttachmentStoreOp
  , -- No documentation found for Nested "VkAttachmentDescription" "vkInitialLayout"
  vkInitialLayout :: VkImageLayout
  , -- No documentation found for Nested "VkAttachmentDescription" "vkFinalLayout"
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
-- | VkAttachmentReference - Structure specifying an attachment reference
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @layout@ /must/ not be @VK_IMAGE_LAYOUT_UNDEFINED@ or
--     @VK_IMAGE_LAYOUT_PREINITIALIZED@
--
-- == Valid Usage (Implicit)
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout', 'VkSubpassDescription'
data VkAttachmentReference = VkAttachmentReference
  { -- No documentation found for Nested "VkAttachmentReference" "vkAttachment"
  vkAttachment :: Word32
  , -- No documentation found for Nested "VkAttachmentReference" "vkLayout"
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
-- | VkSubpassDescription - Structure specifying a subpass description
--
-- = Description
-- #_description#
--
-- The contents of an attachment within the render area become undefined at
-- the start of a subpass __S__ if all of the following conditions are
-- true:
--
-- -   The attachment is used as a color, depth\/stencil, or resolve
--     attachment in any subpass in the render pass.
--
-- -   There is a subpass __S1__ that uses or preserves the attachment, and
--     a subpass dependency from __S1__ to __S__.
--
-- -   The attachment is not used or preserved in subpass __S__.
--
-- Once the contents of an attachment become undefined in subpass __S__,
-- they remain undefined for subpasses in subpass dependency chains
-- starting with subpass __S__ until they are written again. However, they
-- remain valid for subpasses in other subpass dependency chains starting
-- with subpass __S1__ if those subpasses use or preserve the attachment.
--
-- == Valid Usage
--
-- -   @pipelineBindPoint@ /must/ be @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   @colorAttachmentCount@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxColorAttachments@
--
-- -   If the first use of an attachment in this render pass is as an input
--     attachment, and the attachment is not also used as a color or
--     depth\/stencil attachment in the same subpass, then @loadOp@ /must/
--     not be @VK_ATTACHMENT_LOAD_OP_CLEAR@
--
-- -   If @pResolveAttachments@ is not @NULL@, for each resolve attachment
--     that does not have the value @VK_ATTACHMENT_UNUSED@, the
--     corresponding color attachment /must/ not have the value
--     @VK_ATTACHMENT_UNUSED@
--
-- -   If @pResolveAttachments@ is not @NULL@, the sample count of each
--     element of @pColorAttachments@ /must/ be anything other than
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   Each element of @pResolveAttachments@ /must/ have a sample count of
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   Each element of @pResolveAttachments@ /must/ have the same
--     'Graphics.Vulkan.Core10.Core.VkFormat' as its corresponding color
--     attachment
--
-- -   All attachments in @pColorAttachments@ that are not
--     @VK_ATTACHMENT_UNUSED@ /must/ have the same sample count
--
-- -   If @pDepthStencilAttachment@ is not @VK_ATTACHMENT_UNUSED@ and any
--     attachments in @pColorAttachments@ are not @VK_ATTACHMENT_UNUSED@,
--     they /must/ have the same sample count
--
-- -   If any input attachments are @VK_ATTACHMENT_UNUSED@, then any
--     pipelines bound during the subpass /must/ not access those input
--     attachments from the fragment shader
--
-- -   The @attachment@ member of each element of @pPreserveAttachments@
--     /must/ not be @VK_ATTACHMENT_UNUSED@
--
-- -   Each element of @pPreserveAttachments@ /must/ not also be an element
--     of any other member of the subpass description
--
-- -   If any attachment is used as both an input attachment and a color or
--     depth\/stencil attachment, then each use /must/ use the same
--     @layout@
--
-- == Valid Usage (Implicit)
--
-- -   @flags@ /must/ be a valid combination of
--     'VkSubpassDescriptionFlagBits' values
--
-- -   @pipelineBindPoint@ /must/ be a valid 'VkPipelineBindPoint' value
--
-- -   If @inputAttachmentCount@ is not @0@, @pInputAttachments@ /must/ be
--     a valid pointer to an array of @inputAttachmentCount@ valid
--     @VkAttachmentReference@ structures
--
-- -   If @colorAttachmentCount@ is not @0@, @pColorAttachments@ /must/ be
--     a valid pointer to an array of @colorAttachmentCount@ valid
--     @VkAttachmentReference@ structures
--
-- -   If @colorAttachmentCount@ is not @0@, and @pResolveAttachments@ is
--     not @NULL@, @pResolveAttachments@ /must/ be a valid pointer to an
--     array of @colorAttachmentCount@ valid @VkAttachmentReference@
--     structures
--
-- -   If @pDepthStencilAttachment@ is not @NULL@,
--     @pDepthStencilAttachment@ /must/ be a valid pointer to a valid
--     @VkAttachmentReference@ structure
--
-- -   If @preserveAttachmentCount@ is not @0@, @pPreserveAttachments@
--     /must/ be a valid pointer to an array of @preserveAttachmentCount@
--     @uint32_t@ values
--
-- = See Also
-- #_see_also#
--
-- 'VkAttachmentReference', 'VkPipelineBindPoint',
-- 'VkRenderPassCreateInfo', 'VkSubpassDescriptionFlags'
data VkSubpassDescription = VkSubpassDescription
  { -- No documentation found for Nested "VkSubpassDescription" "vkFlags"
  vkFlags :: VkSubpassDescriptionFlags
  , -- No documentation found for Nested "VkSubpassDescription" "vkPipelineBindPoint"
  vkPipelineBindPoint :: VkPipelineBindPoint
  , -- No documentation found for Nested "VkSubpassDescription" "vkInputAttachmentCount"
  vkInputAttachmentCount :: Word32
  , -- No documentation found for Nested "VkSubpassDescription" "vkPInputAttachments"
  vkPInputAttachments :: Ptr VkAttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "vkColorAttachmentCount"
  vkColorAttachmentCount :: Word32
  , -- No documentation found for Nested "VkSubpassDescription" "vkPColorAttachments"
  vkPColorAttachments :: Ptr VkAttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "vkPResolveAttachments"
  vkPResolveAttachments :: Ptr VkAttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "vkPDepthStencilAttachment"
  vkPDepthStencilAttachment :: Ptr VkAttachmentReference
  , -- No documentation found for Nested "VkSubpassDescription" "vkPreserveAttachmentCount"
  vkPreserveAttachmentCount :: Word32
  , -- No documentation found for Nested "VkSubpassDescription" "vkPPreserveAttachments"
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
-- | VkSubpassDependency - Structure specifying a subpass dependency
--
-- = Description
-- #_description#
--
-- If @srcSubpass@ is equal to @dstSubpass@ then the 'VkSubpassDependency'
-- describes a
-- <{html_spec_relative}#synchronization-pipeline-barriers-subpass-self-dependencies subpass self-dependency>,
-- and only constrains the pipeline barriers allowed within a subpass
-- instance. Otherwise, when a render pass instance which includes a
-- subpass dependency is submitted to a queue, it defines a memory
-- dependency between the subpasses identified by @srcSubpass@ and
-- @dstSubpass@.
--
-- If @srcSubpass@ is equal to @VK_SUBPASS_EXTERNAL@, the first
-- <{html_spec_relative}#synchronization-dependencies-scopes synchronization scope>
-- includes commands that occur earlier in
-- <{html_spec_relative}#synchronization-submission-order submission order>
-- than the
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBeginRenderPass' used
-- to begin the render pass instance. Otherwise, the first set of commands
-- includes all commands submitted as part of the subpass instance
-- identified by @srcSubpass@ and any load, store or multisample resolve
-- operations on attachments used in @srcSubpass@. In either case, the
-- first synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@.
--
-- If @dstSubpass@ is equal to @VK_SUBPASS_EXTERNAL@, the second
-- <{html_spec_relative}#synchronization-dependencies-scopes synchronization scope>
-- includes commands that occur later in
-- <{html_spec_relative}#synchronization-submission-order submission order>
-- than the
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdEndRenderPass' used
-- to end the render pass instance. Otherwise, the second set of commands
-- includes all commands submitted as part of the subpass instance
-- identified by @dstSubpass@ and any load, store or multisample resolve
-- operations on attachments used in @dstSubpass@. In either case, the
-- second synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. It is also limited to access types in the
-- <{html_spec_relative}#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@.
--
-- The second
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. It is also limited to access types in the
-- <{html_spec_relative}#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@.
--
-- The
-- <{html_spec_relative}#synchronization-dependencies-available-and-visible availability and visibility operations>
-- defined by a subpass dependency affect the execution of
-- <{html_spec_relative}#renderpass-layout-transitions image layout transitions>
-- within the render pass.
--
-- __Note__
--
-- For non-attachment resources, the memory dependency expressed by subpass
-- dependency is nearly identical to that of a
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkMemoryBarrier' (with
-- matching @srcAccessMask@\/@dstAccessMask@ parameters) submitted as a
-- part of a
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
-- (with matching @srcStageMask@\/@dstStageMask@ parameters). The only
-- difference being that its scopes are limited to the identified subpasses
-- rather than potentially affecting everything before and after.
--
-- For attachments however, subpass dependencies work more like an
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkImageMemoryBarrier'
-- defined similarly to the
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkMemoryBarrier' above,
-- the queue family indices set to @VK_QUEUE_FAMILY_IGNORED@, and layouts
-- as follows:
--
-- -   The equivalent to @oldLayout@ is the attachment’s layout according
--     to the subpass description for @srcSubpass@.
--
-- -   The equivalent to @newLayout@ is the attachment’s layout according
--     to the subpass description for @dstSubpass@.
--
-- == Valid Usage
--
-- -   If @srcSubpass@ is not @VK_SUBPASS_EXTERNAL@, @srcStageMask@ /must/
--     not include @VK_PIPELINE_STAGE_HOST_BIT@
--
-- -   If @dstSubpass@ is not @VK_SUBPASS_EXTERNAL@, @dstStageMask@ /must/
--     not include @VK_PIPELINE_STAGE_HOST_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   @srcSubpass@ /must/ be less than or equal to @dstSubpass@, unless
--     one of them is @VK_SUBPASS_EXTERNAL@, to avoid cyclic dependencies
--     and ensure a valid execution order
--
-- -   @srcSubpass@ and @dstSubpass@ /must/ not both be equal to
--     @VK_SUBPASS_EXTERNAL@
--
-- -   If @srcSubpass@ is equal to @dstSubpass@, @srcStageMask@ and
--     @dstStageMask@ /must/ only contain one of
--     @VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT@,
--     @VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT@,
--     @VK_PIPELINE_STAGE_VERTEX_INPUT_BIT@,
--     @VK_PIPELINE_STAGE_VERTEX_SHADER_BIT@,
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@,
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@,
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@,
--     @VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT@,
--     @VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT@,
--     @VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT@,
--     @VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT@,
--     @VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT@, or
--     @VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT@
--
-- -   If @srcSubpass@ is equal to @dstSubpass@ and not all of the stages
--     in @srcStageMask@ and @dstStageMask@ are
--     <{html_spec_relative}#synchronization-framebuffer-regions framebuffer-space stages>,
--     the
--     <{html_spec_relative}#synchronization-pipeline-stages-order logically latest>
--     pipeline stage in @srcStageMask@ /must/ be
--     <{html_spec_relative}#synchronization-pipeline-stages-order logically earlier>
--     than or equal to the
--     <{html_spec_relative}#synchronization-pipeline-stages-order logically earliest>
--     pipeline stage in @dstStageMask@
--
-- -   Any access flag included in @srcAccessMask@ /must/ be supported by
--     one of the pipeline stages in @srcStageMask@, as specified in the
--     <{html_spec_relative}#synchronization-access-types-supported table of supported access types>.
--
-- -   Any access flag included in @dstAccessMask@ /must/ be supported by
--     one of the pipeline stages in @dstStageMask@, as specified in the
--     <{html_spec_relative}#synchronization-access-types-supported table of supported access types>.
--
-- == Valid Usage (Implicit)
--
-- -   @srcStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @srcStageMask@ /must/ not be @0@
--
-- -   @dstStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @dstStageMask@ /must/ not be @0@
--
-- -   @srcAccessMask@ /must/ be a valid combination of 'VkAccessFlagBits'
--     values
--
-- -   @dstAccessMask@ /must/ be a valid combination of 'VkAccessFlagBits'
--     values
--
-- -   @dependencyFlags@ /must/ be a valid combination of
--     'VkDependencyFlagBits' values
--
-- = See Also
-- #_see_also#
--
-- 'VkAccessFlags', 'VkDependencyFlags',
-- 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlags',
-- 'VkRenderPassCreateInfo'
data VkSubpassDependency = VkSubpassDependency
  { -- No documentation found for Nested "VkSubpassDependency" "vkSrcSubpass"
  vkSrcSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency" "vkDstSubpass"
  vkDstSubpass :: Word32
  , -- No documentation found for Nested "VkSubpassDependency" "vkSrcStageMask"
  vkSrcStageMask :: VkPipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency" "vkDstStageMask"
  vkDstStageMask :: VkPipelineStageFlags
  , -- No documentation found for Nested "VkSubpassDependency" "vkSrcAccessMask"
  vkSrcAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkSubpassDependency" "vkDstAccessMask"
  vkDstAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkSubpassDependency" "vkDependencyFlags"
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
-- | VkRenderPassCreateInfo - Structure specifying parameters of a newly
-- created render pass
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   If any two subpasses operate on attachments with overlapping ranges
--     of the same @VkDeviceMemory@ object, and at least one subpass writes
--     to that area of @VkDeviceMemory@, a subpass dependency /must/ be
--     included (either directly or via some intermediate subpasses)
--     between them
--
-- -   If the @attachment@ member of any element of @pInputAttachments@,
--     @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@, or the attachment indexed by any element
--     of @pPreserveAttachments@ in any element of @pSubpasses@ is bound to
--     a range of a @VkDeviceMemory@ object that overlaps with any other
--     attachment in any subpass (including the same subpass), the
--     @VkAttachmentDescription@ structures describing them /must/ include
--     @VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT@ in @flags@
--
-- -   If the @attachment@ member of any element of @pInputAttachments@,
--     @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@, or any element of @pPreserveAttachments@
--     in any element of @pSubpasses@ is not @VK_ATTACHMENT_UNUSED@, it
--     /must/ be less than @attachmentCount@
--
-- -   The value of each element of the @pPreserveAttachments@ member in
--     each element of @pSubpasses@ /must/ not be @VK_ATTACHMENT_UNUSED@
--
-- -   For any member of @pAttachments@ with a @loadOp@ equal to
--     @VK_ATTACHMENT_LOAD_OP_CLEAR@, the first use of that attachment
--     /must/ not specify a @layout@ equal to
--     @VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@.
--
-- -   For any element of @pDependencies@, if the @srcSubpass@ is not
--     @VK_SUBPASS_EXTERNAL@, all stage flags included in the
--     @srcStageMask@ member of that dependency /must/ be a pipeline stage
--     supported by the
--     <{html_spec_relative}#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the source subpass.
--
-- -   For any element of @pDependencies@, if the @dstSubpass@ is not
--     @VK_SUBPASS_EXTERNAL@, all stage flags included in the
--     @dstStageMask@ member of that dependency /must/ be a pipeline stage
--     supported by the
--     <{html_spec_relative}#synchronization-pipeline-stages-types pipeline>
--     identified by the @pipelineBindPoint@ member of the source subpass.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo'
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- -   If @attachmentCount@ is not @0@, @pAttachments@ /must/ be a valid
--     pointer to an array of @attachmentCount@ valid
--     @VkAttachmentDescription@ structures
--
-- -   @pSubpasses@ /must/ be a valid pointer to an array of @subpassCount@
--     valid @VkSubpassDescription@ structures
--
-- -   If @dependencyCount@ is not @0@, @pDependencies@ /must/ be a valid
--     pointer to an array of @dependencyCount@ valid @VkSubpassDependency@
--     structures
--
-- -   @subpassCount@ /must/ be greater than @0@
--
-- = See Also
-- #_see_also#
--
-- 'VkAttachmentDescription', 'VkRenderPassCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'VkSubpassDependency',
-- 'VkSubpassDescription', 'vkCreateRenderPass'
data VkRenderPassCreateInfo = VkRenderPassCreateInfo
  { -- No documentation found for Nested "VkRenderPassCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "vkFlags"
  vkFlags :: VkRenderPassCreateFlags
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "vkAttachmentCount"
  vkAttachmentCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "vkPAttachments"
  vkPAttachments :: Ptr VkAttachmentDescription
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "vkSubpassCount"
  vkSubpassCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "vkPSubpasses"
  vkPSubpasses :: Ptr VkSubpassDescription
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "vkDependencyCount"
  vkDependencyCount :: Word32
  , -- No documentation found for Nested "VkRenderPassCreateInfo" "vkPDependencies"
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
-- | VkFramebufferCreateInfo - Structure specifying parameters of a newly
-- created framebuffer
--
-- = Description
-- #_description#
--
-- Applications /must/ ensure that all accesses to memory that backs image
-- subresources used as attachments in a given renderpass instance either
-- happen-before the
-- <{html_spec_relative}#renderpass-load-store-ops load operations> for
-- those attachments, or happen-after the
-- <{html_spec_relative}#renderpass-load-store-ops store operations> for
-- those attachments.
--
-- __Note__
--
-- This restriction means that the render pass has full knowledge of all
-- uses of all of the attachments, so that the implementation is able to
-- make correct decisions about when and how to perform layout transitions,
-- when to overlap execution of subpasses, etc.
--
-- It is legal for a subpass to use no color or depth\/stencil attachments,
-- and rather use shader side effects such as image stores and atomics to
-- produce an output. In this case, the subpass continues to use the
-- @width@, @height@, and @layers@ of the framebuffer to define the
-- dimensions of the rendering area, and the @rasterizationSamples@ from
-- each pipeline’s
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'
-- to define the number of samples used in rasterization; however, if
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceFeatures'::@variableMultisampleRate@
-- is @VK_FALSE@, then all pipelines to be bound with a given
-- zero-attachment subpass /must/ have the same value for
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'::@rasterizationSamples@.
--
-- == Valid Usage
--
-- -   @attachmentCount@ /must/ be equal to the attachment count specified
--     in @renderPass@
--
-- -   Each element of @pAttachments@ that is used as a color attachment or
--     resolve attachment by @renderPass@ /must/ have been created with a
--     @usage@ value including @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@
--
-- -   Each element of @pAttachments@ that is used as a depth\/stencil
--     attachment by @renderPass@ /must/ have been created with a @usage@
--     value including @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@
--
-- -   Each element of @pAttachments@ that is used as an input attachment
--     by @renderPass@ /must/ have been created with a @usage@ value
--     including @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@
--
-- -   Each element of @pAttachments@ /must/ have been created with an
--     'Graphics.Vulkan.Core10.Core.VkFormat' value that matches the
--     'Graphics.Vulkan.Core10.Core.VkFormat' specified by the
--     corresponding @VkAttachmentDescription@ in @renderPass@
--
-- -   Each element of @pAttachments@ /must/ have been created with a
--     @samples@ value that matches the @samples@ value specified by the
--     corresponding @VkAttachmentDescription@ in @renderPass@
--
-- -   Each element of @pAttachments@ /must/ have dimensions at least as
--     large as the corresponding framebuffer dimension
--
-- -   Each element of @pAttachments@ /must/ only specify a single mip
--     level
--
-- -   Each element of @pAttachments@ /must/ have been created with the
--     identity swizzle
--
-- -   @width@ /must/ be greater than @0@.
--
-- -   @width@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxFramebufferWidth@
--
-- -   @height@ /must/ be greater than @0@.
--
-- -   @height@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxFramebufferHeight@
--
-- -   @layers@ /must/ be greater than @0@.
--
-- -   @layers@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxFramebufferLayers@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @renderPass@ /must/ be a valid @VkRenderPass@ handle
--
-- -   If @attachmentCount@ is not @0@, @pAttachments@ /must/ be a valid
--     pointer to an array of @attachmentCount@ valid @VkImageView@ handles
--
-- -   Both of @renderPass@, and the elements of @pAttachments@ that are
--     valid handles /must/ have been created, allocated, or retrieved from
--     the same @VkDevice@
--
-- = See Also
-- #_see_also#
--
-- 'VkFramebufferCreateFlags',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageView',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateFramebuffer'
data VkFramebufferCreateInfo = VkFramebufferCreateInfo
  { -- No documentation found for Nested "VkFramebufferCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "vkFlags"
  vkFlags :: VkFramebufferCreateFlags
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "vkRenderPass"
  vkRenderPass :: VkRenderPass
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "vkAttachmentCount"
  vkAttachmentCount :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "vkPAttachments"
  vkPAttachments :: Ptr VkImageView
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "vkWidth"
  vkWidth :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "vkHeight"
  vkHeight :: Word32
  , -- No documentation found for Nested "VkFramebufferCreateInfo" "vkLayers"
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
-- | VkAccessFlags - Bitmask of VkAccessFlagBits
--
-- = Description
-- #_description#
--
-- @VkAccessFlags@ is a bitmask type for setting a mask of zero or more
-- 'VkAccessFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkAccessFlagBits',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkMemoryBarrier',
-- 'VkSubpassDependency'
type VkAccessFlags = VkAccessFlagBits
-- | VkSubpassDescriptionFlags - Bitmask of VkSubpassDescriptionFlagBits
--
-- = Description
-- #_description#
--
-- @VkSubpassDescriptionFlags@ is a bitmask type for setting a mask of zero
-- or more 'VkSubpassDescriptionFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkSubpassDescription', 'VkSubpassDescriptionFlagBits'
type VkSubpassDescriptionFlags = VkSubpassDescriptionFlagBits
-- | VkAttachmentDescriptionFlags - Bitmask of
-- VkAttachmentDescriptionFlagBits
--
-- = Description
-- #_description#
--
-- @VkAttachmentDescriptionFlags@ is a bitmask type for setting a mask of
-- zero or more 'VkAttachmentDescriptionFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkAttachmentDescription', 'VkAttachmentDescriptionFlagBits'
type VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlagBits
-- | VkDependencyFlags - Bitmask of VkDependencyFlagBits
--
-- = Description
-- #_description#
--
-- @VkDependencyFlags@ is a bitmask type for setting a mask of zero or more
-- 'VkDependencyFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkDependencyFlagBits', 'VkSubpassDependency',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
type VkDependencyFlags = VkDependencyFlagBits
