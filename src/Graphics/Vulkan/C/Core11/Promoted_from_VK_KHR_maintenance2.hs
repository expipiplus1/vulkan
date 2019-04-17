{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkImageViewUsageCreateInfo(..)
  , VkInputAttachmentAspectReference(..)
  , VkPhysicalDevicePointClippingProperties(..)
  , VkPipelineTessellationDomainOriginStateCreateInfo(..)
  , VkPointClippingBehavior(..)
  , pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
  , pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
  , VkRenderPassInputAttachmentAspectCreateInfo(..)
  , VkTessellationDomainOrigin(..)
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
  , pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
  , pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
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
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkImageUsageFlags
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlags
  )


-- | VkImageViewUsageCreateInfo - Specify the intended usage of an image view
--
-- = Description
--
-- When this structure is chained to @VkImageViewCreateInfo@ the @usage@
-- field overrides the implicit @usage@ parameter inherited from image
-- creation time and its value is used instead for the purposes of
-- determining the valid usage conditions of
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkImageViewUsageCreateInfo = VkImageViewUsageCreateInfo
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @usage@ /must/ not be @0@
  vkUsage :: VkImageUsageFlags
  }
  deriving (Eq, Show)

instance Storable VkImageViewUsageCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageViewUsageCreateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageViewUsageCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageViewUsageCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkUsage (poked :: VkImageViewUsageCreateInfo))

instance Zero VkImageViewUsageCreateInfo where
  zero = VkImageViewUsageCreateInfo zero
                                    zero
                                    zero
-- | VkInputAttachmentAspectReference - Structure specifying a subpass\/input
-- attachment pair and an aspect mask that /can/ be read.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'VkRenderPassInputAttachmentAspectCreateInfo'
data VkInputAttachmentAspectReference = VkInputAttachmentAspectReference
  { -- | @subpass@ is an index into the @pSubpasses@ array of the parent
  -- @VkRenderPassCreateInfo@ structure.
  vkSubpass :: Word32
  , -- | @inputAttachmentIndex@ is an index into the @pInputAttachments@ of the
  -- specified subpass.
  vkInputAttachmentIndex :: Word32
  , -- | @aspectMask@ /must/ not be @0@
  vkAspectMask :: VkImageAspectFlags
  }
  deriving (Eq, Show)

instance Storable VkInputAttachmentAspectReference where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkInputAttachmentAspectReference <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSubpass (poked :: VkInputAttachmentAspectReference))
                *> poke (ptr `plusPtr` 4) (vkInputAttachmentIndex (poked :: VkInputAttachmentAspectReference))
                *> poke (ptr `plusPtr` 8) (vkAspectMask (poked :: VkInputAttachmentAspectReference))

instance Zero VkInputAttachmentAspectReference where
  zero = VkInputAttachmentAspectReference zero
                                          zero
                                          zero
-- | VkPhysicalDevicePointClippingProperties - Structure describing the point
-- clipping behavior supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDevicePointClippingProperties@ structure
-- describe the following implementation-dependent limit:
--
-- = Description
--
-- If the @VkPhysicalDevicePointClippingProperties@ structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkPointClippingBehavior',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkPhysicalDevicePointClippingProperties = VkPhysicalDevicePointClippingProperties
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pointClippingBehavior@ is the point clipping behavior supported by the
  -- implementation, and is of type 'VkPointClippingBehavior'.
  vkPointClippingBehavior :: VkPointClippingBehavior
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDevicePointClippingProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDevicePointClippingProperties <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDevicePointClippingProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDevicePointClippingProperties))
                *> poke (ptr `plusPtr` 16) (vkPointClippingBehavior (poked :: VkPhysicalDevicePointClippingProperties))

instance Zero VkPhysicalDevicePointClippingProperties where
  zero = VkPhysicalDevicePointClippingProperties zero
                                                 zero
                                                 zero
-- | VkPipelineTessellationDomainOriginStateCreateInfo - Structure specifying
-- the orientation of the tessellation domain
--
-- = Description
--
-- If the @VkPipelineTessellationDomainOriginStateCreateInfo@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineTessellationStateCreateInfo',
-- it controls the origin of the tessellation domain. If this structure is
-- not present, it is as if @domainOrigin@ were
-- @VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'VkTessellationDomainOrigin'
data VkPipelineTessellationDomainOriginStateCreateInfo = VkPipelineTessellationDomainOriginStateCreateInfo
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @domainOrigin@ /must/ be a valid 'VkTessellationDomainOrigin' value
  vkDomainOrigin :: VkTessellationDomainOrigin
  }
  deriving (Eq, Show)

instance Storable VkPipelineTessellationDomainOriginStateCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPipelineTessellationDomainOriginStateCreateInfo <$> peek (ptr `plusPtr` 0)
                                                               <*> peek (ptr `plusPtr` 8)
                                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineTessellationDomainOriginStateCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineTessellationDomainOriginStateCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkDomainOrigin (poked :: VkPipelineTessellationDomainOriginStateCreateInfo))

instance Zero VkPipelineTessellationDomainOriginStateCreateInfo where
  zero = VkPipelineTessellationDomainOriginStateCreateInfo zero
                                                           zero
                                                           zero
-- ** VkPointClippingBehavior

-- | VkPointClippingBehavior - Enum specifying the point clipping behavior
--
-- = See Also
--
-- 'VkPhysicalDevicePointClippingProperties'
newtype VkPointClippingBehavior = VkPointClippingBehavior Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkPointClippingBehavior where
  showsPrec _ VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES = showString "VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES"
  showsPrec _ VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY = showString "VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY"
  showsPrec p (VkPointClippingBehavior x) = showParen (p >= 11) (showString "VkPointClippingBehavior " . showsPrec 11 x)

instance Read VkPointClippingBehavior where
  readPrec = parens ( choose [ ("VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES",       pure VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES)
                             , ("VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY", pure VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPointClippingBehavior")
                        v <- step readPrec
                        pure (VkPointClippingBehavior v)
                        )
                    )

-- | @VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES@ specifies that the
-- primitive is discarded if the vertex lies outside any clip plane,
-- including the planes bounding the view volume.
pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES :: VkPointClippingBehavior
pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES = VkPointClippingBehavior 0

-- | @VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY@ specifies that the
-- primitive is discarded only if the vertex lies outside any user clip
-- plane.
pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY :: VkPointClippingBehavior
pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY = VkPointClippingBehavior 1
-- | VkRenderPassInputAttachmentAspectCreateInfo - Structure specifying, for
-- a given subpass\/input attachment pair, which aspect /can/ be read.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkInputAttachmentAspectReference',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkRenderPassInputAttachmentAspectCreateInfo = VkRenderPassInputAttachmentAspectCreateInfo
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @aspectReferenceCount@ /must/ be greater than @0@
  vkAspectReferenceCount :: Word32
  , -- | @pAspectReferences@ /must/ be a valid pointer to an array of
  -- @aspectReferenceCount@ valid @VkInputAttachmentAspectReference@
  -- structures
  vkPAspectReferences :: Ptr VkInputAttachmentAspectReference
  }
  deriving (Eq, Show)

instance Storable VkRenderPassInputAttachmentAspectCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkRenderPassInputAttachmentAspectCreateInfo <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassInputAttachmentAspectCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassInputAttachmentAspectCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkAspectReferenceCount (poked :: VkRenderPassInputAttachmentAspectCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPAspectReferences (poked :: VkRenderPassInputAttachmentAspectCreateInfo))

instance Zero VkRenderPassInputAttachmentAspectCreateInfo where
  zero = VkRenderPassInputAttachmentAspectCreateInfo zero
                                                     zero
                                                     zero
                                                     zero
-- ** VkTessellationDomainOrigin

-- | VkTessellationDomainOrigin - Enum describing tessellation domain origin
--
-- = Description
--
-- This enum affects how the @VertexOrderCw@ and @VertexOrderCcw@
-- tessellation execution modes are interpreted, since the winding is
-- defined relative to the orientation of the domain.
--
-- = See Also
--
-- 'VkPipelineTessellationDomainOriginStateCreateInfo'
newtype VkTessellationDomainOrigin = VkTessellationDomainOrigin Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkTessellationDomainOrigin where
  showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT = showString "VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT"
  showsPrec _ VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT = showString "VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT"
  showsPrec p (VkTessellationDomainOrigin x) = showParen (p >= 11) (showString "VkTessellationDomainOrigin " . showsPrec 11 x)

instance Read VkTessellationDomainOrigin where
  readPrec = parens ( choose [ ("VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT", pure VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT)
                             , ("VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT", pure VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkTessellationDomainOrigin")
                        v <- step readPrec
                        pure (VkTessellationDomainOrigin v)
                        )
                    )

-- | @VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT@ specifies that the origin of
-- the domain space is in the upper left corner, as shown in figure
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#img-tessellation-topology-ul {html_spec_relative}#img-tessellation-topology-ul>.
pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT :: VkTessellationDomainOrigin
pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT = VkTessellationDomainOrigin 0

-- | @VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT@ specifies that the origin of
-- the domain space is in the lower left corner, as shown in figure
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#img-tessellation-topology-ll {html_spec_relative}#img-tessellation-topology-ll>.
pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT :: VkTessellationDomainOrigin
pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT = VkTessellationDomainOrigin 1
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT = VkImageCreateFlagBits 0x00000080
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT"
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT = VkImageCreateFlagBits 0x00000100
-- | @VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL@: /must/
-- only be used as a depth\/stencil attachment in a @VkFramebuffer@, where
-- the stencil aspect is read-only, and\/or as a read-only image in a
-- shader (which /can/ be read as a sampled image, combined image\/sampler
-- and\/or input attachment) where only the stencil aspect is accessed.
-- This layout is valid only for image subresources of images created with
-- the @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ usage bit enabled.
-- Only image views created with a @usage@ value including
-- @VK_IMAGE_USAGE_SAMPLED_BIT@ /can/ be used as a sampled image or
-- combined image\/sampler in a shader. Similarly, only image views created
-- with a @usage@ value including @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@
-- /can/ be used as input attachments.
pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL = VkImageLayout 1000117001
-- | @VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL@: /must/
-- only be used as a depth\/stencil attachment in a @VkFramebuffer@, where
-- the depth aspect is read-only, and\/or as a read-only image in a shader
-- (which /can/ be read as a sampled image, combined image\/sampler and\/or
-- input attachment) where only the depth aspect is accessed. This layout
-- is valid only for image subresources of images created with the
-- @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ usage bit enabled. Only
-- image views created with a @usage@ value including
-- @VK_IMAGE_USAGE_SAMPLED_BIT@ /can/ be used as a sampled image or
-- combined image\/sampler in a shader. Similarly, only image views created
-- with a @usage@ value including @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@
-- /can/ be used as input attachments.
pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL = VkImageLayout 1000117000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO = VkStructureType 1000117002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES = VkStructureType 1000117000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO = VkStructureType 1000117003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO = VkStructureType 1000117001
