{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkPointClippingBehavior(..)
  , pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
  , pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
  , VkTessellationDomainOrigin(..)
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
  , VkInputAttachmentAspectReference(..)
  , VkRenderPassInputAttachmentAspectCreateInfo(..)
  , VkPhysicalDevicePointClippingProperties(..)
  , VkImageViewUsageCreateInfo(..)
  , VkPipelineTessellationDomainOriginStateCreateInfo(..)
  ) where

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
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageUsageFlags
  , VkImageCreateFlagBits(..)
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlags
  )


-- ** VkPointClippingBehavior

-- | VkPointClippingBehavior - Enum specifying the point clipping behaviour
newtype VkPointClippingBehavior = VkPointClippingBehavior Int32
  deriving (Eq, Ord, Storable)

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

-- No documentation found for Nested "VkPointClippingBehavior" "VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES"
pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES :: VkPointClippingBehavior
pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES = VkPointClippingBehavior 0

-- No documentation found for Nested "VkPointClippingBehavior" "VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY"
pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY :: VkPointClippingBehavior
pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY = VkPointClippingBehavior 1
-- ** VkTessellationDomainOrigin

-- | VkTessellationDomainOrigin - Enum describing tessellation domain origin
newtype VkTessellationDomainOrigin = VkTessellationDomainOrigin Int32
  deriving (Eq, Ord, Storable)

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

-- No documentation found for Nested "VkTessellationDomainOrigin" "VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT"
pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT :: VkTessellationDomainOrigin
pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT = VkTessellationDomainOrigin 0

-- No documentation found for Nested "VkTessellationDomainOrigin" "VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT"
pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT :: VkTessellationDomainOrigin
pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT = VkTessellationDomainOrigin 1
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL"
pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL = VkImageLayout 1000117000
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL"
pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL = VkImageLayout 1000117001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES = VkStructureType 1000117000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO = VkStructureType 1000117001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO = VkStructureType 1000117002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO = VkStructureType 1000117003
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT = VkImageCreateFlagBits 0x00000080
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT"
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT = VkImageCreateFlagBits 0x00000100
-- | VkInputAttachmentAspectReference - Structure specifying a subpass\/input
-- attachment pair and an aspect mask that /can/ be read.
data VkInputAttachmentAspectReference = VkInputAttachmentAspectReference
  { -- No documentation found for Nested "VkInputAttachmentAspectReference" "vkSubpass"
  vkSubpass :: Word32
  , -- No documentation found for Nested "VkInputAttachmentAspectReference" "vkInputAttachmentIndex"
  vkInputAttachmentIndex :: Word32
  , -- No documentation found for Nested "VkInputAttachmentAspectReference" "vkAspectMask"
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
-- | VkRenderPassInputAttachmentAspectCreateInfo - Structure specifying, for
-- a given subpass\/input attachment pair, which aspect /can/ be read.
data VkRenderPassInputAttachmentAspectCreateInfo = VkRenderPassInputAttachmentAspectCreateInfo
  { -- No documentation found for Nested "VkRenderPassInputAttachmentAspectCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassInputAttachmentAspectCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassInputAttachmentAspectCreateInfo" "vkAspectReferenceCount"
  vkAspectReferenceCount :: Word32
  , -- No documentation found for Nested "VkRenderPassInputAttachmentAspectCreateInfo" "vkPAspectReferences"
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
-- | VkPhysicalDevicePointClippingProperties - Structure describing the point
-- clipping behavior supported by an implementation
data VkPhysicalDevicePointClippingProperties = VkPhysicalDevicePointClippingProperties
  { -- No documentation found for Nested "VkPhysicalDevicePointClippingProperties" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDevicePointClippingProperties" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDevicePointClippingProperties" "vkPointClippingBehavior"
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
-- | VkImageViewUsageCreateInfo - Specify the intended usage of an image view
data VkImageViewUsageCreateInfo = VkImageViewUsageCreateInfo
  { -- No documentation found for Nested "VkImageViewUsageCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageViewUsageCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageViewUsageCreateInfo" "vkUsage"
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
-- | VkPipelineTessellationDomainOriginStateCreateInfo - Structure specifying
-- the orientation of the tessellation domain
data VkPipelineTessellationDomainOriginStateCreateInfo = VkPipelineTessellationDomainOriginStateCreateInfo
  { -- No documentation found for Nested "VkPipelineTessellationDomainOriginStateCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineTessellationDomainOriginStateCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineTessellationDomainOriginStateCreateInfo" "vkDomainOrigin"
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
