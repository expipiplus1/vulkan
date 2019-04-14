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


-- No documentation found for TopLevel "VkImageViewUsageCreateInfo"
data VkImageViewUsageCreateInfo = VkImageViewUsageCreateInfo
  { -- No documentation found for Nested "VkImageViewUsageCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageViewUsageCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageViewUsageCreateInfo" "usage"
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
-- No documentation found for TopLevel "VkInputAttachmentAspectReference"
data VkInputAttachmentAspectReference = VkInputAttachmentAspectReference
  { -- No documentation found for Nested "VkInputAttachmentAspectReference" "subpass"
  vkSubpass :: Word32
  , -- No documentation found for Nested "VkInputAttachmentAspectReference" "inputAttachmentIndex"
  vkInputAttachmentIndex :: Word32
  , -- No documentation found for Nested "VkInputAttachmentAspectReference" "aspectMask"
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
-- No documentation found for TopLevel "VkPhysicalDevicePointClippingProperties"
data VkPhysicalDevicePointClippingProperties = VkPhysicalDevicePointClippingProperties
  { -- No documentation found for Nested "VkPhysicalDevicePointClippingProperties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDevicePointClippingProperties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDevicePointClippingProperties" "pointClippingBehavior"
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
-- No documentation found for TopLevel "VkPipelineTessellationDomainOriginStateCreateInfo"
data VkPipelineTessellationDomainOriginStateCreateInfo = VkPipelineTessellationDomainOriginStateCreateInfo
  { -- No documentation found for Nested "VkPipelineTessellationDomainOriginStateCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineTessellationDomainOriginStateCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineTessellationDomainOriginStateCreateInfo" "domainOrigin"
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
-- ** VkPointClippingBehavior

-- No documentation found for TopLevel "VkPointClippingBehavior"
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
-- No documentation found for TopLevel "VkRenderPassInputAttachmentAspectCreateInfo"
data VkRenderPassInputAttachmentAspectCreateInfo = VkRenderPassInputAttachmentAspectCreateInfo
  { -- No documentation found for Nested "VkRenderPassInputAttachmentAspectCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassInputAttachmentAspectCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassInputAttachmentAspectCreateInfo" "aspectReferenceCount"
  vkAspectReferenceCount :: Word32
  , -- No documentation found for Nested "VkRenderPassInputAttachmentAspectCreateInfo" "pAspectReferences"
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
-- ** VkTessellationDomainOrigin

-- No documentation found for TopLevel "VkTessellationDomainOrigin"
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
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT = VkImageCreateFlagBits 0x00000080
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT"
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT = VkImageCreateFlagBits 0x00000100
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL"
pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL = VkImageLayout 1000117001
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL"
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
