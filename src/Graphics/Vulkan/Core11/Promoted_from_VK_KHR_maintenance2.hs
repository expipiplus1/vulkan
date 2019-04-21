{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2
  ( withCStructImageViewUsageCreateInfo
  , fromCStructImageViewUsageCreateInfo
  , ImageViewUsageCreateInfo(..)
  , withCStructInputAttachmentAspectReference
  , fromCStructInputAttachmentAspectReference
  , InputAttachmentAspectReference(..)
  , withCStructPhysicalDevicePointClippingProperties
  , fromCStructPhysicalDevicePointClippingProperties
  , PhysicalDevicePointClippingProperties(..)
  , withCStructPipelineTessellationDomainOriginStateCreateInfo
  , fromCStructPipelineTessellationDomainOriginStateCreateInfo
  , PipelineTessellationDomainOriginStateCreateInfo(..)
  , PointClippingBehavior
  , pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
  , pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
  , PointClippingBehaviorKHR
  , withCStructRenderPassInputAttachmentAspectCreateInfo
  , fromCStructRenderPassInputAttachmentAspectCreateInfo
  , RenderPassInputAttachmentAspectCreateInfo(..)
  , TessellationDomainOrigin
  , pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
  , pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
  , TessellationDomainOriginKHR
  , pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkImageViewUsageCreateInfo(..)
  , VkInputAttachmentAspectReference(..)
  , VkPhysicalDevicePointClippingProperties(..)
  , VkPipelineTessellationDomainOriginStateCreateInfo(..)
  , VkPointClippingBehavior(..)
  , VkRenderPassInputAttachmentAspectCreateInfo(..)
  , VkTessellationDomainOrigin(..)
  , pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
  , pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageUsageFlags
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlags
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
  , pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  )



-- | VkImageViewUsageCreateInfo - Specify the intended usage of an image view
--
-- = Description
--
-- When this structure is chained to
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo' the @usage@
-- field overrides the implicit @usage@ parameter inherited from image
-- creation time and its value is used instead for the purposes of
-- determining the valid usage conditions of
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo'.
--
-- Unresolved directive in VkImageViewUsageCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkImageViewUsageCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ImageViewUsageCreateInfo = ImageViewUsageCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ImageViewUsageCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewUsageCreateInfo" "usage"
  usage :: ImageUsageFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageViewUsageCreateInfo' and
-- marshal a 'ImageViewUsageCreateInfo' into it. The 'VkImageViewUsageCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageViewUsageCreateInfo :: ImageViewUsageCreateInfo -> (VkImageViewUsageCreateInfo -> IO a) -> IO a
withCStructImageViewUsageCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImageViewUsageCreateInfo)) (\pPNext -> cont (VkImageViewUsageCreateInfo VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO pPNext (usage (marshalled :: ImageViewUsageCreateInfo))))

-- | A function to read a 'VkImageViewUsageCreateInfo' and all additional
-- structures in the pointer chain into a 'ImageViewUsageCreateInfo'.
fromCStructImageViewUsageCreateInfo :: VkImageViewUsageCreateInfo -> IO ImageViewUsageCreateInfo
fromCStructImageViewUsageCreateInfo c = ImageViewUsageCreateInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageViewUsageCreateInfo)))
                                                                 <*> pure (vkUsage (c :: VkImageViewUsageCreateInfo))

instance Zero ImageViewUsageCreateInfo where
  zero = ImageViewUsageCreateInfo Nothing
                                  zero



-- | VkInputAttachmentAspectReference - Structure specifying a subpass\/input
-- attachment pair and an aspect mask that /can/ be read.
--
-- == Valid Usage
--
-- Unresolved directive in VkInputAttachmentAspectReference.txt -
-- include::{generated}\/validity\/structs\/VkInputAttachmentAspectReference.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo'
data InputAttachmentAspectReference = InputAttachmentAspectReference
  { -- No documentation found for Nested "InputAttachmentAspectReference" "subpass"
  subpass :: Word32
  , -- No documentation found for Nested "InputAttachmentAspectReference" "inputAttachmentIndex"
  inputAttachmentIndex :: Word32
  , -- No documentation found for Nested "InputAttachmentAspectReference" "aspectMask"
  aspectMask :: ImageAspectFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkInputAttachmentAspectReference' and
-- marshal a 'InputAttachmentAspectReference' into it. The 'VkInputAttachmentAspectReference' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructInputAttachmentAspectReference :: InputAttachmentAspectReference -> (VkInputAttachmentAspectReference -> IO a) -> IO a
withCStructInputAttachmentAspectReference marshalled cont = cont (VkInputAttachmentAspectReference (subpass (marshalled :: InputAttachmentAspectReference)) (inputAttachmentIndex (marshalled :: InputAttachmentAspectReference)) (aspectMask (marshalled :: InputAttachmentAspectReference)))

-- | A function to read a 'VkInputAttachmentAspectReference' and all additional
-- structures in the pointer chain into a 'InputAttachmentAspectReference'.
fromCStructInputAttachmentAspectReference :: VkInputAttachmentAspectReference -> IO InputAttachmentAspectReference
fromCStructInputAttachmentAspectReference c = InputAttachmentAspectReference <$> pure (vkSubpass (c :: VkInputAttachmentAspectReference))
                                                                             <*> pure (vkInputAttachmentIndex (c :: VkInputAttachmentAspectReference))
                                                                             <*> pure (vkAspectMask (c :: VkInputAttachmentAspectReference))

instance Zero InputAttachmentAspectReference where
  zero = InputAttachmentAspectReference zero
                                        zero
                                        zero



-- | VkPhysicalDevicePointClippingProperties - Structure describing the point
-- clipping behavior supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPhysicalDevicePointClippingProperties'
-- structure describe the following implementation-dependent limit:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPhysicalDevicePointClippingProperties'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in VkPhysicalDevicePointClippingProperties.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDevicePointClippingProperties.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPointClippingBehavior',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDevicePointClippingProperties = PhysicalDevicePointClippingProperties
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDevicePointClippingProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevicePointClippingProperties" "pointClippingBehavior"
  pointClippingBehavior :: PointClippingBehavior
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDevicePointClippingProperties' and
-- marshal a 'PhysicalDevicePointClippingProperties' into it. The 'VkPhysicalDevicePointClippingProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDevicePointClippingProperties :: PhysicalDevicePointClippingProperties -> (VkPhysicalDevicePointClippingProperties -> IO a) -> IO a
withCStructPhysicalDevicePointClippingProperties marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDevicePointClippingProperties)) (\pPNext -> cont (VkPhysicalDevicePointClippingProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES pPNext (pointClippingBehavior (marshalled :: PhysicalDevicePointClippingProperties))))

-- | A function to read a 'VkPhysicalDevicePointClippingProperties' and all additional
-- structures in the pointer chain into a 'PhysicalDevicePointClippingProperties'.
fromCStructPhysicalDevicePointClippingProperties :: VkPhysicalDevicePointClippingProperties -> IO PhysicalDevicePointClippingProperties
fromCStructPhysicalDevicePointClippingProperties c = PhysicalDevicePointClippingProperties <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDevicePointClippingProperties)))
                                                                                           <*> pure (vkPointClippingBehavior (c :: VkPhysicalDevicePointClippingProperties))

instance Zero PhysicalDevicePointClippingProperties where
  zero = PhysicalDevicePointClippingProperties Nothing
                                               zero



-- | VkPipelineTessellationDomainOriginStateCreateInfo - Structure specifying
-- the orientation of the tessellation domain
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPipelineTessellationDomainOriginStateCreateInfo'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineTessellationStateCreateInfo',
-- it controls the origin of the tessellation domain. If this structure is
-- not present, it is as if @domainOrigin@ were
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT'.
--
-- Unresolved directive in
-- VkPipelineTessellationDomainOriginStateCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineTessellationDomainOriginStateCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkTessellationDomainOrigin'
data PipelineTessellationDomainOriginStateCreateInfo = PipelineTessellationDomainOriginStateCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineTessellationDomainOriginStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineTessellationDomainOriginStateCreateInfo" "domainOrigin"
  domainOrigin :: TessellationDomainOrigin
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineTessellationDomainOriginStateCreateInfo' and
-- marshal a 'PipelineTessellationDomainOriginStateCreateInfo' into it. The 'VkPipelineTessellationDomainOriginStateCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineTessellationDomainOriginStateCreateInfo :: PipelineTessellationDomainOriginStateCreateInfo -> (VkPipelineTessellationDomainOriginStateCreateInfo -> IO a) -> IO a
withCStructPipelineTessellationDomainOriginStateCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PipelineTessellationDomainOriginStateCreateInfo)) (\pPNext -> cont (VkPipelineTessellationDomainOriginStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO pPNext (domainOrigin (marshalled :: PipelineTessellationDomainOriginStateCreateInfo))))

-- | A function to read a 'VkPipelineTessellationDomainOriginStateCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineTessellationDomainOriginStateCreateInfo'.
fromCStructPipelineTessellationDomainOriginStateCreateInfo :: VkPipelineTessellationDomainOriginStateCreateInfo -> IO PipelineTessellationDomainOriginStateCreateInfo
fromCStructPipelineTessellationDomainOriginStateCreateInfo c = PipelineTessellationDomainOriginStateCreateInfo <$> -- Univalued Member elided
                                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineTessellationDomainOriginStateCreateInfo)))
                                                                                                               <*> pure (vkDomainOrigin (c :: VkPipelineTessellationDomainOriginStateCreateInfo))

instance Zero PipelineTessellationDomainOriginStateCreateInfo where
  zero = PipelineTessellationDomainOriginStateCreateInfo Nothing
                                                         zero


-- | VkPointClippingBehavior - Enum specifying the point clipping behavior
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPhysicalDevicePointClippingProperties'
type PointClippingBehavior = VkPointClippingBehavior


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES'
-- specifies that the primitive is discarded if the vertex lies outside any
-- clip plane, including the planes bounding the view volume.
pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES :: (a ~ PointClippingBehavior) => a
pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES = VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY'
-- specifies that the primitive is discarded only if the vertex lies
-- outside any user clip plane.
pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY :: (a ~ PointClippingBehavior) => a
pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY = VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY

-- No documentation found for TopLevel "PointClippingBehaviorKHR"
type PointClippingBehaviorKHR = PointClippingBehavior


-- | VkRenderPassInputAttachmentAspectCreateInfo - Structure specifying, for
-- a given subpass\/input attachment pair, which aspect /can/ be read.
--
-- = Description
--
-- Unresolved directive in VkRenderPassInputAttachmentAspectCreateInfo.txt
-- -
-- include::{generated}\/validity\/structs\/VkRenderPassInputAttachmentAspectCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkInputAttachmentAspectReference',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data RenderPassInputAttachmentAspectCreateInfo = RenderPassInputAttachmentAspectCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "RenderPassInputAttachmentAspectCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassInputAttachmentAspectCreateInfo" "pAspectReferences"
  aspectReferences :: Vector InputAttachmentAspectReference
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRenderPassInputAttachmentAspectCreateInfo' and
-- marshal a 'RenderPassInputAttachmentAspectCreateInfo' into it. The 'VkRenderPassInputAttachmentAspectCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRenderPassInputAttachmentAspectCreateInfo :: RenderPassInputAttachmentAspectCreateInfo -> (VkRenderPassInputAttachmentAspectCreateInfo -> IO a) -> IO a
withCStructRenderPassInputAttachmentAspectCreateInfo marshalled cont = withVec withCStructInputAttachmentAspectReference (aspectReferences (marshalled :: RenderPassInputAttachmentAspectCreateInfo)) (\pPAspectReferences -> maybeWith withSomeVkStruct (next (marshalled :: RenderPassInputAttachmentAspectCreateInfo)) (\pPNext -> cont (VkRenderPassInputAttachmentAspectCreateInfo VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO pPNext (fromIntegral (Data.Vector.length (aspectReferences (marshalled :: RenderPassInputAttachmentAspectCreateInfo)))) pPAspectReferences)))

-- | A function to read a 'VkRenderPassInputAttachmentAspectCreateInfo' and all additional
-- structures in the pointer chain into a 'RenderPassInputAttachmentAspectCreateInfo'.
fromCStructRenderPassInputAttachmentAspectCreateInfo :: VkRenderPassInputAttachmentAspectCreateInfo -> IO RenderPassInputAttachmentAspectCreateInfo
fromCStructRenderPassInputAttachmentAspectCreateInfo c = RenderPassInputAttachmentAspectCreateInfo <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassInputAttachmentAspectCreateInfo)))
                                                                                                   -- Length valued member elided
                                                                                                   <*> (Data.Vector.generateM (fromIntegral (vkAspectReferenceCount (c :: VkRenderPassInputAttachmentAspectCreateInfo))) (((fromCStructInputAttachmentAspectReference <=<) . peekElemOff) (vkPAspectReferences (c :: VkRenderPassInputAttachmentAspectCreateInfo))))

instance Zero RenderPassInputAttachmentAspectCreateInfo where
  zero = RenderPassInputAttachmentAspectCreateInfo Nothing
                                                   Data.Vector.empty


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
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPipelineTessellationDomainOriginStateCreateInfo'
type TessellationDomainOrigin = VkTessellationDomainOrigin


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT'
-- specifies that the origin of the domain space is in the upper left
-- corner, as shown in figure
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#img-tessellation-topology-ul>.
pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT :: (a ~ TessellationDomainOrigin) => a
pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT = VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT'
-- specifies that the origin of the domain space is in the lower left
-- corner, as shown in figure
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#img-tessellation-topology-ll>.
pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT :: (a ~ TessellationDomainOrigin) => a
pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT = VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT

-- No documentation found for TopLevel "TessellationDomainOriginKHR"
type TessellationDomainOriginKHR = TessellationDomainOrigin
