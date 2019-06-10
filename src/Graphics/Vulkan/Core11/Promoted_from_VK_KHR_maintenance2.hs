{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  ImageViewUsageCreateInfo(..)
  , 
#endif
  InputAttachmentAspectReference(..)
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDevicePointClippingProperties(..)
  , PipelineTessellationDomainOriginStateCreateInfo(..)
#endif
  , PointClippingBehavior
  , pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
  , pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
  , PointClippingBehaviorKHR
#if defined(VK_USE_PLATFORM_GGP)
  , RenderPassInputAttachmentAspectCreateInfo(..)
#endif
  , TessellationDomainOrigin
  , pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
  , pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
  , TessellationDomainOriginKHR
  , pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern IMAGE_CREATE_EXTENDED_USAGE_BIT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  , pattern STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  , pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkPointClippingBehavior(..)
  , VkTessellationDomainOrigin(..)
  , pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
  , pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageUsageFlags
  )
#endif
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( ImageAspectFlags
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern IMAGE_CREATE_EXTENDED_USAGE_BIT
  )
import Graphics.Vulkan.Core10.Image
  ( pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageViewUsageCreateInfo"
data ImageViewUsageCreateInfo = ImageViewUsageCreateInfo
  { -- No documentation found for Nested "ImageViewUsageCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageViewUsageCreateInfo" "usage"
  usage :: ImageUsageFlags
  }
  deriving (Show, Eq)

instance Zero ImageViewUsageCreateInfo where
  zero = ImageViewUsageCreateInfo Nothing
                                  zero

#endif


-- No documentation found for TopLevel "VkInputAttachmentAspectReference"
data InputAttachmentAspectReference = InputAttachmentAspectReference
  { -- No documentation found for Nested "InputAttachmentAspectReference" "subpass"
  subpass :: Word32
  , -- No documentation found for Nested "InputAttachmentAspectReference" "inputAttachmentIndex"
  inputAttachmentIndex :: Word32
  , -- No documentation found for Nested "InputAttachmentAspectReference" "aspectMask"
  aspectMask :: ImageAspectFlags
  }
  deriving (Show, Eq)

instance Zero InputAttachmentAspectReference where
  zero = InputAttachmentAspectReference zero
                                        zero
                                        zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDevicePointClippingProperties"
data PhysicalDevicePointClippingProperties = PhysicalDevicePointClippingProperties
  { -- No documentation found for Nested "PhysicalDevicePointClippingProperties" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDevicePointClippingProperties" "pointClippingBehavior"
  pointClippingBehavior :: PointClippingBehavior
  }
  deriving (Show, Eq)

instance Zero PhysicalDevicePointClippingProperties where
  zero = PhysicalDevicePointClippingProperties Nothing
                                               zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineTessellationDomainOriginStateCreateInfo"
data PipelineTessellationDomainOriginStateCreateInfo = PipelineTessellationDomainOriginStateCreateInfo
  { -- No documentation found for Nested "PipelineTessellationDomainOriginStateCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineTessellationDomainOriginStateCreateInfo" "domainOrigin"
  domainOrigin :: TessellationDomainOrigin
  }
  deriving (Show, Eq)

instance Zero PipelineTessellationDomainOriginStateCreateInfo where
  zero = PipelineTessellationDomainOriginStateCreateInfo Nothing
                                                         zero

#endif

-- No documentation found for TopLevel "PointClippingBehavior"
type PointClippingBehavior = VkPointClippingBehavior


{-# complete POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES, POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY :: PointClippingBehavior #-}


-- No documentation found for Nested "PointClippingBehavior" "POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES"
pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES :: (a ~ PointClippingBehavior) => a
pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES = VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES


-- No documentation found for Nested "PointClippingBehavior" "POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY"
pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY :: (a ~ PointClippingBehavior) => a
pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY = VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY

-- No documentation found for TopLevel "PointClippingBehaviorKHR"
type PointClippingBehaviorKHR = PointClippingBehavior


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkRenderPassInputAttachmentAspectCreateInfo"
data RenderPassInputAttachmentAspectCreateInfo = RenderPassInputAttachmentAspectCreateInfo
  { -- No documentation found for Nested "RenderPassInputAttachmentAspectCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassInputAttachmentAspectCreateInfo" "pAspectReferences"
  aspectReferences :: Vector InputAttachmentAspectReference
  }
  deriving (Show, Eq)

instance Zero RenderPassInputAttachmentAspectCreateInfo where
  zero = RenderPassInputAttachmentAspectCreateInfo Nothing
                                                   mempty

#endif

-- No documentation found for TopLevel "TessellationDomainOrigin"
type TessellationDomainOrigin = VkTessellationDomainOrigin


{-# complete TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT, TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT :: TessellationDomainOrigin #-}


-- No documentation found for Nested "TessellationDomainOrigin" "TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT"
pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT :: (a ~ TessellationDomainOrigin) => a
pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT = VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT


-- No documentation found for Nested "TessellationDomainOrigin" "TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT"
pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT :: (a ~ TessellationDomainOrigin) => a
pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT = VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT

-- No documentation found for TopLevel "TessellationDomainOriginKHR"
type TessellationDomainOriginKHR = TessellationDomainOrigin
