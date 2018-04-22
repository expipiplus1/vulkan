{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_maintenance2
  ( pattern VK_KHR_MAINTENANCE2_SPEC_VERSION
  , pattern VK_KHR_MAINTENANCE2_EXTENSION_NAME
  , VkPointClippingBehaviorKHR
  , VkTessellationDomainOriginKHR
  , VkInputAttachmentAspectReferenceKHR
  , pattern VkInputAttachmentAspectReferenceKHR
  , VkRenderPassInputAttachmentAspectCreateInfoKHR
  , pattern VkRenderPassInputAttachmentAspectCreateInfoKHR
  , VkPhysicalDevicePointClippingPropertiesKHR
  , pattern VkPhysicalDevicePointClippingPropertiesKHR
  , VkImageViewUsageCreateInfoKHR
  , pattern VkImageViewUsageCreateInfoKHR
  , VkPipelineTessellationDomainOriginStateCreateInfoKHR
  , pattern VkPipelineTessellationDomainOriginStateCreateInfoKHR
  , pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
  , pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
  , pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR
  , pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkImageUsageFlags
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkImageAspectFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkImageViewUsageCreateInfo(..)
  , VkInputAttachmentAspectReference(..)
  , VkPhysicalDevicePointClippingProperties(..)
  , VkPipelineTessellationDomainOriginStateCreateInfo(..)
  , VkPointClippingBehavior(..)
  , VkRenderPassInputAttachmentAspectCreateInfo(..)
  , VkTessellationDomainOrigin(..)
  , pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
  , pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
  , pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
  , pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
  , pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
  , pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
  )


-- No documentation found for TopLevel "VK_KHR_MAINTENANCE2_SPEC_VERSION"
pattern VK_KHR_MAINTENANCE2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_MAINTENANCE2_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_MAINTENANCE2_EXTENSION_NAME"
pattern VK_KHR_MAINTENANCE2_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_MAINTENANCE2_EXTENSION_NAME = "VK_KHR_maintenance2"
-- No documentation found for TopLevel "VkPointClippingBehaviorKHR"
type VkPointClippingBehaviorKHR = VkPointClippingBehavior
-- No documentation found for TopLevel "VkTessellationDomainOriginKHR"
type VkTessellationDomainOriginKHR = VkTessellationDomainOrigin
-- No documentation found for TopLevel "VkInputAttachmentAspectReferenceKHR"
type VkInputAttachmentAspectReferenceKHR = VkInputAttachmentAspectReference


-- No documentation found for TopLevel "VkInputAttachmentAspectReferenceKHR"
pattern VkInputAttachmentAspectReferenceKHR :: ("subpass" ::: Word32) -> ("inputAttachmentIndex" ::: Word32) -> ("aspectMask" ::: VkImageAspectFlags) -> VkInputAttachmentAspectReferenceKHR
pattern VkInputAttachmentAspectReferenceKHR vkSubpass vkInputAttachmentIndex vkAspectMask = VkInputAttachmentAspectReference vkSubpass vkInputAttachmentIndex vkAspectMask
-- No documentation found for TopLevel "VkRenderPassInputAttachmentAspectCreateInfoKHR"
type VkRenderPassInputAttachmentAspectCreateInfoKHR = VkRenderPassInputAttachmentAspectCreateInfo


-- No documentation found for TopLevel "VkRenderPassInputAttachmentAspectCreateInfoKHR"
pattern VkRenderPassInputAttachmentAspectCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("aspectReferenceCount" ::: Word32) -> ("pAspectReferences" ::: Ptr VkInputAttachmentAspectReference) -> VkRenderPassInputAttachmentAspectCreateInfoKHR
pattern VkRenderPassInputAttachmentAspectCreateInfoKHR vkSType vkPNext vkAspectReferenceCount vkPAspectReferences = VkRenderPassInputAttachmentAspectCreateInfo vkSType vkPNext vkAspectReferenceCount vkPAspectReferences
-- No documentation found for TopLevel "VkPhysicalDevicePointClippingPropertiesKHR"
type VkPhysicalDevicePointClippingPropertiesKHR = VkPhysicalDevicePointClippingProperties


-- No documentation found for TopLevel "VkPhysicalDevicePointClippingPropertiesKHR"
pattern VkPhysicalDevicePointClippingPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("pointClippingBehavior" ::: VkPointClippingBehavior) -> VkPhysicalDevicePointClippingPropertiesKHR
pattern VkPhysicalDevicePointClippingPropertiesKHR vkSType vkPNext vkPointClippingBehavior = VkPhysicalDevicePointClippingProperties vkSType vkPNext vkPointClippingBehavior
-- No documentation found for TopLevel "VkImageViewUsageCreateInfoKHR"
type VkImageViewUsageCreateInfoKHR = VkImageViewUsageCreateInfo


-- No documentation found for TopLevel "VkImageViewUsageCreateInfoKHR"
pattern VkImageViewUsageCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("usage" ::: VkImageUsageFlags) -> VkImageViewUsageCreateInfoKHR
pattern VkImageViewUsageCreateInfoKHR vkSType vkPNext vkUsage = VkImageViewUsageCreateInfo vkSType vkPNext vkUsage
-- No documentation found for TopLevel "VkPipelineTessellationDomainOriginStateCreateInfoKHR"
type VkPipelineTessellationDomainOriginStateCreateInfoKHR = VkPipelineTessellationDomainOriginStateCreateInfo


-- No documentation found for TopLevel "VkPipelineTessellationDomainOriginStateCreateInfoKHR"
pattern VkPipelineTessellationDomainOriginStateCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("domainOrigin" ::: VkTessellationDomainOrigin) -> VkPipelineTessellationDomainOriginStateCreateInfoKHR
pattern VkPipelineTessellationDomainOriginStateCreateInfoKHR vkSType vkPNext vkDomainOrigin = VkPipelineTessellationDomainOriginStateCreateInfo vkSType vkPNext vkDomainOrigin
-- No documentation found for TopLevel "VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR"
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR = VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
-- No documentation found for TopLevel "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR"
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR = VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR"
pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR = VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR"
pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR :: VkImageLayout
pattern VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR = VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
-- No documentation found for TopLevel "VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR"
pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR :: VkPointClippingBehavior
pattern VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR = VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES
-- No documentation found for TopLevel "VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR"
pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR :: VkPointClippingBehavior
pattern VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR = VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY
-- No documentation found for TopLevel "VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR"
pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR :: VkTessellationDomainOrigin
pattern VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR = VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT
-- No documentation found for TopLevel "VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR"
pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR :: VkTessellationDomainOrigin
pattern VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR = VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT
