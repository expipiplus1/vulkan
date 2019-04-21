{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.ImageView
  ( ComponentSwizzle
  , ImageView
  , ImageViewCreateFlagBits
  , ImageViewCreateFlags
  , ImageViewType
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentSwizzle
  , VkImageViewCreateFlagBits
  , VkImageViewType
  , VkImageView
  )


-- | VkComponentSwizzle - Specify how a component is swizzled
--
-- = Description
--
-- Setting the identity swizzle on a component is equivalent to setting the
-- identity mapping on that component. That is:
--
-- > +-----------------------------------+-----------------------------------+
-- > | Component                         | Identity Mapping                  |
-- > +===================================+===================================+
-- > | @components.r@                    | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |                                   | ew.VK_COMPONENT_SWIZZLE_R'        |
-- > +-----------------------------------+-----------------------------------+
-- > | @components.g@                    | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |                                   | ew.VK_COMPONENT_SWIZZLE_G'        |
-- > +-----------------------------------+-----------------------------------+
-- > | @components.b@                    | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |                                   | ew.VK_COMPONENT_SWIZZLE_B'        |
-- > +-----------------------------------+-----------------------------------+
-- > | @components.a@                    | 'Graphics.Vulkan.C.Core10.ImageVi |
-- > |                                   | ew.VK_COMPONENT_SWIZZLE_A'        |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Component Mappings Equivalent To
-- > 'Graphics.Vulkan.C.Core10.ImageView.VK_COMPONENT_SWIZZLE_IDENTITY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkComponentMapping'
type ComponentSwizzle = VkComponentSwizzle

-- | VkImageView - Opaque handle to an image view object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkCreateImageView',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkDestroyImageView'
type ImageView = VkImageView

-- | VkImageViewCreateFlagBits - Bitmask specifying additional parameters of
-- an image view
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateFlags'
type ImageViewCreateFlagBits = VkImageViewCreateFlagBits

-- | VkImageViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateFlags' is a bitmask
-- type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo'
type ImageViewCreateFlags = ImageViewCreateFlagBits

-- | VkImageViewType - Image view types
--
-- = Description
--
-- The exact image view type is partially implicit, based on the image’s
-- type and sample count, as well as the view creation parameters as
-- described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-views-compatibility image view compatibility table>
-- for 'Graphics.Vulkan.C.Core10.ImageView.vkCreateImageView'. This table
-- also shows which SPIR-V @OpTypeImage@ @Dim@ and @Arrayed@ parameters
-- correspond to each image view type.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo'
type ImageViewType = VkImageViewType
