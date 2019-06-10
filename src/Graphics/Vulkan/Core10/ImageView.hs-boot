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
  , VkImageView
  , VkImageViewCreateFlagBits
  , VkImageViewType
  )


-- No documentation found for TopLevel "ComponentSwizzle"
type ComponentSwizzle = VkComponentSwizzle

-- No documentation found for TopLevel "ImageView"
type ImageView = VkImageView

-- No documentation found for TopLevel "ImageViewCreateFlagBits"
type ImageViewCreateFlagBits = VkImageViewCreateFlagBits

-- No documentation found for TopLevel "ImageViewCreateFlags"
type ImageViewCreateFlags = ImageViewCreateFlagBits

-- No documentation found for TopLevel "ImageViewType"
type ImageViewType = VkImageViewType
