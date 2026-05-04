{-# LANGUAGE OverloadedLists #-}

-- | Tiny helpers for the boilerplate that each rendering example needs:
-- a framebuffer over a single image view, and a vanilla 2D color image view.
module Framebuffer
  ( Framebuffer.createFramebuffer
  , Framebuffer.createImageView
  ) where

import           Control.Monad.Trans.Resource   ( MonadResource
                                                , ReleaseKey
                                                , allocate
                                                )
import           Vulkan.Core10                 as Vk
                                         hiding ( withImage )
import           Vulkan.Core10                 as Extent2D (Extent2D(..))
import           Vulkan.Core10                 as ImageViewCreateInfo
                                                ( ImageViewCreateInfo(..) )
import           Vulkan.Zero

-- | Create a framebuffer covering the whole image with a single attachment.
createFramebuffer
  :: MonadResource m
  => Device
  -> RenderPass
  -> ImageView
  -> Extent2D
  -> m (ReleaseKey, Framebuffer)
createFramebuffer dev renderPass imageView imageSize =
  let framebufferCreateInfo :: FramebufferCreateInfo '[]
      framebufferCreateInfo = zero { renderPass  = renderPass
                                   , attachments = [imageView]
                                   , width       = Extent2D.width imageSize
                                   , height      = Extent2D.height imageSize
                                   , layers      = 1
                                   }
  in  withFramebuffer dev framebufferCreateInfo Nothing allocate

-- | Vanilla 2D color image view covering the whole image.
createImageView
  :: MonadResource m
  => Device
  -> Format
  -> Image
  -> m (ReleaseKey, ImageView)
createImageView dev format image =
  withImageView dev imageViewCreateInfo Nothing allocate
 where
  imageViewCreateInfo = zero
    { ImageViewCreateInfo.image = image
    , viewType                  = IMAGE_VIEW_TYPE_2D
    , format                    = format
    , components                = zero { r = COMPONENT_SWIZZLE_IDENTITY
                                       , g = COMPONENT_SWIZZLE_IDENTITY
                                       , b = COMPONENT_SWIZZLE_IDENTITY
                                       , a = COMPONENT_SWIZZLE_IDENTITY
                                       }
    , subresourceRange          = zero { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                                       , baseMipLevel   = 0
                                       , levelCount     = 1
                                       , baseArrayLayer = 0
                                       , layerCount     = 1
                                       }
    }
