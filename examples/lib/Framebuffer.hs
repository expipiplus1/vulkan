{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Framebuffer
  ( Framebuffer.createFramebuffer
  , Framebuffer.createImageView
  ) where

import           AutoApply
import           Control.Monad.Trans.Resource
import           HasVulkan
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Core10 as Extent2D (Extent2D(..))
import           Vulkan.Core10 as ImageViewCreateInfo (ImageViewCreateInfo(..))
import           Vulkan.Zero

autoapplyDecs
  (<> "'")
  [ 'getDevice
  , 'getPhysicalDevice
  , 'getInstance
  , 'getAllocator
  , 'noAllocationCallbacks
  , 'noPipelineCache
  ]
  [ 'allocate ]
  [ 'withFramebuffer
  , 'withImageView
  ]

-- | Create a framebuffer filling the whole image.
createFramebuffer
  :: (MonadResource m, HasVulkan m)
  => RenderPass
  -> ImageView
  -> Extent2D
  -> m (ReleaseKey, Framebuffer)
createFramebuffer renderPass imageView imageSize = do
  -- Create a framebuffer
  let framebufferCreateInfo :: FramebufferCreateInfo '[]
      framebufferCreateInfo = zero { renderPass  = renderPass
                                   , attachments = [imageView]
                                   , width       = Extent2D.width imageSize
                                   , height      = Extent2D.height imageSize
                                   , layers      = 1
                                   }
  withFramebuffer' framebufferCreateInfo

-- | Create a pretty vanilla ImageView covering the whole image
createImageView
  :: (MonadResource m, HasVulkan m)
  => Format
  -> Image
  -> m (ReleaseKey, ImageView)
createImageView format = \image ->
  withImageView' imageViewCreateInfo { ImageViewCreateInfo.image = image }
 where
  imageViewCreateInfo = zero
    { viewType         = IMAGE_VIEW_TYPE_2D
    , format           = format
    , components       = zero { r = COMPONENT_SWIZZLE_IDENTITY
                              , g = COMPONENT_SWIZZLE_IDENTITY
                              , b = COMPONENT_SWIZZLE_IDENTITY
                              , a = COMPONENT_SWIZZLE_IDENTITY
                              }
    , subresourceRange = zero { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                              , baseMipLevel   = 0
                              , levelCount     = 1
                              , baseArrayLayer = 0
                              , layerCount     = 1
                              }
    }
