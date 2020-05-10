{-# LANGUAGE OverloadedLists #-}
module Framebuffer
  ( Framebuffer.createFramebuffer
  ) where

import           Control.Monad.Trans.Resource
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Zero

import           MonadVulkan

-- Create the most vanilla rendering pipeline
createFramebuffer
  :: RenderPass -> ImageView -> Extent2D -> V (ReleaseKey, Framebuffer)
createFramebuffer renderPass imageView imageSize = do
  -- Create a framebuffer
  let framebufferCreateInfo :: FramebufferCreateInfo '[]
      framebufferCreateInfo = zero { renderPass  = renderPass
                                   , attachments = [imageView]
                                   , width       = width (imageSize :: Extent2D)
                                   , height = height (imageSize :: Extent2D)
                                   , layers      = 1
                                   }
  withFramebuffer' framebufferCreateInfo
