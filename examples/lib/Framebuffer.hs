{-# LANGUAGE OverloadedLists #-}

{-| Tiny helpers for the boilerplate that each rendering example needs:
a framebuffer over a single image view, and a vanilla 2D color image view.
-}
module Framebuffer
  ( Framebuffer.createFramebuffer
  , Framebuffer.createImageView
  , Framebuffer.createFramebuffers
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate, release)
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import qualified Data.Vector as V
import RefCounted (RefCounted, newRefCounted)
import Vulkan.Core10 as Extent2D (Extent2D (..))
import Vulkan.Core10 as ImageViewCreateInfo (ImageViewCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

-- | Create a framebuffer covering the whole image with a single attachment.
createFramebuffer
  :: (MonadResource m)
  => Vk.Device
  -> Vk.RenderPass
  -> Vk.ImageView
  -> Vk.Extent2D
  -> m (ReleaseKey, Vk.Framebuffer)
createFramebuffer dev renderPass imageView Vk.Extent2D{width, height} =
  Vk.withFramebuffer dev framebufferCreateInfo Nothing allocate
  where
    framebufferCreateInfo :: Vk.FramebufferCreateInfo '[]
    framebufferCreateInfo =
      zero
        { Vk.renderPass = renderPass
        , Vk.attachments = [imageView]
        , Vk.width = width
        , Vk.height = height
        , Vk.layers = 1
        }

{- | Build one framebuffer per image view at the given extent. The returned
'RefCounted' frees them all when no in-flight frame still uses them — call
'releaseRefCounted' after a swapchain swap.
-}
createFramebuffers
  :: (MonadResource m)
  => Vk.Device
  -> Vk.RenderPass
  -> Vector Vk.ImageView
  -> Vk.Extent2D
  -> m (Vector Vk.Framebuffer, RefCounted)
createFramebuffers dev rp ivs imageSize = do
  (keys, fbs) <- fmap V.unzip . V.forM ivs $ \iv ->
    Framebuffer.createFramebuffer dev rp iv imageSize
  rel <- newRefCounted (traverse_ release keys)
  pure (fbs, rel)

-- | Vanilla 2D color image view covering the whole image.
createImageView
  :: (MonadResource m)
  => Vk.Device
  -> Vk.Format
  -> Vk.Image
  -> m (ReleaseKey, Vk.ImageView)
createImageView dev format image =
  Vk.withImageView dev imageViewCreateInfo Nothing allocate
  where
    imageViewCreateInfo =
      zero
        { ImageViewCreateInfo.image = image
        , viewType = Vk.IMAGE_VIEW_TYPE_2D
        , format = format
        , components =
            zero
              { Vk.r = Vk.COMPONENT_SWIZZLE_IDENTITY
              , Vk.g = Vk.COMPONENT_SWIZZLE_IDENTITY
              , Vk.b = Vk.COMPONENT_SWIZZLE_IDENTITY
              , Vk.a = Vk.COMPONENT_SWIZZLE_IDENTITY
              }
        , subresourceRange =
            zero
              { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
              , Vk.baseMipLevel = 0
              , Vk.levelCount = 1
              , Vk.baseArrayLayer = 0
              , Vk.layerCount = 1
              }
        }
