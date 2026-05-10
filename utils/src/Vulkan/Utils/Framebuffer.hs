{-# LANGUAGE OverloadedLists #-}

{-| Tiny helpers for the boilerplate that each rendering example needs:
a framebuffer over a single image view, and a vanilla 2D color image view.
-}
module Vulkan.Utils.Framebuffer
  ( createFramebuffer
  , createFramebuffers
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate, register, release)
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.Core10 as Extent2D (Extent2D (..))
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

{- | Build one framebuffer per image view at the given extent. Releasing the
returned 'ReleaseKey' frees them all — fire it after a swapchain swap.
-}
createFramebuffers
  :: (MonadResource m)
  => Vk.Device
  -> Vk.RenderPass
  -> Vector Vk.ImageView
  -> Vk.Extent2D
  -> m (Vector Vk.Framebuffer, ReleaseKey)
createFramebuffers dev rp ivs imageSize = do
  (keys, fbs) <- fmap V.unzip . V.forM ivs $ \iv ->
    createFramebuffer dev rp iv imageSize
  groupKey <- register (traverse_ release keys)
  pure (fbs, groupKey)
