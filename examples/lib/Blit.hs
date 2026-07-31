{-# LANGUAGE OverloadedLists #-}

-- | Shared image blit for the frame-graph examples.
module Blit
  ( blitImage
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Vulkan.Core10 as Vk

{- | Blit a whole colour image onto another of the same extent, source in
@TRANSFER_SRC_OPTIMAL@ and destination in @TRANSFER_DST_OPTIMAL@ (nearest
filter; handles the RGBA→BGRA swapchain conversion).
-}
blitImage :: (MonadIO m) => Vk.Extent2D -> Vk.Image -> Vk.Image -> Vk.CommandBuffer -> m ()
blitImage extent src dst cb =
  Vk.cmdBlitImage
    cb
    src
    Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    dst
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    [ Vk.ImageBlit
        { Vk.srcSubresource = layers
        , Vk.srcOffsets = offsets
        , Vk.dstSubresource = layers
        , Vk.dstOffsets = offsets
        }
    ]
    Vk.FILTER_NEAREST
  where
    layers = Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT 0 0 1
    offsets = case extent of
      Vk.Extent2D w h -> (Vk.Offset3D 0 0 0, Vk.Offset3D (fromIntegral w) (fromIntegral h) 1)
