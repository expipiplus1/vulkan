{-# LANGUAGE OverloadedLists #-}

module Vulkan.Utils.ImageBarrier
  ( cmdTransitionForColorAttachment
  , cmdTransitionForPresent
  ) where

import Control.Monad.IO.Class (MonadIO)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

cmdTransitionForColorAttachment :: (MonadIO m) => Vk.CommandBuffer -> Vk.Image -> m ()
cmdTransitionForColorAttachment cb image =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    zero
    []
    []
    [ SomeStruct
        zero
          { Vk.srcAccessMask = zero
          , Vk.dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          , Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED
          , Vk.newLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , Vk.image = image
          , Vk.subresourceRange = colorSubresourceRange
          }
    ]

cmdTransitionForPresent :: (MonadIO m) => Vk.CommandBuffer -> Vk.Image -> m ()
cmdTransitionForPresent cb image =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    zero
    []
    []
    [ SomeStruct
        zero
          { Vk.srcAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
          , Vk.dstAccessMask = zero
          , Vk.oldLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
          , Vk.newLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
          , Vk.image = image
          , Vk.subresourceRange = colorSubresourceRange
          }
    ]

colorSubresourceRange :: Vk.ImageSubresourceRange
colorSubresourceRange =
  zero
    { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
    , Vk.baseMipLevel = 0
    , Vk.levelCount = 1
    , Vk.baseArrayLayer = 0
    , Vk.layerCount = 1
    }
