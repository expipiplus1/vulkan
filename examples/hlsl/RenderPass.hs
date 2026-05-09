{-# LANGUAGE OverloadedLists #-}

module RenderPass
  ( createRenderPass
  ) where

import Control.Monad.Trans.Resource
import Data.Bits
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

-- | Create a renderpass with a single subpass that clears + presents.
createRenderPass
  :: (MonadResource m)
  => Vk.Device
  -> Vk.Format
  -> m (ReleaseKey, Vk.RenderPass)
createRenderPass dev imageFormat =
  Vk.withRenderPass
    dev
    zero
      { Vk.attachments = [attachmentDescription]
      , Vk.subpasses = [subpass]
      , Vk.dependencies = [subpassDependency]
      }
    Nothing
    allocate
  where
    attachmentDescription :: Vk.AttachmentDescription
    attachmentDescription =
      zero
        { Vk.format = imageFormat
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
        , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
        , Vk.stencilLoadOp = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
        , Vk.stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        , Vk.finalLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
        }
    subpass :: Vk.SubpassDescription
    subpass =
      zero
        { Vk.pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
        , Vk.colorAttachments =
            [ zero
                { Vk.attachment = 0
                , Vk.layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                }
            ]
        }
    subpassDependency :: Vk.SubpassDependency
    subpassDependency =
      zero
        { Vk.srcSubpass = Vk.SUBPASS_EXTERNAL
        , Vk.dstSubpass = 0
        , Vk.srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.srcAccessMask = zero
        , Vk.dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
        , Vk.dstAccessMask =
            Vk.ACCESS_COLOR_ATTACHMENT_READ_BIT
              .|. Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        }
