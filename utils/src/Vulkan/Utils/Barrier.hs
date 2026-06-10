{-# LANGUAGE OverloadedLists #-}

{-| Whole-resource pipeline barriers. The @transition*@ helpers issue the
common swapchain-and-attachment transitions as complete
'Vk.cmdPipelineBarrier' calls; 'imageBarrier' is the building block behind
them, exposed for assembling barriers they don't cover (handing a rendered
image to a compute pass, a storage image to a blit, …) — combining several
into one 'Vk.cmdPipelineBarrier' call where the stage scopes allow.
'bufferBarrier' is its buffer-flavoured sibling (a compute-written vertex
SSBO handed to the vertex shader, …).
-}
module Vulkan.Utils.Barrier
  ( transitionColorAttachment
  , transitionPresent
  , transitionDepthAttachment
  , imageBarrier
  , bufferBarrier
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Bits ((.|.))
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

transitionColorAttachment :: (MonadIO m) => Vk.CommandBuffer -> Vk.Image -> m ()
transitionColorAttachment cb image =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        zero
        Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        Vk.IMAGE_LAYOUT_UNDEFINED
        Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        image
    ]

transitionPresent :: (MonadIO m) => Vk.CommandBuffer -> Vk.Image -> m ()
transitionPresent cb image =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        zero
        Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
        image
    ]

{- | Transition a depth image from @UNDEFINED@ (discarding any previous contents)
to @DEPTH_ATTACHMENT_OPTIMAL@, ready to be used as a depth attachment. Issue it
before rendering whenever the attachment is about to be cleared on load — every
frame, like the colour images.

The destination scope covers both fragment-test stages with read and write
access: the @loadOp@ clear and late depth writes are writes, the depth test
itself is a read, and all of them must see the transition.
-}
transitionDepthAttachment :: (MonadIO m) => Vk.CommandBuffer -> Vk.Image -> m ()
transitionDepthAttachment cb image =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    (Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT)
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_DEPTH_BIT
        zero
        ( Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
            .|. Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
        )
        Vk.IMAGE_LAYOUT_UNDEFINED
        Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
        image
    ]

{- | A whole-image memory barrier over the given aspect, from one
(access mask, layout) scope to another, with no queue-family ownership
transfer. The execution scopes (stage masks) live on the enclosing
'Vk.cmdPipelineBarrier' call, shared by every barrier in it.
-}
imageBarrier
  :: Vk.ImageAspectFlags
  -> Vk.AccessFlags
  -- ^ Source access mask.
  -> Vk.AccessFlags
  -- ^ Destination access mask.
  -> Vk.ImageLayout
  -- ^ Old layout; @UNDEFINED@ discards the image's previous contents.
  -> Vk.ImageLayout
  -- ^ New layout.
  -> Vk.Image
  -> SomeStruct Vk.ImageMemoryBarrier
imageBarrier aspect srcAccessMask dstAccessMask oldLayout newLayout image =
  SomeStruct
    zero
      { Vk.srcAccessMask = srcAccessMask
      , Vk.dstAccessMask = dstAccessMask
      , Vk.oldLayout = oldLayout
      , Vk.newLayout = newLayout
      , Vk.image = image
      , Vk.subresourceRange =
          zero
            { Vk.aspectMask = aspect
            , Vk.baseMipLevel = 0
            , Vk.levelCount = 1
            , Vk.baseArrayLayer = 0
            , Vk.layerCount = 1
            }
      }

{- | A whole-buffer memory barrier from one access scope to another, with no
queue-family ownership transfer. As with 'imageBarrier', the execution
scopes (stage masks) live on the enclosing 'Vk.cmdPipelineBarrier' call.
-}
bufferBarrier
  :: Vk.AccessFlags
  -- ^ Source access mask.
  -> Vk.AccessFlags
  -- ^ Destination access mask.
  -> Vk.Buffer
  -> SomeStruct Vk.BufferMemoryBarrier
bufferBarrier srcAccessMask dstAccessMask buffer =
  SomeStruct
    zero
      { Vk.srcAccessMask = srcAccessMask
      , Vk.dstAccessMask = dstAccessMask
      , Vk.buffer = buffer
      , Vk.offset = 0
      , Vk.size = Vk.WHOLE_SIZE
      }
