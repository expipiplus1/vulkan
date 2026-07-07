{-| A 'FG.Resource' for Vulkan images that places layout-transition barriers
automatically.

A 'ManagedImage' carries the image plus its tracked 'ImageState' (layout,
stage, access). A pass declares an access with a 'Usage' (encoded into
'FG.Flags' by 'usageFlags'); the 'FG.preRead' / 'FG.preWrite' hooks diff the
tracked state against the usage's target and, when they differ, record the
'Vk.cmdPipelineBarrier' and update the tracked state.

Import-only: the graph tracks the layout and places barriers but does not own
the allocation (see the package README).
-}
module Vulkan.Utils.FrameGraph.Image
  ( ManagedImage (..)
  , newManagedImage
  , ImageDesc (..)
  , ImageState (..)
  , undefinedState
  , Usage (..)
  , usageState
  , usageFlags
  , flagsUsage
  , transitionImageTo
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)

import Fragr qualified as FG
import Vulkan.Core10 qualified as Vk
import Vulkan.Utils.Barrier (imageBarrier)
import Vulkan.Zero (zero)

-- | An image whose layout/stage/access the frame graph tracks and transitions.
data ManagedImage = ManagedImage
  { image :: Vk.Image
  , aspect :: Vk.ImageAspectFlags
  , stateRef :: IORef ImageState
  }

-- | Wrap an image, starting from 'undefinedState' (contents undefined).
newManagedImage :: (MonadIO m) => Vk.Image -> Vk.ImageAspectFlags -> m ManagedImage
{-# INLINE newManagedImage #-}
newManagedImage image aspect = do
  stateRef <- liftIO (newIORef undefinedState)
  pure ManagedImage{image, aspect, stateRef}

-- | The synchronization state an image is currently left in.
data ImageState = ImageState
  { layout :: Vk.ImageLayout
  , stage :: Vk.PipelineStageFlags
  , access :: Vk.AccessFlags
  }
  deriving stock (Eq, Show)

-- | Freshly created / never-transitioned: undefined layout, top of pipe.
undefinedState :: ImageState
undefinedState =
  ImageState
    { layout = Vk.IMAGE_LAYOUT_UNDEFINED
    , stage = Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    , access = zero
    }

{- | How a pass uses an image, i.e. the 'ImageState' it must be in for that
access. Encodes into 'FG.Flags' with 'usageFlags'.
-}
data Usage
  = ColorAttachment
  | DepthAttachment
  | SampledFragment
  | TransferSrc
  | TransferDst
  | Present
  | StorageWrite
  deriving stock (Eq, Ord, Enum, Bounded, Show)

{- | The target state each 'Usage' requires. Stage/access mirror the
@Vulkan.Utils.Barrier@ @transition*@ helpers.
-}
usageState :: Usage -> ImageState
usageState = \case
  ColorAttachment ->
    ImageState
      Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
      Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
  DepthAttachment ->
    ImageState
      Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
      (Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT)
      (Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT .|. Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)
  SampledFragment ->
    ImageState
      Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
      Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
      Vk.ACCESS_SHADER_READ_BIT
  TransferSrc ->
    ImageState
      Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      Vk.PIPELINE_STAGE_TRANSFER_BIT
      Vk.ACCESS_TRANSFER_READ_BIT
  TransferDst ->
    ImageState
      Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      Vk.PIPELINE_STAGE_TRANSFER_BIT
      Vk.ACCESS_TRANSFER_WRITE_BIT
  Present ->
    ImageState
      Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
      Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
      zero
  StorageWrite ->
    ImageState
      Vk.IMAGE_LAYOUT_GENERAL
      Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
      Vk.ACCESS_SHADER_WRITE_BIT

-- | Encode a 'Usage' as the 'FG.Flags' passed to 'FG.readWith' / 'FG.writeWith'.
usageFlags :: Usage -> FG.Flags
usageFlags = FG.Flags . fromIntegral . fromEnum

-- | Decode 'FG.Flags' produced by 'usageFlags'.
flagsUsage :: FG.Flags -> Usage
flagsUsage (FG.Flags w) = toEnum (fromIntegral w)

{- | Record the barrier bringing the image into the 'Usage''s state, if it is
not already in it, and update the tracked state. Standalone counterpart to the
hook path, for barriers recorded outside a pass.
-}
transitionImageTo :: (MonadIO m) => Vk.CommandBuffer -> ManagedImage -> Usage -> m ()
{-# INLINE transitionImageTo #-}
transitionImageTo cb mi usage = do
  cur <- liftIO (readIORef mi.stateRef)
  let next = usageState usage
  when (cur /= next) do
    Vk.cmdPipelineBarrier
      cb
      cur.stage
      next.stage
      zero
      []
      []
      [imageBarrier mi.aspect cur.access next.access cur.layout next.layout mi.image]
    liftIO (writeIORef mi.stateRef next)

-- | Descriptor for a 'ManagedImage'; carries a label for visualization output.
newtype ImageDesc = ImageDesc {label :: Text}

instance FG.Resource ManagedImage where
  type Desc ManagedImage = ImageDesc
  type Alloc ManagedImage = ()
  type Ctx ManagedImage = Vk.CommandBuffer

  createResource _ _ =
    error "ManagedImage is import-only: allocate the image and use importResource"

  destroyResource _ _ _ = pure ()

  preRead _ flags cb mi = transitionImageTo cb mi (flagsUsage flags)
  preWrite _ flags cb mi = transitionImageTo cb mi (flagsUsage flags)

  describeDesc d = d.label
