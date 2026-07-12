{-| A 'FG.Resource' for Vulkan images that places layout-transition barriers
automatically.

A 'ManagedImage' carries the image plus its tracked 'ImageState' (layout,
stage, access). A pass declares an access with a 'Usage' (the instance's
'FG.Flags' type); the 'FG.preRead' / 'FG.preWrite' hooks diff the
tracked state against the usage's target and queue the barrier into the
'Recorder''s per-pass batch, one @vkCmdPipelineBarrier@ per pass — the
'transitionImageTo' rules, plus semaphore chaining when the access hops
queues.

Import-only: the graph tracks the layout and places barriers but does not own
the allocation (see the package README).
-}
module Vulkan.Utils.FrameGraph.Image
  ( ManagedImage (..)
  , newManagedImage
  , newManagedImageMip
  , newManagedImageLayer
  , newManagedImageSlice
  , ImageDesc (..)
  , importManagedImage
  , importScratchImage
  , describedAs
  , imageInfo
  , describedImage
  , describedMip
  , describedSlice
  , ImageState (..)
  , undefinedState
  , Usage (..)
  , usageState
  , transitionImageTo
  , transitionImagesTo
  , sliceLayers
  , copyManagedImageToHost
  ) where

import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Word (Word32)

import Fragr qualified as FG
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.Core10 qualified as Vk
import Vulkan.Utils.FrameGraph.Recorder (Recorder, flushBarriers, queueBarrier, recorderQueue)
import Vulkan.Zero (zero)

{- | An image, or an arbitrary @(mip × array-layer)@ slice of it, whose
layout/stage/access the frame graph tracks and transitions.

The @range@ is the barrier's subresource range, so any slicing granularity is one
'ManagedImage' per slice over the same 'Vk.Image', each tracked independently — the
intra-image barriers fall out of that. A whole-image wrapper ('newManagedImage')
covers all mips+layers as one unit (e.g. a multiview render); per-mip
('newManagedImageMip', a bloom pyramid) or per-layer ('newManagedImageLayer', a
cubemap face / array element) wrappers give finer control. Slices tracked
separately must not overlap.
-}
data ManagedImage = ManagedImage
  { image :: Vk.Image
  , range :: Vk.ImageSubresourceRange
  , stateRef :: IORef ImageState
  , queueRef :: IORef FG.QueueId
  , info :: Text
  {- ^ Human-readable summary (format/extent, see 'imageInfo') shown by
  visualization output; attach with 'describedAs'.
  -}
  }

-- | Wrap a whole image (all mips + layers, monolithic), starting from 'undefinedState'.
newManagedImage :: (MonadIO m) => Vk.Image -> Vk.ImageAspectFlags -> m ManagedImage
{-# INLINE newManagedImage #-}
newManagedImage image aspect = newManaged image (Vk.ImageSubresourceRange aspect 0 Vk.REMAINING_MIP_LEVELS 0 Vk.REMAINING_ARRAY_LAYERS)

-- | Wrap a single mip level (all its layers), tracked independently of the others.
newManagedImageMip :: (MonadIO m) => Vk.Image -> Vk.ImageAspectFlags -> Word32 -> m ManagedImage
{-# INLINE newManagedImageMip #-}
newManagedImageMip image aspect mip = newManaged image (Vk.ImageSubresourceRange aspect mip 1 0 1)

-- | Wrap a single array layer / cubemap face (mip 0), tracked independently.
newManagedImageLayer :: (MonadIO m) => Vk.Image -> Vk.ImageAspectFlags -> Word32 -> m ManagedImage
{-# INLINE newManagedImageLayer #-}
newManagedImageLayer image aspect layer = newManaged image (Vk.ImageSubresourceRange aspect 0 1 layer 1)

-- | Wrap an arbitrary @(mip × layer)@ slice (e.g. one light's 6 cube faces in an array).
newManagedImageSlice :: (MonadIO m) => Vk.Image -> Vk.ImageAspectFlags -> Word32 -> Word32 -> Word32 -> Word32 -> m ManagedImage
{-# INLINE newManagedImageSlice #-}
newManagedImageSlice image aspect baseMip levelCount baseLayer layerCount =
  newManaged image (Vk.ImageSubresourceRange aspect baseMip levelCount baseLayer layerCount)

newManaged :: (MonadIO m) => Vk.Image -> Vk.ImageSubresourceRange -> m ManagedImage
{-# INLINE newManaged #-}
newManaged image range = do
  stateRef <- liftIO (newIORef undefinedState)
  queueRef <- liftIO (newIORef (FG.QueueId 0))
  pure ManagedImage{image, range, stateRef, queueRef, info = ""}

{- | Attach a summary (e.g. 'imageInfo') shown next to the resource's name
in visualization output.
-}
describedAs :: Text -> ManagedImage -> ManagedImage
describedAs t mi = mi{info = t}

-- | The conventional 'describedAs' summary: the format (sans prefix) and extent.
imageInfo :: Vk.Format -> Vk.Extent2D -> Text
imageInfo format (Vk.Extent2D w h) =
  Text.pack (drop (Text.length "FORMAT_") (show format) <> " " <> show w <> "x" <> show h)

{- | 'newManagedImage' with the 'imageInfo' description attached, stating the
allocation's format/extent once.
-}
describedImage :: (MonadIO m) => Vk.Format -> Vk.Extent2D -> Vk.Image -> Vk.ImageAspectFlags -> m ManagedImage
describedImage format ext image aspect = describedAs (imageInfo format ext) <$> newManagedImage image aspect

-- | 'newManagedImageMip' with the mip's 'imageInfo' description attached.
describedMip :: (MonadIO m) => Vk.Format -> Vk.Extent2D -> Vk.Image -> Vk.ImageAspectFlags -> Word32 -> m ManagedImage
describedMip format ext image aspect mip = describedAs (imageInfo format ext) <$> newManagedImageMip image aspect mip

-- | A mip-0 layer range via 'newManagedImageSlice', with the 'imageInfo' description attached.
describedSlice :: (MonadIO m) => Vk.Format -> Vk.Extent2D -> Vk.Image -> Vk.ImageAspectFlags -> Word32 -> Word32 -> m ManagedImage
describedSlice format ext image aspect baseLayer layerCount = describedAs (imageInfo format ext) <$> newManagedImageSlice image aspect 0 1 baseLayer layerCount

instance FG.Resource ManagedImage where
  type Desc ManagedImage = ImageDesc
  type Alloc ManagedImage = ()
  type Ctx ManagedImage = Recorder
  type Flags ManagedImage = Usage

  createResource _ _ =
    error "ManagedImage is import-only: allocate the image and use importResource"

  destroyResource _ _ _ = pure ()

  preRead _ usage rec mi = queueTransition rec mi usage
  preWrite _ usage rec mi = queueTransition rec mi usage

  -- Producer-side handoff: transition into the consuming access's state in
  -- the producing queue's buffer (fired only for cross-queue data edges).
  preRelease _ usage rec mi = queueTransition rec mi usage

  describeDesc d = d.info

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
access. The per-access payload of 'FG.readWith' / 'FG.writeWith'.
-}
data Usage
  = ColorAttachment
  | DepthAttachment
  | TransferSrc
  | TransferDst
  | Present
  | -- | Storage read/write in the given shader stage (compute, fragment, …).
    StorageRead Vk.PipelineStageFlags
  | StorageWrite Vk.PipelineStageFlags
  | -- | Sampled in the given shader stage (fragment, compute, …).
    Sampled Vk.PipelineStageFlags
  | -- | Read by the host after a fence (@GENERAL@, the layout mapped linear images live in).
    HostRead
  deriving stock (Eq, Ord, Show)

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
  StorageRead stage ->
    ImageState Vk.IMAGE_LAYOUT_GENERAL stage Vk.ACCESS_SHADER_READ_BIT
  StorageWrite stage ->
    ImageState Vk.IMAGE_LAYOUT_GENERAL stage Vk.ACCESS_SHADER_WRITE_BIT
  Sampled stage ->
    ImageState Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL stage Vk.ACCESS_SHADER_READ_BIT
  HostRead ->
    ImageState Vk.IMAGE_LAYOUT_GENERAL Vk.PIPELINE_STAGE_HOST_BIT Vk.ACCESS_HOST_READ_BIT

{- | Whether the 'Usage' writes the image (and so needs a barrier even when the
state is unchanged — only read-after-read can skip it).
-}
usageWrites :: Usage -> Bool
usageWrites = \case
  ColorAttachment -> True
  DepthAttachment -> True
  TransferDst -> True
  StorageWrite _ -> True
  TransferSrc -> False
  Present -> False
  StorageRead _ -> False
  Sampled _ -> False
  HostRead -> False

{- | Record the barrier bringing the image into the 'Usage''s state and update
the tracked state. Standalone counterpart to the hook path, for barriers
recorded outside a pass; treats the access as same-queue.

A write 'Usage' records the barrier even when the state is unchanged — a
same-state write still needs the execution+memory dependency against the
previous access. Only a read of an already-matching state skips it.
-}
transitionImageTo :: (MonadIO m) => Vk.CommandBuffer -> ManagedImage -> Usage -> m ()
{-# INLINE transitionImageTo #-}
transitionImageTo cb mi usage = transitionImagesTo cb [(mi, usage)]

{- | 'transitionImageTo' over a batch: one @vkCmdPipelineBarrier@, OR-ed stage masks.

The images must be tracked separately (distinct non-overlapping slices):
barriers in one command are unordered, so two entries for the same slice
would race.
-}
transitionImagesTo :: (MonadIO m) => Vk.CommandBuffer -> [(ManagedImage, Usage)] -> m ()
transitionImagesTo cb accesses = do
  (srcs, dsts, barriers) <- foldM collect (zero, zero, []) accesses
  unless (null barriers) $
    Vk.cmdPipelineBarrier cb srcs dsts zero [] [] (V.fromList barriers)
  where
    collect acc@(srcs, dsts, barriers) (mi, usage) = do
      lastQueue <- liftIO (readIORef mi.queueRef)
      nextTransition lastQueue mi usage >>= \case
        Nothing -> pure acc
        Just (src, dst, barrier) -> pure (srcs .|. src, dsts .|. dst, barrier : barriers)

{- | Copy an image into a host-readable one via the trackers.

The source moves to @TRANSFER_SRC@ from whatever state it is actually in, the
destination through @TRANSFER_DST@ to 'HostRead' — no assumed layouts, no
hand-rolled host barrier. Copies the first mip and layer of each wrapper's
slice (the aspects must match).
-}
copyManagedImageToHost :: (MonadIO m) => Vk.CommandBuffer -> Vk.Extent2D -> ManagedImage -> ManagedImage -> m ()
copyManagedImageToHost cb (Vk.Extent2D w h) src cpu = do
  transitionImagesTo cb [(src, TransferSrc), (cpu, TransferDst)]
  Vk.cmdCopyImage
    cb
    src.image
    (usageState TransferSrc).layout
    cpu.image
    (usageState TransferDst).layout
    [Vk.ImageCopy (sliceLayers src) (Vk.Offset3D 0 0 0) (sliceLayers cpu) (Vk.Offset3D 0 0 0) (Vk.Extent3D w h 1)]
  transitionImageTo cb cpu HostRead

-- | The slice's first mip and layer, as a transfer command's subresource.
sliceLayers :: ManagedImage -> Vk.ImageSubresourceLayers
sliceLayers mi = Vk.ImageSubresourceLayers mi.range.aspectMask mi.range.baseMipLevel mi.range.baseArrayLayer 1

{- | The hook path: 'transitionImageTo' rules, but queued and queue-aware.

The barrier goes into the 'Recorder''s per-pass batch (flushed before the
exec callback), and when the access lands on a different queue than the
previous one it is chained to the driver's inter-queue semaphore instead —
source scope becomes the destination stage with no access mask, since the
semaphore already provides execution ordering and memory availability. The
driver's wait @dstStageMask@ must cover the usage's stage (both then chain),
and cross-family access needs CONCURRENT sharing: no ownership
release/acquire pair is emitted, so an EXCLUSIVE image's contents are
undefined on the new family.
-}
queueTransition :: (MonadIO m) => Recorder -> ManagedImage -> Usage -> m ()
queueTransition rec mi usage = do
  queue <- recorderQueue rec
  nextTransition queue mi usage >>= traverse_ \(srcStage, dstStage, barrier) ->
    queueBarrier rec srcStage dstStage barrier

{- | Diff the tracked state against the 'Usage''s target and advance it.

Hands back the @(srcStage, dstStage, barrier)@ still to be recorded — the
caller commits to recording it (immediately or batched) before the access
runs.
-}
nextTransition
  :: (MonadIO m)
  => FG.QueueId
  -> ManagedImage
  -> Usage
  -> m (Maybe (Vk.PipelineStageFlags, Vk.PipelineStageFlags, SomeStruct Vk.ImageMemoryBarrier))
nextTransition queue mi usage = liftIO do
  cur <- readIORef mi.stateRef
  lastQueue <- readIORef mi.queueRef
  let
    next = usageState usage
    cross = queue /= lastQueue
    srcStage = if cross then next.stage else cur.stage
    srcAccess = if cross then zero else cur.access
    -- Cross-queue same-state accesses are ordered by the semaphore alone;
    -- same-queue writes need the barrier even with the state unchanged.
    needed = cur /= next || (usageWrites usage && not cross)
  when cross (writeIORef mi.queueRef queue)
  if needed
    then do
      writeIORef mi.stateRef next
      pure $
        Just
          ( srcStage
          , next.stage
          , SomeStruct
              zero
                { Vk.srcAccessMask = srcAccess
                , Vk.dstAccessMask = next.access
                , Vk.oldLayout = cur.layout
                , Vk.newLayout = next.layout
                , -- IGNORED (not 0) so the barrier is a plain transition, no
                  -- ownership transfer: cross-family access needs CONCURRENT sharing.
                  Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.image = mi.image
                , Vk.subresourceRange = mi.range
                }
          )
    else pure Nothing

{- | Descriptor for a 'ManagedImage'; carries the image's 'describedAs'
summary for visualization output (the resource name travels separately).
-}
newtype ImageDesc = ImageDesc {info :: Text}

{- | Import a 'ManagedImage' under @name@, as an observed resource.

Also claims the graph's 'FG.addPreExec slot for 'flushBarriers', so the
hook-queued barriers are recorded under any driver — the adapter owns that
slot; wrap the flush rather than replacing it.

Writers of the image become side effects ('FG.importResource'): right for
presentables and anything read outside the graph (readbacks, a next-frame
sampler). For targets only this graph's passes consume, use
'importScratchImage' so demand culling applies.
-}
importManagedImage :: (MonadIO m) => FG.FrameGraph Recorder () -> Text -> ManagedImage -> m (FG.Handle ManagedImage)
importManagedImage graph name mi = do
  FG.addPreExec graph flushBarriers
  FG.importResource graph name (ImageDesc mi.info) mi

{- | 'importManagedImage' via 'FG.importScratch', keeping writers subject to demand culling.

The image (and its layout tracking) persists between graphs, but its contents
are only ever consumed through this graph. Passes that feed a between-graphs
consumer must say 'FG.setSideEffect' themselves.
-}
importScratchImage :: (MonadIO m) => FG.FrameGraph Recorder () -> Text -> ManagedImage -> m (FG.Handle ManagedImage)
importScratchImage graph name mi = do
  FG.addPreExec graph flushBarriers
  FG.importScratch graph name (ImageDesc mi.info) mi
