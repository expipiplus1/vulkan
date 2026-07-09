{-| A 'FG.Resource' for Vulkan images that places layout-transition barriers
automatically.

A 'ManagedImage' carries the image plus its tracked 'ImageState' (layout,
stage, access). A pass declares an access with a 'Usage' (encoded into
'FG.Flags' by 'usageFlags'); the 'FG.preRead' / 'FG.preWrite' hooks diff the
tracked state against the usage's target and record the
'Vk.cmdPipelineBarrier' and update the tracked state. Only a read of an
already-matching state skips the barrier; a write always records one (WAW).
Accesses that hop queues chain to the driver's semaphore instead (see
'transitionImageTo').

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
import Data.Bits ((.&.), (.|.))
import Data.Coerce (coerce)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Word (Word32)

import Fragr qualified as FG
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.Core10 qualified as Vk
import Vulkan.Utils.FrameGraph.Recorder (Recorder, recorderCommandBuffer, recorderQueue)
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
  pure ManagedImage{image, range, stateRef, queueRef}

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
  | -- | Storage read/write in the given shader stage (compute, fragment, …).
    StorageRead Vk.PipelineStageFlags
  | StorageWrite Vk.PipelineStageFlags
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
  StorageRead stage ->
    ImageState Vk.IMAGE_LAYOUT_GENERAL stage Vk.ACCESS_SHADER_READ_BIT
  StorageWrite stage ->
    ImageState Vk.IMAGE_LAYOUT_GENERAL stage Vk.ACCESS_SHADER_WRITE_BIT

{- | Encode a 'Usage' as the 'FG.Flags' passed to 'FG.readWith' / 'FG.writeWith'.
The six fixed usages are small tags; the storage usages set the high marker bit
and pack their read/write bit and shader stage into the rest — so the all-ones
'FG.flagsIgnored' sentinel is never produced.
-}
usageFlags :: Usage -> FG.Flags
usageFlags = \case
  ColorAttachment -> FG.Flags 0
  DepthAttachment -> FG.Flags 1
  SampledFragment -> FG.Flags 2
  TransferSrc -> FG.Flags 3
  TransferDst -> FG.Flags 4
  Present -> FG.Flags 5
  StorageRead stage -> FG.Flags (storageMarker .|. stageBits stage)
  StorageWrite stage -> FG.Flags (storageMarker .|. writeBit .|. stageBits stage)

-- | Decode 'FG.Flags' produced by 'usageFlags'.
flagsUsage :: FG.Flags -> Usage
flagsUsage (FG.Flags w)
  | w .&. storageMarker /= 0 =
      let stage = coerce (w .&. stageMask)
      in if w .&. writeBit /= 0 then StorageWrite stage else StorageRead stage
  | otherwise = case w of
      0 -> ColorAttachment
      1 -> DepthAttachment
      2 -> SampledFragment
      3 -> TransferSrc
      4 -> TransferDst
      _ -> Present

-- Bit layout packing a storage usage's stage into the 32-bit 'FG.Flags'.
storageMarker, writeBit, stageMask :: Word32
storageMarker = 0x80000000
writeBit = 0x40000000
stageMask = 0x3FFFFFFF

stageBits :: Vk.PipelineStageFlags -> Word32
stageBits stage = coerce stage .&. stageMask

{- | Whether the 'Usage' writes the image (and so needs a barrier even when the
state is unchanged — only read-after-read can skip it).
-}
usageWrites :: Usage -> Bool
usageWrites = \case
  ColorAttachment -> True
  DepthAttachment -> True
  TransferDst -> True
  StorageWrite _ -> True
  SampledFragment -> False
  TransferSrc -> False
  Present -> False
  StorageRead _ -> False

{- | Record the barrier bringing the image into the 'Usage''s state and update
the tracked state. Standalone counterpart to the hook path, for barriers
recorded outside a pass; treats the access as same-queue.

A write 'Usage' records the barrier even when the state is unchanged — a
same-state write still needs the execution+memory dependency against the
previous access. Only a read of an already-matching state skips it.
-}
transitionImageTo :: (MonadIO m) => Vk.CommandBuffer -> ManagedImage -> Usage -> m ()
{-# INLINE transitionImageTo #-}
transitionImageTo cb mi usage = do
  lastQueue <- liftIO (readIORef mi.queueRef)
  transitionOnQueue cb lastQueue mi usage

{- | The hook path: like 'transitionImageTo', but when the access lands on a
different queue than the previous one the barrier is chained to the driver's
inter-queue semaphore instead — source scope becomes the destination stage with
no access mask, since the semaphore already provides execution ordering and
memory availability. The driver's wait @dstStageMask@ must cover the usage's
stage (both then chain), and cross-family access needs CONCURRENT sharing: no
ownership release/acquire pair is emitted, so an EXCLUSIVE image's contents are
undefined on the new family.
-}
transitionOnQueue :: (MonadIO m) => Vk.CommandBuffer -> FG.QueueId -> ManagedImage -> Usage -> m ()
transitionOnQueue cb queue mi usage = do
  cur <- liftIO (readIORef mi.stateRef)
  lastQueue <- liftIO (readIORef mi.queueRef)
  let
    next = usageState usage
    cross = queue /= lastQueue
    srcStage = if cross then next.stage else cur.stage
    srcAccess = if cross then zero else cur.access
    -- Cross-queue same-state accesses are ordered by the semaphore alone;
    -- same-queue writes need the barrier even with the state unchanged.
    needed = cur /= next || (usageWrites usage && not cross)
  when needed do
    Vk.cmdPipelineBarrier
      cb
      srcStage
      next.stage
      zero
      []
      []
      [ SomeStruct
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
      ]
    liftIO (writeIORef mi.stateRef next)
  liftIO (writeIORef mi.queueRef queue)

-- | Descriptor for a 'ManagedImage'; carries a label for visualization output.
newtype ImageDesc = ImageDesc {label :: Text}

{- | Import a 'ManagedImage' under @name@, labelling the graph node with the same
name (so the label never drifts from the handle).
-}
importManagedImage :: (MonadIO m) => FG.FrameGraph Recorder () -> Text -> ManagedImage -> m FG.Handle
importManagedImage graph name mi = FG.importResource graph name (ImageDesc name) mi

instance FG.Resource ManagedImage where
  type Desc ManagedImage = ImageDesc
  type Alloc ManagedImage = ()
  type Ctx ManagedImage = Recorder

  createResource _ _ =
    error "ManagedImage is import-only: allocate the image and use importResource"

  destroyResource _ _ _ = pure ()

  preRead _ flags rec mi = do
    cb <- recorderCommandBuffer rec
    queue <- recorderQueue rec
    transitionOnQueue cb queue mi (flagsUsage flags)
  preWrite _ flags rec mi = do
    cb <- recorderCommandBuffer rec
    queue <- recorderQueue rec
    transitionOnQueue cb queue mi (flagsUsage flags)

  describeDesc d = d.label
