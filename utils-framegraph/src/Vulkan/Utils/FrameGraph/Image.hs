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
  , SliceRegistry
  , newSliceRegistry
  , forgetImage
  , claimOwnership
  , ImageDesc (..)
  , importManagedImage
  , importScratchImage
  , importOwnedImage
  , describedAs
  , sharedAcrossQueues
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
  , queueTransition
  , transferOwnership
  , sliceLayers
  , copyManagedImageToHost
  ) where

import Control.Monad (filterM, foldM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits ((.&.), (.|.))
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', mkWeakIORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Word (Word32)
import GHC.Stack (HasCallStack)
import System.Mem (performGC)
import System.Mem.Weak (Weak, deRefWeak)

import Fragr qualified as FG
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.Core10 qualified as Vk
import Vulkan.Utils.FrameGraph.Recorder (Accessor (..), Recorder, TransferSide (..), chainedNode, flushBarriers, markChained, overlappingRanges, queueBarrier, recorderFamily, recorderHost, recorderQueue, recorderSameFamily)
import Vulkan.Zero (zero)

{- | An image, or an arbitrary @(mip × array-layer)@ slice of it, whose
layout/stage/access the frame graph tracks and transitions.

The @range@ is the barrier's subresource range, so any slicing granularity is one
'ManagedImage' per slice over the same 'Vk.Image', each tracked independently — the
intra-image barriers fall out of that. A whole-image wrapper ('newManagedImage')
covers all mips+layers as one unit (e.g. a multiview render); per-mip
('newManagedImageMip', a bloom pyramid) or per-layer ('newManagedImageLayer', a
cubemap face / array element) wrappers give finer control. Slices tracked
separately must not overlap — wrapping checks that against the image's live
wrappers in the renderer's 'SliceRegistry' and fails fast, since two trackers
over one subresource diverge silently (wrong old layouts, missed barriers).
-}
data ManagedImage = ManagedImage
  { image :: Vk.Image
  , range :: Vk.ImageSubresourceRange
  , stateRef :: IORef ImageState
  , queueRef :: IORef (Maybe FG.QueueId)
  -- ^ The device queue that last accessed it; 'Nothing' until one has.
  , releasedRef :: IORef (Maybe ImageState)
  {- ^ The state a pending ownership release saw, so the acquiring half can
  build the barrier that matches it exactly.
  -}
  , shared :: Bool
  {- ^ The allocation is @SHARING_MODE_CONCURRENT@ across the families the
  graph uses it on ('sharedAcrossQueues'). An unmarked resource accessed
  across queues is fatal: no ownership transfer is emitted, so its contents
  would be undefined on the new family.
  -}
  , info :: Text
  {- ^ Human-readable summary (format/extent, see 'imageInfo') shown by
  visualization output; attach with 'describedAs'.
  -}
  }

-- | Wrap a whole image (all mips + layers, monolithic), starting from 'undefinedState'.
newManagedImage :: (HasCallStack, MonadIO m) => SliceRegistry -> Vk.Image -> Vk.ImageAspectFlags -> m ManagedImage
{-# INLINE newManagedImage #-}
newManagedImage reg image aspect = newManaged reg image (Vk.ImageSubresourceRange aspect 0 Vk.REMAINING_MIP_LEVELS 0 Vk.REMAINING_ARRAY_LAYERS)

-- | Wrap a single mip level (all its layers), tracked independently of the others.
newManagedImageMip :: (HasCallStack, MonadIO m) => SliceRegistry -> Vk.Image -> Vk.ImageAspectFlags -> Word32 -> m ManagedImage
{-# INLINE newManagedImageMip #-}
newManagedImageMip reg image aspect mip = newManaged reg image (Vk.ImageSubresourceRange aspect mip 1 0 1)

-- | Wrap a single array layer / cubemap face (mip 0), tracked independently.
newManagedImageLayer :: (HasCallStack, MonadIO m) => SliceRegistry -> Vk.Image -> Vk.ImageAspectFlags -> Word32 -> m ManagedImage
{-# INLINE newManagedImageLayer #-}
newManagedImageLayer reg image aspect layer = newManaged reg image (Vk.ImageSubresourceRange aspect 0 1 layer 1)

-- | Wrap an arbitrary @(mip × layer)@ slice (e.g. one light's 6 cube faces in an array).
newManagedImageSlice :: (HasCallStack, MonadIO m) => SliceRegistry -> Vk.Image -> Vk.ImageAspectFlags -> Word32 -> Word32 -> Word32 -> Word32 -> m ManagedImage
{-# INLINE newManagedImageSlice #-}
newManagedImageSlice reg image aspect baseMip levelCount baseLayer layerCount =
  newManaged reg image (Vk.ImageSubresourceRange aspect baseMip levelCount baseLayer layerCount)

newManaged :: (HasCallStack, MonadIO m) => SliceRegistry -> Vk.Image -> Vk.ImageSubresourceRange -> m ManagedImage
{-# INLINE newManaged #-}
newManaged reg image range = liftIO do
  stateRef <- newIORef undefinedState
  registerSlice reg image range stateRef
  queueRef <- newIORef Nothing
  releasedRef <- newIORef Nothing
  pure ManagedImage{releasedRef, image, range, stateRef, queueRef, shared = False, info = ""}

{- | A registry of live wrappers per image, enforcing the non-overlap
contract at wrap time ('registerSlice').

One per renderer, created where the images' owning scope begins (the
@ResourceT@ the render loop runs in): replacing the renderer replaces the
registry, so a dead scope's wrappers cannot poison the next one's over the
persisting Vulkan context.
-}
newtype SliceRegistry = SliceRegistry (IORef (Map Vk.Image [SliceEntry]))

-- | One live wrapper's range in a 'SliceRegistry' bucket.
data SliceEntry = SliceEntry
  { range :: Vk.ImageSubresourceRange
  , weak :: Weak (IORef ImageState)
  }

newSliceRegistry :: (MonadIO m) => m SliceRegistry
newSliceRegistry = liftIO (SliceRegistry <$> newIORef Map.empty)

{- | Check the range against the image's live wrappers and record it, fatally
on overlap.

Entries are weak, keyed on each wrapper's 'stateRef': a dropped wrapper's
tracker can never diverge again, so its range frees on collection. That also
keeps a recycled 'Vk.Image' handle (destroy, then create getting the same
value) from clashing with the destroyed image's wrappers.
-}
registerSlice :: (HasCallStack) => SliceRegistry -> Vk.Image -> Vk.ImageSubresourceRange -> IORef ImageState -> IO ()
registerSlice (SliceRegistry registry) image range stateRef = do
  live0 <- pruneLive
  -- A clash may be a dropped wrapper the GC has not reached yet: collect
  -- before accusing.
  live <- if any clash live0 then performGC *> pruneLive else pure live0
  case filter clash live of
    [] -> pure ()
    clashes ->
      error
        ( "Vulkan.Utils.FrameGraph: wrapping "
            <> show range
            <> " of "
            <> show image
            <> " overlaps a live ManagedImage over "
            <> show (map (.range) clashes)
            <> "; slices tracked separately must not overlap"
        )
  -- No finalizer: dead entries are pruned on the image's next wrap (or by
  -- 'forgetImage'), keeping this insert the map's only writer — a finalizer
  -- racing it could resurrect the entry it just removed.
  weak <- mkWeakIORef stateRef (pure ())
  atomicModifyIORef' registry \m -> (Map.insert image (SliceEntry{range, weak} : live) m, ())
  where
    clash e = overlappingRanges range e.range
    pruneLive = do
      m <- readIORef registry
      filterM (fmap isJust . deRefWeak . (.weak)) (Map.findWithDefault [] image m)

{- | Drop every wrapper registered over the image, reachable or not.

The deterministic half of deregistration: weak entries only free once
nothing holds the wrapper, but a destroyed image's wrappers may stay
reachable through scopes that outlive it (another in-flight frame's slot).
Register this next to the image's destruction so a recycled handle cannot
clash with them.
-}
forgetImage :: (MonadIO m) => SliceRegistry -> Vk.Image -> m ()
forgetImage (SliceRegistry registry) image =
  liftIO (atomicModifyIORef' registry \m -> (Map.delete image m, ()))

{- | Attach a summary (e.g. 'imageInfo') shown next to the resource's name
in visualization output.
-}
describedAs :: Text -> ManagedImage -> ManagedImage
describedAs t ManagedImage{..} = ManagedImage{info = t, ..}

{- | Mark the allocation as @SHARING_MODE_CONCURRENT@ across the families it
is used on.

Required before any cross-family access the graph cannot transfer
ownership for: an @EXCLUSIVE@ image's contents are undefined on the new
family, so crossing without this is fatal, not silent. Apply before
importing — imports read it through 'FG.isShared', exempting the image
from the schedule's single-owner validation.
-}
sharedAcrossQueues :: ManagedImage -> ManagedImage
sharedAcrossQueues mi = mi{shared = True}

-- | The conventional 'describedAs' summary: the format (sans prefix) and extent.
imageInfo :: Vk.Format -> Vk.Extent2D -> Text
imageInfo format (Vk.Extent2D w h) =
  Text.pack (drop (Text.length "FORMAT_") (show format) <> " " <> show w <> "x" <> show h)

{- | 'newManagedImage' with the 'imageInfo' description attached, stating the
allocation's format/extent once.
-}
describedImage :: (HasCallStack, MonadIO m) => SliceRegistry -> Vk.Format -> Vk.Extent2D -> Vk.Image -> Vk.ImageAspectFlags -> m ManagedImage
describedImage reg format ext image aspect = describedAs (imageInfo format ext) <$> newManagedImage reg image aspect

-- | 'newManagedImageMip' with the mip's 'imageInfo' description attached.
describedMip :: (HasCallStack, MonadIO m) => SliceRegistry -> Vk.Format -> Vk.Extent2D -> Vk.Image -> Vk.ImageAspectFlags -> Word32 -> m ManagedImage
describedMip reg format ext image aspect mip = describedAs (imageInfo format ext) <$> newManagedImageMip reg image aspect mip

-- | A mip-0 layer range via 'newManagedImageSlice', with the 'imageInfo' description attached.
describedSlice :: (HasCallStack, MonadIO m) => SliceRegistry -> Vk.Format -> Vk.Extent2D -> Vk.Image -> Vk.ImageAspectFlags -> Word32 -> Word32 -> m ManagedImage
describedSlice reg format ext image aspect baseLayer layerCount = describedAs (imageInfo format ext) <$> newManagedImageSlice reg image aspect 0 1 baseLayer layerCount

instance FG.Resource ManagedImage where
  type Desc ManagedImage = ImageDesc
  type Alloc ManagedImage = ()
  type Ctx ManagedImage = Recorder
  type Flags ManagedImage = Usage

  createResource _ _ =
    error "ManagedImage is import-only: allocate the image and use importResource"

  destroyResource _ _ _ = pure ()

  preRead h _ usage rec mi = queueTransition rec (FG.handleId h) mi usage
  preWrite h _ usage rec mi = queueTransition rec (FG.handleId h) mi usage

  -- The two halves of a cross-queue hand-off, fired on the producing and the
  -- consuming side of each data edge.
  preRelease h _ usage peer rec mi = transferOwnership Release rec (FG.handleId h) peer mi usage
  preAcquire h _ usage peer rec mi = transferOwnership Acquire rec (FG.handleId h) peer mi usage

  isShared mi = mi.shared

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
      nextTransition (maybe HostAccess DeviceQueue lastQueue) (\_ _ -> True) False mi usage >>= \case
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
exec callback), and when the access rides a prior synchronization — a
cross-queue hop (the driver's semaphore) or a split-barrier event the pass
waited on ('chainedNode') — it chains to it instead: source scope becomes
the destination stage with no access mask, since the semaphore/event
already provides execution ordering and memory availability. The driver's
wait @dstStageMask@ / event scope must cover the usage's stage (both then
chain). Same-family queues share freely; crossing to another /family/
needs CONCURRENT sharing or an ownership acquire this pass performed —
otherwise an EXCLUSIVE image's contents are undefined there.
-}
queueTransition :: (MonadIO m) => Recorder -> Int -> ManagedImage -> Usage -> m ()
queueTransition rec node mi usage = do
  queue <- recorderQueue rec
  chained0 <- chainedNode rec node
  hosted <- recorderHost rec
  sameFamily <- recorderSameFamily rec
  nextTransition (if hosted then HostAccess else DeviceQueue queue) sameFamily chained0 mi usage >>= traverse_ \(srcStage, dstStage, barrier) ->
    queueBarrier rec srcStage dstStage barrier

{- | The producer- and consumer-side halves of a cross-queue hand-off.

On a @CONCURRENT@ image ('sharedAcrossQueues') the release half carries the
layout transition producer-side — its source scope stays on a queue that
supports it — and the acquire half is a no-op: the driver's semaphore
already orders the two, and no family owns the image.

On an @EXCLUSIVE@ image the pair is a real queue-family ownership transfer:
the same barrier is recorded twice, once in each queue's buffer, with both
family indices named. The halves must match exactly, so both are computed
from the state the release saw — the acquire is what advances the tracked
state, and it marks the node chained so the consumer's own declared access
does not place a second barrier on top of it.

Same-family queues own nothing to transfer: the release still moves the
layout, the acquire still just advances the tracking.

A hand-off to the host is neither: the release carries the full dependency
(a semaphore signal makes device writes available to the device domain
only, so the host half needs a real @HOST@ destination scope), and the
acquire is bookkeeping.
-}
transferOwnership :: (MonadIO m) => TransferSide -> Recorder -> Int -> FG.QueueId -> ManagedImage -> Usage -> m ()
transferOwnership side rec node peer mi usage = do
  hosted <- recorderHost rec
  queue <- recorderQueue rec
  ourFamily <- recorderFamily rec queue
  peerFamily <- recorderFamily rec peer
  cur <- liftIO (readIORef mi.stateRef)
  released <- liftIO (readIORef mi.releasedRef)
  let
    next = usageState usage
    -- The producer is the release's queue and the acquire's peer.
    (srcFamily, dstFamily) = case side of
      Release -> (ourFamily, peerFamily)
      Acquire -> (peerFamily, ourFamily)
    -- The host owns nothing (it is not a family), and a CONCURRENT image is
    -- owned by no one: those hand-offs ride the semaphore alone.
    owned =
      not mi.shared
        && not hosted
        && srcFamily /= dstFamily
        && srcFamily /= Vk.QUEUE_FAMILY_IGNORED
        && dstFamily /= Vk.QUEUE_FAMILY_IGNORED
    -- The consumer is the host: only the release's barrier can make the
    -- device's writes visible to it (the schedule's timeline wait cannot).
    toHost = next.stage .&. Vk.PIPELINE_STAGE_HOST_BIT /= zero
    -- Both halves describe the same barrier, so the acquire builds its own
    -- from the state the release saw.
    from = case side of
      Release -> cur
      Acquire -> fromMaybe cur released
    barrier =
      SomeStruct
        zero
          { Vk.srcAccessMask = case side of
              Release -> from.access
              Acquire -> zero
          , Vk.dstAccessMask = case side of
              Release -> if toHost then next.access else zero
              Acquire -> next.access
          , Vk.oldLayout = from.layout
          , Vk.newLayout = next.layout
          , Vk.srcQueueFamilyIndex = if owned then srcFamily else Vk.QUEUE_FAMILY_IGNORED
          , Vk.dstQueueFamilyIndex = if owned then dstFamily else Vk.QUEUE_FAMILY_IGNORED
          , Vk.image = mi.image
          , Vk.subresourceRange = mi.range
          }
    -- A release's destination scope and an acquire's source scope are ignored
    -- by the spec; the halves must otherwise be identical.
    (srcStage, dstStage) = case side of
      Release -> (from.stage, if toHost then next.stage else Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT)
      Acquire -> (Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT, next.stage)
  case side of
    Release -> do
      -- The release performs the layout transition (its barrier executes on
      -- the producer's queue), so the tracked state advances with it.
      when (owned || toHost || cur /= next) do
        queueBarrier rec srcStage dstStage barrier
        liftIO (writeIORef mi.stateRef next)
      liftIO (writeIORef mi.releasedRef (Just cur))
    Acquire -> do
      -- An owned acquire without its armed release half would record an
      -- unmatched barrier from a guessed state: a schedule bug, not a
      -- recoverable condition.
      when (owned && isNothing released) $
        error
          ( "Vulkan.Utils.FrameGraph: ownership acquire of "
              <> show mi.info
              <> " without a pending release; the schedule must pair the halves"
          )
      when owned $ queueBarrier rec srcStage dstStage barrier
      liftIO do
        -- The host is not a device queue: recording it as the last one would
        -- make the next device access look cross-queue (cf. 'nextTransition'),
        -- and its release already advanced the state (the @toHost@ arm) — the
        -- driver defers host passes, so a late write here would rewind the
        -- device-side tracking.
        unless hosted do
          writeIORef mi.stateRef next
          writeIORef mi.queueRef (Just queue)
        -- Only the owned half consumed the hand-off; a melted acquire (host,
        -- shared, same family) must leave the slot for a pending owned one.
        when owned $ writeIORef mi.releasedRef Nothing
      markChained rec node

{- | Diff the tracked state against the 'Usage''s target and advance it.

Hands back the @(srcStage, dstStage, barrier)@ still to be recorded — the
caller commits to recording it (immediately or batched) before the access
runs.
-}
nextTransition
  :: (MonadIO m)
  => Accessor
  -> (FG.QueueId -> FG.QueueId -> Bool)
  -- ^ whether two queues belong to one family (share ownership)
  -> Bool
  -- ^ an ownership acquire already synchronized it ('chainedNode')
  -> ManagedImage
  -> Usage
  -> m (Maybe (Vk.PipelineStageFlags, Vk.PipelineStageFlags, SomeStruct Vk.ImageMemoryBarrier))
nextTransition accessor sameFamily marked mi usage = liftIO do
  cur <- readIORef mi.stateRef
  lastQueue <- readIORef mi.queueRef
  let
    next = usageState usage
    -- A first access owns nothing yet, and the host is not a queue family (its
    -- accesses order through the schedule's timeline and the producer's
    -- release barrier), so neither crosses ownership.
    crossQueue = case (accessor, lastQueue) of
      (DeviceQueue q, Just prev) -> q /= prev
      _ -> False
    crossFamily =
      crossQueue && case (accessor, lastQueue) of
        (DeviceQueue q, Just prev) -> not (sameFamily q prev)
        _ -> False
    -- A cross-queue hop rides the driver's semaphore even within one family.
    chained = crossQueue || marked
    -- Crossing to a new family without a transfer: the contents are undefined
    -- there, so the access acquires by discarding them (it writes — the guard
    -- below rejects a read).
    discards = crossFamily && not mi.shared
    srcStage = if chained then next.stage else cur.stage
    srcAccess = if chained then zero else cur.access
    -- Semaphore/event-ordered same-state accesses need no barrier of their
    -- own; unchained writes need one even with the state unchanged.
    needed = cur /= next || (usageWrites usage && not chained)
  -- An unshared (EXCLUSIVE) resource reaching another family without an
  -- ownership transfer ('transferOwnership') has undefined contents there. A
  -- write that does not read them is still fine — it acquires by discarding
  -- (see 'discards') — but a read would see garbage, so it is fatal.
  when (crossFamily && not mi.shared && not (usageWrites usage)) $
    error
      ( "Vulkan.Utils.FrameGraph: cross-family read of an unshared resource ("
          <> show mi.info
          <> ") the graph never handed over: it must be produced on the reading family, "
          <> "marked 'sharedAcrossQueues' (CONCURRENT), or written by a pass the graph "
          <> "can transfer ownership from"
      )
  case accessor of
    DeviceQueue q -> writeIORef mi.queueRef (Just q)
    HostAccess -> pure ()
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
                , Vk.oldLayout = if discards then Vk.IMAGE_LAYOUT_UNDEFINED else cur.layout
                , Vk.newLayout = next.layout
                , -- IGNORED (not 0): a plain transition, not an ownership
                  -- transfer ('transferOwnership' emits those).
                  Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.image = mi.image
                , Vk.subresourceRange = mi.range
                }
          )
    else
      pure Nothing

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
  disarmHandOff mi
  FG.importResource graph name (ImageDesc mi.info) mi

{- | Drop a hand-off a previous graph's melted acquire left armed.

Run at import, so the unpaired-acquire check sees only this graph's
releases — a stale slot would let it pass and record a barrier from a
frames-old state.
-}
disarmHandOff :: (MonadIO m) => ManagedImage -> m ()
disarmHandOff mi = liftIO (writeIORef mi.releasedRef Nothing)

{- | 'importManagedImage' via 'FG.importScratch', keeping writers subject to demand culling.

The image (and its layout tracking) persists between graphs, but its contents
are only ever consumed through this graph. Passes that feed a between-graphs
consumer must say 'FG.setSideEffect' themselves.
-}
importScratchImage :: (MonadIO m) => FG.FrameGraph Recorder () -> Text -> ManagedImage -> m (FG.Handle ManagedImage)
importScratchImage graph name mi = do
  FG.addPreExec graph flushBarriers
  disarmHandOff mi
  FG.importScratch graph name (ImageDesc mi.info) mi

{- | 'importManagedImage' declaring the queue that owns the image across the
frame boundary ('FG.importOwned'), read off the wrapper's own tracking: a
first touch on another family this frame derives a real release / acquire
pair — the release recorded on the owning queue — instead of the fatal
cross-family read. An image no device queue has touched yet imports
plainly.
-}
importOwnedImage :: (MonadIO m) => FG.FrameGraph Recorder () -> Text -> ManagedImage -> m (FG.Handle ManagedImage)
importOwnedImage graph name mi =
  liftIO (readIORef mi.queueRef) >>= \case
    Nothing -> importManagedImage graph name mi
    Just owner -> do
      FG.addPreExec graph flushBarriers
      disarmHandOff mi
      FG.importOwned graph name (ImageDesc mi.info) mi owner

{- | Declare the queue that owns the image, established outside any graph.

For producers the adapters cannot see — a fenced one-shot bake, an upload
queue — so 'importOwnedImage' has an owner to derive the first hand-off
from. In-graph accesses track this themselves.
-}
claimOwnership :: (MonadIO m) => FG.QueueId -> ManagedImage -> m ()
claimOwnership queue mi = liftIO (writeIORef mi.queueRef (Just queue))
