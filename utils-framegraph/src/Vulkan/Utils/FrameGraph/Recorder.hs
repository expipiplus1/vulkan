{-| Command-buffer routing and barrier batching for driving 'FG.executeQueued'.

A 'Recorder' is a mutable slot holding the command buffer the current pass — and
the barrier hooks it fires — record into, plus the batch of barriers those hooks
have queued for the pass ('queueBarrier', emitted as one command by
'flushBarriers' from the graph's 'FG.setPreExec' point). 'recordingBackend' is
the topology-agnostic 'FG.QueueBackend' that points the recorder at each pass's
queue buffer; 'recordGraph' wraps the whole record step (fresh recorder, flush
installed, run, close every buffer), leaving each driver to supply only its own
submit policy.

Nothing here is resource-specific beyond the barrier payload types (one lane
per kind): it is the execution seam any 'FG.Resource' adapter records through.
-}
module Vulkan.Utils.FrameGraph.Recorder
  ( Recorder
  , newRecorder
  , setRecorder
  , recorderCommandBuffer
  , recorderQueue
  , queueBarrier
  , queueBufferBarrier
  , flushBarriers
  , recordingCommandBuffer
  , recordingBackend
  , recordGraph
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits ((.&.), (.|.))
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V

import Fragr qualified as FG
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.Core10 qualified as Vk
import Vulkan.Zero (zero)

{- | The command buffer the barrier hooks (and a pass's exec callback) record
into, tagged with its queue so resource adapters can tell when consecutive
accesses cross queues, plus the pass's pending barrier batch. For single-queue
'FG.execute' it holds one buffer for the whole frame; the multi-queue
'FG.executeQueued' driver swaps it per pass so each queue's work lands in that
queue's buffer.
-}
data Recorder = Recorder
  { slot :: IORef (FG.QueueId, Vk.CommandBuffer)
  , pending :: IORef Barriers
  }

-- | The barriers queued for the current pass, OR-ing the stage scopes.
data Barriers = Barriers
  { srcStage :: !Vk.PipelineStageFlags
  , dstStage :: !Vk.PipelineStageFlags
  , images :: [SomeStruct Vk.ImageMemoryBarrier]
  , buffers :: [SomeStruct Vk.BufferMemoryBarrier]
  }

noBarriers :: Barriers
noBarriers = Barriers zero zero [] []

-- | A recorder pointed at an initial buffer on queue 0; swap it with 'setRecorder'.
newRecorder :: (MonadIO m) => Vk.CommandBuffer -> m Recorder
newRecorder cb = liftIO $ Recorder <$> newIORef (FG.QueueId 0, cb) <*> newIORef noBarriers

-- | Point the recorder at the queue's command buffer the next passes record into.
setRecorder :: (MonadIO m) => Recorder -> FG.QueueId -> Vk.CommandBuffer -> m ()
setRecorder rec queue cb = liftIO (writeIORef rec.slot (queue, cb))

-- | The command buffer currently selected.
recorderCommandBuffer :: (MonadIO m) => Recorder -> m Vk.CommandBuffer
{-# INLINE recorderCommandBuffer #-}
recorderCommandBuffer rec = liftIO (snd <$> readIORef rec.slot)

-- | The queue the current pass records on.
recorderQueue :: (MonadIO m) => Recorder -> m FG.QueueId
{-# INLINE recorderQueue #-}
recorderQueue rec = liftIO (fst <$> readIORef rec.slot)

{- | Queue a barrier into the current pass's batch instead of recording it.

A barrier overlapping a subresource already in the batch flushes it first:
barriers in one command are unordered, and an overlapping pair (a pass
reading then writing one image) is a dependent chain of layout transitions
that needs the command split to stay ordered.

The batch is emitted by 'flushBarriers', which the graph must fire between
the hooks and the exec callback (installed via 'FG.setPreExec' by the image
adapter's import and by 'recordGraph') — a driver whose resources queue
through another path must install it itself, or the queued barriers are
never recorded.
-}
queueBarrier
  :: (MonadIO m)
  => Recorder
  -> Vk.PipelineStageFlags
  -> Vk.PipelineStageFlags
  -> SomeStruct Vk.ImageMemoryBarrier
  -> m ()
queueBarrier rec src dst barrier = do
  Barriers{images} <- liftIO (readIORef rec.pending)
  when (any (overlapping barrier) images) (flushBarriers rec)
  liftIO $ modifyIORef' rec.pending \b ->
    b{srcStage = b.srcStage .|. src, dstStage = b.dstStage .|. dst, images = barrier : b.images}

-- | 'queueBarrier' for the buffer lane; overlap is per whole buffer.
queueBufferBarrier
  :: (MonadIO m)
  => Recorder
  -> Vk.PipelineStageFlags
  -> Vk.PipelineStageFlags
  -> SomeStruct Vk.BufferMemoryBarrier
  -> m ()
queueBufferBarrier rec src dst barrier@(SomeStruct new) = do
  Barriers{buffers} <- liftIO (readIORef rec.pending)
  when (any (\(SomeStruct b) -> b.buffer == new.buffer) buffers) (flushBarriers rec)
  liftIO $ modifyIORef' rec.pending \b ->
    b{srcStage = b.srcStage .|. src, dstStage = b.dstStage .|. dst, buffers = barrier : b.buffers}

-- | Whether two image barriers touch overlapping subresources.
overlapping :: SomeStruct Vk.ImageMemoryBarrier -> SomeStruct Vk.ImageMemoryBarrier -> Bool
overlapping (SomeStruct a) (SomeStruct b) =
  a.image == b.image
    && ra.aspectMask .&. rb.aspectMask /= zero
    && spans ra.baseMipLevel ra.levelCount rb.baseMipLevel rb.levelCount
    && spans ra.baseArrayLayer ra.layerCount rb.baseArrayLayer rb.layerCount
  where
    ra = a.subresourceRange
    rb = b.subresourceRange
    -- The REMAINING_* sentinels are maxBound: treat as extending to the end.
    spans baseA countA baseB countB = baseA < end baseB countB && baseB < end baseA countA
    end base count = if count == maxBound then maxBound else base + count

{- | Record the pending batch as one @vkCmdPipelineBarrier@ into the current
buffer and clear it; a no-op when nothing is queued.

The stage masks are the OR of every queued barrier's — a slightly wider (never
weaker) dependency than per-barrier commands, the price of batching.
-}
flushBarriers :: (MonadIO m) => Recorder -> m ()
flushBarriers rec = liftIO do
  Barriers{srcStage, dstStage, images, buffers} <- readIORef rec.pending
  case (images, buffers) of
    ([], []) -> pure ()
    _ -> do
      writeIORef rec.pending noBarriers
      (_queue, cb) <- readIORef rec.slot
      Vk.cmdPipelineBarrier cb srcStage dstStage zero [] (V.fromList buffers) (V.fromList images)

-- | The command buffer the executing pass records into ('recorderCommandBuffer' of the 'FG.Exec' context).
recordingCommandBuffer :: FG.Exec Recorder alloc Vk.CommandBuffer
{-# INLINE recordingCommandBuffer #-}
recordingCommandBuffer = recorderCommandBuffer =<< FG.askCtx

{- | A 'FG.QueueBackend' that routes each pass's recording to its queue's command
buffer (via @cbFor@) and does nothing else.

The topology-agnostic core an 'FG.executeQueued' driver is built on: it only
points the 'Recorder' at the right buffer before each pass. The rest of a
schedule — timeline waits/signals, split-barrier events, queue-family ownership
— is the caller's to realise from 'FG.PassSync' / 'FG.snapshot' around it. On a
single-queue schedule @cbFor = const theOnlyBuffer@ and it degenerates to
recording everything into one buffer.
-}
recordingBackend :: Recorder -> (FG.QueueId -> Vk.CommandBuffer) -> FG.QueueBackend
recordingBackend recorder cbFor =
  FG.QueueBackend
    { FG.beforePass = \psync -> setRecorder recorder psync.queue (cbFor psync.queue)
    , FG.afterPass = \_ -> pure ()
    , FG.completed = pure []
    }

{- | Record a compiled graph into the given per-queue command buffers: point a
fresh recorder at the first buffer, install the 'flushBarriers' flush, drive
'FG.executeQueued' (routing each pass to @cbFor@), then end every buffer. The
caller supplies only the submit that follows.

Runs without a 'FG.RecycleQueue': import-only adapters never retire a
resource, so there is nothing to reclaim (allocate transients in the frame's own
resource scope instead). The first buffer is the primary the recorder starts on;
all buffers are ended.
-}
recordGraph
  :: (MonadIO m)
  => (FG.QueueId -> Vk.CommandBuffer)
  -> NonEmpty Vk.CommandBuffer
  -> FG.FrameGraph Recorder ()
  -> m ()
recordGraph cbFor buffers graph = do
  recorder <- newRecorder (NE.head buffers)
  FG.setPreExec graph flushBarriers
  FG.executeQueued graph (recordingBackend recorder cbFor) Nothing recorder ()
  traverse_ Vk.endCommandBuffer buffers
