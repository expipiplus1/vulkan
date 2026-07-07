{-| Command-buffer routing for driving 'FG.executeQueued'.

A 'Recorder' is a mutable slot holding the command buffer the current pass — and
the barrier hooks it fires — record into. 'recordingBackend' is the
topology-agnostic 'FG.QueueBackend' that points the recorder at each pass's
queue buffer; 'recordGraph' wraps the whole record step (fresh recorder, run,
close every buffer), leaving each driver to supply only its own submit policy.

Nothing here is image-specific: it is the execution seam any 'FG.Resource'
adapter records through.
-}
module Vulkan.Utils.FrameGraph.Recorder
  ( Recorder
  , newRecorder
  , setRecorder
  , recorderCommandBuffer
  , recordingBackend
  , recordGraph
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

import Fragr qualified as FG
import Vulkan.Core10 qualified as Vk

{- | The command buffer the barrier hooks (and a pass's exec callback) record
into. For single-queue 'FG.execute' it holds one buffer for the whole frame;
the multi-queue 'FG.executeQueued' driver swaps it per pass so each queue's
work lands in that queue's buffer.
-}
newtype Recorder = Recorder (IORef Vk.CommandBuffer)

-- | A recorder pointed at an initial buffer; swap it with 'setRecorder'.
newRecorder :: (MonadIO m) => Vk.CommandBuffer -> m Recorder
newRecorder cb = Recorder <$> liftIO (newIORef cb)

-- | Point the recorder at the command buffer the next passes record into.
setRecorder :: (MonadIO m) => Recorder -> Vk.CommandBuffer -> m ()
setRecorder (Recorder ref) cb = liftIO (writeIORef ref cb)

-- | The command buffer currently selected.
recorderCommandBuffer :: (MonadIO m) => Recorder -> m Vk.CommandBuffer
{-# INLINE recorderCommandBuffer #-}
recorderCommandBuffer (Recorder ref) = liftIO (readIORef ref)

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
    { FG.beforePass = \psync -> setRecorder recorder (cbFor psync.queue)
    , FG.afterPass = \_ -> pure ()
    , FG.completed = pure []
    }

{- | Record a compiled graph into the given per-queue command buffers: point a
fresh recorder at the first buffer, drive 'FG.executeQueued' (routing each pass
to @cbFor@), then end every buffer. The caller supplies only the submit that
follows.

The 'FG.RecycleQueue' is internal and inert: import-only adapters never retire a
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
  recycle <- FG.newRecycleQueue
  FG.executeQueued graph (recordingBackend recorder cbFor) recycle recorder ()
  traverse_ Vk.endCommandBuffer buffers
