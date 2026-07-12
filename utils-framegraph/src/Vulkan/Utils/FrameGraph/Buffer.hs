{-| A 'FG.Resource' for Vulkan buffers that places memory barriers automatically.

The buffer sibling of "Vulkan.Utils.FrameGraph.Image": a 'ManagedBuffer'
carries the buffer plus its tracked 'BufferState' (stage, access — buffers
have no layout), a pass declares an access with a 'Usage', and the hooks diff
the tracked state against the usage's target and queue a
@VkBufferMemoryBarrier@ into the 'Recorder''s per-pass batch. Barriers cover
the whole buffer; queue hops chain to the driver's semaphore exactly as the
image adapter's do (and cross-family access likewise needs CONCURRENT
sharing).

Import-only: the graph tracks the state and places barriers but does not own
the allocation.
-}
module Vulkan.Utils.FrameGraph.Buffer
  ( ManagedBuffer (..)
  , newManagedBuffer
  , BufferDesc (..)
  , importManagedBuffer
  , importScratchBuffer
  , describedAs
  , BufferState (..)
  , freshState
  , Usage (..)
  , usageState
  , transitionBufferTo
  , transitionBuffersTo
  ) where

import Control.Monad (foldM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Vector qualified as V

import Fragr qualified as FG
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.Core10 qualified as Vk
import Vulkan.Utils.FrameGraph.Recorder (Recorder, flushBarriers, queueBufferBarrier, recorderQueue)
import Vulkan.Zero (zero)

-- | A buffer whose stage/access the frame graph tracks and barriers.
data ManagedBuffer = ManagedBuffer
  { buffer :: Vk.Buffer
  , stateRef :: IORef BufferState
  , queueRef :: IORef FG.QueueId
  , info :: Text
  -- ^ Human-readable summary shown by visualization output; attach with 'describedAs'.
  }

-- | Wrap a buffer, starting from 'freshState'.
{-# INLINE newManagedBuffer #-}
newManagedBuffer :: (MonadIO m) => Vk.Buffer -> m ManagedBuffer
newManagedBuffer buffer = liftIO do
  stateRef <- newIORef freshState
  queueRef <- newIORef (FG.QueueId 0)
  pure ManagedBuffer{buffer, stateRef, queueRef, info = ""}

-- | Attach a summary shown next to the resource's name in visualization output.
describedAs :: Text -> ManagedBuffer -> ManagedBuffer
describedAs t mb = mb{info = t}

instance FG.Resource ManagedBuffer where
  type Desc ManagedBuffer = BufferDesc
  type Alloc ManagedBuffer = ()
  type Ctx ManagedBuffer = Recorder
  type Flags ManagedBuffer = Usage

  createResource _ _ =
    error "ManagedBuffer is import-only: allocate the buffer and use importResource"

  destroyResource _ _ _ = pure ()

  preRead _ usage rec mb = queueTransition rec mb usage
  preWrite _ usage rec mb = queueTransition rec mb usage

  -- Producer-side handoff, as in the image adapter.
  preRelease _ usage rec mb = queueTransition rec mb usage

  describeDesc d = d.info

-- | The synchronization state a buffer's last access left it in.
data BufferState = BufferState
  { stage :: Vk.PipelineStageFlags
  , access :: Vk.AccessFlags
  }
  deriving stock (Eq, Show)

{- | Never accessed on the device since the last host-synchronized point (a
fence-waited setup submit): top of pipe, no access to make available.
-}
freshState :: BufferState
freshState = BufferState Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT zero

-- | How a pass uses a buffer, i.e. the 'BufferState' it must be in for that access.
data Usage
  = -- | @vkCmdDraw*Indirect@ / @vkCmdDispatchIndirect@ command source.
    IndirectRead
  | TransferSrc
  | TransferDst
  | -- | Storage read/write in the given shader stage (compute, vertex, …).
    StorageRead Vk.PipelineStageFlags
  | StorageWrite Vk.PipelineStageFlags
  | -- | Read-modify-write storage access (atomics).
    StorageReadWrite Vk.PipelineStageFlags
  | -- | Read by the host, after the schedule's timeline wait reaches it.
    HostRead
  | -- | Written by the host (a mapped upload) before device consumers.
    HostWrite
  deriving stock (Eq, Ord, Show)

-- | The target state each 'Usage' requires.
usageState :: Usage -> BufferState
usageState = \case
  IndirectRead -> BufferState Vk.PIPELINE_STAGE_DRAW_INDIRECT_BIT Vk.ACCESS_INDIRECT_COMMAND_READ_BIT
  TransferSrc -> BufferState Vk.PIPELINE_STAGE_TRANSFER_BIT Vk.ACCESS_TRANSFER_READ_BIT
  TransferDst -> BufferState Vk.PIPELINE_STAGE_TRANSFER_BIT Vk.ACCESS_TRANSFER_WRITE_BIT
  StorageRead stage -> BufferState stage Vk.ACCESS_SHADER_READ_BIT
  StorageWrite stage -> BufferState stage Vk.ACCESS_SHADER_WRITE_BIT
  StorageReadWrite stage -> BufferState stage (Vk.ACCESS_SHADER_READ_BIT .|. Vk.ACCESS_SHADER_WRITE_BIT)
  HostRead -> BufferState Vk.PIPELINE_STAGE_HOST_BIT Vk.ACCESS_HOST_READ_BIT
  HostWrite -> BufferState Vk.PIPELINE_STAGE_HOST_BIT Vk.ACCESS_HOST_WRITE_BIT

{- | Whether the 'Usage' writes the buffer (and so needs a barrier even when the
state is unchanged — only read-after-read can skip it).
-}
usageWrites :: Usage -> Bool
usageWrites = \case
  TransferDst -> True
  StorageWrite _ -> True
  StorageReadWrite _ -> True
  HostWrite -> True
  IndirectRead -> False
  TransferSrc -> False
  StorageRead _ -> False
  HostRead -> False

{- | Record the barrier bringing the buffer into the 'Usage''s state and update
the tracked state. Standalone counterpart to the hook path, for barriers
recorded outside a pass; treats the access as same-queue.
-}
transitionBufferTo :: (MonadIO m) => Vk.CommandBuffer -> ManagedBuffer -> Usage -> m ()
{-# INLINE transitionBufferTo #-}
transitionBufferTo cb mb usage = transitionBuffersTo cb [(mb, usage)]

{- | 'transitionBufferTo' over a batch: one @vkCmdPipelineBarrier@, OR-ed stage masks.

The buffers must be distinct: barriers in one command are unordered, so two
entries for the same buffer would race.
-}
transitionBuffersTo :: (MonadIO m) => Vk.CommandBuffer -> [(ManagedBuffer, Usage)] -> m ()
transitionBuffersTo cb accesses = do
  (srcs, dsts, barriers) <- foldM collect (zero, zero, []) accesses
  unless (null barriers) $
    Vk.cmdPipelineBarrier cb srcs dsts zero [] (V.fromList barriers) []
  where
    collect acc@(srcs, dsts, barriers) (mb, usage) = do
      lastQueue <- liftIO (readIORef mb.queueRef)
      nextTransition lastQueue mb usage >>= \case
        Nothing -> pure acc
        Just (src, dst, barrier) -> pure (srcs .|. src, dsts .|. dst, barrier : barriers)

{- | The hook path: 'transitionBufferTo' rules, but queued and queue-aware.

Queue hops chain to the driver's semaphore like the image adapter's: the
driver's wait @dstStageMask@ must cover the usage's stage, and cross-family
access needs CONCURRENT sharing.
-}
queueTransition :: (MonadIO m) => Recorder -> ManagedBuffer -> Usage -> m ()
queueTransition rec mb usage = do
  queue <- recorderQueue rec
  nextTransition queue mb usage >>= traverse_ \(srcStage, dstStage, barrier) ->
    queueBufferBarrier rec srcStage dstStage barrier

{- | Diff the tracked state against the 'Usage''s target and advance it.

A read whose state differs from the previous one still emits a barrier (the
chain through it is what orders a later write after both reads); only a read
of an already-matching state skips it.
-}
nextTransition
  :: (MonadIO m)
  => FG.QueueId
  -> ManagedBuffer
  -> Usage
  -> m (Maybe (Vk.PipelineStageFlags, Vk.PipelineStageFlags, SomeStruct Vk.BufferMemoryBarrier))
nextTransition queue mb usage = liftIO do
  cur <- readIORef mb.stateRef
  lastQueue <- readIORef mb.queueRef
  let
    next = usageState usage
    cross = queue /= lastQueue
    srcStage = if cross then next.stage else cur.stage
    srcAccess = if cross then zero else cur.access
    -- Cross-queue same-state accesses are ordered by the semaphore alone;
    -- same-queue writes need the barrier even with the state unchanged.
    needed = cur /= next || (usageWrites usage && not cross)
  when cross (writeIORef mb.queueRef queue)
  if needed
    then do
      writeIORef mb.stateRef next
      pure $
        Just
          ( srcStage
          , next.stage
          , SomeStruct
              zero
                { Vk.srcAccessMask = srcAccess
                , Vk.dstAccessMask = next.access
                , Vk.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                , Vk.buffer = mb.buffer
                , Vk.offset = 0
                , Vk.size = Vk.WHOLE_SIZE
                }
          )
    else pure Nothing

{- | Descriptor for a 'ManagedBuffer'; carries the buffer's 'describedAs'
summary for visualization output (the resource name travels separately).
-}
newtype BufferDesc = BufferDesc {info :: Text}

{- | Import a 'ManagedBuffer' under @name@, as an observed resource.

Claims the graph's 'FG.addPreExec slot for 'flushBarriers' like the image
imports do — the adapters share that slot; wrap the flush rather than
replacing it.
-}
importManagedBuffer :: (MonadIO m) => FG.FrameGraph Recorder () -> Text -> ManagedBuffer -> m (FG.Handle ManagedBuffer)
importManagedBuffer graph name mb = do
  FG.addPreExec graph flushBarriers
  FG.importResource graph name (BufferDesc mb.info) mb

-- | 'importManagedBuffer' via 'FG.importScratch', keeping writers subject to demand culling.
importScratchBuffer :: (MonadIO m) => FG.FrameGraph Recorder () -> Text -> ManagedBuffer -> m (FG.Handle ManagedBuffer)
importScratchBuffer graph name mb = do
  FG.addPreExec graph flushBarriers
  FG.importScratch graph name (BufferDesc mb.info) mb
