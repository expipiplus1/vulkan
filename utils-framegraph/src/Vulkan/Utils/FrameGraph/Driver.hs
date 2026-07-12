{-# LANGUAGE PatternSynonyms #-}

{-| Package-level multi-queue submit driver.

'submitGraphQueued' turns a compiled graph into one submit per executing
queue: a one-time primary begun from each queue's pool, recorded through
'recordGraphSyncs', and cross-queue ordering realised with per-run timeline
semaphores straight off the schedule — each wait's value from 'FG.Wait' and
its @waitDstStageMask@ decoded from the accesses it protects ('waitStage',
widened by the pass's 'FG.Transfer' acquires), the same stages the resource
adapters chain their cross-queue barriers to.

Frame-level synchronization is the caller's and arrives as 'SubmitExtras':
swapchain acquire/present semaphores, frames-in-flight timelines, and any
cross-frame hazard on a resource the graphs share (a previous frame's
still-in-flight read is not a pass the compiler can see). Everything inside
one graph is derived; everything between graphs is an extra.

Like the adapters, this driver realises no queue-family ownership transfers
(cross-family resources need CONCURRENT sharing) and collapses the
schedule's split-barrier events into the hooks' immediate barriers.

Hoisting per-pass waits to one submit per queue needs the queue-level
dependency graph to stay acyclic; a schedule where two queues wait on each
other's later passes is rejected before anything is submitted. Splitting
buffers at wait boundaries would lift that; no graph here needs it yet.
-}
module Vulkan.Utils.FrameGraph.Driver
  ( submitGraphQueued
  , SubmitExtras (..)
  , noExtras
  , Submitted (..)
  , waitSubmitted
  , allocateCommandPool
  , allocatePrimary
  , waitStage
  , accessStage
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Bits ((.&.), (.|.))
import Data.Foldable (foldl')
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)

import Fragr qualified as FG
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import Vulkan.Core10 qualified as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import Vulkan.Core10 qualified as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import Vulkan.Core10 qualified as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo (..), waitSemaphoresSafe)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore qualified as SemaphoreWaitInfo (SemaphoreWaitInfo (..))
import Vulkan.Utils.Frame (allocateTimelineSemaphore)
import Vulkan.Utils.FrameGraph.Buffer qualified as Buffer
import Vulkan.Utils.FrameGraph.Image qualified as Image
import Vulkan.Utils.FrameGraph.Recorder (Recorder, recordGraphSyncs)
import Vulkan.Zero (zero)

{- | Frame-level waits and signals spliced into one queue's submit.

They ride next to the schedule-derived ones. Timeline semaphores carry
their value; a binary semaphore's value is ignored (pass 0).
-}
data SubmitExtras = SubmitExtras
  { waits :: [(Vk.Semaphore, Vk.PipelineStageFlags, Word64)]
  , signals :: [(Vk.Semaphore, Word64)]
  }

noExtras :: SubmitExtras
noExtras = SubmitExtras [] []

{- | One queue's completion handle.

Its per-run timeline reaches @value@ once every pass submitted there has
executed. Feed into frames-in-flight bookkeeping, or block on it with
'waitSubmitted'.
-}
data Submitted = Submitted
  { queue :: FG.QueueId
  , semaphore :: Vk.Semaphore
  , value :: Word64
  }

{- | Record a compiled graph and submit it, one submit per executing queue.

The queue table maps the graph's 'FG.QueueId's to real queues and the pools
to take this run's one-time command buffers from (reset the pool to reclaim
them, e.g. per frame in flight). Cross-queue waits come from the schedule;
@extras@ adds the frame-level ones. The per-run timelines live in the
current 'MonadResource' scope, so keep it open until the returned
'Submitted' values are waited on.
-}
submitGraphQueued
  :: (MonadResource m)
  => Vk.Device
  -> (FG.QueueId -> (Vk.Queue, Vk.CommandPool))
  -> (FG.QueueId -> SubmitExtras)
  -> FG.FrameGraph Recorder ()
  -> m [Submitted]
submitGraphQueued dev queueTable extras graph =
  FG.executingQueues graph >>= \case
    [] -> pure []
    qids -> do
      slots <- for qids \qid -> do
        let (vkQueue, pool) = queueTable qid
        cb <- allocatePrimary dev pool
        (_, timeline) <- allocateTimelineSemaphore dev 0
        pure (qid, (vkQueue, cb, timeline))
      let
        table = Map.fromList slots
        slotOf qid =
          Map.findWithDefault
            (error "submitGraphQueued: queue outside the compiled schedule")
            qid
            table
        cbFor qid = let (_, cb, _) = slotOf qid in cb
        timelineOf qid = let (_, _, timeline) = slotOf qid in timeline
      syncs <- recordGraphSyncs cbFor (NE.fromList [cb | (_, (_, cb, _)) <- slots]) graph
      let plan = submitPlan syncs
      when (hoistCyclic plan) $
        error "submitGraphQueued: cross-queue waits form a cycle at submit granularity"
      liftIO $ for slots \(qid, (vkQueue, cb, timeline)) -> do
        let
          (signalValue, foreignWaits) = plan Map.! qid
          ex = extras qid
          derived = [(timelineOf p, stages, value) | (p, (value, stages)) <- Map.toAscList foreignWaits]
          submitWaits = derived <> ex.waits
          submitSignals = (timeline, signalValue) : ex.signals
          submit =
            zero
              { Vk.waitSemaphores = V.fromList [sem | (sem, _, _) <- submitWaits]
              , Vk.waitDstStageMask = V.fromList [st | (_, st, _) <- submitWaits]
              , Vk.commandBuffers = [Vk.commandBufferHandle cb]
              , Vk.signalSemaphores = V.fromList (map fst submitSignals)
              }
              ::& zero
                { waitSemaphoreValues = V.fromList [v | (_, _, v) <- submitWaits]
                , signalSemaphoreValues = V.fromList (map snd submitSignals)
                }
                :& ()
        Vk.queueSubmit vkQueue [SomeStruct submit] Vk.NULL_HANDLE
        pure Submitted{queue = qid, semaphore = timeline, value = signalValue}

{- | The per-queue submit plan derived from the schedule.

For each queue: the value its timeline ends the run at, and per foreign
queue the value to wait for before the submit may start, scoped to the
stages of the accesses those waits protect.

The wait scope is widened by each pass's 'FG.Transfer' acquires: fragr's
watermark dedup drops a later pass's wait — covers included — when an
earlier same-queue pass already awaited a higher value, which is sound for
a full-scope wait but not for a stage-scoped one. Acquires carry every
cross-queue data edge's consuming flags undeduplicated, so folding them in
restores those stages. Cross-queue anti-edges (renaming writes) have no
transfer payload; a dropped anti-edge wait at a stage outside the kept
covers stays unordered — fixing that needs the compiler to keep dropped
waits' covers (see TODO-fragr.md).
-}
submitPlan :: [FG.PassSync] -> Map FG.QueueId (Word64, Map FG.QueueId (Word64, Vk.PipelineStageFlags))
submitPlan syncs =
  Map.fromListWith merge [(s.queue, (s.signal, waitsOf s)) | s <- syncs]
  where
    merge (v1, w1) (v2, w2) = (max v1 v2, Map.unionWith mergeWait w1 w2)
    waitsOf :: FG.PassSync -> Map FG.QueueId (Word64, Vk.PipelineStageFlags)
    waitsOf s =
      Map.fromListWith
        mergeWait
        ( [(w.queue, (w.value, waitStage w)) | w <- s.waits]
            -- Value 0 never raises the kept waits' max; acquires only widen the
            -- stage scope (an acquire's peer always has a kept wait — the first
            -- cross-queue edge is never above the watermark).
            <> [(t.peer, (0, flagsStage t.flags)) | t <- s.acquires]
        )
    mergeWait (v1, s1) (v2, s2) = (max v1 v2, s1 .|. s2)

{- | Whether the plan's hoisted waits turn the queue-level dependencies cyclic.

Hoisting every wait to its queue's submit front is only sound on a DAG;
a cycle would deadlock the device.
-}
hoistCyclic :: Map FG.QueueId (Word64, Map FG.QueueId (Word64, Vk.PipelineStageFlags)) -> Bool
hoistCyclic plan = or [q `Set.member` reachable ds | (q, ds) <- Map.toList edges]
  where
    edges :: Map FG.QueueId (Set FG.QueueId)
    edges = Map.map (Map.keysSet . snd) plan
    reachable ds =
      let ds' = ds <> foldMap (\d -> Map.findWithDefault mempty d edges) ds
      in if ds' == ds then ds else reachable ds'

-- | Block until every 'Submitted' timeline reaches its value.
waitSubmitted :: (MonadIO m) => Vk.Device -> Word64 -> [Submitted] -> m Vk.Result
waitSubmitted _ _ [] = pure Vk.SUCCESS
waitSubmitted dev timeout submitted =
  waitSemaphoresSafe
    dev
    zero
      { SemaphoreWaitInfo.semaphores = V.fromList [s.semaphore | s <- submitted]
      , SemaphoreWaitInfo.values = V.fromList [s.value | s <- submitted]
      }
    timeout

{- | The submit-front @waitDstStageMask@ covering a schedule wait.

The OR of its protected accesses' stages. Matches the adapters' cross-queue
barrier chaining ('Image.queueTransition' hands over at exactly the
consuming usage's stage), so the semaphore and the barrier meet. An access
declared without flags carries no scope: over-synchronize.
-}
waitStage :: FG.Wait -> Vk.PipelineStageFlags
waitStage w
  | null w.covers = Vk.PIPELINE_STAGE_ALL_COMMANDS_BIT
  | otherwise = foldl' (\acc a -> acc .|. accessStage a) zero w.covers

{- | The pipeline stage an access's flags decode to.

Routed to the image or buffer codec by 'Buffer.isBufferFlags'.
-}
accessStage :: FG.Access -> Vk.PipelineStageFlags
accessStage access = flagsStage access.flags

{- | 'accessStage' over bare flags, sanitized for a submit's wait mask.

@HOST@ (forbidden in @pWaitDstStageMask@ — host reads order via the
host-side timeline wait, so the device scope may be anything) and an empty
stage (no scope information) both widen to @ALL_COMMANDS@.
-}
flagsStage :: Maybe FG.Flags -> Vk.PipelineStageFlags
flagsStage = \case
  Nothing -> Vk.PIPELINE_STAGE_ALL_COMMANDS_BIT
  Just flags
    | stage == zero || stage .&. Vk.PIPELINE_STAGE_HOST_BIT /= zero ->
        Vk.PIPELINE_STAGE_ALL_COMMANDS_BIT
    | otherwise -> stage
    where
      stage
        | Buffer.isBufferFlags flags = (Buffer.usageState (Buffer.flagsUsage flags)).stage
        | otherwise = (Image.usageState (Image.flagsUsage flags)).stage

-- | Allocate a command pool for the family, released with the scope.
allocateCommandPool :: (MonadResource m) => Vk.Device -> Word32 -> m Vk.CommandPool
allocateCommandPool dev family = do
  (_, pool) <- Vk.withCommandPool dev zero{CommandPoolCreateInfo.queueFamilyIndex = family} Nothing allocate
  pure pool

-- | Allocate a primary command buffer from the pool and begin it, one-time-submit.
allocatePrimary :: (MonadResource m) => Vk.Device -> Vk.CommandPool -> m Vk.CommandBuffer
allocatePrimary dev pool = do
  (_, cbs) <-
    Vk.withCommandBuffers
      dev
      zero{Vk.commandPool = pool, Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY, Vk.commandBufferCount = 1}
      allocate
  let cb = V.head cbs
  Vk.beginCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
  pure cb
