{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

{-| Package-level multi-queue submit driver.

'submitGraphQueued' turns a compiled graph into one submit per executing
device queue: a one-time primary begun from each queue's pool, cross-queue
ordering realised with per-run timeline semaphores straight off the
schedule — each wait's value from 'FG.Wait' and its @waitDstStageMask@
decoded from the accesses it protects ('waitStage'), the same stages the
resource adapters chain their cross-queue barriers to.

The host is just another queue: passes assigned to the designated host
'FG.QueueId' execute on the CPU after the submits, each waiting its
schedule waits on the real timelines and signalling its own — so readbacks
and mapped uploads take part in the same dependency graph, with the
device-side transitions landing producer-side ('FG.preRelease') and the
host-side hooks tracking state without recording ('setRecorderHost').

Frame-level synchronization is the caller's and arrives as 'SubmitExtras':
swapchain acquire/present semaphores, frames-in-flight timelines, and any
cross-frame hazard on a resource the graphs share (a previous frame's
still-in-flight read is not a pass the compiler can see). Everything inside
one graph is derived; everything between graphs is an extra.

The schedule's split-barrier events are realised as synchronization2
events: one per-run @VkEvent@ each, signalled after the producing pass
(@vkCmdSetEvent2@) and waited before the consuming one (@vkCmdWaitEvents2@)
with a dependency built once from both endpoints' covers — the consumer's
own transitions then chain off the wait ('setEventedNodes'), so the work
scheduled between the endpoints overlaps the dependency.

Cross-queue hand-offs of an @EXCLUSIVE@ resource are realised as queue-family
ownership transfers: the adapters' release and acquire hooks emit the matching
barrier pair, naming the families from this driver's 'QueueSlot's. A
@CONCURRENT@ resource ('sharedAcrossQueues') owns nothing and rides the
semaphore alone.

Each queue's pass stream is cut into segments at wait boundaries
('planSegments'), one submit per segment, so mid-stream cross-queue
dependencies — device ping-pong, device→host→device round trips — schedule
instead of deadlocking.
-}
module Vulkan.Utils.FrameGraph.Driver
  ( submitGraphQueued
  , SubmitConfig (..)
  , QueueSlot (..)
  , submitConfig
  , frameSubmitConfig
  , Submitted (..)
  , waitSubmitted
  , waitStage
  , accessStage
  ) where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.Bits ((.&.), (.|.))
import Data.Coerce (coerce)
import Data.Foldable (foldl', for_, toList, traverse_)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.IntSet qualified as IntSet
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Traversable (for)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import Type.Reflection (eqTypeRep, typeRep, type (:~~:) (HRefl))

import Fragr qualified as FG
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import Vulkan.Core10 qualified as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import Vulkan.Core10 qualified as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import Vulkan.Core10 qualified as EventCreateInfo (EventCreateInfo (..))
import Vulkan.Core10 qualified as Vk
import Vulkan.Core10.Enums.EventCreateFlagBits (EventCreateFlagBits (EVENT_CREATE_DEVICE_ONLY_BIT))
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo (..), signalSemaphore, waitSemaphoresSafe)
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore qualified as SemaphoreSignalInfo (SemaphoreSignalInfo (..))
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore qualified as SemaphoreWaitInfo (SemaphoreWaitInfo (..))
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlagBits2 (..), AccessFlags2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2 (..), PipelineStageFlags2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (DependencyInfo (..), MemoryBarrier2 (..), cmdSetEvent2, cmdWaitEvents2)
import Vulkan.Utils.Frame (Frame (..), SubmitExtras (..), allocatePrimary, allocateTimelineSemaphore, frameSubmitExtras, noExtras)
import Vulkan.Utils.FrameGraph.Buffer qualified as Buffer
import Vulkan.Utils.FrameGraph.Image qualified as Image
import Vulkan.Utils.FrameGraph.Recorder (Recorder, flushBarriers, newRecorder, setEventedNodes, setRecorder, setRecorderFamilies, setRecorderHost)
import Vulkan.Zero (zero)

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

{- | The device side of one 'FG.QueueId': where its passes submit, which
family owns their resources, and the pool their buffers come from.
-}
data QueueSlot = QueueSlot
  { queue :: Vk.Queue
  , family :: Word32
  -- ^ names the sides of an ownership transfer across it
  , pool :: Vk.CommandPool
  }

{- | How 'submitGraphQueued' maps a graph onto the device.

'submitConfig' fills everything but the queue table with inert defaults.

The queue table maps the graph's 'FG.QueueId's to real queues and the pools
to take this run's one-time command buffers from (reset the pool to reclaim
them, e.g. per frame in flight). @extras@ adds the frame-level waits and
signals — waits on the queue's first segment, signals on its last (ignored
for the host queue, which has no submit to splice into).
-}
data SubmitConfig = SubmitConfig
  { device :: Vk.Device
  , queues :: FG.QueueId -> QueueSlot
  , hostQueue :: Maybe FG.QueueId
  -- ^ the host queue: its passes execute on the CPU after the submits
  , extras :: FG.QueueId -> SubmitExtras
  , register :: [Submitted] -> IO ()
  {- ^ Called with every queue's completion once the graph is recorded and
  before anything is submitted:
  wire it to the frame's GPU-work list ('Vulkan.Utils.Frame.fGPUWork') and
  reclamation waits the whole graph with no hand-rolled sync — and no race
  when a submit fails midway, since a registered-but-never-signalled value
  degrades to the recycler's wait timeout instead of reclaiming under
  in-flight work.
  -}
  , deferHost :: Maybe (IO () -> IO ())
  {- ^ Hand the ordered host-pass tail to this runner (e.g. the frame's
  deferred work, executed on the recycle thread) instead of executing it
  before returning; the caller's thread then never blocks on the GPU.

  A deferred host pass must not gate a presented image: at
  @vkQueuePresentKHR@ time every signal the present wait depends on must
  already be submitted (VUID 03268), and the deferred signal is not.
  Meter/readback sinks are fine; a host pass feeding the swapchain chain
  must run inline.
  -}
  }

-- | A 'SubmitConfig' with no host queue, no extras, and no completion sink.
submitConfig :: Vk.Device -> (FG.QueueId -> QueueSlot) -> SubmitConfig
submitConfig device queues =
  SubmitConfig
    { device
    , queues
    , hostQueue = Nothing
    , extras = const noExtras
    , register = const (pure ())
    , deferHost = Nothing
    }

{- | A 'SubmitConfig' wired to the frame.

The canonical frame extras ('frameSubmitExtras') ride 'FG.defaultQueue',
completions register into 'fGPUWork' before anything submits, and the host
tail lands in 'fDeferredWork' — the frame's recycle thread runs it, so the
render thread never blocks on the GPU (mind 'deferHost''s presentation
caveat). Override 'extras' by wrapping the field (it is per-queue) rather
than replacing it.
-}
frameSubmitConfig :: Vk.Device -> Frame -> Word32 -> (FG.QueueId -> QueueSlot) -> SubmitConfig
frameSubmitConfig dev f imageIndex queues =
  (submitConfig dev queues)
    { extras = \q -> if q == FG.defaultQueue then frameSubmitExtras f imageIndex else noExtras
    , register = \submitted ->
        for_ submitted \s ->
          atomicModifyIORef' (fGPUWork f) (\jobs -> ((s.semaphore, s.value) : jobs, ()))
    , deferHost = Just (\hostTail -> modifyIORef' (fDeferredWork f) (hostTail :))
    }

{- | Record a compiled graph and submit it, one submit per segment, then
execute the host queue's passes.

The per-run timelines live in the current 'MonadResource' scope, so keep it
open until the returned 'Submitted' values are waited on.

A queue's passes are cut into segments at wait boundaries ('planSegments'),
so mid-stream cross-queue dependencies — device ping-pong,
device→host→device round trips — schedule instead of deadlocking; each
segment's waits hoist to its own front.

Passes on the designated host queue record nothing: after the device
submits go out, each runs on the calling thread (or the 'deferHost' runner)
— its schedule waits realised as a host timeline wait, its body as plain IO
(peek a readback mapping, write an upload one), its signal as
'signalSemaphore', which already-submitted device work may be waiting on.
The graph must have at least one device pass.
-}
submitGraphQueued
  :: (MonadResource m)
  => SubmitConfig
  -> FG.FrameGraph Recorder ()
  -> m [Submitted]
submitGraphQueued config graph = do
  let
    dev = config.device
    queueTable = config.queues
    hostQueue = config.hostQueue
    extras = config.extras
  snap <- FG.snapshot graph
  let
    syncs = mapMaybe (.sync) snap.passes
    (hostSyncs, deviceSyncs) = partition (\s -> Just s.queue == hostQueue) syncs
    (segments, routes) = planSegments deviceSyncs
  case syncs of
    [] -> pure []
    _ -> do
      segmentSlots <- for segments \seg -> do
        let slot = queueTable seg.queue
        cb <- allocatePrimary dev slot.pool
        pure (seg, slot.queue, cb)
      let deviceQids = ordNub [seg.queue | seg <- segments]
      timelines <-
        Map.fromList <$> for (deviceQids <> ordNub [s.queue | s <- hostSyncs]) \qid -> do
          (_, timeline) <- allocateTimelineSemaphore dev 0
          pure (qid, timeline)
      buffers <- case NE.nonEmpty [cb | (_, _, cb) <- segmentSlots] of
        Nothing -> error "submitGraphQueued: a host-only graph has nothing to submit"
        Just ne -> pure ne
      let
        timelineOf qid =
          Map.findWithDefault (error "submitGraphQueued: wait on a queue with no executing pass") qid timelines
        cbFor pid = case Map.lookup pid routes of
          Just ix -> let (_, _, cb) = segmentSlots !! ix in cb
          Nothing -> error "submitGraphQueued: pass outside the planned schedule"

      -- Split-barrier events: one per-run VkEvent per schedule event, its
      -- dependency info built once from both endpoints' covers (SetEvent2 and
      -- WaitEvents2 must match exactly).
      let
        orScope (a, b) (c, d) = (a .|. c, b .|. d)
        eventScopes side = Map.fromListWith orScope [(se.event, foldr (orScope . accessScopes) (zero, zero) se.covers) | s <- deviceSyncs, se <- side s]
        srcScopes = eventScopes (.signalEvents)
        dstScopes = eventScopes (.waitEvents)
      events <-
        Map.traverseWithKey
          (\_ _ -> snd <$> Vk.withEvent dev zero{EventCreateInfo.flags = EVENT_CREATE_DEVICE_ONLY_BIT} Nothing allocate)
          srcScopes
      let
        eventOf eid = Map.findWithDefault (error "submitGraphQueued: wait on an event never signalled") eid events
        depInfoOf eid =
          let
            (srcStage, srcAccess) = Map.findWithDefault (zero, zero) eid srcScopes
            (dstStage, dstAccess) = Map.findWithDefault (zero, zero) eid dstScopes
          in
            zero
              { memoryBarriers =
                  [ zero
                      { srcStageMask = srcStage
                      , srcAccessMask = srcAccess
                      , dstStageMask = dstStage
                      , dstAccessMask = dstAccess
                      }
                  ]
              }
              :: DependencyInfo '[]

      let
        deviceDone =
          [ Submitted{queue = qid, semaphore = timelineOf qid, value = v}
          | qid <- deviceQids
          , let v = maximum [seg.signal | seg <- segments, seg.queue == qid]
          ]
        hostDone =
          [ Submitted{queue = qid, semaphore = timelineOf qid, value = v}
          | qid <- ordNub [s.queue | s <- hostSyncs]
          , let v = maximum [s.signal | s <- hostSyncs, s.queue == qid]
          ]
        done = deviceDone <> hostDone

      recorder <- newRecorder (NE.head buffers)
      -- The families an ownership transfer names its two sides from; the host
      -- queue is not a family and owns nothing.
      setRecorderFamilies recorder \q ->
        if Just q == hostQueue then Vk.QUEUE_FAMILY_IGNORED else (queueTable q).family
      deferredRef <- liftIO (newIORef [])
      FG.addPreExec graph flushBarriers
      -- The release hooks queue producer-side barriers after the pass body.
      FG.addPostExec graph flushBarriers
      let backend =
            FG.QueueBackend
              { FG.beforePass = \psync ->
                  if Just psync.queue == hostQueue
                    then setRecorderHost recorder psync.queue
                    else do
                      let cb = cbFor psync.passId
                      setRecorder recorder psync.queue cb
                      setEventedNodes recorder (IntSet.fromList (map FG.accessId (concatMap (.covers) psync.waitEvents)))
                      unless (null psync.waitEvents) $
                        cmdWaitEvents2
                          cb
                          (V.fromList [eventOf se.event | se <- psync.waitEvents])
                          (V.fromList [SomeStruct (depInfoOf se.event) | se <- psync.waitEvents])
              , FG.afterPass = \psync ->
                  unless (Just psync.queue == hostQueue) $
                    for_ psync.signalEvents \se ->
                      cmdSetEvent2 (cbFor psync.passId) (eventOf se.event) (depInfoOf se.event)
              , FG.invoke = \psync body ->
                  if Just psync.queue == hostQueue
                    then modifyIORef' deferredRef ((psync, body) :)
                    else body
              , FG.completed = pure []
              }
      FG.executeQueued graph backend Nothing recorder ()
      traverse_ Vk.endCommandBuffer buffers

      -- Recorded, nothing submitted yet: registering here means a failure while
      -- recording never leaves the recycler waiting on timelines the unwinding
      -- scope destroys, while a submit failing midway still only costs it the
      -- wait timeout.
      liftIO (config.register done)

      let
        firstSeg = Map.fromListWith (\_ old -> old) [(seg.queue, ix) | (ix, (seg, _, _)) <- zip [0 :: Int ..] segmentSlots]
        lastSeg = Map.fromList [(seg.queue, ix) | (ix, (seg, _, _)) <- zip [0 ..] segmentSlots]
      liftIO $ for_ (zip [0 ..] segmentSlots) \(ix, (seg, vkQueue, cb)) -> do
        let
          ex = extras seg.queue
          derived = [(timelineOf p, stages, value) | (p, (value, stages)) <- Map.toAscList seg.fronts]
          submitWaits = derived <> (if Map.lookup seg.queue firstSeg == Just ix then ex.waits else [])
          submitSignals =
            (timelineOf seg.queue, seg.signal)
              : (if Map.lookup seg.queue lastSeg == Just ix then ex.signals else [])
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

      -- The host passes, in schedule order: wait, run, signal — inline, or
      -- handed whole to the 'deferHost' runner.
      liftIO do
        deferred <- reverse <$> readIORef deferredRef
        let hostTail = for_ deferred \(psync, body) -> do
              unless (null psync.waits) $
                void $
                  waitSemaphoresSafe
                    dev
                    zero
                      { SemaphoreWaitInfo.semaphores = V.fromList [timelineOf w.queue | w <- psync.waits]
                      , SemaphoreWaitInfo.values = V.fromList [w.value | w <- psync.waits]
                      }
                    maxBound
              body
              signalSemaphore dev zero{SemaphoreSignalInfo.semaphore = timelineOf psync.queue, SemaphoreSignalInfo.value = psync.signal}
        case config.deferHost of
          Nothing -> hostTail
          Just runner -> unless (null deferred) (runner hostTail)
      pure done

{- | One planned submit: a contiguous run of one queue's passes whose
cross-queue waits all hoist to its front.
-}
data SegmentPlan = SegmentPlan
  { queue :: FG.QueueId
  , fronts :: Map FG.QueueId (Word64, Vk.PipelineStageFlags)
  {- ^ per producer queue: the timeline value to wait for, at the covered
  accesses' stages
  -}
  , signal :: Word64
  -- ^ the value the segment's submit signals (its passes' max)
  }

{- | Cut each queue's pass stream into segments at wait boundaries.

A pass joins its queue's open segment when every wait it carries is already
implied by the segment's front (same producer, value not above the front's;
its stages widen the mask). Anything else — a higher value, a producer the
segment has not waited on — closes the segment and opens a new one fronted
by the pass's own waits. Front waits therefore only reference passes
registered before the segment's first pass, which makes the segment graph
acyclic by construction: no cycle check, no rejected schedules.

Waiting a mid-segment value completes when that segment's submit does — a
timeline wait is @>=@, so coarsening the signal points is sound.
-}
planSegments :: [FG.PassSync] -> ([SegmentPlan], Map Int Int)
planSegments syncs = (toList segs, routes)
  where
    (_, segs, routes) = foldl' step (Map.empty, Seq.empty, Map.empty) syncs
    step
      :: (Map FG.QueueId Int, Seq.Seq SegmentPlan, Map Int Int)
      -> FG.PassSync
      -> (Map FG.QueueId Int, Seq.Seq SegmentPlan, Map Int Int)
    step (open, acc, rts) s =
      let
        needs = Map.fromListWith mergeWait [(w.queue, (w.value, waitStage w)) | w <- s.waits]
        covered seg =
          Map.foldrWithKey
            (\p (v, _) ok -> ok && maybe False ((v <=) . fst) (Map.lookup p seg.fronts))
            True
            needs
      in
        case Map.lookup s.queue open of
          Just ix
            | seg <- Seq.index acc ix
            , covered seg ->
                ( open
                , Seq.adjust
                    (\sg -> SegmentPlan{queue = sg.queue, fronts = Map.unionWith mergeWait sg.fronts needs, signal = max sg.signal s.signal})
                    ix
                    acc
                , Map.insert s.passId ix rts
                )
          _ ->
            let ix = Seq.length acc
            in ( Map.insert s.queue ix open
               , acc Seq.|> SegmentPlan{queue = s.queue, fronts = needs, signal = s.signal}
               , Map.insert s.passId ix rts
               )
    mergeWait (v1, st1) (v2, st2) = (max v1 v2, st1 .|. st2)

-- | Order-preserving dedup for the short queue lists.
ordNub :: (Ord a) => [a] -> [a]
ordNub = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | x `Set.member` seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

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

Dispatched on the access's resource type; an adapter this module does not
know about carries no decodable scope and over-synchronizes.
-}
accessStage :: FG.Access -> Vk.PipelineStageFlags
accessStage (FG.Access h flags) = resourceStage h flags

{- | An access's synchronization2 scope (stage + access mask), for an event's
dependency info.

Same dispatch and over-synchronization rules as 'accessStage'.
-}
accessScopes :: FG.Access -> (PipelineStageFlags2, AccessFlags2)
accessScopes (FG.Access (_ :: FG.Handle r) flags) = case flags of
  Nothing -> fullScope
  Just f
    | Just HRefl <- eqTypeRep (typeRep @r) (typeRep @Image.ManagedImage) ->
        fromState (Image.usageState f).stage (Image.usageState f).access
    | Just HRefl <- eqTypeRep (typeRep @r) (typeRep @Buffer.ManagedBuffer) ->
        fromState (Buffer.usageState f).stage (Buffer.usageState f).access
    | otherwise -> fullScope
  where
    fullScope = (PIPELINE_STAGE_2_ALL_COMMANDS_BIT, ACCESS_2_MEMORY_READ_BIT .|. ACCESS_2_MEMORY_WRITE_BIT)
    fromState st ac
      | st == zero || st .&. Vk.PIPELINE_STAGE_HOST_BIT /= zero = fullScope
      | otherwise = (stage2 st, access2 ac)

-- The synchronization1 bits are valid synchronization2 bits verbatim.
stage2 :: Vk.PipelineStageFlags -> PipelineStageFlags2
stage2 s = coerce (fromIntegral (coerce s :: Word32) :: Word64)

access2 :: Vk.AccessFlags -> AccessFlags2
access2 a = coerce (fromIntegral (coerce a :: Word32) :: Word64)

{- | The stage a resource's typed flags decode to, sanitized for a wait mask.

@HOST@ (forbidden in @pWaitDstStageMask@ — host reads order via the
host-side timeline wait, so the device scope may be anything), an empty
stage, and an unknown resource type (no scope information) all widen to
@ALL_COMMANDS@.
-}
resourceStage :: forall r. (FG.Resource r) => FG.Handle r -> Maybe (FG.Flags r) -> Vk.PipelineStageFlags
resourceStage _handle = \case
  Nothing -> Vk.PIPELINE_STAGE_ALL_COMMANDS_BIT
  Just flags
    | stage == zero || stage .&. Vk.PIPELINE_STAGE_HOST_BIT /= zero ->
        Vk.PIPELINE_STAGE_ALL_COMMANDS_BIT
    | otherwise -> stage
    where
      stage
        | Just HRefl <- eqTypeRep (typeRep @r) (typeRep @Image.ManagedImage) = (Image.usageState flags).stage
        | Just HRefl <- eqTypeRep (typeRep @r) (typeRep @Buffer.ManagedBuffer) = (Buffer.usageState flags).stage
        | otherwise = Vk.PIPELINE_STAGE_ALL_COMMANDS_BIT
