module Main (main) where

import Control.Exception (ErrorCall, try)
import Control.Monad (void)
import Data.Bits ((.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (sort, subsequences)
import Data.Word (Word32, Word64)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Fragr qualified as FG
import Vulkan.CStruct.Extends (SomeStruct (..))
import Vulkan.Core10 qualified as Vk
import Vulkan.Utils.FrameGraph.Aliasing (Candidate (..), happensBefore, planAliases, scheduleOf)
import Vulkan.Utils.FrameGraph.Buffer (ManagedBuffer (..), newManagedBuffer)
import Vulkan.Utils.FrameGraph.Buffer qualified as Buffer
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), newManagedImage, newManagedImageLayer, newManagedImageMip, newSliceRegistry)
import Vulkan.Utils.FrameGraph.Image qualified as Image
import Vulkan.Utils.FrameGraph.Recorder (Barriers (..), Recorder, TransferSide (..), chainedNode, newRecorder, recorderQueue, recordingBackend, setRecorder, setRecorderFamilies, setRecorderHost, takeBarriers)
import Vulkan.Zero (zero)

main :: IO ()
main = defaultMain (testGroup "vulkan-utils-framegraph" [ordering, aliasing, exhaustive, slices, transfers, boundary])

----------------------------------------------------------------
-- Slice wrap registry
----------------------------------------------------------------

{- | Expect the wrap to be rejected; touch @alive@ after, so the clashing
wrapper stays reachable through the check's garbage collection.
-}
rejectedOver :: ManagedImage -> IO ManagedImage -> IO ()
rejectedOver alive wrap =
  try @ErrorCall wrap >>= \case
    Left _ -> void (readIORef alive.stateRef)
    Right _ -> assertFailure "overlapping wrap accepted"

img :: Vk.Image
img = Vk.Image 1

slices :: TestTree
slices =
  testGroup
    "slice wrap registry"
    [ testCase "disjoint mips of one image wrap fine" do
        reg <- newSliceRegistry
        _ <- newManagedImageMip reg img Vk.IMAGE_ASPECT_COLOR_BIT 0
        void (newManagedImageMip reg img Vk.IMAGE_ASPECT_COLOR_BIT 1)
    , testCase "a second wrapper over a live mip is fatal" do
        reg <- newSliceRegistry
        a <- newManagedImageMip reg img Vk.IMAGE_ASPECT_COLOR_BIT 0
        rejectedOver a (newManagedImageMip reg img Vk.IMAGE_ASPECT_COLOR_BIT 0)
    , testCase "a whole-image wrapper clashes with a live mip" do
        reg <- newSliceRegistry
        a <- newManagedImageMip reg img Vk.IMAGE_ASPECT_COLOR_BIT 3
        rejectedOver a (newManagedImage reg img Vk.IMAGE_ASPECT_COLOR_BIT)
    , testCase "disjoint aspects of one subresource wrap fine" do
        reg <- newSliceRegistry
        _ <- newManagedImage reg img Vk.IMAGE_ASPECT_DEPTH_BIT
        void (newManagedImage reg img Vk.IMAGE_ASPECT_STENCIL_BIT)
    , testCase "mip vs layer wrappers clash where they intersect" do
        reg <- newSliceRegistry
        -- Both cover (mip 0, layer 0).
        a <- newManagedImageMip reg img Vk.IMAGE_ASPECT_COLOR_BIT 0
        rejectedOver a (newManagedImageLayer reg img Vk.IMAGE_ASPECT_COLOR_BIT 0)
    , testCase "dropping the old wrapper legalizes the re-wrap" do
        reg <- newSliceRegistry
        void (newManagedImage reg img Vk.IMAGE_ASPECT_COLOR_BIT)
        -- The registry collects before accusing, so the dropped wrapper's
        -- entry dies here instead of poisoning the image forever.
        void (newManagedImage reg img Vk.IMAGE_ASPECT_COLOR_BIT)
    , -- The renderer-scope semantics: a new scope's registry owes nothing to
      -- the old one's wrappers, even while they are still reachable.
      testCase "a fresh registry accepts a handle another one holds live" do
        old <- newSliceRegistry
        a <- newManagedImage old img Vk.IMAGE_ASPECT_COLOR_BIT
        reg <- newSliceRegistry
        _ <- newManagedImage reg img Vk.IMAGE_ASPECT_COLOR_BIT
        void (readIORef a.stateRef)
    ]

----------------------------------------------------------------
-- Ownership transfer: the QFOT pair semantics, no device
----------------------------------------------------------------

-- | Queues 0 and 2 share family 0; queue 1 is family 1; queue 3 is the host.
family :: FG.QueueId -> Word32
family (FG.QueueId q) = case q of
  0 -> 0
  1 -> 1
  2 -> 0
  _ -> Vk.QUEUE_FAMILY_IGNORED

{- | A recorder over a null command buffer: the hooks' barriers are only ever
drained with 'takeBarriers', never flushed into it — so every hook call is
followed by a drain, keeping the batch's overlap flush unreachable.
-}
fakeRecorder :: IO Recorder
fakeRecorder = do
  rec <- newRecorder zero
  setRecorderFamilies rec family
  pure rec

onQueue :: Recorder -> Int -> IO ()
onQueue rec q = setRecorder rec (FG.QueueId q) zero

-- | Freshly wrapped (through @wrap@), brought to 'Image.ColorAttachment' on queue 0.
producedImageWith :: (ManagedImage -> ManagedImage) -> Recorder -> IO ManagedImage
producedImageWith wrap rec = do
  reg <- newSliceRegistry
  mi <- wrap <$> newManagedImage reg img Vk.IMAGE_ASPECT_COLOR_BIT
  onQueue rec 0
  Image.queueTransition rec 0 mi Image.ColorAttachment
  _ <- takeBarriers rec
  pure mi

producedImage :: Recorder -> IO ManagedImage
producedImage = producedImageWith id

-- | Freshly wrapped and storage-written on queue 0.
producedBuffer :: Recorder -> IO ManagedBuffer
producedBuffer rec = do
  mb <- newManagedBuffer buf
  onQueue rec 0
  Buffer.queueTransition rec 0 mb storageWrite
  _ <- takeBarriers rec
  pure mb

buf :: Vk.Buffer
buf = Vk.Buffer 2

-- | The batch's single image barrier: (families, layouts, access masks).
imageHalf :: Barriers -> IO ((Word32, Word32), (Vk.ImageLayout, Vk.ImageLayout), (Vk.AccessFlags, Vk.AccessFlags))
imageHalf b = case b.images of
  [SomeStruct ib] ->
    pure
      ( (ib.srcQueueFamilyIndex, ib.dstQueueFamilyIndex)
      , (ib.oldLayout, ib.newLayout)
      , (ib.srcAccessMask, ib.dstAccessMask)
      )
  bs -> assertFailure ("expected one image barrier, got " <> show (length bs))

-- | The batch's single buffer barrier: (families, access masks).
bufferHalf :: Barriers -> IO ((Word32, Word32), (Vk.AccessFlags, Vk.AccessFlags))
bufferHalf b = case b.buffers of
  [SomeStruct bb] ->
    pure
      ( (bb.srcQueueFamilyIndex, bb.dstQueueFamilyIndex)
      , (bb.srcAccessMask, bb.dstAccessMask)
      )
  bs -> assertFailure ("expected one buffer barrier, got " <> show (length bs))

sampled :: Image.Usage
sampled = Image.Sampled Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT

storage :: Image.Usage
storage = Image.StorageRead Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT

storageWrite :: Buffer.Usage
storageWrite = Buffer.StorageWrite Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT

storageRead :: Buffer.Usage
storageRead = Buffer.StorageRead Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT

transfers :: TestTree
transfers =
  testGroup
    "ownership transfer"
    [ testCase "an owned hand-off records matching release/acquire halves" do
        rec <- fakeRecorder
        mi <- producedImage rec
        Image.transferOwnership Release rec 7 (FG.QueueId 1) mi sampled
        (relFams, relLayouts, relAccess) <- imageHalf =<< takeBarriers rec
        relFams @?= (0, 1)
        relLayouts @?= (Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
        relAccess @?= (Vk.ACCESS_COLOR_ATTACHMENT_READ_BIT .|. Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT, zero)
        -- The release performs the transition; the slot keeps the state it saw.
        readIORef mi.releasedRef >>= (@?= Just (Image.usageState Image.ColorAttachment))
        readIORef mi.stateRef >>= (@?= Image.usageState sampled)
        onQueue rec 1
        Image.transferOwnership Acquire rec 7 (FG.QueueId 0) mi sampled
        (acqFams, acqLayouts, acqAccess) <- imageHalf =<< takeBarriers rec
        -- The spec wants the halves identical up to the ignored scopes.
        acqFams @?= relFams
        acqLayouts @?= relLayouts
        acqAccess @?= (zero, Vk.ACCESS_SHADER_READ_BIT)
        readIORef mi.releasedRef >>= (@?= Nothing)
        readIORef mi.queueRef >>= (@?= Just (FG.QueueId 1))
        chainedNode rec 7 >>= assertBool "the acquire marks the node chained"
    , testCase "a melted acquire leaves the hand-off for a pending owned one" do
        rec <- fakeRecorder
        mi <- producedImage rec
        -- One version, two transfers: a host readback plus an owned hand-off
        -- to family 1 — the shape a host + device fan-out compiles to.
        Image.transferOwnership Release rec 7 (FG.QueueId 3) mi Image.HostRead
        _ <- takeBarriers rec
        Image.transferOwnership Release rec 8 (FG.QueueId 1) mi storage
        (relFams, relLayouts, _) <- imageHalf =<< takeBarriers rec
        relFams @?= (0, 1)
        -- The host's melted acquire must not spend the slot the owned one needs.
        setRecorderHost rec (FG.QueueId 3)
        Image.transferOwnership Acquire rec 7 (FG.QueueId 0) mi Image.HostRead
        readIORef mi.releasedRef >>= (@?= Just (Image.usageState Image.HostRead))
        onQueue rec 1
        Image.transferOwnership Acquire rec 8 (FG.QueueId 0) mi storage
        (acqFams, acqLayouts, _) <- imageHalf =<< takeBarriers rec
        acqFams @?= relFams
        acqLayouts @?= relLayouts
        readIORef mi.releasedRef >>= (@?= Nothing)
    , testCase "a late host acquire does not rewind the device tracking" do
        rec <- fakeRecorder
        mi <- producedImage rec
        -- The same fan-out, but hooks in the Driver's real order: it defers
        -- host passes, so the owned acquire lands before the host's melted one.
        Image.transferOwnership Release rec 7 (FG.QueueId 3) mi Image.HostRead
        _ <- takeBarriers rec
        Image.transferOwnership Release rec 8 (FG.QueueId 1) mi storage
        _ <- takeBarriers rec
        onQueue rec 1
        Image.transferOwnership Acquire rec 8 (FG.QueueId 0) mi storage
        _ <- takeBarriers rec
        setRecorderHost rec (FG.QueueId 3)
        Image.transferOwnership Acquire rec 7 (FG.QueueId 0) mi Image.HostRead
        readIORef mi.stateRef >>= (@?= Image.usageState storage)
        readIORef mi.queueRef >>= (@?= Just (FG.QueueId 1))
    , testCase "an owned acquire without its release half is fatal" do
        rec <- fakeRecorder
        mi <- producedImage rec
        Image.transferOwnership Release rec 7 (FG.QueueId 1) mi sampled
        _ <- takeBarriers rec
        onQueue rec 1
        Image.transferOwnership Acquire rec 7 (FG.QueueId 0) mi sampled
        _ <- takeBarriers rec
        -- The slot is single-use: a duplicate acquire cannot replay the
        -- consumed hand-off, and recording a guessed half would be worse.
        try @ErrorCall (Image.transferOwnership Acquire rec 8 (FG.QueueId 0) mi sampled) >>= \case
          Left _ -> pure ()
          Right () -> assertFailure "an unpaired owned acquire was accepted"
    , testCase "a CONCURRENT hand-off is a producer-side transition, no families named" do
        rec <- fakeRecorder
        mi <- producedImageWith Image.sharedAcrossQueues rec
        Image.transferOwnership Release rec 7 (FG.QueueId 1) mi sampled
        (fams, layouts, _) <- imageHalf =<< takeBarriers rec
        fams @?= (Vk.QUEUE_FAMILY_IGNORED, Vk.QUEUE_FAMILY_IGNORED)
        layouts @?= (Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
        onQueue rec 1
        Image.transferOwnership Acquire rec 7 (FG.QueueId 0) mi sampled
        acq <- takeBarriers rec
        assertBool "the acquire half melts" (null acq.images)
    , testCase "a same-family hop chains to the semaphore, keeping contents" do
        rec <- fakeRecorder
        mi <- producedImage rec
        onQueue rec 2
        Image.queueTransition rec 1 mi sampled
        (fams, layouts, access) <- imageHalf =<< takeBarriers rec
        fams @?= (Vk.QUEUE_FAMILY_IGNORED, Vk.QUEUE_FAMILY_IGNORED)
        layouts @?= (Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
        -- The semaphore already made the writes available.
        fst access @?= zero
    , testCase "a cross-family write acquires by discarding" do
        rec <- fakeRecorder
        mi <- producedImage rec
        onQueue rec 1
        Image.queueTransition rec 1 mi Image.TransferDst
        (fams, layouts, _) <- imageHalf =<< takeBarriers rec
        fams @?= (Vk.QUEUE_FAMILY_IGNORED, Vk.QUEUE_FAMILY_IGNORED)
        layouts @?= (Vk.IMAGE_LAYOUT_UNDEFINED, Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL)
    , testCase "a cross-family read of an unshared image is fatal" do
        rec <- fakeRecorder
        mi <- producedImage rec
        onQueue rec 1
        try @ErrorCall (Image.queueTransition rec 1 mi sampled) >>= \case
          Left _ -> pure ()
          Right () -> assertFailure "an unhanded cross-family read was accepted"
    , testCase "the buffer pair mirrors the image rules, minus layout" do
        rec <- fakeRecorder
        mb <- producedBuffer rec
        Buffer.transferOwnership Release rec 7 (FG.QueueId 1) mb storageRead
        (relFams, relAccess) <- bufferHalf =<< takeBarriers rec
        relFams @?= (0, 1)
        relAccess @?= (Vk.ACCESS_SHADER_WRITE_BIT, zero)
        readIORef mb.releasedRef >>= (@?= Just (Buffer.usageState storageWrite))
        onQueue rec 1
        Buffer.transferOwnership Acquire rec 7 (FG.QueueId 0) mb storageRead
        (acqFams, acqAccess) <- bufferHalf =<< takeBarriers rec
        acqFams @?= relFams
        acqAccess @?= (zero, Vk.ACCESS_SHADER_READ_BIT)
        readIORef mb.releasedRef >>= (@?= Nothing)
        readIORef mb.queueRef >>= (@?= Just (FG.QueueId 1))
    , testCase "a melted buffer release records nothing but arms the slot" do
        rec <- fakeRecorder
        mb <- producedBuffer rec
        -- No layout to move: a same-family release is pure bookkeeping.
        Buffer.transferOwnership Release rec 7 (FG.QueueId 2) mb storageRead
        melted <- takeBarriers rec
        assertBool "a same-family release records no barrier" (null melted.buffers)
        onQueue rec 2
        Buffer.transferOwnership Acquire rec 7 (FG.QueueId 0) mb storageRead
        readIORef mb.releasedRef >>= (@?= Just (Buffer.usageState storageWrite))
    ]

----------------------------------------------------------------
-- Frame-boundary ownership: importOwned* driven end to end, no device
----------------------------------------------------------------

{- | A graph collecting every non-empty barrier batch as (queue, batch).
The drains are installed /before/ the import claims the flush slot, so
'Vulkan.Utils.FrameGraph.Recorder.flushBarriers' always finds an empty
batch and the zero command buffer is never recorded into.
-}
drainingGraph :: IO (FG.FrameGraph Recorder (), IORef [(FG.QueueId, Barriers)])
drainingGraph = do
  g <- FG.newFrameGraph @Recorder @()
  batches <- newIORef []
  let drain r = do
        q <- recorderQueue r
        b <- takeBarriers r
        case (b.images, b.buffers) of
          ([], []) -> pure ()
          _ -> modifyIORef' batches ((q, b) :)
  FG.addPreExec g drain
  FG.addPostExec g drain
  pure (g, batches)

runBoundary :: Recorder -> FG.FrameGraph Recorder () -> [(FG.QueueId, FG.FamilyId)] -> IO ()
runBoundary rec g partition = do
  FG.compileWith partition g
  FG.executeQueued g (recordingBackend rec (const zero)) Nothing rec ()

-- | The collected batches: an acquire on top of the release it consumed.
boundaryPair :: IORef [(FG.QueueId, Barriers)] -> IO ((FG.QueueId, Barriers), (FG.QueueId, Barriers))
boundaryPair batches =
  readIORef batches >>= \case
    [acq, rel] -> pure (rel, acq)
    bs -> assertFailure ("expected the release and acquire halves, got " <> show (length bs))

boundary :: TestTree
boundary =
  testGroup
    "frame-boundary ownership"
    [ testCase "an owned image import pairs a foreign first read across frames" do
        rec <- fakeRecorder
        mi <- producedImage rec
        (g, batches) <- drainingGraph
        h <- Image.importOwnedImage g "ext" mi
        _ <- FG.addPass g "Sample" (FG.setQueue (FG.QueueId 1) *> FG.readWith h sampled *> FG.setSideEffect) (\_ -> pure ())
        runBoundary rec g [(FG.QueueId 0, FG.FamilyId 0), (FG.QueueId 1, FG.FamilyId 1)]
        ((qr, rel), (qa, acq)) <- boundaryPair batches
        qr @?= FG.QueueId 0
        qa @?= FG.QueueId 1
        (relFams, relLayouts, _) <- imageHalf rel
        (acqFams, acqLayouts, _) <- imageHalf acq
        relFams @?= (0, 1)
        acqFams @?= relFams
        relLayouts @?= (Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
        acqLayouts @?= relLayouts
        readIORef mi.releasedRef >>= (@?= Nothing)
        readIORef mi.queueRef >>= (@?= Just (FG.QueueId 1))
    , testCase "the pair melts within the owner's family" do
        rec <- fakeRecorder
        mi <- producedImage rec
        (g, batches) <- drainingGraph
        h <- Image.importOwnedImage g "ext" mi
        _ <- FG.addPass g "Sample" (FG.setQueue (FG.QueueId 2) *> FG.readWith h sampled *> FG.setSideEffect) (\_ -> pure ())
        runBoundary rec g [(FG.QueueId 0, FG.FamilyId 0), (FG.QueueId 2, FG.FamilyId 0)]
        readIORef batches >>= \case
          [(q, rel)] -> do
            q @?= FG.QueueId 0
            (fams, layouts, _) <- imageHalf rel
            -- A same-family hand-off is a producer-side transition, no QFOT.
            fams @?= (Vk.QUEUE_FAMILY_IGNORED, Vk.QUEUE_FAMILY_IGNORED)
            layouts @?= (Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)
          bs -> assertFailure ("expected one melted release, got " <> show (length bs))
    , testCase "an untouched wrapper imports plainly" do
        rec <- fakeRecorder
        reg <- newSliceRegistry
        mi <- newManagedImage reg img Vk.IMAGE_ASPECT_COLOR_BIT
        (g, batches) <- drainingGraph
        h <- Image.importOwnedImage g "ext" mi
        _ <- FG.addPass g "Draw" (FG.setQueue (FG.QueueId 1) *> FG.writeWith_ h Image.ColorAttachment) (\_ -> pure ())
        runBoundary rec g [(FG.QueueId 0, FG.FamilyId 0), (FG.QueueId 1, FG.FamilyId 1)]
        readIORef batches >>= \case
          [(q, b)] -> do
            q @?= FG.QueueId 1
            (fams, layouts, _) <- imageHalf b
            fams @?= (Vk.QUEUE_FAMILY_IGNORED, Vk.QUEUE_FAMILY_IGNORED)
            layouts @?= (Vk.IMAGE_LAYOUT_UNDEFINED, Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL)
          bs -> assertFailure ("expected one plain transition, got " <> show (length bs))
    , testCase "an owned buffer import pairs the same way, minus layout" do
        rec <- fakeRecorder
        mb <- producedBuffer rec
        (g, batches) <- drainingGraph
        h <- Buffer.importOwnedBuffer g "ext" mb
        _ <- FG.addPass g "ReadBack" (FG.setQueue (FG.QueueId 1) *> FG.readWith h storageRead *> FG.setSideEffect) (\_ -> pure ())
        runBoundary rec g [(FG.QueueId 0, FG.FamilyId 0), (FG.QueueId 1, FG.FamilyId 1)]
        ((qr, rel), (qa, acq)) <- boundaryPair batches
        qr @?= FG.QueueId 0
        qa @?= FG.QueueId 1
        (relFams, relAccess) <- bufferHalf rel
        (acqFams, acqAccess) <- bufferHalf acq
        relFams @?= (0, 1)
        acqFams @?= relFams
        relAccess @?= (Vk.ACCESS_SHADER_WRITE_BIT, zero)
        acqAccess @?= (zero, Vk.ACCESS_SHADER_READ_BIT)
        readIORef mb.queueRef >>= (@?= Just (FG.QueueId 1))
    ]

----------------------------------------------------------------
-- Exhaustive: every small schedule, against an independent oracle
----------------------------------------------------------------

{- | Every schedule of @n@ passes over @q@ queues, with every combination of
waits — a pass may wait each foreign queue's current watermark, or not.

That is the whole space the compiler can hand us at this size, so the checks
below are not samples: they are closed over it.
-}
schedules :: Int -> Int -> [[FG.PassSync]]
schedules n q = go 0 (replicate q 0) []
  where
    go i counters acc
      | i == n = [reverse acc]
      | otherwise = do
          queue <- [0 .. q - 1]
          -- Wait on any subset of the other queues' latest signals.
          waitMask <- subsequences [j | j <- [0 .. q - 1], j /= queue]
          let
            signal = (counters !! queue) + 1
            counters' = [if j == queue then signal else c | (j, c) <- zip [0 ..] counters]
            waits = [(j, counters !! j) | j <- waitMask, counters !! j > 0]
          go (i + 1) counters' (pass i queue signal waits : acc)

{- | The relation, derived a second way: build the edges explicitly and take
their transitive closure. Deliberately unlike the vector clock it checks —
an oracle that shared the implementation would prove nothing.
-}
oracle :: [FG.PassSync] -> Int -> Int -> Bool
oracle syncs = \i j -> IntSet.member i (IntMap.findWithDefault IntSet.empty j reach)
  where
    indexed = zip [0 ..] syncs
    -- Direct predecessors: the previous pass on the same queue, and every
    -- pass whose signal a wait names at or below its watermark.
    preds s =
      [ i
      | (i, p) <- indexed
      , p.queue == s.queue && p.signal < s.signal
          || or [p.queue == w.queue && p.signal <= w.value | w <- s.waits]
      ]
    -- Transitive closure, accumulated in position order: every predecessor is
    -- at an earlier position, so its own reach set is already in the map.
    reach = foldl step IntMap.empty indexed
    step m (j, s) =
      let ps = preds s
      in IntMap.insert j (IntSet.unions (IntSet.fromList ps : [IntMap.findWithDefault IntSet.empty i m | i <- ps])) m

exhaustive :: TestTree
exhaustive =
  testGroup
    "exhaustive (5 passes, 3 queues, all wait combinations)"
    [ testCase "happensBefore agrees with the closure oracle everywhere" do
        let bad =
              [ (syncs, i, j)
              | syncs <- schedules 5 3
              , let s = scheduleOf syncs
              , let ref = oracle syncs
              , i <- [0 .. 4]
              , j <- [0 .. 4]
              , happensBefore s i j /= ref i j
              ]
        assertBool (show (take 1 bad)) (null bad)
    , -- The invariant the whole feature rests on: anything the planner puts in
      -- one block must be pairwise ordered, so the tenancies cannot overlap.
      testCase "every planned group is pairwise ordered" do
        let bad =
              [ (syncs, x, y)
              | syncs <- schedules 5 3
              , let s = scheduleOf syncs
              , -- One entry per pass, plus a long-lived one spanning the run.
              cs <- [[Candidate i (i, i) | i <- [0 .. 4]], [Candidate 9 (0, 4), Candidate 0 (1, 1), Candidate 1 (2, 3)]]
              , g <- planAliases s cs
              , x <- g
              , y <- g
              , x /= y
              , not (happensBefore s (snd x.live) (fst y.live))
              , not (happensBefore s (snd y.live) (fst x.live))
              ]
        assertBool (show (take 1 bad)) (null bad)
    ]

-- A pass on a queue, signalling a value, waiting on foreign queues.
pass :: Int -> Int -> Word64 -> [(Int, Word64)] -> FG.PassSync
pass pid queue signal waits =
  FG.PassSync
    { FG.passId = pid
    , FG.name = "p"
    , FG.queue = FG.QueueId queue
    , FG.waits = [FG.Wait{FG.queue = FG.QueueId q, FG.value = v, FG.covers = []} | (q, v) <- waits]
    , FG.signal = signal
    , FG.waitEvents = []
    , FG.signalEvents = []
    , FG.acquires = []
    , FG.releases = []
    }

groupIds :: [[Candidate]] -> [[Int]]
groupIds = sort . map (map (.entryId))

ordering :: TestTree
ordering =
  testGroup
    "happensBefore"
    [ testCase "submission order on one queue" do
        let s = scheduleOf [pass 0 0 1 [], pass 1 0 2 []]
        assertBool "0 before 1" (happensBefore s 0 1)
        assertBool "1 not before 0" (not (happensBefore s 1 0))
    , testCase "a pass is not before itself" do
        let s = scheduleOf [pass 0 0 1 []]
        assertBool "irreflexive" (not (happensBefore s 0 0))
    , -- The case positions alone get wrong: two queues, no wait between them.
      -- Position 0 precedes position 1, but they run concurrently.
      testCase "concurrent queues are unordered despite positions" do
        let s = scheduleOf [pass 0 0 1 [], pass 1 1 1 []]
        assertBool "0 not before 1" (not (happensBefore s 0 1))
        assertBool "1 not before 0" (not (happensBefore s 1 0))
    , testCase "a wait orders across queues" do
        -- q1's pass waits for q0's value 1.
        let s = scheduleOf [pass 0 0 1 [], pass 1 1 1 [(0, 1)]]
        assertBool "0 before 1" (happensBefore s 0 1)
        assertBool "1 not before 0" (not (happensBefore s 1 0))
    , testCase "ordering is transitive through a third queue" do
        -- q0:p0 -> q1:p1 (waits q0) -> q2:p2 (waits q1). p0 must precede p2.
        let s = scheduleOf [pass 0 0 1 [], pass 1 1 1 [(0, 1)], pass 2 2 1 [(1, 1)]]
        assertBool "0 before 2 transitively" (happensBefore s 0 2)
        assertBool "2 not before 0" (not (happensBefore s 2 0))
    , testCase "a wait on a later value still orders the earlier pass" do
        -- Watermarked waits name a value, not a pass: waiting q0's 2 must
        -- also observe q0's pass that signalled 1.
        let s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 1 1 [(0, 2)]]
        assertBool "0 before 2" (happensBefore s 0 2)
        assertBool "1 before 2" (happensBefore s 1 2)
    , testCase "a wait does not order passes after the signal" do
        -- q1 waits q0's value 1; q0's *later* pass (value 2) is not ordered.
        let s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 1 1 [(0, 1)]]
        assertBool "1 not before 2" (not (happensBefore s 1 2))
    ]

aliasing :: TestTree
aliasing =
  testGroup
    "planAliases"
    [ testCase "sequential lifetimes on one queue share a block" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 0 3 []]
          cs = [Candidate 10 (0, 0), Candidate 11 (1, 2)]
        groupIds (planAliases s cs) @?= [[10, 11]]
    , testCase "overlapping lifetimes do not" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 0 2 []]
          cs = [Candidate 10 (0, 1), Candidate 11 (1, 1)]
        groupIds (planAliases s cs) @?= [[10], [11]]
    , -- The silent-corruption case: disjoint positions, concurrent queues.
      testCase "disjoint positions on concurrent queues do NOT share" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 1 1 []]
          cs = [Candidate 10 (0, 0), Candidate 11 (1, 1)]
        groupIds (planAliases s cs) @?= [[10], [11]]
    , testCase "a wait makes the same two safe to share" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 1 1 [(0, 1)]]
          cs = [Candidate 10 (0, 0), Candidate 11 (1, 1)]
        groupIds (planAliases s cs) @?= [[10, 11]]
    , testCase "a group admits a third only if ordered against all of it" do
        -- p0,p1,p2 on q0 (ordered); p3 on q1, unordered with everything.
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 0 3 [], pass 3 1 1 []]
          cs = [Candidate 10 (0, 0), Candidate 11 (1, 1), Candidate 12 (2, 2), Candidate 13 (3, 3)]
        groupIds (planAliases s cs) @?= [[10, 11, 12], [13]]
    , testCase "groups come back in takeover order" do
        let
          s = scheduleOf [pass 0 0 1 [], pass 1 0 2 [], pass 2 0 3 []]
          cs = [Candidate 12 (2, 2), Candidate 10 (0, 0), Candidate 11 (1, 1)]
        map (map (.entryId)) (planAliases s cs) @?= [[10, 11, 12]]
    ]
