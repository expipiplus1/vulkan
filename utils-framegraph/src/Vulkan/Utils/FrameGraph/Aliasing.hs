{-| Plan-time memory aliasing for graph-owned transients.

Two entries whose lifetimes cannot overlap may share one allocation, decided
from the compiled schedule before anything is recorded — no run-time
reclamation, because a backend's @destroyResource@ fires while /recording/,
long before the GPU is done with the memory.

The raw material is 'FG.EntryInfo.live': each entry's first and last
executing pass, as positions in execution order. Disjoint ranges are
necessary but __not sufficient__, and the gap is the whole point of this
module: positions order the executing passes /globally/, while passes on
different queues run concurrently. Two ranges can be disjoint in position
and still overlap in time.

So aliasing rides on the schedule's happens-before relation, not on the
positions: entry @a@ may hand its memory to @b@ only if @a@'s last pass
happens-before @b@'s first — same queue (submission order, plus the takeover
barrier), or a timeline wait that transitively orders the two. 'happensBefore'
derives exactly that from the 'FG.PassSync' waits the compiler already emits.

The caller supplies the compatibility classes (size, alignment, memory type):
'planAliases' only answers "may these share", and returns each group in
takeover order so the backend can seed the newcomer's barrier from its
predecessor's final state.
-}
module Vulkan.Utils.FrameGraph.Aliasing
  ( Candidate (..)
  , planAliases
  , happensBefore
  , Schedule
  , scheduleOf
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Data.Word (Word64)

import Fragr qualified as FG

{- | An entry in the running for aliasing: its id and its live range, as
inclusive positions into the executing passes ('FG.EntryInfo.live').

Only entries the graph /owns/ belong here — an import's memory is the
caller's whatever its range says ('FG.EntryInfo.imported' tells them apart).
-}
data Candidate = Candidate
  { entryId :: Int
  , live :: (Int, Int)
  }
  deriving (Eq, Show)

{- | The happens-before relation over one run's executing passes, indexed by
position (the same positions 'FG.EntryInfo.live' reports).

Per pass: the timeline value it signals on its own queue, and a vector clock
of what it has necessarily observed on every queue.
-}
newtype Schedule = Schedule (V.Vector (FG.QueueId, Word64, Map FG.QueueId Word64))

{- | Derive the relation from the compiled schedule.

Takes the executing passes /in execution order/ — the driver's
@mapMaybe (.sync) snapshot.passes@, which is the very list
'FG.EntryInfo.live' indexes.

Each pass observes its queue predecessor's clock and, for every 'FG.Wait',
the clock of the pass whose signal it waits on; then it stamps its own
signal. Waits carry a watermark value rather than an exact signal, so the
lookup takes the latest signal at or below it.

A wait can only name a value an earlier pass signalled (a consumer cannot
hold a handle its producer has not yet made), so the lookup always lands.
Were it ever to miss, the clock stays empty and the pass looks ordered after
nothing — fewer aliases, never an unsound one.
-}
scheduleOf :: [FG.PassSync] -> Schedule
scheduleOf syncs = Schedule (V.fromList (reverse clocks))
  where
    -- Fold in execution order, carrying: each queue's latest clock, and the
    -- clock at each (queue, signalled value) so a wait can look one up.
    (_, _, clocks) = foldl step (Map.empty, Map.empty, []) syncs

    step
      :: ( Map FG.QueueId (Map FG.QueueId Word64)
         , Map (FG.QueueId, Word64) (Map FG.QueueId Word64)
         , [(FG.QueueId, Word64, Map FG.QueueId Word64)]
         )
      -> FG.PassSync
      -> ( Map FG.QueueId (Map FG.QueueId Word64)
         , Map (FG.QueueId, Word64) (Map FG.QueueId Word64)
         , [(FG.QueueId, Word64, Map FG.QueueId Word64)]
         )
    step (latest, atValue, acc) s =
      let
        inherited = Map.findWithDefault Map.empty s.queue latest
        waited =
          [ Map.findWithDefault Map.empty (q, v') atValue
          | w <- s.waits
          , let q = w.queue
          , -- the newest signal on q at or below the watermark
          Just (v', _) <- [Map.lookupLE (q, w.value) atValue >>= keyOn q]
          ]
        merged = Map.unionsWith max (inherited : waited)
        clock = Map.insertWith max s.queue s.signal merged
      in
        ( Map.insert s.queue clock latest
        , Map.insert (s.queue, s.signal) clock atValue
        , (s.queue, s.signal, clock) : acc
        )

    -- lookupLE crosses queue boundaries in the (queue, value) key order;
    -- keep it only when it landed on the queue we asked about.
    keyOn q ((q', v), c) = if q == q' then Just (v, c) else Nothing

{- | Does the pass at the first position necessarily complete before the pass
at the second begins?

True when the second's clock has observed the first's signal — same queue
(submission order), or a wait chain that transitively reaches it.
-}
happensBefore :: Schedule -> Int -> Int -> Bool
happensBefore (Schedule passes) i j
  | i == j = False
  | otherwise = case (passes V.!? i, passes V.!? j) of
      (Just (queue, signal, _), Just (_, _, after)) ->
        Map.findWithDefault 0 queue after >= signal
      _ -> False

{- | Pack candidates into groups that may share one allocation.

The caller has already split them into a compatibility class (same memory
type, and a block big enough for the largest). Within a class, two entries
may share only if the schedule orders one's last pass before the other's
first — 'happensBefore', not merely disjoint positions.

Each group comes back in takeover order, so the backend can seed a
newcomer's aliasing barrier from its predecessor's final state (its contents
are undefined, but the memory dependency on the previous user is real).
-}
planAliases :: Schedule -> [Candidate] -> [[Candidate]]
planAliases sched = foldl place [] . sortOn (fst . (.live))
  where
    -- Greedy: first group whose every member is ordered against this one.
    place groups c = case break (all (compatible c)) groups of
      (before, g : after) -> before <> ((g <> [c]) : after)
      (before, []) -> before <> [[c]]

    compatible c m = ordered m c || ordered c m
    ordered x y = happensBefore sched (snd x.live) (fst y.live)
