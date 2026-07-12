# TODO: fragr

Feedback from driving fragr with two real Vulkan consumers (a multi-pass
deferred/post-processing frame of ~15 passes across 2 queues, and a
compute+blit+present frame with per-frame topology changes).

## Round 1 — implemented, verified in use (2026-07-10)

All of the first feedback round landed and checks out against both
consumers (adapter ported, examples build and run, the async cross-queue
path passes its determinism checks):

- `Flags` widened to `Word64`; `flagsIgnored` sentinel replaced by
  `Maybe Flags` (`read`/`write` = no flags, no hook). The adapter now packs
  a full stage mask without dodging a magic value, with headroom for sync2.
- `readWith` returns `()`.
- `setPreExec` — the hook-flush seam between pre-access hooks and the pass
  callback.
- `preRelease`/`preAcquire` hooks on the `Resource` contract;
  `releases`/`acquires` are `Transfer`s carrying the consuming flags.
- `waits` are `Wait`s and events are `SyncEvent`s, each listing the
  accesses they protect — wait/barrier scopes are now derivable.
- `executeQueued` takes `Maybe RecycleQueue`, with the upfront
  `RecycleQueueRequired` check.
- `finalize` names the terminal-state idiom; `executingQueues` exposes the
  schedule's queues; build-per-frame is documented as the design.

## Round 2 — implemented, verified in use (2026-07-10, `9845b76`)

All follow-ups landed and check out against both consumers (ported, all
examples build, headless determinism checks pass on the async path):

- `finalize` places its synthetic pass on the producing pass's queue —
  no manufactured cross-queue edge.
- `setPostExec` — the release-side flush seam, symmetric to `setPreExec`.
- `addPass_` / `write_` / `writeWith_` for sink passes and terminal
  writes; with `readWith` returning `()`, sinks (present, readback,
  metering) now register with zero discarded binds.
- The `Transfer` merge contract and the `Nothing`-covers over-synchronize
  fallback are documented.

Beyond the ask, the BlockArguments restructure replaced both callback
shapes with monads, and it reads better than what we asked for:

- Setup runs in `Build` — declarations implicitly target the open pass, so
  the `b`-threading (`FG.readWith b h flags`) is gone and setup helpers
  are plain `Build` blocks.
- Exec runs in `Exec` (`MonadIO`/`MonadUnliftIO`) with `askCtx`/`get`/
  `getDesc` — the `\_data resources ctx ->` prefix collapsed to at most
  `\_ ->`, and adapters can offer context projections as `Exec` actions
  (ours now has `recordingCommandBuffer`).

## Round 3 — nothing blocking

One observation, not an ask: after the port, every remaining `\_ ->` exec
hole (8 across the two consumers) is a mid-chain pass whose setup returns
a handle the *caller* threads onward while the exec callback — closing
over prebaked descriptor sets — ignores it. The pass-data channel only
pays off for transients fetched via `get`; import-only consumers never
use it. A caller-only variant would erase those holes, but that may be
one variant too many.

## Adapter follow-up (ours, not fragr's): implement preRelease

DONE 2026-07-12 (typed-handles-flags branches): both adapters implement
`preRelease = queueTransition` with the consumer's typed flags, the driver
installs the `addPostExec` flush, and the two manual pass-tail
`transitionImageTo` hand-offs in `Scene.hs` are gone. Verified 0 hazards
under sync validation on all three drivers.

## Round 5 — QueueBackend.invoke (host-as-a-queue) — IMPLEMENTED on the branch

Landed in the fragr working tree together with round-4 review: `QueueBackend`
gained `invoke :: PassSync -> IO () -> IO ()`, wrapping each pass's whole step
(beforePass, hooks+body, afterPass). Device queues run it inline (recording);
the vulkan driver stashes a designated host queue's passes and runs them after
the submits — schedule waits as `vkWaitSemaphores`, signal as
`vkSignalSemaphore` — so host readbacks/uploads join the dependency graph
(demonstrated by visibility-buffer headless's `host.readback` pass). Transient
retirement stays safe through the RecycleQueue's `completed`-gated path.
Submit segmentation at wait boundaries landed driver-side too, so mid-graph
device→host→device round trips schedule now.

## Round 4 — watermark dedup discards covers a stage-scoped backend needs — FIXED upstream

`Compile.waitsMap`'s per-(consumer,producer) watermark drops a later pass's
`Wait` when an earlier same-queue pass already awaited a ≥ value — together
with its `covers` ("a dropped wait needs no scope"). That is sound only for a
full-scope wait: a Vulkan semaphore wait orders subsequent commands *limited
to `waitDstStageMask`*, and `Sync.hs` explicitly invites backends to derive
that mask from `covers`. A dropped wait whose accesses sit at a stage absent
from the kept waits' covers is then unordered (reachable: register B1 reading
A@2 at FRAGMENT, then B2 reading A@1 at COMPUTE — B2's wait is dropped, the
submit mask is FRAGMENT-only).

Fixed on the fragr branch (with a regression test): dropped waits migrate
their covers to the kept wait, all edge kinds included. The driver's interim
acquire-widening workaround is retired.

## Round 6 — RecycleQueue granularity vs record-then-submit backends

Observation, not (yet) an ask. For a backend that records the whole graph
and only then submits (ours), the RecycleQueue's runtime value-gated
reclamation is superseded twice over: coarse lifetimes by the frame scope
(`fResources` + `fGPUWork`, which `SubmitConfig.register` now feeds from the
schedule), and fine-grained transient reuse by *plan-time aliasing* — the
backend holds the whole schedule before recording, so overlapping transients
with disjoint lifetimes can share an allocation deterministically, no
`completed` polling involved. Runtime reclamation only pays in an
execute-as-you-go backend. If plan-time aliasing lands, the useful fragr
surface is lifetime info per entry (first/last executing user — recoverable
from a snapshot today) rather than the RecycleQueue machinery.
