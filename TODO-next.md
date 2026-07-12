# TODO next

The frame graph's remaining work, roughly in dependency order. Everything
here is unblocked; nothing is a known bug.

## Plan-time transient aliasing

The payoff of `EntryInfo.live` (fragr `2f85715`): entries whose live ranges do
not overlap are never resident together, so they can share one allocation.
Decided before recording — no reclamation bookkeeping, which is why fragr's
`RecycleQueue` stays unused (see `TODO-fragr.md` round 6).

Shape: the driver reads the live ranges off `FG.snapshot`, packs disjoint
ranges into shared backing memory, and the adapters bind to a suballocation
(VMA: `vmaAllocateMemory` + `vmaBindImageMemory2` with an offset) instead of
owning one. Two obligations follow:

- Aliased resources need an **aliasing barrier** at each handover — the new
  user's contents are undefined, so it must start from `UNDEFINED`. The
  tracker already does exactly this for a fresh resource (`undefinedState`),
  so it composes with the hook path: reset the tracked state when an entry
  takes over an aliased block.
- The adapters stop being import-only for the aliased class. `createResource`
  binds, `destroyResource` unbinds; everything else stays.

Worth measuring before building: the fullscreen targets (colorHDR, tone,
display, normals, aoBlur, the bloom base) are same-extent and mostly
short-lived, so they are the candidates. This is a **memory** win, not a perf
one, and it is the only item here that can corrupt silently if the ranges are
wrong — hence the fragr-side test landing first.

## Queue-family ownership transfers

The `sharedAcrossQueues` guard (`bc1550c9e`) makes an unshared cross-queue
access fatal instead of silent, but the real transfer is still unimplemented:
everything that crosses is CONCURRENT.

`FG.preRelease` / `FG.preAcquire` already fire on the right sides with the
consuming access's flags, and `PassSync.releases` / `.acquires` carry
`Transfer{handle, peer, flags}`. What is missing is a `QueueId -> family` map
(the driver has the queue table; family is not in it) so the barrier pair can
set real `srcQueueFamilyIndex` / `dstQueueFamilyIndex`. The release barrier's
dst scope and the acquire's src scope are ignored by spec and the pair must
otherwise match — the same exact-match discipline as the sync2 event pair
already in `Driver.depInfoOf`.

Only worth doing when something actually wants EXCLUSIVE (a large dedicated
target where CONCURRENT's cost shows up). Until then the guard is the feature.

## Uploads through the host queue

`orbs.upload` is a tracked graph pass now (`904b31bcd`) but still uses
`cmdUpdateBuffer` from a device queue. `Buffer.HostWrite` exists and is
unexercised: the tables could be mapped and written by a **host pass**
instead, which is the last unused half of the host-queue design (readbacks
are covered by headless's `host.readback` and `host.meter`).

Constraint from `baeb5522c` / `aa532c0e7`: a deferred host pass must never
gate a presented image (VUID-vkQueuePresentKHR-pWaitSemaphores-03268 — the
present's wait chain cannot depend on a signal that is not yet submitted). An
upload pass at the *head* of the frame is fine inline; it is only the tail
sinks that want `deferHost`.

## Smaller

- **`fGPUWork` partial-submit window** is closed for recording failures
  (`bc1550c9e` registers after recording), but a submit failing midway still
  leaves the earlier submits' completions registered — which is the safe
  direction (the recycler waits, times out after 1s, warns). Nothing to do
  unless the 1s stall is ever unacceptable.
- **`newManagedImageLayer`** still has no caller. Find one (a per-face shadow
  refresh would) or trim it.
- **Headless debug dumps** run unconditionally: 6 graph executions and ~11
  artifacts per run. Gate behind `--dump-debug`; careful, `dumpDebug`'s depth
  readback feeds a PASS/FAIL check, so only its PNG writes are separable.
- **fragr is local-pathed** in `stack.yaml` (`../fragr`, branch
  `typed-handles-flags`). Push it and re-pin to the commit before release.
