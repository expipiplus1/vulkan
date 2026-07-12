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

## Streaming cave chunks

The QFOT machinery is in place (see below), so the shape the cave wants is
now expressible: generate a chunk's geometry on the compute (or a transfer)
queue, hand it to graphics with an ownership transfer, and drop the chunks
that fall out of reach. What is missing is the *content* side — a chunk
allocator with a free list, per-chunk draw ranges in the object table, and a
residency policy — not any synchronization work.

Note the one QFOT rule that shapes the design: a resource whose contents you
still care about must be handed over by a graph edge (producer pass →
consumer pass). A queue picking up an EXCLUSIVE resource it did not receive
may only *discard* it (the adapters do exactly this on a cross-family write:
`UNDEFINED` old layout). So a chunk's generate → draw hand-off must be edges
in one graph, or the chunk must be CONCURRENT.

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
