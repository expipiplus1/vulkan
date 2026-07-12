# TODO

Live work, roughly in dependency order. Nothing here is a known bug (those are
in [FIXME.md](FIXME.md)); items verified done moved to [stale.md](stale.md).
Upstream asks for fragr live in [TODO-fragr.md](TODO-fragr.md).

# Frame graph

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

The QFOT machinery is in place (`d28955c02`), so the shape the cave wants is
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
in one graph, or the chunk must be CONCURRENT. The `recordShadows` bake would
join the same graph (see [FIXME.md](FIXME.md)).

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

## Release plumbing

- **fragr is local-pathed** in `stack.yaml` (`../fragr`, branch
  `typed-handles-flags`). Push it and re-pin to the commit before release.
- **`fGPUWork` partial-submit window** is closed for recording failures
  (`bc1550c9e` registers after recording), but a submit failing midway still
  leaves the earlier submits' completions registered — the safe direction (the
  recycler waits, times out after 1s, warns). Nothing to do unless that 1s
  stall is ever unacceptable.

# Scene (visibility-buffer)

The auto-exposure work is mostly landed — clamp, EV-space asymmetric
adaptation, metering off a bloom mip, exposure through an SSBO, specular under
shadow. What survives triage:

## The meter is still a log-average

`Pipeline/Luminance/Shader.hs:40` still sums `log(max(L, 1e-4))`. Log-average
is symmetric in log space — a black pixel at the floor pulls the mean down
exactly as hard as a bright pixel pulls it up. It is robust to bright outliers
and maximally fragile to dark ones, and a cave is nothing but dark ones. The
`1e-4` is not a noise floor, it is a vote: every empty pixel asserts "the scene
is 1e-4 nits".

The fix is a **luminance histogram + percentile mean** (Tardif / Narkowicz).
Bin `log2(L)` into 256 bins over a fixed EV window, bin 0 reserved for
sub-floor pixels. A reduce pass averages bin centers across a pixel-percentile
band — discard the bottom ~60% and top ~5%, drop bin 0 wholly. Black rock and
empty background are then excluded by construction rather than by a magic
constant, and the meter converges on the lit chamber.

The max-exposure clamp (now 8) hides the worst of it; this is the actual fix.

## Authorable light units

Intensity reads as `0.0x` because it is candela-like (radiance = I/d²) and the
world is sub-unit. Add a scene constant `metersPerUnit`, multiply `dot(dv,dv)`
by `metersPerUnit²`, and `intensity` becomes honest candela. Then optionally a
**photometric key** (`EV100 = log2(L*100/12.5)`, `exposure = 1/(1.2 *
2^EV100)`) — blunt: that is `0.18/L` up to a constant, so it buys
authorability and a place to hang exposure compensation, not a different image.
Touches `Lights.hs`, the shade shader and the tonemap together: one change or
none.

## Cheap polish

- **Center-weighted metering.** Weight the histogram increment by a Gaussian
  (sigma ~0.35 of frame) or `1 - r²`, so "point at the dark floor and it
  brightens" feels like a camera rather than a bug. Cheap once the histogram
  exists; fixes nothing on its own.
- **Bloom composite is a lerp, not an add.** `mix(hdrC, bloomC,
  pc.bloomStrength)` (`Pipeline/Tonemap/Shader.hs:62`) dims the source by 4%
  rather than adding energy. Defensible. Noted, not a problem.
- **Specular AA from analytic derivatives** — roughness widening / Toksvig off
  the reconstructed normal/roughness derivatives; the payoff of dropping
  barycentrics on shiny curved knots.

## Residual reuse sweep

Mostly landed (12 of 14 pipeline modules are on `Vulkan.Utils.SpirV.Pipeline`;
Gamma/Luminance/Tonemap use `Vulkan.Utils.Descriptors`). Left: `Pipeline/Bloom.hs`
still hand-rolls its descriptor writes, and two pipeline modules are off the
reflected-layout path. Finish or decide not to.
