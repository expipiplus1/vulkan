# Stale ledger

Entries retired from TODO/FIXME/NOTE, with what made them stale. Verified
against the tree on 2026-07-12. Kept because the *reasoning* in several of
these is still the best record of why the code looks the way it does.

## Done — frame graph (were FIXME.md)

| entry | verdict |
|---|---|
| Package-level multi-queue driver | Done. `Driver.submitGraphQueued` is the queue-map driver; both hand-rolled bridges deleted. Its own follow-ups (segmentation, host passes, sync2 events, `frameSubmitConfig`) all landed too. |
| Orb uploads live outside the graph | Done. The tables are `ManagedBuffer`s, `orbs.upload` declares its writes and the consumers their reads — **zero** `cmdPipelineBarrier` calls left in `recordOrbUploads` (verified). The bake caveat survives as a live FIXME. |

The multi-queue entry's own text is now partly *wrong*, not just done: it
describes "the buffer codec grew a kind-marker bit so mixed access lists
route". That codec no longer exists — fragr's typed `Flags r` replaced the
whole `Word64` encoding, and the driver dispatches on the resource type. Do
not mine that paragraph for design intent.

## Done — auto-exposure (were TODO-scene-lights.md)

| entry | verdict |
|---|---|
| Max-exposure clamp | Done. `Exposure.defaults` is `maxExposure = 8` (was 20). |
| Adapt in EV space, per second, asymmetrically | Done. `Exposure.adapt` smooths `log2` with `1 - exp(-dt/tau)`, `tau = 0.4` toward bright / `2.0` toward dark. |
| `specular()` never calls `shadowVis()` | Done. `Shade/Shader.hs:113` folds `shadowVis` into `radiance`, which both `irr` and `spec` consume — glints no longer leak through walls. |
| `specular()` ignores `colInt.w` | Done (was already marked done in place, with the Blinn-Phong energy normalization that made it survivable). |
| Meter a downsampled mip | Done. The luminance pass reduces a bloom mip (`lumMipFor`), not the full-res target. |
| GPU-resident exposure / drop the readback lag | Done, differently and better than proposed. Exposure lives in a metering SSBO the tonemap reads (`Tonemap/Shader.hs:28`); headless writes it from an in-graph **host meter pass**, so the frame tonemaps at its own metered exposure with no readback lag at all. Windowed keeps a lagged pre-write on purpose — a deferred host pass may not gate a presented image (VUID 03268). |
| `AMBIENT = 0.05` is too high | Superseded. Ambient is no longer a shader constant: it is a push-constant field, SSAO-attenuated. Retune it there if the outside view still reads as mud. |
| `SHADOW_FAR = 3.0` has not moved since `worldScale` went 1.4 → 2 | Superseded — the numbers are three rescales out of date. `worldScale` is now **64** and `SHADOW_FAR` is a specialization constant (`Pipeline/Common.hs:170`, default 3.0) fed from `Shadow.Params`. Whether it is *tuned* is untested; if far corners lose shadows, that is where to look. |
| The whole "root cause / benches" analysis | Stale as a bug report — the interior/outside luminance tables predate the rescale, the specular fix, SSAO and the new tonemap chain, so the numbers mean nothing now. Its *diagnosis* of the log-average meter is still correct and lives on in TODO.md. |

## Done — misc (were TODO-misc.md)

| entry | verdict |
|---|---|
| Shared GLSL struct prelude for Object/Vertex/MeshEntry | Done. `Pipeline/Common.hs` splices `vertexStruct` / `objectStruct` / `meshEntryStruct` / `lightStruct` as `GlslChunk`s. |
| Broad reuse sweep (SpirV.Pipeline, Descriptors.*) | Mostly done — 12 of 14 pipeline modules on `SpirV.Pipeline`, Gamma/Luminance/Tonemap on `Descriptors`. The residue is one live bullet in TODO.md, not a sweep. |
| Cave `cmd[]` word indices / meshId as push constants | Dropped. The unified mesh + object-table renderer removed the hand-indexed draw-command words this was about. |

## Still valid, moved not retired

- `NOTE-depth-effects.md` → `.mgmt/NOTE.md`. Re-verified: `HiZ.format` is
  still `R32_SFLOAT` (min-only, reverse-Z, half-res), so the note's central
  claim — min serves only culling, everything else wants max, widen to rg16f
  *once* when SSR forces it — is unchanged and still the decision record.
- `TODO-fragr.md` → `.mgmt/TODO-fragr.md`. Rounds 1–5 are implemented
  upstream; round 4 (watermark covers) and round 5 (invoke seam) landed on the
  `typed-handles-flags` branch, round 6 (RecycleQueue vs record-then-submit) is
  an open observation.
