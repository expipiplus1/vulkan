# Depth pyramid consumers

Which flavor of the depth machinery each prospective effect needs, so the
pyramid grows once, deliberately. Today: single-channel min-reduce
(`HiZ.format` = r32f), reverse-Z, half-res base, built same-frame after
geometry.

## Min (farthest) — what we already have

- **Two-phase occlusion culling** — render last frame's visible set, rebuild
  the pyramid (already post-geometry), cull the rest against the same frame.
  Kills the one-frame popping of the current previous-frame cull.
- **Froxel fog early-out** — terminate a froxel column at the farthest opaque
  depth in its tile.

## Max (nearest) — the screen-space marching family

- **SSR / SSGI** — hi-z ray skip needs "nearest surface in this cell" to hop
  empty space conservatively. SSGI is the same marcher pointed at the
  hemisphere, accumulating diffuse; the AO blur machinery denoises it.
- **Contact shadows** — short screen-space march toward each light, catching
  the small-scale occlusion the EVSM cubes blur away.
- **Nearest-depth upsampling** — composite any half-res effect (AO, fog, SSR)
  by picking the half-res sample whose depth bracket contains the full-res
  pixel; wants min *and* max per quad.

## Min+max bounds per tile

- **Tiled/clustered light culling** — per-tile depth bounds shrink the light
  list. Pays off only past a handful of lights.

## Linear view depth only, no pyramid

- **Depth of field** — CoC from view z, half-res gather, bilateral composite.
- **Soft particles**, **screen-space fog falloff**.
- **TAA / motion-blur reprojection** — depth for disocclusion rejection.

These read the `zNear/d` linearization or the SSAO prepass `normals.w` — that
target is effectively a half-res linear-depth+normal G-lite.

## Takeaway

Min-only serves exactly one consumer (culling). Nearly everything else wants
the nearest-depth channel, so when SSR forces the decision, widen `HiZ.format`
to rg16f min+max once — cull keeps `.r`, marchers skip on `.g` — instead of
growing a second pyramid per effect. The reduce change is symmetric; the cost
is the pyramid's bandwidth/footprint doubling (small at half-res base).
