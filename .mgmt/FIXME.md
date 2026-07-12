# FIXME

Known-wrong or known-missing things. Verified against the tree 2026-07-12;
entries that turned out to be fixed moved to [stale.md](stale.md).

## Trim or exercise newManagedImageLayer

`newManagedImageSlice` earned its keep (the EVSM array's static/orb slices,
tracked through the bake, the `shadow.orbs` pass and the face dump), but
`newManagedImageLayer` still ships with no caller (verified: no references
outside its own definition). Find one — a per-face shadow refresh would — or
trim it until one exists.

## Gate the headless debug dumps

`Headless.hs` unconditionally runs a full graph execution per debug-material
view (4) plus the beauty render, and 3 copy-submit dumps, writing ~11
artifacts into CWD (`debug-mat-*.png`, `debug-luminance.{hdr,png}`,
`debug-shadow.png`, `debug-instance.png`, `debug-triangle.png`,
`debug-depth.png`, `visibility-buffer.dot`).

Add a `--dump-debug` flag (or env check) so the default path renders once and
saves one PNG. Careful: `dumpDebug` is load-bearing — its depth readback
produces `voidPixels` for a PASS/FAIL check, so only its PNG writes are
separable — and the dump ordering matters (`dumpLumProbe` before the debug
reruns overwrite the probe, `dumpShadowFace` last since it leaves the static
moments slice in TRANSFER_SRC with no in-graph reader to transition it back).

The original entry said 6 executions; the meter-then-re-render double run is
gone (the host meter pass tonemaps at the frame's own exposure), so it is 5.

## recordShadows is the last hand-synchronized submit

The EVSM bake runs outside any graph, as a fence-waited setup one-shot. Fine
today — nothing else touches those slices until frames start — but it is now
the only hand-synchronized submit left in the example, and a streaming cave
would want it in-graph (see [TODO.md](TODO.md), streaming chunks).
