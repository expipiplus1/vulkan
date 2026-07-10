# vulkan-utils-framegraph

Vulkan adapter for the [`fragr`](https://gitlab.com/dpwiz/fragr) frame graph:
resource types whose `preRead` / `preWrite` hooks place `cmdPipelineBarrier`
image transitions automatically, so passes declare *what* they access and the
graph records the barriers.

`Vulkan.Utils.FrameGraph.Image` provides `ManagedImage` — an image plus a
tracked `ImageState` (layout, stage, access). Declare an access with a `Usage`
(encoded into `Fragr.Flags` via `usageFlags`); the hook diffs the tracked
state against the usage's target and emits the transition, then updates the
tracked state.

```haskell
import Fragr qualified as FG
import Vulkan.Utils.FrameGraph.Image (ImageDesc (..), Usage (..), newManagedImage, usageFlags)

offscreen <- newManagedImage image Vk.IMAGE_ASPECT_COLOR_BIT
h <- FG.importResource g "offscreen" (ImageDesc "offscreen") offscreen
h' <- -- in a pass setup: FG.writeWith h (usageFlags ColorAttachment)
      -- a later pass: FG.readWith h' (usageFlags SampledFragment)
```

`Ctx ManagedImage` is the `CommandBuffer` the barriers record into; run the
graph with `FG.execute g cmdBuffer ()`.

## Scope

`ManagedImage` participates as an *imported* resource: the graph tracks its
layout and places barriers but does not own its allocation. Graph-owned
transient images with deferred (frames-in-flight-safe) reclamation are future
work — that needs `FG.executeQueued` + a `RecycleQueue`, since single-queue
`execute` would free a transient during recording, before the GPU has run.

The layout-diff model inserts a barrier whenever the target `ImageState`
differs from the tracked one, and also for every write access with the state
unchanged (same-state WAW); only read-after-read skips the barrier.

When consecutive accesses land on different queues the barrier's source scope
is replaced by the destination stage with no access mask: execution ordering
and memory availability must come from the driver's inter-queue semaphore, and
the barrier chains to its wait. Two caveats follow: the submit wait's
`dstStageMask` must cover the consuming usage's stage, and cross-queue-family
access is only supported for CONCURRENT-shared images — no ownership
release/acquire pair is emitted (`PassSync` acquires/releases are dropped by
`recordingBackend`), so an EXCLUSIVE image's contents are undefined on the new
family.
