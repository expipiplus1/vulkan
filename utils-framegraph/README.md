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
h' <- -- in a pass setup: FG.writeWith b h (usageFlags ColorAttachment)
      -- a later pass: FG.readWith b h' (usageFlags SampledFragment)
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
differs from the tracked one. It does not (yet) insert write-after-write /
read-after-write barriers when the layout, stage and access are all unchanged.
