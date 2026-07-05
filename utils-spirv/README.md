# vulkan-utils-spirv

Generate Haskell data types and Vulkan descriptor-set / pipeline-layout
`*CreateInfo` values from compiled SPIR-V, at compile time, via
[`spirv-reflect`](https://hackage.haskell.org/package/spirv-reflect-ffi)
reflection and [`gl-block`](https://hackage.haskell.org/package/gl-block)
std140/std430 layout.

## Types from a shader

A Template Haskell splice generates a record — with a std140/std430 `Storable`
derived via gl-block — for every uniform / storage / push-constant block the
shader declares:

```haskell
import Vulkan.Utils.SpirV.TH (reflectShaderTypes)

-- e.g. `Scene { view :: Mat4, lightDir :: Vec3, time :: Float }` (geomancy types),
-- ready to poke straight into a mapped buffer.
reflectShaderTypes "shaders/scene.vert.spv"
```

## Pipeline layout from reflection

`allocateReflectedLayout` merges the descriptor-set layouts and push-constant
ranges across a family of shaders — stage flags OR-ed, shared blocks
cross-checked — into one `PipelineLayout`. `allocateGraphicsPipeline` then builds
each pipeline against it, folding in the vertex stage's reflected vertex input:

```haskell
import Data.SpirV.Reflect.FFI (loadBytes)
import Vulkan.Utils.DynamicRendering qualified as Dynamic
import Vulkan.Utils.SpirV.Pipeline (allocateGraphicsPipeline, allocateReflectedLayout)
import Vulkan.Zero (zero)

vertModule <- loadBytes vertSpv
fragModule <- loadBytes fragSpv

-- one layout for the whole family
(_, layout) <- allocateReflectedLayout dev [vertModule, fragModule]

(_, pipeline) <-
  allocateGraphicsPipeline dev layout
    zero{Dynamic.colorFormats = [colorFormat], Dynamic.depthFormat = Just depthFormat}
    () -- specialization; () for none
    [(vertModule, vertSpv), (fragModule, fragSpv)]
```

## Compile-time stage composition

`reflectStageSig` emits a per-shader signature; `MatchInterface` /
`CompatibleResources` then check — at compile time — that the fragment inputs
match the vertex outputs and that any shared descriptor blocks agree. A mismatch
is a type error, not a validation-layer message at runtime:

```haskell
import Vulkan.Utils.SpirV.Stage (CompatibleResources, MatchInterface, reflectStageSig)

reflectStageSig "VertSig" "shaders/scene.vert.spv"
reflectStageSig "FragSig" "shaders/scene.frag.spv"

-- only type-checks if the two stages compose
pipelineComposes :: (MatchInterface VertSig FragSig, CompatibleResources VertSig FragSig) => Bool
pipelineComposes = True
```

## Examples

Four end-to-end, validation-clean programs under
[`examples/`](../examples): `compute-reflect`, `pathtrace-reflect` (buffer
device address / BVH), `mesh-reflect` (a vertex shader driving a z-prepass and a
shaded pass off one merged layout), and `texture-reflect` (colour-attachment-as-
texture with reflected vertex attributes).
