# vulkan-examples

## The examples

### `info`

For the vulkan instance and all physical devices it dumps layer and extension
properties to stdout. It also prints features and properties for all physical
devices.

### The Triangle

- `triangle-sdl2` using `vulkan-init-sdl2`
- `triangle-glfw` using `vulkan-inid-glfw`
- `triangle-headless` using `vulkan-utils` (without the windowing)
- `triangle-dynamic` featuring dynamic rendering.
- `hlsl` the shader is authored in HLSL.

### `resize`

A nice example of rendering into a window which can be resized. It uses the
recycling `Frame` machinery from `lib/` (timeline semaphores + a recycle
channel for binary semaphores and command pools).

It renders a Julia set according the mouse position in the window.

The [`resourcet` package](https://hackage.haskell.org/package/resourcet) is
used to ensure resources are deallocated.

### `compute`

This example renders an image of [the Julia
set](https://en.wikipedia.org/wiki/Julia_set) at `f(z) = z^2 -0.8 + 0.156i`.

This rendering is performed by a compute shader writing to a buffer.

This program includes examples of:

- Buffer allocation with VulkanMemoryAllocator.
- Descriptor set creation and binding
- Compute shader dipatch
- Convenient shader creation using the `Vulkan.Utils.ShaderQQ.comp` QuasiQuoter

### Shader reflection

Mostly some test cases for working with Haskell data generated from SPIR-V reflection.

- `mesh-reflect`
- `texture-reflect`
- `compute-reflect`, same as `compute`.
- `pathtrace-reflect`, the Ray Tracing in One Weekend scene, compute version. Does not actually use ray tracing hardware.

### `rays`

An example of using the `VK_KHR_ray_tracing_pipeline` extension.

### `glitch-post`

Dynamic rendering test case. A compute-draw-compute sandwich synched to a music player.

### `visibility-buffer`

Framegraph megademo, cramming **lots** of stuff for the sake of it.
It is so sprawling it has its own "[anatomy of a frame](./visibility-buffer/docs/index.md)" page.

## Building and Running

You'll need to have `glslangValidator` in `$PATH` when compiling as shaders are
built in a QuasiQuoter.

There's a `default.nix` file in this directory which build and load the other
packages in this repo. This is useful to get a Hoogle database with `vulkan`,
`VulkanMemoryAllocator` and friends for developing examples. To use this with
cabal you'll need to make a `cabal.project` file in this directory to stop
cabal wanting to configure all the packages in this repo.

```yaml
packages:
  ./
```

### macOS

Jonathan Merritt has made an excellent video detailing how to set up everything 
necessary for running the examples on macOS 
[here](https://www.youtube.com/watch?v=BaBt-CNBfd0).

### Building with Nix while not on NixOS

Vulkan and OpenGL programs have some trouble running on non-NixOS OSs when
built with nix.

If you provision `libvulkan.so` (the Vulkan loader) with nix and you're not on
NixOS, you'll have to use [NixGL](https://github.com/guibou/nixGL) to run your
programs. For this reason it's recommended to use the system-provided
`libvulkan.so`. Make sure to change the package list in `stack.yaml` if you do
this.

### Running the examples with Stack on NixOS

If you run into

```
error: XDG_RUNTIME_DIR not set in the environment.
triangle-sdl2: SDLCallFailed {sdlExceptionCaller = "SDL.Init.init", sdlFunction = "SDL_Init", sdlExceptionError = "No available video device"}
```

It's because the pure shell instantiated by `stack --nix` doesn't include the
`XDG_RUNTIME_DIR` variable which is necessary to find the vulkan ICD files. To
fix this pass the `--no-nix-pure` flag to stack thusly:

```bash
stack --system-ghc --nix --no-nix-pure run triangle-sdl2
```

### Running the examples with SwiftShader

Note that not all of the examples run under SwiftShader.

```bash
# This will set VK_ICD_FILENAMES to the swiftshader ICD
# It will also enable the timeline-semaphore emulation layer as swiftshader
# lacks support
nix-shell --arg withSwiftshader true
cabal run hlsl
```

### Troubleshooting

For the examples using SDL (`resize`, `triangle-sdl2`):

- If SDL is unable to find `libvulkan.so`, you can set either `LD_LIBRARY_PATH`
  or `SDL_VULKAN_LIBRARY`, it must find the same `libvulkan.so` that the
  `triangle-sdl2` binary was compiled against.

- If you run into the exception `DLCallFailed {sdlExceptionCaller =
  "SDL.Video.Vulkan.vkLoadLibrary", sdlFunction = "SDL_Vulkan_LoadLibrary",
  sdlExceptionError = "Installed Vulkan doesn't implement the VK_KHR_surface
  extension"}` it might be because the vulkan loader is unable to find the
  driver. To check if this is the case you can set `VK_ICD_FILENAMES` to the
  icd json file of your desired driver.
