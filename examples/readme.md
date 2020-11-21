# vulkan-examples

## The examples

### `info`

For the vulkan instance and all physical devices it dumps layer and extension
properties to stdout. It also prints features and properties for all physical
devices.

### `resize`

A nice example of rendering into a window which can be resized. It's not a
single file `triangle` like `sdl-triangle`, but rather builds a couple of nice
abstractions to make the code a little nicer.

It renders a Julia set according the mouse position in the window.

The [`resourcet` package](https://hackage.haskell.org/package/resourcet) is
used to ensure resources are deallocated.

The [`autoapply` package](https://hackage.haskell.org/package/autoapply) is
used to write the boilerplate of passing some global handles to vulkan
functions.

### `hlsl`

A nicer example of rendering into a window which can be resized, the shaders
are written in HLSL and compiled with the `glslc` tool from
[Shaderc](https://github.com/google/shaderc).

If you don't have this tool installed then you might want to turn off the Cabal
flag `have-shaderc` to stop this example from building.

It's very similar to *resize* but has been tidied up in a few places.

It renders a triangle.

The [`resourcet` package](https://hackage.haskell.org/package/resourcet) is
used to ensure resources are deallocated.

The [`autoapply` package](https://hackage.haskell.org/package/autoapply) is
used to write the boilerplate of passing some global handles to vulkan
functions.

### `offscreen`

This example:

- Renders a triangle to an image
- Copies the image contents to a CPU-mapped image
- Writes that image to "triangle.png"

It is a pretty minimal example of rendering something.

Like the `resize` example,
[`resourcet`](https://hackage.haskell.org/package/resourcet) and
[`autoapply`](https://hackage.haskell.org/package/autoapply) are used to make
resource and global management less painful.

### `compute`

This example renders an image of [the Julia
set](https://en.wikipedia.org/wiki/Julia_set) at `f(z) = z^2 -0.8 + 0.156i`.

This rendering is performed by a compute shader writing to a buffer.

This program includes examples of:

- Buffer allocation with VulkanMemoryAllocator.
- Descriptor set creation and binding
- Compute shader dipatch
- Convenient shader creation using the `Vulkan.Utils.ShaderQQ.comp` QuasiQuoter

Like the `resize` example,
[`resourcet`](https://hackage.haskell.org/package/resourcet) and
[`autoapply`](https://hackage.haskell.org/package/autoapply) are used to make
resource and global management less painful.

### `sdl-triangle`

This opens a window using SDL and renders a triangle.

The `managed` package is used for ensuring resources are deallocated.

Exit with `q`, `escape` or the window exit button.

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
sdl-triangle: SDLCallFailed {sdlExceptionCaller = "SDL.Init.init", sdlFunction = "SDL_Init", sdlExceptionError = "No available video device"}
```

It's because the pure shell instantiated by `stack --nix` doesn't include the
`XDG_RUNTIME_DIR` variable which is necessary to find the vulkan ICD files. To
fix this pass the `--no-nix-pure` flag to stack thusly:

```bash
stack --system-ghc --nix --no-nix-pure run sdl-triangle
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

For the examples using SDL (`resize`, `sdl-triangle`):

- If SDL is unable to find `libvulkan.so`, you can set either `LD_LIBRARY_PATH`
  or `SDL_VULKAN_LIBRARY`, it must find the same `libvulkan.so` that the
  `sdl-triangle` binary was compiled against.

- If you run into the exception `DLCallFailed {sdlExceptionCaller =
  "SDL.Video.Vulkan.vkLoadLibrary", sdlFunction = "SDL_Vulkan_LoadLibrary",
  sdlExceptionError = "Installed Vulkan doesn't implement the VK_KHR_surface
  extension"}` it might be because the vulkan loader is unable to find the
  driver. To check if this is the case you can set `VK_ICD_FILENAMES` to the
  icd json file of your desired driver.
