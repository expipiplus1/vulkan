# vulkan-examples

## The examples

### `info`

For the vulkan instance and all physical devices it dumps layer and extension
properties to stdout. It also prints features and properties for all physical
devices.

### `sdl-triangle`

This opens a window using SDL and renders a triangle.

You'll need to have `glslangValidator` in `$PATH` when compiling as shaders are
built in a QuasiQuoter.

If SDL is unable to find `libvulkan.so`, you can set either `LD_LIBRARY_PATH`
or `SDL_VULKAN_LIBRARY`, it must find the same `libvulkan.so` that the
`sdl-triangle` binary was compiled against.

If you run into the exception `DLCallFailed {sdlExceptionCaller = "SDL.Video.Vulkan.vkLoadLibrary", sdlFunction = "SDL_Vulkan_LoadLibrary", sdlExceptionError = "Installed Vulkan doesn't implement the VK_KHR_surface extension"}`
it might be because the vulkan loader is unable to find the driver. To check if
this is the case you can set `VK_ICD_FILENAMES` to the icd json file of your
desired driver.

Exit with `q`, `escape` or the window exit button.

## Building with Nix while not on NixOS

Vulkan and OpenGL programs have some trouble running on non-NixOS OSs when
built with nix.

If you provision `libvulkan.so` (the Vulkan loader) with nix and you're not on
NixOS, you'll have to use [NixGL](https://github.com/guibou/nixGL) to run your
programs. For this reason it's recommended to use the system-provided
`libvulkan.so`. Make sure to change the package list in `stack.yaml` if you do
this.

## Running the examples with Stack on NixOS

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
