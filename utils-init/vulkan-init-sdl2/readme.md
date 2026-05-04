# vulkan-init-sdl2

Vulkan initialization helpers for SDL2 windows. Provides the SDL-specific
glue around `vulkan-utils` so apps can build a Vulkan `Instance` and a
`SurfaceKHR` from an `SDL.Window` with a couple of calls.
