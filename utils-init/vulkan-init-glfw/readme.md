# vulkan-init-glfw

Vulkan initialization helpers for GLFW windows. Provides the GLFW-specific
glue around `vulkan-utils` so apps can build a Vulkan `Instance` and a
`SurfaceKHR` from a `GLFW.Window` with a couple of calls.
