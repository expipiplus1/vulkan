# OpenXR Haskell bindings

Slightly high level Haskell bindings to the OpenXR API.

Please see the [`vulkan` readme](https://github.com/expipiplus1/vulkan/#vulkan)
for information on using these bindings, the style is very similar.

## Vulkan integration

The `use-vulkan-types` Cabal flag makes it so that Vulkan types are reexported
in `OpenXR.VulkanTypes` from the
[`vulkan`](https://hackage.haskell.org/package/vulkan) package instead of being
redefined in this package. It is enabled by default.
