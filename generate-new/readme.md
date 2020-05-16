# How to use

## The `vk` generator

- Depends on documentation having been built in `Vulkan-Docs`
- Outputs the `vulkan` source to a directory called `out`
  - I usually symlink this to `../src`
- For faster iteration documentation generation can be disabled by setting
  `doLoadDocs` to `False` in `vk/Main.hs`

To generate the docs in Vulkan-Docs, note that this requires quite a recent
nixpkgs with `he` and `escape-string-regexp` (One with
https://github.com/NixOS/nixpkgs/pull/86773 merged):

```bash
cd Vulkan-Docs
# Generating all the reference pages (which we need) is a side effect of
# building man/apispec.txt, generated builds their dependencies.
nix-shell -p python3 asciidoctor gnumake nodejs nodePackages.he nodePackages.escape-string-regexp --run "./makeAllExts man/apispec.txt generated"
```

## The `vma` generator

- Depends on documentation having been built in
  `../VulkanMemoryAllocator/VulkanMemoryAllocator`
- Outputs the `VulkanMemoryAllocator` source to a directory called `out-vma`
  - I usually symlink this to `../VulkanMemoryAllocator/src`

I usually run the generators with `ghci $(HIE_BIOS_OUTPUT=/dev/stdout ./flags.sh $(pwd)/vk/Main.hs) vk/Main.hs +RTS -N16`

To generate the docbook documentation required by `vma`:

In an environment with `doxygen` (`nix-shell -p doxygen`), in the
`VulkanMemoryAllocator/VulkanMemoryAllocator` directory.

```bash
(cd src && sed -i -e 's|^GENERATE_DOCBOOK.*|GENERATE_DOCBOOK=YES|' -e 's|^BRIEF_MEMBER_DESC.*|BRIEF_MEMBER_DESC=NO|' Doxyfile && doxygen Doxyfile)
```

The docbook documentation will be in `docs/docbook`.

# TODO

- List instead of vector?
  Most of the arrays passed to vulkan (in my code at least) are just a single
  element. All of them are less than 5 or 6 elements I think. The marshaling
  code doesn't need O(1) random access, perhaps lists would be more convenient.

- pay attention to `noautovalidity`, At the moment this is ignored, but it's a
  hint that there's something slightly non-standard happening.

- Neaten zero writing, we write some unnecessary zero bytes already set by
  calloc.

- We sometimes use calloc where alloc would do, when passing in space for "out"
  parameters for instance

- non-optional arrays/structs can be allocated at the same time as their parent
  struct, no need for two allocations

- Unions should probably not use tuple when storing several values,
  `VkClearColorValue` for instance

- Add tests to make sure that `allocaEmptyCStruct peekCStruct` always works,
  and is equal to `zero`.

- Make `PFN_vkVoidFunction` not a `FunPtr` as it can't be called without
  casting anyway.

- Groups of `lift`ed actions which don't return anything can be grouped under
  one `lift`:

- If we move to peeking and poking ByteArrays using `RecordDotSyntax` it would
  be nice to have alternative virtual members for using nicer types than tuples
  for vectors and matricies. For example it would be nice to use `linear`s
  `Mat34` to set `VkAccelerationStructureInstanceKHR::transform`.

- Don't peek the same value for every bitfield component

## To check

During development unfinished bits not to forget are listed here.

# VMA TODOs

## TODO

- Make `buildStatsString` return a ByteString
- `pHeapSizeLimit` length
- `VmaDefragmentationPassInfo::pMoves`

# More bindings to make

- chaoticbob/SPIRV-Reflect

- ValveSoftware/openvr

- cgltf
