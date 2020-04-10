# How to use

## The `vk` generator

- Depends on documentation having been built in `Vulkan-Docs`
- Outputs the `vulkan` source to a directory called `out`
  - I usually symlink this to `../src`
- For faster iteration documentation generation can be disabled by setting
  `doLoadDocs` to `False` in `vk/Main.hs`

To generate the docs in Vulkan-Docs

```bash
cd Vulkan-Docs
nix-shell -p python3 asciidoctor gnumake --run './makeAllExts allman'
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

- Neaten zero writing, we write some unnecessary zero bytes already set by
  calloc.

- We sometimes use calloc where alloc would do, when passing in space for "out"
  parameters for instance

- non-optional arrays/structs can be allocated at the same time as their parent
  struct, no need for two allocations

- `VkSubpassDescription` shouldn't have `resolveAttachments` be an
  `Either Word32 Vector` it's constrained to be the same length as colorAttachments

- Unions should probably not use tuple when storing several values,
  `VkClearColorValue` for instance

- Add tests to make sure that `allocaEmptyCStruct peekCStruct` always works,
  and is equal to `zero`.

- Make `PFN_vkVoidFunction` not a `FunPtr` as it can't be called without
  casting anyway.

- Groups of `lift`ed actions which don't return anything can be grouped under
  one `lift`:

# VMA TODOs

## TODO

- Make `buildStatsString` return a ByteString
- `pHeapSizeLimit` length
- `VmaDefragmentationPassInfo::pMoves`

# More bindings to make

- chaoticbob/SPIRV-Reflect

- ValveSoftware/openvr
