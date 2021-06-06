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
nix-shell -p python3 asciidoctor gnumake nodejs nodePackages.he nodePackages.escape-string-regexp --run "./makeAllExts refpages generated"
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

- OpenXR bindings, the spec looks very similar to the vulkan spec:
  https://github.com/KhronosGroup/OpenXR-Docs

- List instead of vector?
  Most of the arrays passed to vulkan (in my code at least) are just a single
  element. All of them are less than 5 or 6 elements I think. The marshaling
  code doesn't need O(1) random access, perhaps lists would be more convenient.

- pay attention to `noautovalidity`, At the moment this is ignored, but it's a
  hint that there's something slightly non-standard happening.
  - Additionally anything which is `returnedonly=true` should never appear in a
    negative position

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

- Use `selection` and `selector` attributes in the xml to generate the
  `UnionDiscriminator` table automatically.

- Replace `Aabbs` with `AABBs`

- Elide union tag

## Video:

- Parse header files (this is already done for VMA, so the code is more or less
  written)
- Integrate these definitions in `parseSpec`

The complication is that lots of the structs there use bitfields, so proper
support for them needs to be done...

At the moment the extensions are disabled in Bespoke.hs

## To check

During development unfinished bits not to forget are listed here.

### OpenXR

- [x] Handle levels
  - They are all "session"

- [x] Bespoke vulkan types should be just in sizes

- [ ] Remove vulkan stuff from bespokeSizes
  - Would be nice to generate it automatically, but it's fine as it is

- [x] Fix runAsciidoctor

- [x] These are passed as pointers ,(see Bespoke.MarshalParams.isPassAsPointerType')

  - [ ] `VkInstanceCreateInfo`, `VkAllocationCallbacks`, `VkDeviceCreateInfo`
  - [x] `LARGE_INTEGER`
  - [x] `timespec`

- [x] remove XR.RenderParams.vulkanNameOverrides

- [x] check
  - [x] `XrCompositionLayerBaseHeader` (the 'type' member doesn't have a single value)
  - [x] `XrEventDataBuffer->varying`
  - [x] `XrSpatialGraphNodeSpaceCreateInfoMSFT->nodeId`
  - [x] `XrSpatialGraphNodeSpaceCreateInfoMSFT`
  - [x] `XrEventDataBuffer`
  - [x] `xrCreateInstance`: Unable to find ref named cmds
  - [x] `xrResultToString`: Unhandled ByteString conversion to Ptr NonConst Char
  - [x] `xrStructureTypeToString`: Unhandled ByteString conversion to Ptr NonConst Char
  - [x] `xrPathToString`: Getting the unpreserved haskell type for char. This case should be implemented if this char is not better represented by a bytestring
  - [x] `xrCreateSwapchainAndroidSurfaceKHR`: Unable to get size for TypeName `jobject`
  - [x] `xrGetInputSourceLocalizedName`: Getting the unpreserved haskell type for char. This case should be implemented if this char is not better represented by a bytestring
  - [x] `xrGetVulkanInstanceExtensionsKHR`: Getting the unpreserved haskell type for char. This case should be implemented if this char is not better represented by a bytestring
  - [x] `xrGetVulkanDeviceExtensionsKHR`: Getting the unpreserved haskell type for char. This case should be implemented if this char is not better represented by a bytestring
  - [x] `xrCreateVulkanInstanceKHR`: Unable to get size for TypeName `VkResult`
  - [x] `xrCreateVulkanDeviceKHR`: Unable to get size for TypeName `VkResult`
  - [x] `xrCreateSpatialAnchorFromPerceptionAnchorMSFT`: Unable to get size for TypeName `IUnknown`

- [x] Check we have all of
  - [x] `NULL_SYSTEM_ID`
  - [x] `NULL_PATH`
  - [x] `SUCCEEDED`
  - [x] `FAILED`
  - [x] `UNQUALIFIED_SUCCESS`
  - [x] `NO_DURATION`
  - [x] `INFINITE_DURATION`
  - [x] `MIN_HAPTIC_DURATION`
  - [x] `FREQUENCY_UNSPECIFIED`
  - [x] `MAX_EVENT_DATA_SIZE`
  - [ ] `MAY_ALIAS`
    - no need

- [x] Make Vulkan integration optional, have a module which either defines
  opaque handles or reexports types from `vulkan`

- [x] make Xr stuff in `bespokeModules`

- [x] dual-use commands have a different style in OpenXR... they're wrong now
  - [x] `xrEnumerateApiLayerProperties`
  - [x] `xrEnumerateInstanceExtensionProperties`
  - [x] `xrEnumerateSwapchainFormats`
  - [x] `xrEnumerateReferenceSpaces`
  - [x] `xrEnumerateViewConfigurations`
  - [x] `xrEnumerateEnvironmentBlendModes`
  - [x] `xrEnumerateViewConfigurationViews`
  - [x] `xrLocateViews`
  - [x] `xrPathToString`
  - [x] `xrEnumerateBoundSourcesForAction`
  - [x] `xrGetInputSourceLocalizedName`
  - [x] `xrGetVulkanInstanceExtensionsKHR`
  - [x] `xrGetVulkanDeviceExtensionsKHR`
  - [ ] `xrLoadControllerModelMSFT`
    - Should return a bytestring or storable vector
  - [x] `xrEnumerateDisplayRefreshRatesFB`
  - [x] `xrEnumerateColorSpacesFB`

- [x] Implement OpenXR's polymorphic types

- [x] change `\CFloat a -> a` to `coerce @CFloat @Float`

- [x] Put modules under `OpenXR` instead of `Vulkan`

- [x] OpenXR brackets

- [ ] Remove uses of WildCardT

- [ ] Remove or improve special handling for `CompositionLayerBaseHeader`
  - Wait for https://github.com/KhronosGroup/OpenXR-Docs/issues/67

- [x] Remove or improve special handling for `enumerateSwapchainImages`

- [ ] Remove uses of `schemeTypeNegativeWithContext` in `Render.Command` (there
  are TODOs) in the code.

- [ ] Some structs have an in-out purpose, see https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters
  - [ ] affects at least xrGetVisibilityMaskKHR and a few extension functions.
  - [ ] Look at every use of "CapacityInput" or "CountOutput"

- [x] Fix or remove `XR_EXT_conformance_automation`
  - Annoying to fix because it passes structs by value so would need a C shim

- [x] Expose XR_CURRENT_API_VERSION

- [ ] Auto update functionality for OpenXR

### 1.2.162

- [x] optionalness on VkWriteDescriptorSetAccelerationStructureKHR->pAccelerationStructures
- [x] VkAccelerationStructureVersionKHR
- [x] VkAccelerationStructureInstanceKHR
- [x] VkAccelerationStructureBuildGeometryInfoKHR
- [x] VkAccelerationStructureBuildGeometryInfoKHR->pGeometries (esp optionalness)
- [x] VkAccelerationStructureBuildGeometryInfoKHR->ppGeometries (esp optionalness)
  - [x] (this maps to the same name as pGeometries!)
  - This has been removed from the Haskell bindings
- [x] VkAccelerationStructureCreateInfoKHR->offset
- [x] VkAccelerationStructureVersionInfoKHR->pVersionData
- [x] vkCmdBuildAccelerationStructuresIndirectKHR,
  - The `Ptr Word32` type has leaked through, this should be `Vector (Vector Word32)`
  - [x] Fix this non-critical issue
  - [ ] It's correct now, but it doesn't check the length of the array
- [x] vkCmdBuildAccelerationStructuresKHR, vkBuildAccelerationStructuresKHR
  - [x] This is still incorrect as one can't pass multiple
    `AccelerationStructureBuildRangeInfoKHR`'s per
    `AccelerationStructureBuildGeometryInfoKHR`. It's no more broken than
    before though.
  - [ ] Fixed, although there's no length checking
- [x] vkGetAccelerationStructureBuildSizesKHR
  - [ ] It's correct, but it doesn't check the length of the
    `maxPrimitiveCount` array like it would with a sibling array of same length
- [x] pNext is now optional, this breaks the Zero instances

Optional TODOs:

- [x] cmdTraceRaysKHR is not using the storable instance of StridedDeviceAddressRegionKHR
  - same for destroyAccelerationStructureKHR not using allocation callbacks
  - These are due to the `indirectStruct` case of the `normal` poker
  - This is no problem, commands have to allocate structs on the stack anyway
- [x] likewise for `pokeCStruct` of `RayTracingPipelineCreateInfoKHR{..}`
  - This one is because we're not looking past pointers when checking for
    direct dependents
  - Fixed
- [x] Documentation on bracketing functions still mentions the "first"
  argument, this should be chagned to "last"
- [ ] AccelerationStructureGeometryAabbsDataKHR could use a simple poke for its
  DeviceOrHostAddressConstKHR (is this true for the union in
  AccelerationStructureGeometryKHR too)
- [x] Now is the time to remove the top level tuple from TransformMatrixKHR


# VMA TODOs

## TODO

- Make `buildStatsString` return a ByteString
- `pHeapSizeLimit` length
- `VmaDefragmentationPassInfo::pMoves`

# More bindings to make

- chaoticbob/SPIRV-Reflect

- ValveSoftware/openvr

- cgltf
