## Changes to the spec

# How to use


## TODO

Complete zero writing, we write some unnecessary zero bytes

non-optional arrays/structs can be allocated at the same time as their parent
struct.

`VkSubpassDescription` shouldn't have an `Either Word32 Vector..` member as
that vector is constrained to be the same length.

Make VkClearColorValue not use a tuple

Make sure that `allocaEmptyCStruct peekCStruct` always works, and is equal to
zero.

Make `PFN_vkVoidFunction` not a `FunPtr` as it can't be called without casting
anyway.

## Stmts

Groups of `lift`ed actions which don't return anything can be grouped under one lift:

## Make note of illegal zero instances

## check
"vkAllocateDescriptorSets": TODO: allocating vector with member specified lenghts
"vkAllocateCommandBuffers": TODO: allocating vector with member specified lenghts
vkAllocateDescriptorSets


getPhysicalDeviceXlibPresentationSupportKHR -- wrong

## To generate the docs in Vulkan-Docs

```bash
cd Vulkan-Docs
nix-shell -p python3 asciidoctor gnumake --run './makeAllExts allman'
```

# VMA

## To check

- `pHeapSizeLimit` length
- `VmaDefragmentationPassInfo::pMoves`
- `VmaAllocatorCreateInfo::pVulkanFunctions` This should be created from the
  function table in `Device`
