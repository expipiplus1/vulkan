# Generate

This program generates the haskell source from the Vulkan xml specification. 

## Developing

To regenerate the  bindings:

```
./generate.sh \
  /home/j/projects/vulkan/src/ \
  /home/j/projects/vulkan/vulkan.cabal \
  /home/j/src/Vulkan-Docs
```

To see Pandoc's representation of a man page

```
runhaskell \
  src/Documentation/RunAsciiDoctor.hs \
  ~/src/Vulkan-Docs/ \
  ~/src/Vulkan-Docs/man/vkCmdSetDepthBias.txt \
  | pandoc --from docbook --to native`
```

## Wrapper ideas

Several classes of functions:

- ones like vkGetDisplayPlaneSupportedDisplaysKHR:
  - Two functionalities
    - Either get a number of properties by passing NULL for the array
    - Get a set of arrays
  - Implement with more than one function?
    - getNumDisplayPlaneSupportedDisplaysKHR
    - getDisplayPlaneSupportedDisplaysKHR
    - getAllDisplayPlaneSupportedDisplaysKHR
  - Have `Get` in the name
  - Can return `INCOMPLETE`


- like vkCmdSetEvent
  - No return value
  - No pointers, just inputs

- like vkAllocateCommandBuffers
  - Return length depends on length specified in allocation info!

- like vkCreateRenderPass
  - Return single value

- like vkDestroyDescriptorSetLayout
  - pairs with create, takes const ptr
  - allocation callback must match

- like vkCreateGraphicsPipelines
  - takes returns list of same length

Idioms:

- Length/Array pairs

TODO:
  Take a closer look at `vkCmdBindDescriptorSets`
  Take a closer look at `vkUpdateDescriptorSetWithTemplate`
  Don't expose things where the documentation is `is reserved for future use and is ignored`

`vkGetSemaphoreFdKHR` requires the user to be careful not to leak resources

`vkCmdBindVertexBuffers` requires two vectors to be the same. Should we pass in non-storable vectors of tuples?
