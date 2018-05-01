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
  Don't marshal windowing system values

`vkGetSemaphoreFdKHR` requires the user to be careful not to leak resources

`vkCmdBindVertexBuffers` requires two vectors to be the same. Should we pass in non-storable vectors of tuples?


## Marshalling structs:

- `VkDescriptorSetLayoutBindingFlagsCreateInfoEXT` has a complex rule for the
'bindingCount' member. Look into this.
- `VkSubpassDescription` is interesting too, too vectors of the same length,
  one of which is optional
  - Implement with `Either (Vector A) (Vector (A, B))`?
  - Implement with `(Vector A, Maybe (Vector B))` and truncate the longer Vector?
  - Use sized vectors: `(Vector n A, Maybe (Vector n B))`

- Need to read pNext chains
  - For example those returned by vkGetPhysicalDeviceFeatures2KHR
  - Only one of each type of struct can be returned from each function, could
    we just return a tuple: `(A, Maybe B, Maybe C, ..)` covering all the
    possible extensions?

- `VkPipelineCoverageToColorStateCreateInfoNV` Has an optional member which
  dictates a bool parameter

- Raise an issue for structs which extend a returnedonly struct which are not
  returnedonly

- Should wrap the function pointers in VkAllocationCallbacks

- The names for sibling vector members could be improved
