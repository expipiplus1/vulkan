## To examine

VkShaderModuleCreateInfo, should take a bytestring for shader
code

## Changes to the spec

To prevent a name clash between the constructors of
`VkClearColorValue` and `VkPerformanceCounterResultKHR` the latter have had
`Counter` prefixed.

## TODO Important

generate `bracket create destroy` functions.

generate `enumerateAll` and `gerNum` functions.

throw exceptions instead of always returning vkresult.

stop everything being dumped in `Core10.Extra`.

## TODO

Make sure it's easy to marshal types such as `VkDispatchIndirectCommand`
(`Storable` instance).

Better module allocation by grouping segments and having a module for elements
unique to that group.

Make sure optional bitmasks aren't represented with Maybe (they are zeroable)

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

init -> Get a list = l of (Ref, Action)

run an action
  - it calls use r where (r, a) is in l, a is evaluated there and
    then and the result stored so it doesn't rerun next time

sibling info either by:
  - knot tying
  - topsorting and traversing siblings to get l, accumulating refs on the way

for pokes annotate the pointer with the type

Groups of `lift`ed actions which don't return anything can be grouped under one lift:

```haskell
pokeCStruct p VkSubpassDescriptionDepthStencilResolve{..} f = evalContT $ do
  lift $ poke (p `plusPtr` 0) (VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE)
  lift $ poke (p `plusPtr` 8) (pNext)
  lift $ poke (p `plusPtr` 16) (depthResolveMode)
  lift $ poke (p `plusPtr` 20) (stencilResolveMode)
  pDepthStencilResolveAttachment'' <- case (pDepthStencilResolveAttachment) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ poke (p `plusPtr` 24) pDepthStencilResolveAttachment''
  lift $ f
```

## check
"vkCmdSetBlendConstants": Unhandled direct poke from Tupled 4 (Normal Float) to Ptr Const Float
"vkCmdClearColorImage": Unhandled TypeName "VkClearColorValue" conversion to: Ptr Const (TypeName "VkClearColorValue")
"vkAllocateDescriptorSets": TODO: allocating vector with member specified lenghts
"vkAllocateCommandBuffers": TODO: allocating vector with member specified lenghts
vkAllocateDescriptorSets

