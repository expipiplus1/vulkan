## To examine

VkShaderModuleCreateInfo, should take a bytestring for shader
code

## Changes to the spec

To prevent a name clash between the constructors of
`VkClearColorValue` and `VkPerformanceCounterResultKHR` the latter have had
`Counter` prefixed.

## TODO Important

stop everything being dumped in `Core10.Extra`.

## TODO

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

Groups of `lift`ed actions which don't return anything can be grouped under one lift:

## check
"vkCmdSetBlendConstants": Unhandled direct poke from Tupled 4 (Normal Float) to Ptr Const Float
"vkCmdClearColorImage": Unhandled TypeName "VkClearColorValue" conversion to: Ptr Const (TypeName "VkClearColorValue")
"vkAllocateDescriptorSets": TODO: allocating vector with member specified lenghts
"vkAllocateCommandBuffers": TODO: allocating vector with member specified lenghts
vkAllocateDescriptorSets

