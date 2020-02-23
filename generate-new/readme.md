## To examine

VkShaderModuleCreateInfo, should take a bytestring for shader
code

## Changes to the spec

To prevent a name clash between the constructors of
`VkClearColorValue` and `VkPerformanceCounterResultKHR` the latter have had
`Counter` prefixed.

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
