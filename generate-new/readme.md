## To examine

VkShaderModuleCreateInfo, should take a bytestring for shader
code

## Changes to the spec

To prevent a name clash between the constructors of
`VkClearColorValue` and `VkPerformanceCounterResultKHR` the latter have had
`Counter` prefixed.
