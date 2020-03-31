
`getPhysicalDeviceXlibPresentationSupportKHR`
 should be `:: PhysicalDevice ->  Word32 ->  Ptr Display ->  VisualID ->  IO Bool`

`VkPipelineCoverageToColorStateCreateInfoNV` should use `Maybe Word32` to
represent `coverageToColorLocation` and `coverageToColorEnable`

`vkGetAccelerationStructureMemoryRequirementsNV`

Shouldn't treat display as a return value
  - `getRandROutputDisplayEXT`
  - `acquireXlibDisplayEXT`

`DrmFormatModifierPropertiesListEXT` Should allow for returning properties in a pointer

`PipelineCreationFeedbackCreateInfoEXT` likewise

## Things which pass void pointers

These should be treated as Bytestrings,
  - `PipelineCacheCreateInfo`
  - `ValidationCacheCreateInfoEXT`
  - `ShaderModuleCreateInfo`

These should be polymorphic Storables
  - `SpecializationInfo`
  - `WriteDescriptorSetInlineUniformBlockEXT`
  - `DebugMarkerObjectTagInfoEXT`
  - `DebugUtilsObjectTagInfoEXT`

`ExportFenceWin32HandleInfoKHR.attributes` should be a `Ptr SECURITY_ATTRIBUTES`
