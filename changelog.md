# Change Log

## [2.1.0.0] - 2018-04-22
  - Expose dynamic loader from Graphics.Vulkan.Dymamic
  - Turn on platform specific features by default
  - Make all foreign imports unsafe
  - Add option for enabling safe calls

## [2.0.0.1] - 2018-04-21
  - Improved documentation (links, tables, math)
  - Use cpphs for preprocessing

## [2.0.0.0] - 2018-04-20
  - Total rewrite of the generator
  - All extensions are in here
  - Split into core versions
  - Documentation on most things

## [1.7.0.0] - 2016-04-13
  - Fix struct member names containing digits being erroneously truncated

## [1.6.0.0] - 2016-03-30
  - Add DebugReport, Display, DisplaySwapchain and Swapchain extension modules

## [1.5.1.0] - 2016-03-07
 - Add enumeration values for VkCompositeAlphaFlagsKHR and VkSurfaceTransformFlagsKHR

## [1.5.0.0] - 2016-03-01
 - Remove VK_NULL_HANDLE
 - Remove Vulkan.Graphics.HeaderBoilerplate

## [1.3.2.0] - 2016-02-29
 - Add type signatures to version functions

## [1.3.1.0] - 2016-02-29
 - Add version information and generator to Graphics.Vulkan.Version

## [1.3.0.0] - 2016-02-29
 - Add Read and Show instances for bitmasks

## [1.2.0.0] - 2016-02-29
 - Add Read and Show instances for enumerations

## [1.0.0.0] - 2016-02-25
 - Split the interface up into many modules.

## [0.2.0.0] - 2016-02-22
 - Make wildcard matches lazy for `sizeof` and `alignment`

## [0.1.0.0] - 2016-02-21
 - Initial release targeting Vulkan 1.0.3 Core
