# Change Log

## WIP
  - Bump API version to 1.2.141
  - Generate haddocks for command parameters, #92
  - Eq instances for some simple structs, #94
  - Generate safe FFI versions of blocking functions (Those with "wait" in the
    name or which can return "TIMEOUT"), #96
  - Add cabal flag for enable deriving Generic for structs, #99
  - Add `complete` pragmas for `::&` and `:&`

## [3.3] - 2020-05-07
  - Move package from `Graphics.Vulkan` to just `Vulkan`, #60
  - Bump API version to 1.2.140
  - Make the continuation the last argument to 'bracket' functions, discussion
    on #49
  - Begin/End bracket pairs are now called 'useXXX' rather than 'withXXX', #66
  - Begin/End bracket pairs where it's not necessary to 'End' on an exception
    have a simplified type, discussion on #49
  - Clarify optional vector lengths by preserving the length member, #71
  - Infer lengths of preserved length members when they are 0
  - Throw an exception when trying to call a null function pointer, #42
  - Implement HasObjectType class to automate getting VkObjectType, #54
  - Add constraints to check that structs are correctly extended
  - Simplify type of `withDescriptorSets`, it no longer requires the user
    specifying the `DescriptorPool` twice, #81
  - Wrap with SomeStruct extensible structs in Vector arguments to commands, #82

  Thanks to @dpwiz for helping with this release!

## [3.2.0.0] - 2020-05-02
  - Update API version 1.2.139
  - Bracket functions now take as an argument a function to consume a pair of
    begin/end actions
  - Change `Either Word32 vec` to `vec` where the non-empty length is
    constrained to be the length of another known vector
  - Move `Counter` prefix on `PerformanceCounterResult` to be a suffix
  - Drop dependency on `vector-sized`, it was only used internally

## [3.1.0.0] - 2020-03-10
  - Commands are now in MonadIO
  - Improved documentation
  - More `bracket` functions for `cmd`s
  - Add `:&` and `::&` pattern synonyms for extensible struct construction

## [3.0.0.0] - 2020-02-29
  - Rewrite, bindings are now much more idiomatic Haskell
  - Add `sdl-triangle` and `info` examples

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
