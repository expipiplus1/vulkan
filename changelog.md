# Change Log

## WIP
- Make zero instance for `TransformMatrixKHR` return identity matrix. See
  https://github.com/expipiplus1/vulkan/issues/240

## [3.8.3] - 2021-01-04
- Bump API version to v1.2.166

## [3.8.2] - 2020-12-14
- Bump API version to v1.2.165

## [3.8.1] - 2020-12-08
- Bump API version to v1.2.164

- Add cabal flag trace-calls to enable calling traceEventIO before and after
  every Vulkan command

## [3.8] - 2020-11-30

- Bump API version to v1.2.163
- Add `Vulkan.Requirement`, a module for specifying requirements for `Device`s
  and `Instance`s
- Expose SPIR-V Extensions and Capabilities in `Vulkan.SPIRVRequirements`
- Expose extension dependencies in `Vulkan.Extensions.Dependencies`
- Squash some warnings

Thanks to @sheaf for their help with this release!

## [3.7] - 2020-11-24
- Bump API version to v1.2.162
  - This is a breaking change to anyone using VK_KHR_ray_tracing (which no
    longer exists)
- Add bracketing functions for `withRayTracingPipelinesKHR` and
  `withRayTracingPipelinesNV`
- Add all possible storable instances for Vulkan structs
- Remove tuples from the constructors of `ClearColorValue`
- Unpack top level tuple in `TransformMatrixKHR`, the `matrix` record accessor
  has been split into `matrixRow0`, `matrixRow1`, and `matrixRow2`
- Add extension documentation to extension modules.
- Tweak ordering of documentation in Haddocks to make it more user-friendly

## [3.6.15] - 2020-11-16
- Bump API version to v1.2.161

## [3.6.14] - 2020-11-15

- Add `FiniteBits` instance for Flags
- Fix getting function pointers for functions which have aliases (those which
  have been promoted to core versions mostly)

## [3.6.13] - 2020-11-09
  - Bump API version to v1.2.160

## [3.6.12] - 2020-11-03

  - Bump API version to v1.2.159
  - Include enum aliases defined in `<enums>` sections, until now only those
    enum aliases in `features` or `extensions` were used.

## [3.6.11.1] - 2020-11-01

- Raise bound on base

## [3.6.11] - 2020-10-27
  - Bump API version to v1.2.158

## [3.6.10] - 2020-10-12
  - Bump API version to v1.2.157

## [3.6.9] - 2020-10-07
  - Bump API version to v1.2.156

## [3.6.8] - 2020-09-28
  - Bump API version to v1.2.155

## [3.6.7] - 2020-09-25
  - Bump API version to v1.2.154

## [3.6.6] - 2020-08-27
  - Bump API version to 1.2.152
  - NoDuplicateRecordFields for Vulkan.Dynamic

## [3.6.5] - 2020-08-17
  - Bump API version to 1.2.151

## [3.6.4] - 2020-08-11
  - Bump API version to 1.2.150

## [3.6.3] - 2020-08-05
  - Bump API version to 1.2.149

## [3.6.2] - 2020-07-21
  - Bump API version to 1.2.148

## [3.6.1] - 2020-07-14
  - Bump API version to 1.2.147

## [3.6] - 2020-07-05
  - Bump API version to 1.2.146

## [3.5] - 2020-06-22
  - Bump API version to 1.2.145
  - Bump API version to 1.2.143
  - Bump API version to 1.2.144
    - Only documentation and version number changes

## [3.4] - 2020-06-03
  - Bump API version to 1.2.142
    - The spec includes specific locations for types, so we use them
    - BaseType -> FundamentalTypes
  - Add `withSomeStruct` helper

## [3.3.1] - 2020-05-18
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
