# Change Log

## WIP

## [0.3.12] - 2021-01-09

- Calling traceEventIO before and after every VulkanMemoryAllocator command if
  `vulkan` was compiled with the `trace-calls` flag
- Don't bother poking empty vectors in `withZeroCStruct`
- Bump VMA, fixes compilation issue when vma-recording is enabled.

## [0.3.11] - 2020-11-30

- Raise upper bound on `vulkan`

## [0.3.10] - 2020-11-24

- Documentation fixes

## [0.3.9] - 2020-11-15

- Derive `FiniteBits` for bitmasks

## [0.3.8] - 2020-11-12

- Bump VMA
  - Documentation chages
  - Potential overflow fix https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator/issues/153

## [0.3.7.1] - 2020-11-01

- Bump VMA, just documentation changes
- Raise bound on base

## [0.3.7] - 2020-08-27
  - Bugfix https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator/issues/143
  - Documentation changes

## [0.3.6] - 2020-08-05
  - Fix call to getBudget
    - Now works on devices with more than one memory heap!

## [0.3.5] - 2020-07-14
  - Bump VMA, no interface changes

## [0.3.4] - 2020-07-05
  - Bump VMA, just documentation change

## [0.3.3] - 2020-06-22
  - Bump VMA

## [0.3.2] - 2020-06-03
  - Bump VMA version (nfc, just comments)
  - Compile with new `vulkan` version

## [0.3.1] - 2020-05-18
  - Eq instances for some simple structs, #94
  - Add cabal flag for enable deriving Generic for structs, #99

## [0.3] - 2020-05-07
  - Move package from `Vulkan.VulkanMemoryAllocator` to just `VulkanMemoryAllocator`, #60
  - Make the continuation the last argument to 'bracket' functions, discussion
    on #49
  - Begin/End bracket pairs are now called 'useXXX' rather than 'withXXX', #66
  - Begin/End bracket pairs where it's not necessary to 'End' on an exception
    have a simplified type, discussion on #49
  - Add constraints to check that structs are correctly extended

## [0.2.0.0] - 2020-05-02
  - Bump VMA version to 2020-04-24
  - Bracket functions now take as an argument a function to consume a pair of
    begin/end actions

## [0.1.0.0] - 2020-03-10
  - Commands are now in MonadIO
  - Add `bracket` functions
  - Correct use of `Bool` over `Bool32`

## [0.0.0.0] - 2020-04-07
  - Initial release
