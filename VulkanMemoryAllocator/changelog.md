# Change Log

## WIP
- Bump VMA

## [0.10.4] - 2022-10-02
- More robust linking against C++ standard library
- Raise upper bound on `vulkan`

## [0.10.3.1] - 2022-09-27
- Enable c++17 to get VMA aligned alloc

## [0.10.3] - 2022-09-25
- Add Vulkan 1.3 functions to VulkanFunctions

## [0.10.2] - 2022-09-24
- Raise upper bound on `vulkan`

## [0.10.1] - 2022-09-06
- Bump VMA to current (latest 3.0.1)
- Raise upper bound on `vulkan`

## [0.10] - 2022-03-31
- Bump VMA to 3.0.0
  - Several breaking changes
- Raise upper bound on `vulkan`

## [0.9] - 2022-02-05
- Bump VMA
  - Vulkan 1.3 support
- Raise upper bound on `vulkan`

## [0.8] - 2022-01-14
- Bump VMA
  - lost allocations stuff removed
  - Minor changes
  - Requires c++14 now

## [0.7.5] - 2021-11-25
- Bump VMA, adds `getAllocationMemoryProperties`
- Raise upper bound on `vulkan`

## [0.7.4] - 2021-11-08
- Bump VMA, documentation changes

## [0.7.3] - 2021-11-03
- Bump VMA, Adds virtual allocation functionality

## [0.7.2] - 2021-10-14
- Bump VMA, documentation changes and internal improvements
- Raise upper bound on `vulkan`

## [0.7.1] - 2021-08-12
- Raise upper bound on `vulkan`

## [0.7] - 2021-07-23
- Bump VMA, adds vkaCreateBufferWithAlignment and fixes aligned_alloc on OS X.

## [0.6.0.1] - 2021-06-26
- Bump VMA, no functional change
- Use allocaBytes over allocaBytesAligned where possible

## [0.6] - 2021-06-22
- Bump VMA, adding alignment info to PoolCreateInfo

## [0.5.1] - 2021-06-05
- Bump VMA, documentation changes

## [0.5] - 2021-03-31
- Bump VMA, deprecates resizeAllocation

## [0.4] - 2021-02-18
- Bump VMA, support for VK_EXT_memory_priority

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
