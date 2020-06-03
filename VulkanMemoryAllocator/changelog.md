# Change Log

## WIP

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
