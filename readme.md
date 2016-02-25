# Vulkan

Haskell bindings to the Vulkan graphics API

## Building

This package requires GHC 8.0 or higher due to the use of the
`DuplicateRecordFields` and `Strict` language extensions.

I highly recommend compiling with `-O0` at the moment as compiling with
optimizations leads to huge compile times. 


## Current Status

All the core Vulkan functionality is here.

This is currently a 64 bit only library, 32 bit support is on the roadmap.

Examples can be found [here](https://github.com/expipiplus1/vulkan-examples)

