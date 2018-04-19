# Vulkan

Haskell bindings to the Vulkan graphics API

## Building

This package requires GHC 8.0 or higher due to the use of the
`DuplicateRecordFields` and `Strict` language extensions.

## Current Status

All the core Vulkan 1.0 and 1.1 functionality is here as well as all the extensions.

This is currently a 64 bit only library.

Examples can be found [here](https://github.com/expipiplus1/vulkan-examples)

## Differences from the vulkan API

Bools are strongly typed, this doesn't prevent any valid usage apart from
passing the numeric literals `0` and `1` to functions.

## Ideas

TODO: Move these to issues:

Wrappers for passing in size, pointer pair
Wrappers for passing in null terminated list

`withXXX` functions for `create`/`destroy` pairs.

Haddock tables soon:
https://github.com/haskell/haddock/pull/718

