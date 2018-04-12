# Vulkan

Haskell bindings to the Vulkan graphics API

## Building

This package requires GHC 8.0 or higher due to the use of the
`DuplicateRecordFields` and `Strict` language extensions.

## Current Status

All the core Vulkan functionality is here.

This is currently a 64 bit only library, 32 bit support is on the roadmap.

Examples can be found [here](https://github.com/expipiplus1/vulkan-examples)

## Ideas

Use `:::` operator to name parameters.

Wrappers for passing in size, pointer pair
Wrappers for passing in null terminated list

`withXXX` functions for `create`/`destroy` pairs.

Haddock tables soon:
https://github.com/haskell/haddock/pull/718

For each bit if documentation in the man page do something like this:
`asciidoctor --backend docbook5 man/VkAccessFlagBits.txt --out-file - | pandoc --from=docbook --to=native`

Then massage the pandoc representation.

- emphasis on labels
- remove enum code blocks
