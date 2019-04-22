# Vulkan

Haskell bindings to the Vulkan graphics API.

This library comprises two sets of bindings to the Vulkan API.

- "C like" bindings in [Graphics.Vulkan.C](src/Graphics/Vulkan/C).
- High(er) level bindings in [Graphics.Vulkan](src/Graphics/Vulkan).

The high level bindings take care of more marshalling details, and are the
recommended way of using this library.

## Low level bindings

These try to stay as close as possible to the C interface defined by the Vulkan
specification. `Vk` prefixes are preserved. There are a few differences, however:

- Struct members have been given a `vk` prefix, this is to avoid namespace
  pollution when importing these modules unqualified.

- As encouraged by the Vulkan user guide commands are linked dynamically (with
  the sole exception of `vkGetInstanceProcAddr`). The function pointers are
  retrieved by calling `vkGetInstanceProcAddr` and `vkGetDeviceProcAddr`. These
  are stored in two records `InstanceCmds` and `DeviceCmds` which store
  instance level and device level commands respectively. These tables can be
  initialized with the `initInstanceCmds` and `initDeviceCmds` found in
  [Graphics.Vulkan.C.Dynamic](src/Graphics/Vulkan/C/Dynamic.hs).

- Instead of being a type synonym for `Word32`, `VkBool32` is a newtype wrapper
  around `Word32`

A few things to note:

- Types and functions are placed into modules according to the `features` and
  `extensions` portions of the specification. As these sections only mention
  functions, a best guess has to be made for types. Types and constants are
  drawn in transitively according to the dependencies of the functions.

- Vulkan structures are represented as Haskell records, and use a `Storable`
  instance to marshal them to the Vulkan API; this incurrs a copy, if you need
  an api where Vulkan structures can be handled without copying please check
  out the [vulkan-api](https://github.com/achirkin/vulkan#readme) package.

## High level bindings

The modules not under `Graphics.Vulkan.C` are higher level bindings to the API.
These present a interface to Vulkan which is more suited to Haskell and much
less verbose. Nevertheless, it retains access to the full API. If you find
something you can do in the C bindings but not in the high level bindings
please raise an issue.

These bindings are intended to be imported qualified and do not feature the
`Vk` prefixes on commands, structures, members or constants.

### Validity information

These bindings take advantage of the meta information present in the
specification detailing the validity of structures and arguments. A few
examples:

- If a structure or set of command parameters in the specification contains a
  pointer to an array and an associated length, this is replaced with a
  `Vector` in these bindings. When interfacing with Vulkan these bindings
  automatically set the length of the vector.

- If a struct member or command parameters in the specification is a optional
  pointer (it may be null) this is replaced with a `Maybe` value.

- If a struct has a member which can only have one possible value (the most
  common example is the `sType` member, then this value is elided.

Like the C level bindings, commands in these bindings use the dynamic functions
contained in the pointer tables `InstanceCmds` and `DeviceCmds`.

Some Vulkan commands take several arrays which must be the same length. These
are currently exposed as several `Vector` arguments to the Haskell command and
must be the same length. If they are not the same length then they are
truncated to the length of the shortest value.

### Vulkan errors

If a Vulkan command has the `VkResult` type as a return value, this is checked
and a `VulkanException` is thrown if it is not a success code. If the only
success code a command can return is `VK_SUCCESS` then this is elided from the
return type. If a command can return other success codes, for instance
`VK_EVENT_SET` then the success code is exposed.

### Bracketing commands

There are certain sets commands which must be called in pairs, for instance the
`create` and `destroy` commands for using resources. In order to facilitate
safe use of these commands, i.e. ensure that the corresponding `destroy`
command is always called, these bindings expose `with` commands, which use
`bracket` to. These pairs of commands aren't explicit in the specification, so
a list of them is maintained in the generation code, if you see something
missing please open an issue (these pairs are generated in
`Write.Marshal.Bracket`). An example is `withInstance` which calls
`vkCreateInstance` and `vkDestroyInstance`.

### Dual use commands

Certain commands, such as `vkEnumerateDeviceLayerProperties` and
`vkGetDisplayModePropertiesKHR`, have a dual use. If they are not given a
pointer to return an array of results then they instead return the total number
of possible results, otherwise they return a number of results. There is an
idiom in Vulkan which involves calling this function once with a null pointer
to get the total number of queryable values, allocating space for querying that
many values and they calling the function again to get the values. These
bindings expose commands beginning in `getAll` or `enumerateAll` which perform
this idiom for you and just return the full list of results in a `Vector`.

### Structure chains

Most structures in Vulkan have a member called `pNext` which can be a pointer
to another Vulkan structure containing additional information. In these high
level bindings this is witnessed by the `SomeVkStruct` type in
[Graphics.Vulkan.Marshal.SomeVkStruct](src/Graphics/Vulkan/Marshal/SomeVkStruct.hs).
When values are marshalled from Vulkan the structure chain is automatically
populated.

To find a structure of a particular type in the chain the function
`fromSomeVkStructChain :: Typeable a => SomeVkStruct -> Maybe a` can be used.
This will examine the whole structure chain looking for a structure which
matches the required type.

To add a structure `n` to the chain of a structure `x` set the `next` member of
`x` to `Just (SomeVkStruct n)`. When a structure is marshalled to Vulkan space
is allocated for the whole chain and it is filled appropriately.

## Common Features

- There exists a `Zero` typeclass defined in
  [Graphics.Vulkan.C.Core10.Core](src/Graphics/Vulkan/C/Core10/Core.hs). This
  is a class for initializing values with all zero contents and empty arrays.
  Vulkan C style structures initialized with this class have the `sType` member
  initialized to the correct value.

- The library is compiled with `-XStrict` so expect all record members to be
  strict and unpacked.

- Calls to Vulkan are marked as `unsafe` by default. This can be turned off by
  setting the `safe-foreign-calls` flag. This is to reduce FFI overhead,
  however it means that Vulkan functions are unable to safely call Haskell
  code. See the [Haskell
  wiki](https://wiki.haskell.org/Foreign_Function_Interface#Unsafe_calls) for
  more information. This is important to consider if you want to write
  allocation or debug callbacks in Haskell.

## Building

This package requires GHC 8.0 or higher due to the use of the
`DuplicateRecordFields` and `Strict` language extensions.

For instructions on how to regenerate the bindings see [the readme in
./generate](./generate/readme.md).

## Current Status

All the core Vulkan 1.0 and 1.1 functionality is here as well as all the
extensions.

This is currently a 64 bit only library.

Examples can be found [here](https://github.com/expipiplus1/vulkan-examples)
