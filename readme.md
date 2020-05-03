# vulkan

Slightly high level Haskell bindings to the Vulkan graphics API.

These bindings present an interface to Vulkan which looks like more idiomatic
Haskell and which is much less verbose than the C API. Nevertheless, it retains
access to all the functionality. If you find something you can do in the C
bindings but not in these high level bindings please raise an issue.

Practically speaking this means:

- No fiddling with `vkGetInstanceProcAddr` or
  `vkGetDeviceProcAddr` to get function pointers, this is done automatically on
  instance and device creation.

- No setting the `sType` member, this is done automatically.

- No passing length/pointer pairs for arrays, `Vector` is used instead.

- No passing pointers for return values, this is done for you and multiple
  results are returned as elements of a tuple.

- No checking `VkResult` return values for failure, a `VulkanException` will be
  thrown if a Vulkan command returns an error `VkResult`.

- No manual memory management for command parameters or Vulkan structs. You'll
  still have to manage buffer and image memory yourself however.

## Package structure

Types and functions are placed into modules according to the `features` and
`extensions` portions of the specification. As these sections only mention
functions, a best guess has to be made for types. Types and constants are drawn
in transitively according to the dependencies of the functions.

It should be sufficient to import `Vulkan.CoreXX` along with
`Vulkan.Extensions.{whatever extensions you want}`. You might want to import
`Vulkan.Zero` too.

These bindings are intended to be imported qualified and do not feature the
`Vk` prefixes on commands, structures, members or constants.

## Things to know

- Documentation is included more or less verbatim from the Vulkan C API
  documentation. The parameters it references might not map one-to-one with
  what's in these bindings. It should be obvious in most cases what it's trying
  to say. If part of the documentation is misleading or unclear with respect to
  these Haskell bindings please open an issue and we can special case a fix.

- Parameters are named with the `:::` operator where it would be useful; this
  operator simply ignores the string on the left.

- There exists a `Zero` type class defined in
  [Vulkan.Zero](src/Vulkan/Zero.hs). This is a class for initializing values
  with all zero contents and empty arrays. It's very handy when initializing
  structs to use something like `zero { only = _, members = _, i = _, care = _,
  about = _ }`.

- The library is compiled with `-XStrict` so expect all record members to be
  strict (and unboxed when they're small)

- Calls to Vulkan are marked as `unsafe` by default. This can be turned off by
  setting the `safe-foreign-calls` flag. This is to reduce FFI overhead,
  however it means that Vulkan functions are unable to safely call Haskell
  code. See the [Haskell
  wiki](https://wiki.haskell.org/Foreign_Function_Interface#Unsafe_calls) for
  more information. This is important to consider if you want to write
  allocation or debug callbacks in Haskell. It's also important to be aware
  that the garbage collector will not run during these calls.

- As encouraged by the Vulkan user guide, commands are linked dynamically (with
  the sole exception of `vkGetInstanceProcAddr`).
  - The function pointers are attached to any dispatchable handle to save you
    the trouble of passing them around.
  - The function pointers are retrieved by calling `vkGetInstanceProcAddr` and
    `vkGetDeviceProcAddr`.  These are stored in two records `InstanceCmds` and
    `DeviceCmds` which store instance level and device level commands
    respectively. These tables can be initialized with the `initInstanceCmds`
    and `initDeviceCmds` found in [Vulkan.Dynamic](src/Vulkan/Dynamic.hs).

- There are nice `Read` and `Show` instances for the enums and bitmasks. These
  will, where possible, print and parse the pattern synonyms. For example one
  can do the following:

    ```haskell
    > show COMPARE_OP_LESS
    "COMPARE_OP_LESS"
    ```

- Make sure that all the functions you're going to use are not `nullPtr` in
  `InstanceCmds` or `DeviceCmds` before calling them, this package doesn't
  perform any checks. The `*Cmds` records can be found inside any dispatchable
  handle.

### Minor things

- To prevent a name clash between the constructors of
  `VkClearColorValue` and `VkPerformanceCounterResultKHR` the latter have had
  `Counter` suffixed.

- To prevent a name clash between the constructors of
  `DeviceOrHostAddressKHR` and `DeviceOrHostAddressConstKHR` the latter have
  had `Const` suffixed.

## How the C types relate to Haskell types

These bindings take advantage of the meta information present in the
specification detailing the validity of structures and arguments.

- If a structure or set of command parameters in the specification contains a
  pointer to an array and an associated length, this is replaced with a
  `Vector` in these bindings. When interfacing with Vulkan these bindings
  automatically set the length of the vector. If the vector is optional but the
  length is not then `Either Word32 (Vector a)` is used, use `Left n` to
  specify that there are `n` elements which you are not providing.

- If a struct member or command parameters in the specification is a optional
  pointer (it may be null) this is replaced with a `Maybe` value.

- If a struct has a member which can only have one possible value (the most
  common example is the `sType` member, then this member is elided.

- C strings become `ByteString`. This is also the case for fixed length C
  strings, the library will truncate overly long strings in this case.

- Pointers to `void` accompanied by a length in bytes become `ByteString`

- Shader code is represented as `ByteString`

- `VkBool32` becomes `Bool`

- Some Vulkan commands or structs take several arrays which must be the same
  length. These are currently exposed as several `Vector` arguments which must
  be the same length. If they are not the same length an exception is thrown.

If anything is unclear please raise an issue. The marshaling to and from
Haskell and C is automatically generated and I've not checked every single
function. It's possible that there are some commands or structs which could be
represented better in Haskell, if so please also raise an issue.

### Vulkan errors

If a Vulkan command has the `VkResult` type as a return value, this is checked
and a `VulkanException` is thrown if it is not a success code. If the only
success code a command can return is `VK_SUCCESS` then this is elided from the
return type. If a command can return other success codes, for instance
`VK_EVENT_SET` then the success code is exposed.

### Bracketing commands

There are certain sets commands which must be called in pairs, for instance the
`create` and `destroy` commands for using resources. In order to facilitate
safe use of these commands, (i.e. ensure that the corresponding `destroy`
command is always called) these bindings expose similarly named commands
prefixed with `with` (for `Create`/`Destroy` and `Allocate`/`Free` pairs) or
`use` for (`Begin`/`End` pairs). If the command is used in command buffer
building then it is additionally prefixed with `cmd`.

These are higher order functions which take as their first argument a consumer
for a pair of `create` and `destroy` commands.  Values which fit this hole
include `Control.Exception.bracket`, `Control.Monad.Trans.Resource.allocate`
and `(,)`.

An example is `withInstance` which calls `createInstance` and
`destroyInstance`. Notice how the `AllocationCallbacks` parameter is
automatically passed to the `createInstance` and `destroyInstance` command.

```haskell
createInstance
  :: forall a m
   . (PokeChain a, MonadIO m)
  => InstanceCreateInfo a
  -> Maybe AllocationCallbacks
  -> m Instance

destroyInstance
  :: forall m
   . MonadIO m
  => Instance
  -> Maybe AllocationCallbacks
  -> m ()

withInstance
  :: forall a m r
   . (PokeChain a, MonadIO m)
  => (m Instance -> (Instance -> m ()) -> r)
  -> InstanceCreateInfo a
  -> Maybe AllocationCallbacks
  -> r
```

Example usage:

```haskell
import Control.Monad.Trans.Resource (runResourceT, allocate)
-- Create an instance and print its value
main = runResourceT $ do
  (instanceReleaseKey, inst) <- withInstance allocate zero Nothing
  liftIO $ print inst

-- Begin a render pass, draw something and end the render pass
drawTriangle =
  cmdUseRenderPass buffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE bracket_
    $ do
        cmdBindPipeline buffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
        cmdDraw buffer 3 1 0 0
```

These pairs of commands aren't explicit in the specification, so
a list of them is maintained in the generation code, if you see something
missing please open an issue (these pairs are generated in `VK/Bracket.hs`).

### Dual use commands

Certain commands, such as `vkEnumerateDeviceLayerProperties` or
`vkGetDisplayModePropertiesKHR`, have a dual use. If they are not given a
pointer to return an array of results then they instead return the total number
of possible results, otherwise they return a number of results. There is an
idiom in Vulkan which involves calling this function once with a null pointer
to get the total number of queryable values, allocating space for querying that
many values and they calling the function again to get the values. These
bindings expose commands which automatically return all the results. As an
example `enumeratePhysicalDevices` has the type `Instance -> IO (Result, Vector
PhysicalDevice)`.

### Structure chains

Most structures in Vulkan have a member called `pNext` which can be a pointer
to another Vulkan structure containing additional information. In these high
level bindings the head of any struct chain is parameterized over the rest of
the items in the chain. This allows for using *type inference* for getting
struct chain return values out of Vulkan, for example:
`getPhysicalDeviceFeatures2 :: (PokeChain a, PeekChain a) => PhysicalDevice ->
IO (PysicalDeviceFeatures2 a)`; here the variable `a :: [Type]` represents the
structures present in the chain returned from `vkGetPhysicalDeviceFeatures2`.

There exists a GADT `SomeStruct` which captures the case of an unknown tail in
the struct chain. This is also used for nested chains inside structs.

Struct chains inside records are represented as nested tuples: `next ::
(Something, (SomethingElse, (AThirdThing, ())))`

## Building

This package requires GHC 8.6 or higher due to the use of the
`QuantifiedConstraints` language extension.

Make sure you have initialized the `VulkanMemoryAllocator` submodule if you
intend to build the `VulkanMemoryAllocator` package.

If you provision `libvulkan.so` (the Vulkan loader) with nix and you're not on
NixOS, you'll have to use [NixGL](https://github.com/guibou/nixGL) to run your
programs. For this reason it's recommended to use the system-provided
`libvulkan.so`.

For instructions on how to regenerate the bindings see [the readme in
./generate-new](./generate-new/readme.md).

To build the example programs. You'll need to supply the following system
packages:

- `vulkan-loader` (for `libvulkan.so`)
- `vulkan-headers` (for `vulkan.h`)
- `pkg-config` and `SDL2` to build the Haskell `sdl2` package.
- `glslang` (for the `glslangValidator` binary, to build the shaders)

## Examples

There exists a package to build some example programs in the `examples`
directory.

## Current Status

All the core Vulkan 1.0, 1.1, and 1.2 functionality is here as well as all the
extensions.

This is currently a 64 bit only library.

## See also

The [VulkanMemoryAllocator
package](https://hackage.haskell.org/package/VulkanMemoryAllocator-0.1.0.0)
(source in the [VulkanMemoryAllocator directory](./VulkanMemoryAllocator)) has
similarly styled bindings to the [Vulkan Memory
Allocator](https://github.com/GPUOpen-LibrariesAndSDKs/VulkanMemoryAllocator)
library.

The [vulkan-utils](./utils) package (not currently on Hackage) includes a few
utilities for writing programs using these bindings.

For an alternative take on Haskell bindings to Vulkan see the
[vulkan-api](https://github.com/achirkin/vulkan#readme) package. `vulkan-api`
stores Vulkan structs in their C representation as `ByteArray#` whereas this
library allocates structs on the stack and keeps them alive for just the
lifetime of any Vulkan command call.
