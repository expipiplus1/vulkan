# Change Log

## [0.5.11.0] - 2026-06-13

A large additive release: helpers for dynamic rendering, dynamic pipeline
state, specialization constants, synchronization, swapchain/frame management,
and window abstraction. No existing API was removed.

### Pipelines and dynamic rendering
- `Vulkan.Utils.DynamicRendering`: `createPipeline` and
  `createPipelineFromShaders` for render-pass-less pipelines, plus
  `renderingInfo`, `colorAttachmentRenderingInfo`, and
  `dynamicRenderingRequirements`.
- `Vulkan.Utils.DynamicState`: a `DynamicState` record with
  `defaultDynamicState`, `dynamicStateFor`, and `applyDynamicStates`, plus named
  state sets (`allDynamicStates`, `minimalDynamicStates`, `noDynamicStates`,
  `preRasterizationStates`, `fragmentTestStates`, `fragmentOutputStates`,
  `depthOnlyDynamicStates`, `defaultDynamicStatesFor`) covering the dynamic
  states available without vendor or experimental extensions.
- `Vulkan.Utils.Pipeline.Specialization`: the `Specialization` and
  `SpecializationConst` classes with `withSpecialization` /
  `allocateSpecialization` for packing specialization constants into 32-bit
  units.
- `Vulkan.Utils.RenderPass`: `createRenderPass`, `createColorRenderPass`, and a
  generic `createPipeline` / `createPipelineFromShaders`.
- `Vulkan.Utils.Framebuffer`: `createFramebuffer`.
- `Vulkan.Utils.Shader`: `shaderStage` and `shaderModuleStage`.

### Synchronization and descriptors
- `Vulkan.Utils.Barrier`: `imageBarrier`, `bufferBarrier`, and the common
  transitions `transitionColorAttachment`, `transitionDepthAttachment`, and
  `transitionPresent`.
- `Vulkan.Utils.Descriptors`: `bufferWrite` and `imageWrite` for common
  single-binding descriptor writes.
- `Vulkan.Utils.RefCounted`: a reference-counted release primitive
  (`newRefCounted`, `takeRefCounted`, `releaseRefCounted`,
  `resourceTRefCount`).

### Swapchain, frames, and windowing
- `Vulkan.Utils.Swapchain`: `Swapchain` and `SwapchainConfig` with
  `defaultSwapchainConfig`, `allocSwapchain`, `recreateSwapchain`, and
  `threwSwapchainError`.
- `Vulkan.Utils.Frame`: a `Frame` record driving frames-in-flight —
  `advanceFrame`, `runFrame`, `recordCommands`, `queueSubmitFrame`,
  `acquireFrameImage`, `presentFrameImage`, `drainFrames`,
  `withTimelineSemaphore`, and the matching requirements helpers.
- `Vulkan.Utils.VulkanContext`: `VulkanContext` and `RecycledResources` with
  `mkVulkanContext`.
- `Vulkan.Utils.WindowAdapter`: a backend-agnostic `WindowAdapter` record (the
  `vulkan-init-sdl2` and `vulkan-init-glfw` packages provide instances).
- `Vulkan.Utils.WindowLoop`: `runWindowLoop` with the `WindowLoop` record and
  the `noWindowState` / `noOnFrame` / `noOnExit` defaults.
- `Vulkan.Utils.Queues`: a `Queues` record and `withDevice`.
- `Vulkan.Utils.Init.Headless`: `withInstance` for headless setup.

### Dependencies
- Now depends on `unagi-chan` and `unliftio-core`.
- Raised the upper bound on `vulkan` to `< 3.28`.

## [0.5.10.6] - 2023-10-21

## [0.5.10.5] - 2023-10-17

## [0.5.10.4] - 2023-10-17
- Relax bounds on `vulkan`

## [0.5.10.3] - 2023-10-17
- Relax bounds on `vulkan`

## [0.5.10.2] - 2022-12-27

## [0.5.10.1] - 2022-10-02
- Relax bounds on `vulkan`

## [0.5.10] - 2022-09-27
- Improve error messages for requirements TH

## [0.5.9.1] - 2022-09-26
- Fix tests on ghc 9.2

## [0.5.9] - 2022-09-24
- Relax bounds on `vulkan`

## [0.5.8.1] - 2022-09-06
- Add support for GHC 9.4.

## [0.5.8] - 2022-09-06
- Relax bounds on `vulkan`

## [0.5.7] - 2022-03-31
- Relax bounds on `vulkan`

## [0.5.6] - 2022-02-05
- Relax bounds on `vulkan`

## [0.5.5] - 2022-01-14
- Relax bounds on `vulkan`
- Squash warnings

## [0.5.4] - 2021-11-25
- Relax bounds on `vulkan`

## [0.5.3] - 2021-10-14
- Relax bounds on `vulkan`

## [0.5.2] - 2021-08-12
- Relax bounds on `vulkan`

## [0.5.1] - 2021-07-24
- Fix bug where `createInstanceFromRequirements` and
  `createDebugInstanceFromRequirements` would fail for missing layers in
  optional requirements.

## [0.5.0.2] - 2021-06-22
- Relax bounds on `vulkan`

## [0.5.0.1] - 2021-06-09
- Squash warning

## [0.5.0] - 2021-02-24
- Refactor module `Vulkan.Utils.ShaderQQ`
  - Remove `Vulkan.Utils.ShaderQQ`
  - Remove `Vulkan.Utils.ShaderQQ.Shaderc`
  - Provide `glsl`/`hlsl`, `vert` .. `comp`, `rgen` .. `rcall`, `mesh`, `task`, `compileShaderQ`, `compileShader` in each ShaderQQ provider module under `Vulkan.Utils.ShaderQQ` for help compile shaders
  - Add `Vulkan.Utils.ShaderQQ.Backend.Glslang`to help process warning & error messages for glslangValidator
  - Add `Vulkan.Utils.ShaderQQ.Backend.Shaderc`to help process warning & error messages for glslc
  - Add `Vulkan.Utils.ShaderQQ.GLSL.Glslang` to compile glsl shaders for glslangValidator
  - Add `Vulkan.Utils.ShaderQQ.GLSL.Shaderc` to compile glsl shaders for glslc
  - Add `Vulkan.Utils.ShaderQQ.HLSL.Glslang` to compile hlsl shaders for glslangValidator
  - Add `Vulkan.Utils.ShaderQQ.HLSL.Shaderc` to compile hlsl shaders for glslc
- specify `--target-spv` for shaderc ray tracing shaders
- specify `--target-env` for glslang ray tracing shaders
- support pass hlsl entry point to glslangValidator and shaderc
- support pass glsl entry point to glslangValidator

## [0.4.2] - 2021-02-18
- Relax bounds on `vulkan`

## [0.4.1] - 2021-01-09
- Better error message reporting on unsatisfied extension version. See
  https://github.com/expipiplus1/vulkan/issues/249

## [0.4] - 2020-11-30

- Add `Vulkan.Utils.Requirements`, a module with several helpers for the
  `Requirement` types found in `Vulkan.Requirement`
- Change the creation helpers in `Vulkan.Utils.Initialization` to use the new
  `Requirement` types.
- Add QuasiQuoters for creating device requirements in `Vulkan.Utils.Requirements.TH`
- Allow selecting target environment in `compileShader`

## [0.3] - 2020-11-24

- Change type of pickPhysicalDevice to return Nothing instead of throwing
- Add `checkCommandsExp` function to generate an expression checking specified
  commands for non-nullness
- Expose Queue family index for queues assigned with `assignQueues`
- Add `Vulkan.Utils.ShaderQQ.Shaderc` to compile HLSL shaders

## [0.2] - 2020-11-15

- Add `Vulkan.Utils.Misc` for handy functions used in Vulkan programs, but not
  Vulkan specific.
- Add `Vulkan.Utils.Initializaion` for functions to ease creating a Vulkan device.
- Add `Vulkan.Vulkan.Utils.QueueAssignment` to help with easy queue creation.

## [0.1.3] - 2020-11-12

- Add `glsl` interpolating quasiquoter

## [0.1.2.1] - 2020-11-01

- Raise bound on base

## [0.1.2] - 2020-09-27
  - Initial release
    - Shader QuasiQuoters
    - GL format enum conversion
    - Debug helpers
