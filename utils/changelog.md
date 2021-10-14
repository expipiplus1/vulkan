# Change Log

## WIP

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
