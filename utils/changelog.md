# Change Log

## WIP

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
