{-# language CPP #-}
-- | = Name
--
-- VK_AMD_pipeline_compiler_control - device extension
--
-- == VK_AMD_pipeline_compiler_control
--
-- [__Name String__]
--     @VK_AMD_pipeline_compiler_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     184
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_pipeline_compiler_control] @anteru%0A*Here describe the issue or question you have about the VK_AMD_pipeline_compiler_control extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-07-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension introduces 'PipelineCompilerControlCreateInfoAMD'
-- structure that can be chained to a pipeline’s creation information to
-- specify additional flags that affect pipeline compilation.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.ExecutionGraphPipelineCreateInfoAMDX':
--
--     -   'PipelineCompilerControlCreateInfoAMD'
--
-- == New Enums
--
-- -   'PipelineCompilerControlFlagBitsAMD'
--
-- == New Bitmasks
--
-- -   'PipelineCompilerControlFlagsAMD'
--
-- == New Enum Constants
--
-- -   'AMD_PIPELINE_COMPILER_CONTROL_EXTENSION_NAME'
--
-- -   'AMD_PIPELINE_COMPILER_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2019-07-26 (Tobias Hector)
--
--     -   Initial revision.
--
-- == See Also
--
-- 'PipelineCompilerControlCreateInfoAMD',
-- 'PipelineCompilerControlFlagBitsAMD', 'PipelineCompilerControlFlagsAMD'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_pipeline_compiler_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_pipeline_compiler_control  (PipelineCompilerControlCreateInfoAMD) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PipelineCompilerControlCreateInfoAMD

instance ToCStruct PipelineCompilerControlCreateInfoAMD
instance Show PipelineCompilerControlCreateInfoAMD

instance FromCStruct PipelineCompilerControlCreateInfoAMD

