{-# language CPP #-}
-- | = Name
--
-- VK_NV_fragment_coverage_to_color - device extension
--
-- == VK_NV_fragment_coverage_to_color
--
-- [__Name String__]
--     @VK_NV_fragment_coverage_to_color@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     150
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_fragment_coverage_to_color] @jeffbolznv%0A<<Here describe the issue or question you have about the VK_NV_fragment_coverage_to_color extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-05-21
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows the fragment coverage value, represented as an
-- integer bitmask, to be substituted for a color output being written to a
-- single-component color attachment with integer components (e.g.
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8_UINT'). The functionality provided
-- by this extension is different from simply writing the
-- 'Vulkan.Core10.FundamentalTypes.SampleMask' fragment shader output, in
-- that the coverage value written to the framebuffer is taken after
-- stencil test and depth test, as well as after fragment operations such
-- as alpha-to-coverage.
--
-- This functionality may be useful for deferred rendering algorithms,
-- where the second pass needs to know which samples belong to which
-- original fragments.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo':
--
--     -   'PipelineCoverageToColorStateCreateInfoNV'
--
-- == New Bitmasks
--
-- -   'PipelineCoverageToColorStateCreateFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_FRAGMENT_COVERAGE_TO_COLOR_EXTENSION_NAME'
--
-- -   'NV_FRAGMENT_COVERAGE_TO_COLOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV'
--
-- == Version History
--
-- -   Revision 1, 2017-05-21 (Jeff Bolz)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PipelineCoverageToColorStateCreateFlagsNV',
-- 'PipelineCoverageToColorStateCreateInfoNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fragment_coverage_to_color Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_fragment_coverage_to_color  (PipelineCoverageToColorStateCreateInfoNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PipelineCoverageToColorStateCreateInfoNV

instance ToCStruct PipelineCoverageToColorStateCreateInfoNV
instance Show PipelineCoverageToColorStateCreateInfoNV

instance FromCStruct PipelineCoverageToColorStateCreateInfoNV

