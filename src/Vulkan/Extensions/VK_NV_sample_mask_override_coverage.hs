{-# language CPP #-}
-- | = Name
--
-- VK_NV_sample_mask_override_coverage - device extension
--
-- == VK_NV_sample_mask_override_coverage
--
-- [__Name String__]
--     @VK_NV_sample_mask_override_coverage@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     95
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
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_sample_mask_override_coverage:%20&body=@pdaniell-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_sample_mask_override_coverage.html SPV_NV_sample_mask_override_coverage>
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/NV/NV_sample_mask_override_coverage.txt GL_NV_sample_mask_override_coverage>
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_NV_sample_mask_override_coverage@
--
-- The extension provides access to the @OverrideCoverageNV@ decoration
-- under the @SampleMaskOverrideCoverageNV@ capability. Adding this
-- decoration to a variable with the
-- 'Vulkan.Core10.FundamentalTypes.SampleMask' builtin decoration allows
-- the shader to modify the coverage mask and affect which samples are used
-- to process the fragment.
--
-- When using GLSL source-based shader languages, the @override_coverage@
-- layout qualifier from @GL_NV_sample_mask_override_coverage@ maps to the
-- @OverrideCoverageNV@ decoration. To use the @override_coverage@ layout
-- qualifier in GLSL the @GL_NV_sample_mask_override_coverage@ extension
-- must be enabled. Behavior is described in the
-- @GL_NV_sample_mask_override_coverage@ extension spec.
--
-- == New Enum Constants
--
-- -   'NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME'
--
-- -   'NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION'
--
-- == New Variable Decoration
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-samplemask OverrideCoverageNV in SampleMask>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-SampleMaskOverrideCoverageNV SampleMaskOverrideCoverageNV>
--
-- == Version History
--
-- -   Revision 1, 2016-12-08 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_sample_mask_override_coverage Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_sample_mask_override_coverage  ( NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION
                                                              , pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION
                                                              , NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
                                                              , pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
                                                              ) where

import Data.String (IsString)

type NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION"
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION = 1


type NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME = "VK_NV_sample_mask_override_coverage"

-- No documentation found for TopLevel "VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME"
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME = "VK_NV_sample_mask_override_coverage"

