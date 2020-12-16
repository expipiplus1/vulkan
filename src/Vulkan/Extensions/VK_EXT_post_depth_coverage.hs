{-# language CPP #-}
-- | = Name
--
-- VK_EXT_post_depth_coverage - device extension
--
-- == VK_EXT_post_depth_coverage
--
-- [__Name String__]
--     @VK_EXT_post_depth_coverage@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     156
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
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_post_depth_coverage:%20&body=@dgkoch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-07-17
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_post_depth_coverage.html SPV_KHR_post_depth_coverage>
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_post_depth_coverage.txt GL_ARB_post_depth_coverage>
--         and
--         <https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_post_depth_coverage.txt GL_EXT_post_depth_coverage>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_KHR_post_depth_coverage@
--
-- which allows the fragment shader to control whether values in the
-- 'Vulkan.Core10.FundamentalTypes.SampleMask' built-in input variable
-- reflect the coverage after early
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth depth>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil stencil>
-- tests are applied.
--
-- This extension adds a new @PostDepthCoverage@ execution mode under the
-- @SampleMaskPostDepthCoverage@ capability. When this mode is specified
-- along with @EarlyFragmentTests@, the value of an input variable
-- decorated with the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-samplemask >
-- built-in reflects the coverage after the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-fragment-earlytest early fragment tests>
-- are applied. Otherwise, it reflects the coverage before the depth and
-- stencil tests.
--
-- When using GLSL source-based shading languages, the
-- @post_depth_coverage@ layout qualifier from GL_ARB_post_depth_coverage
-- or GL_EXT_post_depth_coverage maps to the @PostDepthCoverage@ execution
-- mode.
--
-- == New Enum Constants
--
-- -   'EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME'
--
-- -   'EXT_POST_DEPTH_COVERAGE_SPEC_VERSION'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-SampleMaskPostDepthCoverage SampleMaskPostDepthCoverage>
--
-- == Version History
--
-- -   Revision 1, 2017-07-17 (Daniel Koch)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_post_depth_coverage Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_post_depth_coverage  ( EXT_POST_DEPTH_COVERAGE_SPEC_VERSION
                                                     , pattern EXT_POST_DEPTH_COVERAGE_SPEC_VERSION
                                                     , EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
                                                     , pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)

type EXT_POST_DEPTH_COVERAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION"
pattern EXT_POST_DEPTH_COVERAGE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_POST_DEPTH_COVERAGE_SPEC_VERSION = 1


type EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME = "VK_EXT_post_depth_coverage"

-- No documentation found for TopLevel "VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME"
pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME = "VK_EXT_post_depth_coverage"

