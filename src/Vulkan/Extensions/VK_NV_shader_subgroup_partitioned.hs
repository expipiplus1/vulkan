{-# language CPP #-}
-- | = Name
--
-- VK_NV_shader_subgroup_partitioned - device extension
--
-- == VK_NV_shader_subgroup_partitioned
--
-- [__Name String__]
--     @VK_NV_shader_subgroup_partitioned@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     199
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_shader_subgroup_partitioned] @jeffbolznv%0A<<Here describe the issue or question you have about the VK_NV_shader_subgroup_partitioned extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-03-17
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_subgroup_partitioned.html SPV_NV_shader_subgroup_partitioned>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GL_NV_shader_subgroup_partitioned.txt GL_NV_shader_subgroup_partitioned>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension enables support for a new class of
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-group-operations group operations>
-- on
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-scope-subgroup subgroups>
-- via the
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GL_NV_shader_subgroup_partitioned.txt GL_NV_shader_subgroup_partitioned>
-- GLSL extension and
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_subgroup_partitioned.html SPV_NV_shader_subgroup_partitioned>
-- SPIR-V extension. Support for these new operations is advertised via the
-- 'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_PARTITIONED_BIT_NV'
-- bit.
--
-- This extension requires Vulkan 1.1, for general subgroup support.
--
-- == New Enum Constants
--
-- -   'NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME'
--
-- -   'NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SubgroupFeatureFlagBits':
--
--     -   'Vulkan.Core11.Enums.SubgroupFeatureFlagBits.SUBGROUP_FEATURE_PARTITIONED_BIT_NV'
--
-- == Version History
--
-- -   Revision 1, 2018-03-17 (Jeff Bolz)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_shader_subgroup_partitioned Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_shader_subgroup_partitioned  ( NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
                                                            , pattern NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
                                                            , NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
                                                            , pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
                                                            ) where

import Data.String (IsString)

type NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION"
pattern NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION = 1


type NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME = "VK_NV_shader_subgroup_partitioned"

-- No documentation found for TopLevel "VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME"
pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME = "VK_NV_shader_subgroup_partitioned"

