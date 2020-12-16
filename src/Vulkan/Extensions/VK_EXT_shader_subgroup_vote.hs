{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_subgroup_vote - device extension
--
-- == VK_EXT_shader_subgroup_vote
--
-- [__Name String__]
--     @VK_EXT_shader_subgroup_vote@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     66
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Deprecated/ by
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-new-features Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_shader_subgroup_vote:%20&body=@dgkoch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-11-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_subgroup_vote.html SPV_KHR_subgroup_vote>
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shader_group_vote.txt GL_ARB_shader_group_vote>
--
-- [__Contributors__]
--
--     -   Neil Henning, Codeplay
--
--     -   Daniel Koch, NVIDIA Corporation
--
-- == Description
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_KHR_subgroup_vote@
--
-- This extension provides new SPIR-V instructions:
--
-- -   @OpSubgroupAllKHR@,
--
-- -   @OpSubgroupAnyKHR@, and
--
-- -   @OpSubgroupAllEqualKHR@.
--
-- to compute the composite of a set of boolean conditions across a group
-- of shader invocations that are running concurrently (a /subgroup/).
-- These composite results may be used to execute shaders more efficiently
-- on a 'Vulkan.Core10.Handles.PhysicalDevice'.
--
-- When using GLSL source-based shader languages, the following shader
-- functions from GL_ARB_shader_group_vote can map to these SPIR-V
-- instructions:
--
-- -   @anyInvocationARB@() → @OpSubgroupAnyKHR@,
--
-- -   @allInvocationsARB@() → @OpSubgroupAllKHR@, and
--
-- -   @allInvocationsEqualARB@() → @OpSubgroupAllEqualKHR@.
--
-- The subgroup across which the boolean conditions are evaluated is
-- implementation-dependent, and this extension provides no guarantee over
-- how individual shader invocations are assigned to subgroups. In
-- particular, a subgroup has no necessary relationship with the compute
-- shader /local workgroup/ — any pair of shader invocations in a compute
-- local workgroup may execute in different subgroups as used by these
-- instructions.
--
-- Compute shaders operate on an explicitly specified group of threads (a
-- local workgroup), but many implementations will also group non-compute
-- shader invocations and execute them concurrently. When executing code
-- like
--
-- > if (condition) {
-- >   result = do_fast_path();
-- > } else {
-- >   result = do_general_path();
-- > }
--
-- where @condition@ diverges between invocations, an implementation might
-- first execute @do_fast_path@() for the invocations where @condition@ is
-- true and leave the other invocations dormant. Once @do_fast_path@()
-- returns, it might call @do_general_path@() for invocations where
-- @condition@ is @false@ and leave the other invocations dormant. In this
-- case, the shader executes __both__ the fast and the general path and
-- might be better off just using the general path for all invocations.
--
-- This extension provides the ability to avoid divergent execution by
-- evaluating a condition across an entire subgroup using code like:
--
-- > if (allInvocationsARB(condition)) {
-- >   result = do_fast_path();
-- > } else {
-- >   result = do_general_path();
-- > }
--
-- The built-in function @allInvocationsARB@() will return the same value
-- for all invocations in the group, so the group will either execute
-- @do_fast_path@() or @do_general_path@(), but never both. For example,
-- shader code might want to evaluate a complex function iteratively by
-- starting with an approximation of the result and then refining the
-- approximation. Some input values may require a small number of
-- iterations to generate an accurate result (@do_fast_path@) while others
-- require a larger number (@do_general_path@). In another example, shader
-- code might want to evaluate a complex function (@do_general_path@) that
-- can be greatly simplified when assuming a specific value for one of its
-- inputs (@do_fast_path@).
--
-- == Deprecated by Vulkan 1.1
--
-- All functionality in this extension is superseded by the core Vulkan 1.1
-- <VkPhysicalDeviceSubgroupProperties.html subgroup operations>.
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME'
--
-- -   'EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-SubgroupVoteKHR SubgroupVoteKHR>
--
-- == Version History
--
-- -   Revision 1, 2016-11-28 (Daniel Koch)
--
--     -   Initial draft
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_subgroup_vote Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_subgroup_vote  ( EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION
                                                      , pattern EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION
                                                      , EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME
                                                      , pattern EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME
                                                      ) where

import Data.String (IsString)

type EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION"
pattern EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_SUBGROUP_VOTE_SPEC_VERSION = 1


type EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME = "VK_EXT_shader_subgroup_vote"

-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME"
pattern EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_SUBGROUP_VOTE_EXTENSION_NAME = "VK_EXT_shader_subgroup_vote"

