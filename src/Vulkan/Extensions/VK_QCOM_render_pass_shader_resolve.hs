{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_render_pass_shader_resolve - device extension
--
-- == VK_QCOM_render_pass_shader_resolve
--
-- [__Name String__]
--     @VK_QCOM_render_pass_shader_resolve@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     172
--
-- [__Revision__]
--     4
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Bill Licea-Kane
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_render_pass_shader_resolve] @wwlk%0A<<Here describe the issue or question you have about the VK_QCOM_render_pass_shader_resolve extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-11-07
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--     None.
--
-- [__Contributors__]
--
--     -   Srihari Babu Alla, Qualcomm
--
--     -   Bill Licea-Kane, Qualcomm
--
--     -   Jeff Leger, Qualcomm
--
-- == Description
--
-- This extension allows a shader resolve to replace fixed-function
-- resolve.
--
-- Fixed-function resolve is limited in function to simple filters of
-- multisample buffers to a single sample buffer.
--
-- Fixed-function resolve is more performance efficient and\/or power
-- efficient than shader resolve for such simple filters.
--
-- Shader resolve allows a shader writer to create complex, non-linear
-- filtering of a multisample buffer in the last subpass of a subpass
-- dependency chain.
--
-- This extension also provides a bit which /can/ be used to enlarge a
-- sample region dependency to a fragment region dependency, so that a
-- framebuffer-region dependency /can/ replace a framebuffer-global
-- dependency in some cases.
--
-- == New Enum Constants
--
-- -   'QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME'
--
-- -   'QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM'
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_SHADER_RESOLVE_BIT_QCOM'
--
-- == Issues
--
-- 1) Should this extension be named render_pass_shader_resolve?
--
-- __RESOLVED__ Yes.
--
-- This is part of suite of small extensions to render pass.
--
-- Following the style guide, instead of following
-- VK_KHR_create_renderpass2.
--
-- 2) Should the VK_SAMPLE_COUNT_1_BIT be required for each
-- pColorAttachment and the DepthStencilAttachent?
--
-- __RESOLVED__ No.
--
-- While this may not be a common use case, and while most fixed-function
-- resolve hardware has this limitation, there is little reason to require
-- a shader resolve to resolve to a single sample buffer.
--
-- 3) Should a shader resolve subpass be the last subpass in a render pass?
--
-- __RESOLVED__ Yes.
--
-- To be more specific, it should be the last subpass in a subpass
-- dependency chain.
--
-- 4) Do we need the
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_QCOM'
-- bit?
--
-- __RESOLVED__ Yes.
--
-- This applies when an input attachment’s sample count is equal to
-- @rasterizationSamples@. Further, if @sampleShading@ is enabled
-- (explicitly or implicitly) then @minSampleShading@ /must/ equal 0.0.
--
-- However, this bit may be set on any subpass, it is not restricted to a
-- shader resolve subpass.
--
-- == Version History
--
-- -   Revision 1, 2019-06-28 (wwlk)
--
--     -   Initial draft
--
-- -   Revision 2, 2019-11-06 (wwlk)
--
--     -   General clean-up\/spec updates
--
--     -   Added issues
--
-- -   Revision 3, 2019-11-07 (wwlk)
--
--     -   Typos
--
--     -   Additional issues
--
--     -   Clarified that a shader resolve subpass is the last subpass in a
--         subpass dependency chain
--
-- -   Revision 4, 2020-01-06 (wwlk)
--
--     -   Change resolution of Issue 1 (/render_pass/, not /renderpass/)
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_render_pass_shader_resolve Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_render_pass_shader_resolve  ( QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION
                                                             , pattern QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION
                                                             , QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME
                                                             , pattern QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME
                                                             ) where

import Data.String (IsString)

type QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION"
pattern QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION = 4


type QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME = "VK_QCOM_render_pass_shader_resolve"

-- No documentation found for TopLevel "VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME"
pattern QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME = "VK_QCOM_render_pass_shader_resolve"

