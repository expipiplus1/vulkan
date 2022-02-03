{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_explicit_vertex_parameter - device extension
--
-- == VK_AMD_shader_explicit_vertex_parameter
--
-- [__Name String__]
--     @VK_AMD_shader_explicit_vertex_parameter@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     22
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
--     -   Qun Lin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_shader_explicit_vertex_parameter] @linqun%0A<<Here describe the issue or question you have about the VK_AMD_shader_explicit_vertex_parameter extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-05-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_shader_explicit_vertex_parameter.html SPV_AMD_shader_explicit_vertex_parameter>
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/AMD/AMD_shader_explicit_vertex_parameter.txt GL_AMD_shader_explicit_vertex_parameter>
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Qun Lin, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Rex Xu, AMD
--
-- == Description
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_shader_explicit_vertex_parameter.html SPV_AMD_shader_explicit_vertex_parameter>
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME'
--
-- -   'AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2016-05-10 (Daniel Rakos)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_shader_explicit_vertex_parameter Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_shader_explicit_vertex_parameter  ( AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION
                                                                  , pattern AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION
                                                                  , AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME
                                                                  , pattern AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME
                                                                  ) where

import Data.String (IsString)

type AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION"
pattern AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_SPEC_VERSION = 1


type AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME = "VK_AMD_shader_explicit_vertex_parameter"

-- No documentation found for TopLevel "VK_AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME"
pattern AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_EXPLICIT_VERTEX_PARAMETER_EXTENSION_NAME = "VK_AMD_shader_explicit_vertex_parameter"

