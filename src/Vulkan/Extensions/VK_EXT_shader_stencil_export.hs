{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_stencil_export - device extension
--
-- == VK_EXT_shader_stencil_export
--
-- [__Name String__]
--     @VK_EXT_shader_stencil_export@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     141
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
--     -   Dominik Witczak
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_shader_stencil_export:%20&body=@dominikwitczakamd%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-07-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_stencil_export.html SPV_EXT_shader_stencil_export>
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shader_stencil_export.txt GL_ARB_shader_stencil_export>
--
-- [__Contributors__]
--
--     -   Dominik Witczak, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Rex Xu, AMD
--
-- == Description
--
-- This extension adds support for the SPIR-V extension
-- @SPV_EXT_shader_stencil_export@, providing a mechanism whereby a shader
-- may generate the stencil reference value per invocation. When stencil
-- testing is enabled, this allows the test to be performed against the
-- value generated in the shader.
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME'
--
-- -   'EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2017-07-19 (Dominik Witczak)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_stencil_export Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_stencil_export  ( EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION
                                                       , pattern EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION
                                                       , EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME
                                                       , pattern EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)

type EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION"
pattern EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION = 1


type EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME = "VK_EXT_shader_stencil_export"

-- No documentation found for TopLevel "VK_EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME"
pattern EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME = "VK_EXT_shader_stencil_export"

