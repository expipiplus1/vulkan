{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_trinary_minmax - device extension
--
-- == VK_AMD_shader_trinary_minmax
--
-- [__Name String__]
--     @VK_AMD_shader_trinary_minmax@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     21
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
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_shader_trinary_minmax.html SPV_AMD_shader_trinary_minmax>
--
-- [__Contact__]
--
--     -   Qun Lin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_shader_trinary_minmax] @linqun%0A*Here describe the issue or question you have about the VK_AMD_shader_trinary_minmax extension* >
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
--     -   This extension provides API support for
--         <https://registry.khronos.org/OpenGL/extensions/AMD/AMD_shader_trinary_minmax.txt GL_AMD_shader_trinary_minmax>
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
-- -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_shader_trinary_minmax.html SPV_AMD_shader_trinary_minmax>
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME'
--
-- -   'AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION'
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_shader_trinary_minmax Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_shader_trinary_minmax  ( AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION
                                                       , pattern AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION
                                                       , AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME
                                                       , pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)

type AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION"
pattern AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION = 1


type AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME = "VK_AMD_shader_trinary_minmax"

-- No documentation found for TopLevel "VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME"
pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME = "VK_AMD_shader_trinary_minmax"

