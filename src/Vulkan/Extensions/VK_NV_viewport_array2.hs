{-# language CPP #-}
-- | = Name
--
-- VK_NV_viewport_array2 - device extension
--
-- == VK_NV_viewport_array2
--
-- [__Name String__]
--     @VK_NV_viewport_array2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     97
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_viewport_array2:%20&body=@dgkoch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-02-15
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_viewport_array2.html SPV_NV_viewport_array2>
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/NV/NV_viewport_array2.txt GL_NV_viewport_array2>
--
--     -   This extension requires the @geometryShader@ and @multiViewport@
--         features.
--
--     -   This extension interacts with the @tessellationShader@ feature.
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_NV_viewport_array2@
--
-- which allows a single primitive to be broadcast to multiple viewports
-- and\/or multiple layers. A new shader built-in output @ViewportMaskNV@
-- is provided, which allows a single primitive to be output to multiple
-- viewports simultaneously. Also, a new SPIR-V decoration is added to
-- control whether the effective viewport index is added into the variable
-- decorated with the @Layer@ built-in decoration. These capabilities allow
-- a single primitive to be output to multiple layers simultaneously.
--
-- This extension allows variables decorated with the @Layer@ and
-- @ViewportIndex@ built-ins to be exported from vertex or tessellation
-- shaders, using the @ShaderViewportIndexLayerNV@ capability.
--
-- This extension adds a new @ViewportMaskNV@ built-in decoration that is
-- available for output variables in vertex, tessellation evaluation, and
-- geometry shaders, and a new @ViewportRelativeNV@ decoration that can be
-- added on variables decorated with @Layer@ when using the
-- @ShaderViewportMaskNV@ capability.
--
-- When using GLSL source-based shading languages, the @gl_ViewportMask@[]
-- built-in output variable and @viewport_relative@ layout qualifier from
-- @GL_NV_viewport_array2@ map to the @ViewportMaskNV@ and
-- @ViewportRelativeNV@ decorations, respectively. Behaviour is described
-- in the @GL_NV_viewport_array2@ extension specificiation.
--
-- Note
--
-- The @ShaderViewportIndexLayerNV@ capability is equivalent to the
-- @ShaderViewportIndexLayerEXT@ capability added by
-- @VK_EXT_shader_viewport_index_layer@.
--
-- == New Enum Constants
--
-- -   'NV_VIEWPORT_ARRAY2_EXTENSION_NAME'
--
-- -   'NV_VIEWPORT_ARRAY2_SPEC_VERSION'
--
-- == New or Modified Built-In Variables
--
-- -   (modified)
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-layer Layer>
--
-- -   (modified)
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-viewportindex ViewportIndex>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-viewportmask ViewportMaskNV>
--
-- == New Variable Decoration
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-layer ViewportRelativeNV in Layer>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ShaderViewportIndexLayerNV ShaderViewportIndexLayerNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ShaderViewportMaskNV ShaderViewportMaskNV>
--
-- == Version History
--
-- -   Revision 1, 2017-02-15 (Daniel Koch)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_viewport_array2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_viewport_array2  ( NV_VIEWPORT_ARRAY2_SPEC_VERSION
                                                , pattern NV_VIEWPORT_ARRAY2_SPEC_VERSION
                                                , NV_VIEWPORT_ARRAY2_EXTENSION_NAME
                                                , pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME
                                                ) where

import Data.String (IsString)

type NV_VIEWPORT_ARRAY2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_VIEWPORT_ARRAY2_SPEC_VERSION"
pattern NV_VIEWPORT_ARRAY2_SPEC_VERSION :: forall a . Integral a => a
pattern NV_VIEWPORT_ARRAY2_SPEC_VERSION = 1


type NV_VIEWPORT_ARRAY2_EXTENSION_NAME = "VK_NV_viewport_array2"

-- No documentation found for TopLevel "VK_NV_VIEWPORT_ARRAY2_EXTENSION_NAME"
pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME = "VK_NV_viewport_array2"

