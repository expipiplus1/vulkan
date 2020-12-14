{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_viewport_index_layer - device extension
--
-- == VK_EXT_shader_viewport_index_layer
--
-- [__Name String__]
--     @VK_EXT_shader_viewport_index_layer@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     163
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
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_shader_viewport_index_layer:%20&body=@dgkoch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-08-08
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_viewport_index_layer.html SPV_EXT_shader_viewport_index_layer>
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shader_viewport_layer_array.txt GL_ARB_shader_viewport_layer_array>,
--         <https://www.khronos.org/registry/OpenGL/extensions/AMD/AMD_vertex_shader_layer.txt GL_AMD_vertex_shader_layer>,
--         <https://www.khronos.org/registry/OpenGL/extensions/AMD/AMD_vertex_shader_viewport_index.txt GL_AMD_vertex_shader_viewport_index>,
--         and
--         <https://www.khronos.org/registry/OpenGL/extensions/NV/NV_viewport_array2.txt GL_NV_viewport_array2>
--
--     -   This extension requires the @multiViewport@ feature.
--
--     -   This extension interacts with the @tessellationShader@ feature.
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Daniel Rakos, AMD
--
--     -   Slawomir Grajeswki, Intel
--
-- == Description
--
-- This extension adds support for the @ShaderViewportIndexLayerEXT@
-- capability from the @SPV_EXT_shader_viewport_index_layer@ extension in
-- Vulkan.
--
-- This extension allows variables decorated with the @Layer@ and
-- @ViewportIndex@ built-ins to be exported from vertex or tessellation
-- shaders, using the @ShaderViewportIndexLayerEXT@ capability.
--
-- When using GLSL source-based shading languages, the @gl_ViewportIndex@
-- and @gl_Layer@ built-in variables map to the SPIR-V @ViewportIndex@ and
-- @Layer@ built-in decorations, respectively. Behaviour of these variables
-- is extended as described in the @GL_ARB_shader_viewport_layer_array@ (or
-- the precursor @GL_AMD_vertex_shader_layer@,
-- @GL_AMD_vertex_shader_viewport_index@, and @GL_NV_viewport_array2@
-- extensions).
--
-- Note
--
-- The @ShaderViewportIndexLayerEXT@ capability is equivalent to the
-- @ShaderViewportIndexLayerNV@ capability added by
-- @VK_NV_viewport_array2@.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2.
--
-- The single @ShaderViewportIndexLayerEXT@ capability from the
-- @SPV_EXT_shader_viewport_index_layer@ extension is replaced by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ShaderViewportIndex ShaderViewportIndex>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ShaderLayer ShaderLayer>
-- capabilities from SPIR-V 1.5 which are enabled by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderOutputViewportIndex shaderOutputViewportIndex>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderOutputLayer shaderOutputLayer>
-- features, respectively. Additionally, if Vulkan 1.2 is supported but
-- this extension is not, these capabilities are optional.
--
-- Enabling both features is equivalent to enabling the
-- @VK_EXT_shader_viewport_index_layer@ extension.
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME'
--
-- -   'EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION'
--
-- == New or Modified Built-In Variables
--
-- -   (modified)
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-layer Layer>
--
-- -   (modified)
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-viewportindex ViewportIndex>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ShaderViewportIndexLayerEXT ShaderViewportIndexLayerEXT>
--
-- == Version History
--
-- -   Revision 1, 2017-08-08 (Daniel Koch)
--
--     -   Internal drafts
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_viewport_index_layer Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_viewport_index_layer  ( EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION
                                                             , pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION
                                                             , EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME
                                                             , pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME
                                                             ) where

import Data.String (IsString)

type EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION"
pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION = 1


type EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME = "VK_EXT_shader_viewport_index_layer"

-- No documentation found for TopLevel "VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME"
pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME = "VK_EXT_shader_viewport_index_layer"

