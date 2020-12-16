{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_draw_parameters - device extension
--
-- == VK_KHR_shader_draw_parameters
--
-- [__Name String__]
--     @VK_KHR_shader_draw_parameters@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     64
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
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_shader_draw_parameters:%20&body=@dgkoch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_shader_draw_parameters.html SPV_KHR_shader_draw_parameters>
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shader_draw_parameters.txt GL_ARB_shader_draw_parameters>
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA Corporation
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   John Kessenich, Google
--
--     -   Stuart Smith, IMG
--
-- == Description
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_KHR_shader_draw_parameters@
--
-- The extension provides access to three additional built-in shader
-- variables in Vulkan:
--
-- -   @BaseInstance@, which contains the @firstInstance@ parameter passed
--     to draw commands,
--
-- -   @BaseVertex@, which contains the @firstVertex@ or @vertexOffset@
--     parameter passed to draw commands, and
--
-- -   @DrawIndex@, which contains the index of the draw call currently
--     being processed from an indirect draw call.
--
-- When using GLSL source-based shader languages, the following variables
-- from @GL_ARB_shader_draw_parameters@ can map to these SPIR-V built-in
-- decorations:
--
-- -   @in int gl_BaseInstanceARB;@ → @BaseInstance@,
--
-- -   @in int gl_BaseVertexARB;@ → @BaseVertex@, and
--
-- -   @in int gl_DrawIDARB;@ → @DrawIndex@.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1,
-- however a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderDrawParameters feature bit was added>
-- to distinguish whether it is actually available or not.
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME'
--
-- -   'KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION'
--
-- == New Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-baseinstance BaseInstance>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-basevertex BaseVertex>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-drawindex DrawIndex>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-DrawParameters DrawParameters>
--
-- == Issues
--
-- 1) Is this the same functionality as @GL_ARB_shader_draw_parameters@?
--
-- __RESOLVED__: It’s actually a superset as it also adds in support for
-- arrayed drawing commands.
--
-- In GL for @GL_ARB_shader_draw_parameters@, @gl_BaseVertexARB@ holds the
-- integer value passed to the parameter to the command that resulted in
-- the current shader invocation. In the case where the command has no
-- @baseVertex@ parameter, the value of @gl_BaseVertexARB@ is zero. This
-- means that @gl_BaseVertexARB@ = @baseVertex@ (for @glDrawElements@
-- commands with @baseVertex@) or 0. In particular there are no
-- @glDrawArrays@ commands that take a @baseVertex@ parameter.
--
-- Now in Vulkan, we have @BaseVertex@ = @vertexOffset@ (for indexed
-- drawing commands) or @firstVertex@ (for arrayed drawing commands), and
-- so Vulkan’s version is really a superset of GL functionality.
--
-- == Version History
--
-- -   Revision 1, 2016-10-05 (Daniel Koch)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_draw_parameters Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_draw_parameters  ( KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION
                                                        , pattern KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION
                                                        , KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
                                                        , pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)

type KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION"
pattern KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION = 1


type KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME = "VK_KHR_shader_draw_parameters"

-- No documentation found for TopLevel "VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME"
pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME = "VK_KHR_shader_draw_parameters"

