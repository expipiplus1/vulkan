{-# language CPP #-}
-- | = Name
--
-- VK_NV_geometry_shader_passthrough - device extension
--
-- == VK_NV_geometry_shader_passthrough
--
-- [__Name String__]
--     @VK_NV_geometry_shader_passthrough@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     96
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
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_geometry_shader_passthrough.html SPV_NV_geometry_shader_passthrough>
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_geometry_shader_passthrough] @dgkoch%0A*Here describe the issue or question you have about the VK_NV_geometry_shader_passthrough extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-02-15
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://registry.khronos.org/OpenGL/extensions/NV/NV_geometry_shader_passthrough.txt GL_NV_geometry_shader_passthrough>
--
--     -   This extension requires the @geometryShader@ feature.
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
-- -   @SPV_NV_geometry_shader_passthrough@
--
-- Geometry shaders provide the ability for applications to process each
-- primitive sent through the graphics pipeline using a programmable
-- shader. However, one common use case treats them largely as a
-- “passthrough”. In this use case, the bulk of the geometry shader code
-- simply copies inputs from each vertex of the input primitive to
-- corresponding outputs in the vertices of the output primitive. Such
-- shaders might also compute values for additional built-in or
-- user-defined per-primitive attributes (e.g., @Layer@) to be assigned to
-- all the vertices of the output primitive.
--
-- This extension provides access to the @PassthroughNV@ decoration under
-- the @GeometryShaderPassthroughNV@ capability. Adding this to a geometry
-- shader input variable specifies that the values of this input are copied
-- to the corresponding vertex of the output primitive.
--
-- When using GLSL source-based shading languages, the @passthrough@ layout
-- qualifier from @GL_NV_geometry_shader_passthrough@ maps to the
-- @PassthroughNV@ decoration. To use the @passthrough@ layout, in GLSL the
-- @GL_NV_geometry_shader_passthrough@ extension must be enabled. Behaviour
-- is described in the @GL_NV_geometry_shader_passthrough@ extension
-- specification.
--
-- == New Enum Constants
--
-- -   'NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME'
--
-- -   'NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION'
--
-- == New Variable Decoration
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#geometry-passthrough-passthrough PassthroughNV>
--     in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#geometry-passthrough Geometry Shader Passthrough>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-GeometryShaderPassthroughNV GeometryShaderPassthroughNV>
--
-- == Issues
--
-- 1) Should we require or allow a passthrough geometry shader to specify
-- the output layout qualifiers for the output primitive type and maximum
-- vertex count in the SPIR-V?
--
-- __RESOLVED__: Yes they should be required in the SPIR-V. Per
-- GL_NV_geometry_shader_passthrough they are not permitted in the GLSL
-- source shader, but SPIR-V is lower-level. It is straightforward for the
-- GLSL compiler to infer them from the input primitive type and to
-- explicitly emit them in the SPIR-V according to the following table.
--
-- +-----------------------------------+------------------------------------------+
-- | Input Layout                      | Implied Output Layout                    |
-- +===================================+==========================================+
-- | points                            | @layout(points, max_vertices=1)@         |
-- +-----------------------------------+------------------------------------------+
-- | lines                             | @layout(line_strip, max_vertices=2)@     |
-- +-----------------------------------+------------------------------------------+
-- | triangles                         | @layout(triangle_strip, max_vertices=3)@ |
-- +-----------------------------------+------------------------------------------+
--
-- 2) How does interface matching work with passthrough geometry shaders?
--
-- __RESOLVED__: This is described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#geometry-passthrough-interface Passthrough Interface Matching>.
-- In GL when using passthough geometry shaders in separable mode, all
-- inputs must also be explicitly assigned location layout qualifiers. In
-- Vulkan all SPIR-V shader inputs (except built-ins) must also have
-- location decorations specified. Redeclarations of built-in variables
-- that add the passthrough layout qualifier are exempted from the rule
-- requiring location assignment because built-in variables do not have
-- locations and are matched by @BuiltIn@ decoration.
--
-- == Sample Code
--
-- Consider the following simple geometry shader in unextended GLSL:
--
-- > layout(triangles) in;
-- > layout(triangle_strip) out;
-- > layout(max_vertices=3) out;
-- >
-- > in Inputs {
-- >     vec2 texcoord;
-- >     vec4 baseColor;
-- > } v_in[];
-- > out Outputs {
-- >     vec2 texcoord;
-- >     vec4 baseColor;
-- > };
-- >
-- > void main()
-- > {
-- >     int layer = compute_layer();
-- >     for (int i = 0; i < 3; i++) {
-- >         gl_Position = gl_in[i].gl_Position;
-- >         texcoord = v_in[i].texcoord;
-- >         baseColor = v_in[i].baseColor;
-- >         gl_Layer = layer;
-- >         EmitVertex();
-- >     }
-- > }
--
-- In this shader, the inputs @gl_Position@, @Inputs.texcoord@, and
-- @Inputs.baseColor@ are simply copied from the input vertex to the
-- corresponding output vertex. The only “interesting” work done by the
-- geometry shader is computing and emitting a @gl_Layer@ value for the
-- primitive.
--
-- The following geometry shader, using this extension, is equivalent:
--
-- > #extension GL_NV_geometry_shader_passthrough : require
-- >
-- > layout(triangles) in;
-- > // No output primitive layout qualifiers required.
-- >
-- > // Redeclare gl_PerVertex to pass through "gl_Position".
-- > layout(passthrough) in gl_PerVertex {
-- >     vec4 gl_Position;
-- > } gl_in[];
-- >
-- > // Declare "Inputs" with "passthrough" to automatically copy members.
-- > layout(passthrough) in Inputs {
-- >     vec2 texcoord;
-- >     vec4 baseColor;
-- > } v_in[];
-- >
-- > // No output block declaration required.
-- >
-- > void main()
-- > {
-- >     // The shader simply computes and writes gl_Layer.  We do not
-- >     // loop over three vertices or call EmitVertex().
-- >     gl_Layer = compute_layer();
-- > }
--
-- == Version History
--
-- -   Revision 1, 2017-02-15 (Daniel Koch)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_geometry_shader_passthrough Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_geometry_shader_passthrough  ( NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION
                                                            , pattern NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION
                                                            , NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
                                                            , pattern NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
                                                            ) where

import Data.String (IsString)

type NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION"
pattern NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION :: forall a . Integral a => a
pattern NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION = 1


type NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME = "VK_NV_geometry_shader_passthrough"

-- No documentation found for TopLevel "VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME"
pattern NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME = "VK_NV_geometry_shader_passthrough"

