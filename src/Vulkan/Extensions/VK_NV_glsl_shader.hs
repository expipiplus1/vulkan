{-# language CPP #-}
-- | = Name
--
-- VK_NV_glsl_shader - device extension
--
-- == VK_NV_glsl_shader
--
-- [__Name String__]
--     @VK_NV_glsl_shader@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     13
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
--     -   /Deprecated/ without replacement
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_glsl_shader] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_NV_glsl_shader extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-02-14
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows GLSL shaders written to the @GL_KHR_vulkan_glsl@
-- extension specification to be used instead of SPIR-V. The implementation
-- will automatically detect whether the shader is SPIR-V or GLSL, and
-- compile it appropriately.
--
-- == Deprecation
--
-- Functionality in this extension is outside of the scope of Vulkan and is
-- better served by a compiler library such as
-- <https://github.com/KhronosGroup/glslang glslang>. No new
-- implementations will support this extension, so applications /should/
-- not use it.
--
-- == New Enum Constants
--
-- -   'NV_GLSL_SHADER_EXTENSION_NAME'
--
-- -   'NV_GLSL_SHADER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
-- == Examples
--
-- __Example 1__
--
-- Passing in GLSL code
--
-- >     char const vss[] =
-- >         "#version 450 core\n"
-- >         "layout(location = 0) in vec2 aVertex;\n"
-- >         "layout(location = 1) in vec4 aColor;\n"
-- >         "out vec4 vColor;\n"
-- >         "void main()\n"
-- >         "{\n"
-- >         "    vColor = aColor;\n"
-- >         "    gl_Position = vec4(aVertex, 0, 1);\n"
-- >         "}\n"
-- >     ;
-- >     VkShaderModuleCreateInfo vertexShaderInfo = { VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO };
-- >     vertexShaderInfo.codeSize = sizeof vss;
-- >     vertexShaderInfo.pCode = vss;
-- >     VkShaderModule vertexShader;
-- >     vkCreateShaderModule(device, &vertexShaderInfo, 0, &vertexShader);
--
-- == Version History
--
-- -   Revision 1, 2016-02-14 (Piers Daniell)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_glsl_shader Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_glsl_shader  ( NV_GLSL_SHADER_SPEC_VERSION
                                            , pattern NV_GLSL_SHADER_SPEC_VERSION
                                            , NV_GLSL_SHADER_EXTENSION_NAME
                                            , pattern NV_GLSL_SHADER_EXTENSION_NAME
                                            ) where

import Data.String (IsString)

type NV_GLSL_SHADER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_SPEC_VERSION"
pattern NV_GLSL_SHADER_SPEC_VERSION :: forall a . Integral a => a
pattern NV_GLSL_SHADER_SPEC_VERSION = 1


type NV_GLSL_SHADER_EXTENSION_NAME = "VK_NV_glsl_shader"

-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_EXTENSION_NAME"
pattern NV_GLSL_SHADER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_GLSL_SHADER_EXTENSION_NAME = "VK_NV_glsl_shader"

