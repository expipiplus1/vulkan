{-# language CPP #-}
-- | = Name
--
-- VK_AMD_gpu_shader_int16 - device extension
--
-- == VK_AMD_gpu_shader_int16
--
-- [__Name String__]
--     @VK_AMD_gpu_shader_int16@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     133
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Deprecated/ by @VK_KHR_shader_float16_int8@ extension
--
--         -   Which in turn was /promoted/ to
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Qun Lin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_gpu_shader_int16] @linqun%0A<<Here describe the issue or question you have about the VK_AMD_gpu_shader_int16 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-04-11
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_gpu_shader_int16.html SPV_AMD_gpu_shader_int16>
--
-- [__Contributors__]
--
--     -   Daniel Rakos, AMD
--
--     -   Dominik Witczak, AMD
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Rex Xu, AMD
--
--     -   Timothy Lottes, AMD
--
--     -   Zhi Cai, AMD
--
-- [__External Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_gpu_shader_int16.html SPV_AMD_gpu_shader_int16>
--
-- == Description
--
-- This extension adds support for using 16-bit integer variables in
-- shaders.
--
-- == Deprecation by @VK_KHR_shader_float16_int8@
--
-- Functionality in this extension was included in
-- @VK_KHR_shader_float16_int8@ extension, when
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures'::@shaderInt16@
-- and
-- 'Vulkan.Extensions.VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8FeaturesKHR'::@shaderFloat16@
-- are enabled.
--
-- == New Enum Constants
--
-- -   'AMD_GPU_SHADER_INT16_EXTENSION_NAME'
--
-- -   'AMD_GPU_SHADER_INT16_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 2, 2019-04-11 (Tobias Hector)
--
--     -   Marked as deprecated
--
-- -   Revision 1, 2017-06-18 (Dominik Witczak)
--
--     -   First version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_gpu_shader_int16 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_gpu_shader_int16  ( AMD_GPU_SHADER_INT16_SPEC_VERSION
                                                  , pattern AMD_GPU_SHADER_INT16_SPEC_VERSION
                                                  , AMD_GPU_SHADER_INT16_EXTENSION_NAME
                                                  , pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME
                                                  ) where

import Data.String (IsString)

type AMD_GPU_SHADER_INT16_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_AMD_GPU_SHADER_INT16_SPEC_VERSION"
pattern AMD_GPU_SHADER_INT16_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_GPU_SHADER_INT16_SPEC_VERSION = 2


type AMD_GPU_SHADER_INT16_EXTENSION_NAME = "VK_AMD_gpu_shader_int16"

-- No documentation found for TopLevel "VK_AMD_GPU_SHADER_INT16_EXTENSION_NAME"
pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME = "VK_AMD_gpu_shader_int16"

