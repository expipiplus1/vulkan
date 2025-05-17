{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_fragment_mask - device extension
--
-- == VK_AMD_shader_fragment_mask
--
-- [__Name String__]
--     @VK_AMD_shader_fragment_mask@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     138
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
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/AMD/SPV_AMD_shader_fragment_mask.html SPV_AMD_shader_fragment_mask>
--
-- [__Contact__]
--
--     -   Aaron Hagan
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_shader_fragment_mask] @AaronHaganAMD%0A*Here describe the issue or question you have about the VK_AMD_shader_fragment_mask extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-08-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/amd/GL_AMD_shader_fragment_mask.txt GL_AMD_shader_fragment_mask>
--
-- [__Contributors__]
--
--     -   Aaron Hagan, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Timothy Lottes, AMD
--
-- == Description
--
-- This extension provides efficient read access to the fragment mask in
-- compressed multisampled color surfaces. The fragment mask is a lookup
-- table that associates color samples with color fragment values.
--
-- From a shader, the fragment mask can be fetched with a call to
-- @fragmentMaskFetchAMD@, which returns a single @uint@ where each
-- subsequent four bits specify the color fragment index corresponding to
-- the color sample, starting from the least significant bit. For example,
-- when eight color samples are used, the color fragment index for color
-- sample 0 will be in bits 0-3 of the fragment mask, for color sample 7
-- the index will be in bits 28-31.
--
-- The color fragment for a particular color sample may then be fetched
-- with the corresponding fragment mask value using the @fragmentFetchAMD@
-- shader function.
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME'
--
-- -   'AMD_SHADER_FRAGMENT_MASK_SPEC_VERSION'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-FragmentMaskAMD FragmentMaskAMD>
--
-- == Examples
--
-- This example shows a shader that queries the fragment mask from a
-- multisampled compressed surface and uses it to query fragment values.
--
-- > #version 450 core
-- >
-- > #extension GL_AMD_shader_fragment_mask: enable
-- >
-- > layout(binding = 0) uniform sampler2DMS       s2DMS;
-- > layout(binding = 1) uniform isampler2DMSArray is2DMSArray;
-- >
-- > layout(binding = 2, input_attachment_index = 0) uniform usubpassInputMS usubpassMS;
-- >
-- > layout(location = 0) out vec4 fragColor;
-- >
-- > void main()
-- > {
-- >     vec4 fragOne = vec4(0.0);
-- >
-- >     uint fragMask = fragmentMaskFetchAMD(s2DMS, ivec2(2, 3));
-- >     uint fragIndex = (fragMask & 0xF0) >> 4;
-- >     fragOne += fragmentFetchAMD(s2DMS, ivec2(2, 3), 1);
-- >
-- >     fragMask = fragmentMaskFetchAMD(is2DMSArray, ivec3(2, 3, 1));
-- >     fragIndex = (fragMask & 0xF0) >> 4;
-- >     fragOne += fragmentFetchAMD(is2DMSArray, ivec3(2, 3, 1), fragIndex);
-- >
-- >     fragMask = fragmentMaskFetchAMD(usubpassMS);
-- >     fragIndex = (fragMask & 0xF0) >> 4;
-- >     fragOne += fragmentFetchAMD(usubpassMS, fragIndex);
-- >
-- >     fragColor = fragOne;
-- > }
--
-- == Version History
--
-- -   Revision 1, 2017-08-16 (Aaron Hagan)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_shader_fragment_mask Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_shader_fragment_mask  ( AMD_SHADER_FRAGMENT_MASK_SPEC_VERSION
                                                      , pattern AMD_SHADER_FRAGMENT_MASK_SPEC_VERSION
                                                      , AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME
                                                      , pattern AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME
                                                      ) where

import Data.String (IsString)

type AMD_SHADER_FRAGMENT_MASK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_SHADER_FRAGMENT_MASK_SPEC_VERSION"
pattern AMD_SHADER_FRAGMENT_MASK_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_FRAGMENT_MASK_SPEC_VERSION = 1


type AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME = "VK_AMD_shader_fragment_mask"

-- No documentation found for TopLevel "VK_AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME"
pattern AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_FRAGMENT_MASK_EXTENSION_NAME = "VK_AMD_shader_fragment_mask"

