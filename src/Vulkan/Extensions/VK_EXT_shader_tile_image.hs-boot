{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_tile_image - device extension
--
-- == VK_EXT_shader_tile_image
--
-- [__Name String__]
--     @VK_EXT_shader_tile_image@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     396
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Version 1.3>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_tile_image] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_shader_tile_image extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_tile_image.adoc VK_EXT_shader_tile_image>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-23
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_tile_image.html SPV_EXT_shader_tile_image>
--
--     -   This extension provides API support for
--         <https://raw.githubusercontent.com/KhronosGroup/GLSL/master/extensions/ext/GL_EXT_shader_tile_image.txt GL_EXT_shader_tile_image>
--
-- [__Contributors__]
--
--     -   Sandeep Kakarlapudi, Arm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   James Fitzpatrick, Imagination
--
--     -   Andrew Garrard, Imagination
--
--     -   Jeff Leger, Qualcomm
--
--     -   Huilong Wang, Huawei
--
--     -   Graeme Leese, Broadcom
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Tobias Hector, AMD
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Shahbaz Youssefi, Google
--
-- == Description
--
-- This extension allows fragment shader invocations to read color, depth
-- and stencil values at their pixel location in rasterization order. The
-- functionality is only available when using dynamic render passes
-- introduced by VK_KHR_dynamic_rendering. Example use cases are
-- programmable blending and deferred shading.
--
-- See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-shader-tileimage-reads fragment shader tile image reads>
-- for more information.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderTileImageFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderTileImagePropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_TILE_IMAGE_EXTENSION_NAME'
--
-- -   'EXT_SHADER_TILE_IMAGE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_PROPERTIES_EXT'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- Color read example.
--
-- > layout( location = 0 /* aliased to color attachment 0 */ ) tileImageEXT highp attachmentEXT color0;
-- > layout( location = 1 /* aliased to color attachment 1 */ ) tileImageEXT highp attachmentEXT color1;
-- >
-- > layout( location = 0 ) out vec4 fragColor;
-- >
-- > void main()
-- > {
-- >     vec4 value = colorAttachmentReadEXT(color0) + colorAttachmentReadEXT(color1);
-- >     fragColor = value;
-- > }
--
-- Depth & Stencil read example.
--
-- > void main()
-- > {
-- >     // read sample 0: works for non-MSAA or MSAA targets
-- >     highp float last_depth = depthAttachmentReadEXT();
-- >     lowp uint last_stencil = stencilAttachmentReadEXT();
-- >
-- >     //..
-- > }
--
-- == Version History
--
-- -   Revision 1, 2023-03-23 (Sandeep Kakarlapudi)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceShaderTileImageFeaturesEXT',
-- 'PhysicalDeviceShaderTileImagePropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_shader_tile_image Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_tile_image  ( PhysicalDeviceShaderTileImageFeaturesEXT
                                                   , PhysicalDeviceShaderTileImagePropertiesEXT
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderTileImageFeaturesEXT

instance ToCStruct PhysicalDeviceShaderTileImageFeaturesEXT
instance Show PhysicalDeviceShaderTileImageFeaturesEXT

instance FromCStruct PhysicalDeviceShaderTileImageFeaturesEXT


data PhysicalDeviceShaderTileImagePropertiesEXT

instance ToCStruct PhysicalDeviceShaderTileImagePropertiesEXT
instance Show PhysicalDeviceShaderTileImagePropertiesEXT

instance FromCStruct PhysicalDeviceShaderTileImagePropertiesEXT

