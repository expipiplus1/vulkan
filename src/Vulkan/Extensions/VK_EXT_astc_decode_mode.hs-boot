{-# language CPP #-}
-- | = Name
--
-- VK_EXT_astc_decode_mode - device extension
--
-- == VK_EXT_astc_decode_mode
--
-- [__Name String__]
--     @VK_EXT_astc_decode_mode@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     68
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_astc_decode_mode] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_astc_decode_mode extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-07
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- The existing specification requires that low dynamic range (LDR) ASTC
-- textures are decompressed to FP16 values per component. In many cases,
-- decompressing LDR textures to a lower precision intermediate result
-- gives acceptable image quality. Source material for LDR textures is
-- typically authored as 8-bit UNORM values, so decoding to FP16 values
-- adds little value. On the other hand, reducing precision of the decoded
-- result reduces the size of the decompressed data, potentially improving
-- texture cache performance and saving power.
--
-- The goal of this extension is to enable this efficiency gain on existing
-- ASTC texture data. This is achieved by giving the application the
-- ability to select the intermediate decoding precision.
--
-- Three decoding options are provided:
--
-- -   Decode to 'Vulkan.Core10.Enums.Format.FORMAT_R16G16B16A16_SFLOAT'
--     precision: This is the default, and matches the required behavior in
--     the core API.
--
-- -   Decode to 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM'
--     precision: This is provided as an option in LDR mode.
--
-- -   Decode to 'Vulkan.Core10.Enums.Format.FORMAT_E5B9G9R9_UFLOAT_PACK32'
--     precision: This is provided as an option in both LDR and HDR mode.
--     In this mode, negative values cannot be represented and are clamped
--     to zero. The alpha component is ignored, and the results are as if
--     alpha was 1.0. This decode mode is optional and support can be
--     queried via the physical device properties.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'ImageViewASTCDecodeModeEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceASTCDecodeFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ASTC_DECODE_MODE_EXTENSION_NAME'
--
-- -   'EXT_ASTC_DECODE_MODE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT'
--
-- == Issues
--
-- 1) Are implementations allowed to decode at a higher precision than what
-- is requested?
--
-- > RESOLUTION: No.
-- > If we allow this, then this extension could be exposed on all
-- > implementations that support ASTC.
-- > But developers would have no way of knowing what precision was actually
-- > used, and thus whether the image quality is sufficient at reduced
-- > precision.
--
-- 2) Should the decode mode be image view state and\/or sampler state?
--
-- > RESOLUTION: Image view state only.
-- > Some implementations treat the different decode modes as different
-- > texture formats.
--
-- == Example
--
-- Create an image view that decodes to
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8G8B8A8_UNORM' precision:
--
-- >     VkImageViewASTCDecodeModeEXT decodeMode =
-- >     {
-- >         VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT, // sType
-- >         NULL, // pNext
-- >         VK_FORMAT_R8G8B8A8_UNORM // decode mode
-- >     };
-- >
-- >     VkImageViewCreateInfo createInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO, // sType
-- >         &decodeMode, // pNext
-- >         // flags, image, viewType set to application-desired values
-- >         VK_FORMAT_ASTC_8x8_UNORM_BLOCK, // format
-- >         // components, subresourceRange set to application-desired values
-- >     };
-- >
-- >     VkImageView imageView;
-- >     VkResult result = vkCreateImageView(
-- >         device,
-- >         &createInfo,
-- >         NULL,
-- >         &imageView);
--
-- == Version History
--
-- -   Revision 1, 2018-08-07 (Jan-Harald Fredriksen)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ImageViewASTCDecodeModeEXT', 'PhysicalDeviceASTCDecodeFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_astc_decode_mode Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_astc_decode_mode  ( ImageViewASTCDecodeModeEXT
                                                  , PhysicalDeviceASTCDecodeFeaturesEXT
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImageViewASTCDecodeModeEXT

instance ToCStruct ImageViewASTCDecodeModeEXT
instance Show ImageViewASTCDecodeModeEXT

instance FromCStruct ImageViewASTCDecodeModeEXT


data PhysicalDeviceASTCDecodeFeaturesEXT

instance ToCStruct PhysicalDeviceASTCDecodeFeaturesEXT
instance Show PhysicalDeviceASTCDecodeFeaturesEXT

instance FromCStruct PhysicalDeviceASTCDecodeFeaturesEXT

