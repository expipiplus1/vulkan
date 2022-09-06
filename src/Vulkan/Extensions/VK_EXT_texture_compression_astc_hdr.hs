{-# language CPP #-}
-- | = Name
--
-- VK_EXT_texture_compression_astc_hdr - device extension
--
-- == VK_EXT_texture_compression_astc_hdr
--
-- [__Name String__]
--     @VK_EXT_texture_compression_astc_hdr@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     67
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_texture_compression_astc_hdr] @janharaldfredriksen-arm%0A<<Here describe the issue or question you have about the VK_EXT_texture_compression_astc_hdr extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-28
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
-- [__IP Status__]
--     No known issues.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- This extension adds support for textures compressed using the Adaptive
-- Scalable Texture Compression (ASTC) High Dynamic Range (HDR) profile.
--
-- When this extension is enabled, the HDR profile is supported for all
-- ASTC formats listed in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#appendix-compressedtex-astc ASTC Compressed Image Formats>.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME'
--
-- -   'EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT'
--
--     -   'FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- This extension has been partially promoted. Functionality in this
-- extension is included in core Vulkan 1.3, with the EXT suffix omitted.
-- However, the feature is made optional in Vulkan 1.3. The original type,
-- enum and command names are still available as aliases of the core
-- functionality.
--
-- == Issues
--
-- 1) Should we add a feature or limit for this functionality?
--
-- Yes. It is consistent with the ASTC LDR support to add a feature like
-- textureCompressionASTC_HDR.
--
-- The feature is strictly speaking redundant as long as this is just an
-- extension; it would be sufficient to just enable the extension. But
-- adding the feature is more forward-looking if wanted to make this an
-- optional core feature in the future.
--
-- 2) Should we introduce new format enums for HDR?
--
-- Yes. Vulkan 1.0 describes the ASTC format enums as UNORM, e.g.
-- 'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_UNORM_BLOCK', so it is
-- confusing to make these contain HDR data. Note that the OpenGL (ES)
-- extensions did not make this distinction because a single ASTC HDR
-- texture may contain both unorm and float blocks. Implementations /may/
-- not be able to distinguish between LDR and HDR ASTC textures internally
-- and just treat them as the same format, i.e. if this extension is
-- supported then sampling from a
-- 'Vulkan.Core10.Enums.Format.FORMAT_ASTC_4x4_UNORM_BLOCK' image format
-- /may/ return HDR results. Applications /can/ get predictable results by
-- using the appropriate image format.
--
-- == Version History
--
-- -   Revision 1, 2019-05-28 (Jan-Harald Fredriksen)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_texture_compression_astc_hdr Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT
                                                              , pattern FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT
                                                              , pattern FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT
                                                              , PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
                                                              , EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION
                                                              , pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION
                                                              , EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
                                                              , pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME
                                                              ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_EXT_texture_compression_astc_hdr (PhysicalDeviceTextureCompressionASTCHDRFeatures)
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_10x10_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_10x5_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_10x6_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_10x8_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_12x10_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_12x12_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_4x4_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_5x4_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_5x5_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_6x5_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_6x6_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_8x5_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_8x6_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.Format (Format(FORMAT_ASTC_8x8_SFLOAT_BLOCK))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES


-- No documentation found for TopLevel "VK_FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT = FORMAT_ASTC_4x4_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT = FORMAT_ASTC_5x4_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT = FORMAT_ASTC_5x5_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT = FORMAT_ASTC_6x5_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT = FORMAT_ASTC_6x6_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT = FORMAT_ASTC_8x5_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT = FORMAT_ASTC_8x6_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT = FORMAT_ASTC_8x8_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT = FORMAT_ASTC_10x5_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT = FORMAT_ASTC_10x6_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT = FORMAT_ASTC_10x8_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT = FORMAT_ASTC_10x10_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT = FORMAT_ASTC_12x10_SFLOAT_BLOCK


-- No documentation found for TopLevel "VK_FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT = FORMAT_ASTC_12x12_SFLOAT_BLOCK


-- No documentation found for TopLevel "VkPhysicalDeviceTextureCompressionASTCHDRFeaturesEXT"
type PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT = PhysicalDeviceTextureCompressionASTCHDRFeatures


type EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION"
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_SPEC_VERSION = 1


type EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME = "VK_EXT_texture_compression_astc_hdr"

-- No documentation found for TopLevel "VK_EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME"
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TEXTURE_COMPRESSION_ASTC_HDR_EXTENSION_NAME = "VK_EXT_texture_compression_astc_hdr"

