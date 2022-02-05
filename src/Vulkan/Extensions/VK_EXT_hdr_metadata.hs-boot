{-# language CPP #-}
-- | = Name
--
-- VK_EXT_hdr_metadata - device extension
--
-- == VK_EXT_hdr_metadata
--
-- [__Name String__]
--     @VK_EXT_hdr_metadata@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     106
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@
--
-- [__Contact__]
--
--     -   Courtney Goeltzenleuchter
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_hdr_metadata] @courtney-g%0A<<Here describe the issue or question you have about the VK_EXT_hdr_metadata extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Courtney Goeltzenleuchter, Google
--
-- == Description
--
-- This extension defines two new structures and a function to assign SMPTE
-- (the Society of Motion Picture and Television Engineers) 2086 metadata
-- and CTA (Consumer Technology Association) 861.3 metadata to a swapchain.
-- The metadata includes the color primaries, white point, and luminance
-- range of the reference monitor, which all together define the color
-- volume containing all the possible colors the reference monitor can
-- produce. The reference monitor is the display where creative work is
-- done and creative intent is established. To preserve such creative
-- intent as much as possible and achieve consistent color reproduction on
-- different viewing displays, it is useful for the display pipeline to
-- know the color volume of the original reference monitor where content
-- was created or tuned. This avoids performing unnecessary mapping of
-- colors that are not displayable on the original reference monitor. The
-- metadata also includes the @maxContentLightLevel@ and
-- @maxFrameAverageLightLevel@ as defined by CTA 861.3.
--
-- While the general purpose of the metadata is to assist in the
-- transformation between different color volumes of different displays and
-- help achieve better color reproduction, it is not in the scope of this
-- extension to define how exactly the metadata should be used in such a
-- process. It is up to the implementation to determine how to make use of
-- the metadata.
--
-- == New Commands
--
-- -   'setHdrMetadataEXT'
--
-- == New Structures
--
-- -   'HdrMetadataEXT'
--
-- -   'XYColorEXT'
--
-- == New Enum Constants
--
-- -   'EXT_HDR_METADATA_EXTENSION_NAME'
--
-- -   'EXT_HDR_METADATA_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HDR_METADATA_EXT'
--
-- == Issues
--
-- 1) Do we need a query function?
--
-- __PROPOSED__: No, Vulkan does not provide queries for state that the
-- application can track on its own.
--
-- 2) Should we specify default if not specified by the application?
--
-- __PROPOSED__: No, that leaves the default up to the display.
--
-- == Version History
--
-- -   Revision 1, 2016-12-27 (Courtney Goeltzenleuchter)
--
--     -   Initial version
--
-- -   Revision 2, 2018-12-19 (Courtney Goeltzenleuchter)
--
--     -   Correct implicit validity for VkHdrMetadataEXT structure
--
-- == See Also
--
-- 'HdrMetadataEXT', 'XYColorEXT', 'setHdrMetadataEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_hdr_metadata Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_hdr_metadata  ( HdrMetadataEXT
                                              , XYColorEXT
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data HdrMetadataEXT

instance ToCStruct HdrMetadataEXT
instance Show HdrMetadataEXT

instance FromCStruct HdrMetadataEXT


data XYColorEXT

instance ToCStruct XYColorEXT
instance Show XYColorEXT

instance FromCStruct XYColorEXT

