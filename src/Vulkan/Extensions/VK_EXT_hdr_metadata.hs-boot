{-# language CPP #-}
-- | = Name
--
-- VK_EXT_hdr_metadata - device extension
--
-- = VK_EXT_hdr_metadata
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
--     3
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--
-- [__Contact__]
--
--     -   Courtney Goeltzenleuchter
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_hdr_metadata] @courtney-g%0A*Here describe the issue or question you have about the VK_EXT_hdr_metadata extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-03-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Courtney Goeltzenleuchter, Google
--
--     -   Sebastian Wick, Red Hat Inc.
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension defines two new structures and a function to assign SMPTE
-- (the Society of Motion Picture and Television Engineers) 2086 metadata
-- and CTA (Consumer Technology Association) 861.3 metadata to a swapchain.
--
-- SMPTE 2086 metadata defines the color volume of the display on which the
-- content was optimized for viewing and includes the color primaries,
-- white point, and luminance range. When such content is reproduced on
-- another display, this metadata can be used by the presentation engine to
-- improve processing of images. For instance, values in the image can
-- first be clamped to the color volume described in the metadata, and then
-- what remains can be remapped to the color volume of the presentation
-- engine.
--
-- CTA 861.3 metadata additionally includes the maximum intended luminance
-- for the content and the maximum average light level across frames.
--
-- This extension does not define exactly how this metadata is used,
-- however, it simply provides a mechanism to provide it to the
-- presentation engine. Presentation engines may process the image based on
-- the metadata before displaying it, resulting in the image being modified
-- outside of Vulkan. For example, the clamping of colors in the image to
-- the color volume may change those values in the image itself.
--
-- The metadata does not override or otherwise influence the color space
-- and color encoding.
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
-- 1) Do we need a query function for the currently specified metadata?
--
-- No, Vulkan does not provide queries for state that the application can
-- track on its own.
--
-- 2) Should we specify default metadata if not specified by the
-- application?
--
-- No, the metadata is optional and the absence of the metadata is
-- well-defined.
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
-- -   Revision 3, 2024-03-26 (Tobias Hector & Sebastian Wick)
--
--     -   Clarifications and removal of erroneous “reference monitor” term
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_hdr_metadata Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_hdr_metadata  ( HdrMetadataEXT
                                              , XYColorEXT
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role HdrMetadataEXT nominal
data HdrMetadataEXT (es :: [Type])

instance ( Extendss HdrMetadataEXT es
         , PokeChain es ) => ToCStruct (HdrMetadataEXT es)
instance Show (Chain es) => Show (HdrMetadataEXT es)

instance ( Extendss HdrMetadataEXT es
         , PeekChain es ) => FromCStruct (HdrMetadataEXT es)


data XYColorEXT

instance ToCStruct XYColorEXT
instance Show XYColorEXT

instance FromCStruct XYColorEXT

