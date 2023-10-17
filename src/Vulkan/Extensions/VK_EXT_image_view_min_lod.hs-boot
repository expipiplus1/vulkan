{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_view_min_lod - device extension
--
-- == VK_EXT_image_view_min_lod
--
-- [__Name String__]
--     @VK_EXT_image_view_min_lod@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     392
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_view_min_lod] @Joshua-Ashton%0A*Here describe the issue or question you have about the VK_EXT_image_view_min_lod extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-11-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Samuel Iglesias Gonsalvez, Igalia
--
--     -   Tobias Hector, AMD
--
--     -   Faith Ekstrand, Intel
--
--     -   Tom Olson, ARM
--
-- == Description
--
-- This extension allows applications to clamp the minimum LOD value during
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-image-level-selection Image Level(s) Selection>,
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-gather Texel Gathering>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-integer-coordinate-operations Integer Texel Coordinate Operations>
-- with a given 'Vulkan.Core10.Handles.ImageView' by
-- 'ImageViewMinLodCreateInfoEXT'::@minLod@.
--
-- This extension may be useful to restrict a
-- 'Vulkan.Core10.Handles.ImageView' to only mips which have been uploaded,
-- and the use of fractional @minLod@ can be useful for smoothly
-- introducing new mip levels when using linear mipmap filtering.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'ImageViewMinLodCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageViewMinLodFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_VIEW_MIN_LOD_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_VIEW_MIN_LOD_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_MIN_LOD_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-07-06 (Joshua Ashton)
--
--     -   Initial version
--
-- == See Also
--
-- 'ImageViewMinLodCreateInfoEXT',
-- 'PhysicalDeviceImageViewMinLodFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_view_min_lod Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_view_min_lod  ( ImageViewMinLodCreateInfoEXT
                                                    , PhysicalDeviceImageViewMinLodFeaturesEXT
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImageViewMinLodCreateInfoEXT

instance ToCStruct ImageViewMinLodCreateInfoEXT
instance Show ImageViewMinLodCreateInfoEXT

instance FromCStruct ImageViewMinLodCreateInfoEXT


data PhysicalDeviceImageViewMinLodFeaturesEXT

instance ToCStruct PhysicalDeviceImageViewMinLodFeaturesEXT
instance Show PhysicalDeviceImageViewMinLodFeaturesEXT

instance FromCStruct PhysicalDeviceImageViewMinLodFeaturesEXT

