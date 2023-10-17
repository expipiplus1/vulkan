{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_sliced_view_of_3d - device extension
--
-- == VK_EXT_image_sliced_view_of_3d
--
-- [__Name String__]
--     @VK_EXT_image_sliced_view_of_3d@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     419
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_sliced_view_of_3d] @zmike%0A*Here describe the issue or question you have about the VK_EXT_image_sliced_view_of_3d extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_image_sliced_view_of_3d.adoc VK_EXT_image_sliced_view_of_3d>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-01-24
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Ricardo Garcia, Igalia
--
--     -   Shahbaz Youssefi, Google
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows creating 3D views of 3D images such that the views
-- contain a subset of the slices in the image, using a Z offset and range,
-- for the purpose of using the views as storage image descriptors. This
-- matches functionality in D3D12 and is primarily intended to support
-- D3D12 emulation.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'ImageViewSlicedCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageSlicedViewOf3DFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_SLICED_VIEW_OF_3D_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_SLICED_VIEW_OF_3D_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.REMAINING_3D_SLICES_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_SLICED_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_SLICED_VIEW_OF_3D_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-10-21 (Mike Blumenkrantz)
--
--     -   Initial revision
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.REMAINING_3D_SLICES_EXT',
-- 'ImageViewSlicedCreateInfoEXT',
-- 'PhysicalDeviceImageSlicedViewOf3DFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_sliced_view_of_3d Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_sliced_view_of_3d  ( ImageViewSlicedCreateInfoEXT
                                                         , PhysicalDeviceImageSlicedViewOf3DFeaturesEXT
                                                         ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImageViewSlicedCreateInfoEXT

instance ToCStruct ImageViewSlicedCreateInfoEXT
instance Show ImageViewSlicedCreateInfoEXT

instance FromCStruct ImageViewSlicedCreateInfoEXT


data PhysicalDeviceImageSlicedViewOf3DFeaturesEXT

instance ToCStruct PhysicalDeviceImageSlicedViewOf3DFeaturesEXT
instance Show PhysicalDeviceImageSlicedViewOf3DFeaturesEXT

instance FromCStruct PhysicalDeviceImageSlicedViewOf3DFeaturesEXT

