{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_2d_view_of_3d - device extension
--
-- == VK_EXT_image_2d_view_of_3d
--
-- [__Name String__]
--     @VK_EXT_image_2d_view_of_3d@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     394
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_maintenance1@ to be enabled for any
--         device-level functionality
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_2d_view_of_3d] @zmike%0A*Here describe the issue or question you have about the VK_EXT_image_2d_view_of_3d extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-02-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Spencer Fricke, Samsung
--
--     -   Ricardo Garcia, Igalia
--
--     -   Graeme Leese, Broadcom
--
--     -   Ralph Potter, Samsung
--
--     -   Stu Smith, AMD
--
--     -   Shahbaz Youssefi, Google
--
--     -   Alex Walters, Imagination
--
-- == Description
--
-- This extension allows a single slice of a 3D image to be used as a 2D
-- view in image descriptors, matching both the functionality of
-- glBindImageTexture in OpenGL with the @layer@ parameter set to true and
-- 2D view binding provided by the extension EGL_KHR_gl_texture_3D_image.
-- It is primarily intended to support GL emulation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImage2DViewOf3DFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_2D_VIEW_OF_3D_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_2D_VIEW_OF_3D_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_2D_VIEW_OF_3D_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-03-25 (Mike Blumenkrantz)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceImage2DViewOf3DFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_2d_view_of_3d Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_2d_view_of_3d  (PhysicalDeviceImage2DViewOf3DFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceImage2DViewOf3DFeaturesEXT

instance ToCStruct PhysicalDeviceImage2DViewOf3DFeaturesEXT
instance Show PhysicalDeviceImage2DViewOf3DFeaturesEXT

instance FromCStruct PhysicalDeviceImage2DViewOf3DFeaturesEXT

