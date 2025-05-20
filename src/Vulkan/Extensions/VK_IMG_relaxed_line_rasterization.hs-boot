{-# language CPP #-}
-- | = Name
--
-- VK_IMG_relaxed_line_rasterization - device extension
--
-- == VK_IMG_relaxed_line_rasterization
--
-- [__Name String__]
--     @VK_IMG_relaxed_line_rasterization@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     111
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   James Fitzpatrick
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_IMG_relaxed_line_rasterization] @jamesfitzpatrick%0A*Here describe the issue or question you have about the VK_IMG_relaxed_line_rasterization extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-10-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Fitzpatrick, Imagination
--
--     -   Andrew Garrard, Imagination
--
--     -   Alex Walters, Imagination
--
-- == Description
--
-- OpenGL specifies that implementations should rasterize lines using the
-- diamond exit rule (a slightly modified version of Bresenham’s
-- algorithm). To implement OpenGL some implementations have a device-level
-- compatibility mode to rasterize lines according to the OpenGL
-- specification.
--
-- This extension allows OpenGL emulation layers to enable the OpenGL
-- compatible line rasterization mode of such implementations.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRelaxedLineRasterizationFeaturesIMG'
--
-- == New Enum Constants
--
-- -   'IMG_RELAXED_LINE_RASTERIZATION_EXTENSION_NAME'
--
-- -   'IMG_RELAXED_LINE_RASTERIZATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RELAXED_LINE_RASTERIZATION_FEATURES_IMG'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-10-22 (James Fitzpatrick)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceRelaxedLineRasterizationFeaturesIMG'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_IMG_relaxed_line_rasterization Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_IMG_relaxed_line_rasterization  (PhysicalDeviceRelaxedLineRasterizationFeaturesIMG) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRelaxedLineRasterizationFeaturesIMG

instance ToCStruct PhysicalDeviceRelaxedLineRasterizationFeaturesIMG
instance Show PhysicalDeviceRelaxedLineRasterizationFeaturesIMG

instance FromCStruct PhysicalDeviceRelaxedLineRasterizationFeaturesIMG

