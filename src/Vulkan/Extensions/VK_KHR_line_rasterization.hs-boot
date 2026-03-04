{-# language CPP #-}
-- | = Name
--
-- VK_KHR_line_rasterization - device extension
--
-- == VK_KHR_line_rasterization
--
-- [__Name String__]
--     @VK_KHR_line_rasterization@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     535
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_line_rasterization] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_KHR_line_rasterization extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-06-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Allen Jensen, NVIDIA
--
--     -   Faith Ekstrand, Intel
--
-- == Description
--
-- This extension adds some line rasterization features that are commonly
-- used in CAD applications and supported in other APIs like OpenGL.
-- Bresenham-style line rasterization is supported, smooth rectangular
-- lines (coverage to alpha) are supported, and stippled lines are
-- supported for all three line rasterization modes.
--
-- == New Commands
--
-- -   'cmdSetLineStippleKHR'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceLineRasterizationFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLineRasterizationPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationLineStateCreateInfoKHR'
--
-- == New Enums
--
-- -   'LineRasterizationModeKHR'
--
-- == New Enum Constants
--
-- -   'KHR_LINE_RASTERIZATION_EXTENSION_NAME'
--
-- -   'KHR_LINE_RASTERIZATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Do we need to support Bresenham-style and smooth lines with more than
-- one rasterization sample? i.e. the equivalent of
-- glDisable(GL_MULTISAMPLE) in OpenGL when the framebuffer has more than
-- one sample?
--
-- __RESOLVED__: Yes. For simplicity, Bresenham line rasterization carries
-- forward a few restrictions from OpenGL, such as not supporting
-- per-sample shading, alpha to coverage, or alpha to one.
--
-- == Version History
--
-- -   Revision 1, 2019-05-09 (Jeff Bolz)
--
--     -   Initial draft
--
-- == See Also
--
-- 'LineRasterizationModeKHR',
-- 'PhysicalDeviceLineRasterizationFeaturesKHR',
-- 'PhysicalDeviceLineRasterizationPropertiesKHR',
-- 'PipelineRasterizationLineStateCreateInfoKHR', 'cmdSetLineStippleKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_line_rasterization Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_line_rasterization  ( PhysicalDeviceLineRasterizationFeaturesKHR
                                                    , PhysicalDeviceLineRasterizationPropertiesKHR
                                                    , PipelineRasterizationLineStateCreateInfoKHR
                                                    , LineRasterizationModeKHR
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceLineRasterizationFeaturesKHR

instance ToCStruct PhysicalDeviceLineRasterizationFeaturesKHR
instance Show PhysicalDeviceLineRasterizationFeaturesKHR

instance FromCStruct PhysicalDeviceLineRasterizationFeaturesKHR


data PhysicalDeviceLineRasterizationPropertiesKHR

instance ToCStruct PhysicalDeviceLineRasterizationPropertiesKHR
instance Show PhysicalDeviceLineRasterizationPropertiesKHR

instance FromCStruct PhysicalDeviceLineRasterizationPropertiesKHR


data PipelineRasterizationLineStateCreateInfoKHR

instance ToCStruct PipelineRasterizationLineStateCreateInfoKHR
instance Show PipelineRasterizationLineStateCreateInfoKHR

instance FromCStruct PipelineRasterizationLineStateCreateInfoKHR


data LineRasterizationModeKHR

