{-# language CPP #-}
-- | = Name
--
-- VK_EXT_line_rasterization - device extension
--
-- == VK_EXT_line_rasterization
--
-- [__Name String__]
--     @VK_EXT_line_rasterization@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     260
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse CAD support>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_line_rasterization:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-09
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
--     -   Jason Ekstrand, Intel
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
-- -   'cmdSetLineStippleEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceLineRasterizationFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLineRasterizationPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationLineStateCreateInfoEXT'
--
-- == New Enums
--
-- -   'LineRasterizationModeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_LINE_RASTERIZATION_EXTENSION_NAME'
--
-- -   'EXT_LINE_RASTERIZATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT'
--
-- == Issues
--
-- > (1) Do we need to support Bresenham-style and smooth lines with more than
-- >     one rasterization sample? i.e. the equivalent of
-- >     glDisable(GL_MULTISAMPLE) in OpenGL when the framebuffer has more than
-- >     one sample?
--
-- > RESOLVED: Yes.
-- > For simplicity, Bresenham line rasterization carries forward a few
-- > restrictions from OpenGL, such as not supporting per-sample shading, alpha
-- > to coverage, or alpha to one.
--
-- == Version History
--
-- -   Revision 1, 2019-05-09 (Jeff Bolz)
--
--     -   Initial draft
--
-- = See Also
--
-- 'LineRasterizationModeEXT',
-- 'PhysicalDeviceLineRasterizationFeaturesEXT',
-- 'PhysicalDeviceLineRasterizationPropertiesEXT',
-- 'PipelineRasterizationLineStateCreateInfoEXT', 'cmdSetLineStippleEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_line_rasterization Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_line_rasterization  ( PhysicalDeviceLineRasterizationFeaturesEXT
                                                    , PhysicalDeviceLineRasterizationPropertiesEXT
                                                    , PipelineRasterizationLineStateCreateInfoEXT
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceLineRasterizationFeaturesEXT

instance ToCStruct PhysicalDeviceLineRasterizationFeaturesEXT
instance Show PhysicalDeviceLineRasterizationFeaturesEXT

instance FromCStruct PhysicalDeviceLineRasterizationFeaturesEXT


data PhysicalDeviceLineRasterizationPropertiesEXT

instance ToCStruct PhysicalDeviceLineRasterizationPropertiesEXT
instance Show PhysicalDeviceLineRasterizationPropertiesEXT

instance FromCStruct PhysicalDeviceLineRasterizationPropertiesEXT


data PipelineRasterizationLineStateCreateInfoEXT

instance ToCStruct PipelineRasterizationLineStateCreateInfoEXT
instance Show PipelineRasterizationLineStateCreateInfoEXT

instance FromCStruct PipelineRasterizationLineStateCreateInfoEXT

