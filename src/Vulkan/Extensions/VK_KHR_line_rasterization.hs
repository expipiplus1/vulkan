{-# language CPP #-}
-- | = Name
--
-- VK_KHR_line_rasterization - device extension
--
-- = VK_KHR_line_rasterization
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
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
--     'Vulkan.Core10.GraphicsPipeline.PipelineRasterizationStateCreateInfo':
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
--     -   'DYNAMIC_STATE_LINE_STIPPLE_KHR'
--
-- -   Extending
--     'Vulkan.Core14.Enums.LineRasterizationMode.LineRasterizationMode':
--
--     -   'LINE_RASTERIZATION_MODE_BRESENHAM_KHR'
--
--     -   'LINE_RASTERIZATION_MODE_DEFAULT_KHR'
--
--     -   'LINE_RASTERIZATION_MODE_RECTANGULAR_KHR'
--
--     -   'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- KHR suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
--
-- When
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#versions-1.4 Version 1.4>
-- is supported, the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-bresenhamLines bresenhamLines>
-- feature must be supported.
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
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_line_rasterization Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_line_rasterization  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_KHR
                                                    , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR
                                                    , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_KHR
                                                    , pattern DYNAMIC_STATE_LINE_STIPPLE_KHR
                                                    , pattern LINE_RASTERIZATION_MODE_DEFAULT_KHR
                                                    , pattern LINE_RASTERIZATION_MODE_RECTANGULAR_KHR
                                                    , pattern LINE_RASTERIZATION_MODE_BRESENHAM_KHR
                                                    , pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR
                                                    , cmdSetLineStippleKHR
                                                    , LineRasterizationModeKHR
                                                    , PhysicalDeviceLineRasterizationFeaturesKHR
                                                    , PhysicalDeviceLineRasterizationPropertiesKHR
                                                    , PipelineRasterizationLineStateCreateInfoKHR
                                                    , KHR_LINE_RASTERIZATION_SPEC_VERSION
                                                    , pattern KHR_LINE_RASTERIZATION_SPEC_VERSION
                                                    , KHR_LINE_RASTERIZATION_EXTENSION_NAME
                                                    , pattern KHR_LINE_RASTERIZATION_EXTENSION_NAME
                                                    ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap (cmdSetLineStipple)
import Vulkan.Core14.Enums.LineRasterizationMode (LineRasterizationMode)
import Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap (PhysicalDeviceLineRasterizationFeatures)
import Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap (PhysicalDeviceLineRasterizationProperties)
import Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap (PipelineRasterizationLineStateCreateInfo)
import Vulkan.Core10.Enums.DynamicState (DynamicState(DYNAMIC_STATE_LINE_STIPPLE))
import Vulkan.Core14.Enums.LineRasterizationMode (LineRasterizationMode(LINE_RASTERIZATION_MODE_BRESENHAM))
import Vulkan.Core14.Enums.LineRasterizationMode (LineRasterizationMode(LINE_RASTERIZATION_MODE_DEFAULT))
import Vulkan.Core14.Enums.LineRasterizationMode (LineRasterizationMode(LINE_RASTERIZATION_MODE_RECTANGULAR))
import Vulkan.Core14.Enums.LineRasterizationMode (LineRasterizationMode(LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_KHR = STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_LINE_STIPPLE_KHR"
pattern DYNAMIC_STATE_LINE_STIPPLE_KHR = DYNAMIC_STATE_LINE_STIPPLE


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_DEFAULT_KHR"
pattern LINE_RASTERIZATION_MODE_DEFAULT_KHR = LINE_RASTERIZATION_MODE_DEFAULT


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_KHR"
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_KHR = LINE_RASTERIZATION_MODE_RECTANGULAR


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_BRESENHAM_KHR"
pattern LINE_RASTERIZATION_MODE_BRESENHAM_KHR = LINE_RASTERIZATION_MODE_BRESENHAM


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR"
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_KHR = LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH


-- No documentation found for TopLevel "vkCmdSetLineStippleKHR"
cmdSetLineStippleKHR = cmdSetLineStipple


-- No documentation found for TopLevel "VkLineRasterizationModeKHR"
type LineRasterizationModeKHR = LineRasterizationMode


-- No documentation found for TopLevel "VkPhysicalDeviceLineRasterizationFeaturesKHR"
type PhysicalDeviceLineRasterizationFeaturesKHR = PhysicalDeviceLineRasterizationFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceLineRasterizationPropertiesKHR"
type PhysicalDeviceLineRasterizationPropertiesKHR = PhysicalDeviceLineRasterizationProperties


-- No documentation found for TopLevel "VkPipelineRasterizationLineStateCreateInfoKHR"
type PipelineRasterizationLineStateCreateInfoKHR = PipelineRasterizationLineStateCreateInfo


type KHR_LINE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_LINE_RASTERIZATION_SPEC_VERSION"
pattern KHR_LINE_RASTERIZATION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_LINE_RASTERIZATION_SPEC_VERSION = 1


type KHR_LINE_RASTERIZATION_EXTENSION_NAME = "VK_KHR_line_rasterization"

-- No documentation found for TopLevel "VK_KHR_LINE_RASTERIZATION_EXTENSION_NAME"
pattern KHR_LINE_RASTERIZATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_LINE_RASTERIZATION_EXTENSION_NAME = "VK_KHR_line_rasterization"

