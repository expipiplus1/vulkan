{-# language CPP #-}
-- | = Name
--
-- VK_EXT_line_rasterization - device extension
--
-- = VK_EXT_line_rasterization
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
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_line_rasterization VK_KHR_line_rasterization>
--         extension
--
--         -   Which in turn was /promoted/ to
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse CAD support>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_line_rasterization] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_line_rasterization extension* >
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
-- == Promotion to @VK_KHR_line_rasterization@
--
-- All functionality in this extension is included in
-- @VK_KHR_line_rasterization@, with the suffix changed to KHR. The
-- original enum names are still available as aliases of the KHR
-- functionality.
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
--     'Vulkan.Core10.GraphicsPipeline.PipelineRasterizationStateCreateInfo':
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
--     -   'DYNAMIC_STATE_LINE_STIPPLE_EXT'
--
-- -   Extending
--     'Vulkan.Core14.Enums.LineRasterizationMode.LineRasterizationMode':
--
--     -   'LINE_RASTERIZATION_MODE_BRESENHAM_EXT'
--
--     -   'LINE_RASTERIZATION_MODE_DEFAULT_EXT'
--
--     -   'LINE_RASTERIZATION_MODE_RECTANGULAR_EXT'
--
--     -   'LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT'
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_line_rasterization Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_line_rasterization  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT
                                                    , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT
                                                    , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT
                                                    , pattern DYNAMIC_STATE_LINE_STIPPLE_EXT
                                                    , pattern LINE_RASTERIZATION_MODE_DEFAULT_EXT
                                                    , pattern LINE_RASTERIZATION_MODE_RECTANGULAR_EXT
                                                    , pattern LINE_RASTERIZATION_MODE_BRESENHAM_EXT
                                                    , pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT
                                                    , cmdSetLineStippleEXT
                                                    , LineRasterizationModeEXT
                                                    , PhysicalDeviceLineRasterizationFeaturesEXT
                                                    , PhysicalDeviceLineRasterizationPropertiesEXT
                                                    , PipelineRasterizationLineStateCreateInfoEXT
                                                    , EXT_LINE_RASTERIZATION_SPEC_VERSION
                                                    , pattern EXT_LINE_RASTERIZATION_SPEC_VERSION
                                                    , EXT_LINE_RASTERIZATION_EXTENSION_NAME
                                                    , pattern EXT_LINE_RASTERIZATION_EXTENSION_NAME
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
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT = STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES


-- No documentation found for TopLevel "VK_DYNAMIC_STATE_LINE_STIPPLE_EXT"
pattern DYNAMIC_STATE_LINE_STIPPLE_EXT = DYNAMIC_STATE_LINE_STIPPLE


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_DEFAULT_EXT"
pattern LINE_RASTERIZATION_MODE_DEFAULT_EXT = LINE_RASTERIZATION_MODE_DEFAULT


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_EXT"
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_EXT = LINE_RASTERIZATION_MODE_RECTANGULAR


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_BRESENHAM_EXT"
pattern LINE_RASTERIZATION_MODE_BRESENHAM_EXT = LINE_RASTERIZATION_MODE_BRESENHAM


-- No documentation found for TopLevel "VK_LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT"
pattern LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT = LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH


-- No documentation found for TopLevel "vkCmdSetLineStippleEXT"
cmdSetLineStippleEXT = cmdSetLineStipple


-- No documentation found for TopLevel "VkLineRasterizationModeEXT"
type LineRasterizationModeEXT = LineRasterizationMode


-- No documentation found for TopLevel "VkPhysicalDeviceLineRasterizationFeaturesEXT"
type PhysicalDeviceLineRasterizationFeaturesEXT = PhysicalDeviceLineRasterizationFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceLineRasterizationPropertiesEXT"
type PhysicalDeviceLineRasterizationPropertiesEXT = PhysicalDeviceLineRasterizationProperties


-- No documentation found for TopLevel "VkPipelineRasterizationLineStateCreateInfoEXT"
type PipelineRasterizationLineStateCreateInfoEXT = PipelineRasterizationLineStateCreateInfo


type EXT_LINE_RASTERIZATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_LINE_RASTERIZATION_SPEC_VERSION"
pattern EXT_LINE_RASTERIZATION_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_LINE_RASTERIZATION_SPEC_VERSION = 1


type EXT_LINE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_line_rasterization"

-- No documentation found for TopLevel "VK_EXT_LINE_RASTERIZATION_EXTENSION_NAME"
pattern EXT_LINE_RASTERIZATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_LINE_RASTERIZATION_EXTENSION_NAME = "VK_EXT_line_rasterization"

