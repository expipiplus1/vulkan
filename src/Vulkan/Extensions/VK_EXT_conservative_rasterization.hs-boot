{-# language CPP #-}
-- | = Name
--
-- VK_EXT_conservative_rasterization - device extension
--
-- == VK_EXT_conservative_rasterization
--
-- [__Name String__]
--     @VK_EXT_conservative_rasterization@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     102
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
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_conservative_rasterization:%20&body=@pdaniell-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-06-09
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_fragment_fully_covered.html SPV_EXT_fragment_fully_covered>
--         if the
--         'PhysicalDeviceConservativeRasterizationPropertiesEXT'::@fullyCoveredFragmentShaderInputVariable@
--         feature is used.
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_post_depth_coverage.html SPV_KHR_post_depth_coverage>if
--         the
--         'PhysicalDeviceConservativeRasterizationPropertiesEXT'::@conservativeRasterizationPostDepthCoverage@
--         feature is used.
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/NV/NV_conservative_raster_underestimation.txt GL_NV_conservative_raster_underestimation>
--         if the
--         'PhysicalDeviceConservativeRasterizationPropertiesEXT'::@fullyCoveredFragmentShaderInputVariable@
--         feature is used.
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Slawomir Grajewski, Intel
--
--     -   Stu Smith, Imagination Technologies
--
-- == Description
--
-- This extension adds a new rasterization mode called conservative
-- rasterization. There are two modes of conservative rasterization;
-- overestimation and underestimation.
--
-- When overestimation is enabled, if any part of the primitive, including
-- its edges, covers any part of the rectangular pixel area, including its
-- sides, then a fragment is generated with all coverage samples turned on.
-- This extension allows for some variation in implementations by
-- accounting for differences in overestimation, where the generating
-- primitive size is increased at each of its edges by some sub-pixel
-- amount to further increase conservative pixel coverage. Implementations
-- can allow the application to specify an extra overestimation beyond the
-- base overestimation the implementation already does. It also allows
-- implementations to either cull degenerate primitives or rasterize them.
--
-- When underestimation is enabled, fragments are only generated if the
-- rectangular pixel area is fully covered by the generating primitive. If
-- supported by the implementation, when a pixel rectangle is fully covered
-- the fragment shader input variable builtin called FullyCoveredEXT is set
-- to true. The shader variable works in either overestimation or
-- underestimation mode.
--
-- Implementations can process degenerate triangles and lines by either
-- discarding them or generating conservative fragments for them.
-- Degenerate triangles are those that end up with zero area after the
-- rasterizer quantizes them to the fixed-point pixel grid. Degenerate
-- lines are those with zero length after quantization.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceConservativeRasterizationPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationConservativeStateCreateInfoEXT'
--
-- == New Enums
--
-- -   'ConservativeRasterizationModeEXT'
--
-- == New Bitmasks
--
-- -   'PipelineRasterizationConservativeStateCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CONSERVATIVE_RASTERIZATION_EXTENSION_NAME'
--
-- -   'EXT_CONSERVATIVE_RASTERIZATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1.1, 2020-09-06 (Piers Daniell)
--
--     -   Add missing SPIR-V and GLSL dependencies.
--
-- -   Revision 1, 2017-08-28 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'ConservativeRasterizationModeEXT',
-- 'PhysicalDeviceConservativeRasterizationPropertiesEXT',
-- 'PipelineRasterizationConservativeStateCreateFlagsEXT',
-- 'PipelineRasterizationConservativeStateCreateInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conservative_rasterization Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_conservative_rasterization  ( PhysicalDeviceConservativeRasterizationPropertiesEXT
                                                            , PipelineRasterizationConservativeStateCreateInfoEXT
                                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceConservativeRasterizationPropertiesEXT

instance ToCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT
instance Show PhysicalDeviceConservativeRasterizationPropertiesEXT

instance FromCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT


data PipelineRasterizationConservativeStateCreateInfoEXT

instance ToCStruct PipelineRasterizationConservativeStateCreateInfoEXT
instance Show PipelineRasterizationConservativeStateCreateInfoEXT

instance FromCStruct PipelineRasterizationConservativeStateCreateInfoEXT

