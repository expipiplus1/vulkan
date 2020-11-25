{-# language CPP #-}
-- | = Name
--
-- VK_NV_shading_rate_image - device extension
--
-- = Registered Extension Number
--
-- 165
--
-- = Revision
--
-- 3
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-07-18
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         {spirv}\/NV\/SPV_NV_shading_rate.html[@SPV_NV_shading_rate@]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_shading_rate_image.txt GL_NV_shading_rate_image>
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Carsten Rohde, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Mathias Schott, NVIDIA
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension allows applications to use a variable shading rate when
-- processing fragments of rasterized primitives. By default, Vulkan will
-- spawn one fragment shader for each pixel covered by a primitive. In this
-- extension, applications can bind a /shading rate image/ that can be used
-- to vary the number of fragment shader invocations across the
-- framebuffer. Some portions of the screen may be configured to spawn up
-- to 16 fragment shaders for each pixel, while other portions may use a
-- single fragment shader invocation for a 4x4 block of pixels. This can be
-- useful for use cases like eye tracking, where the portion of the
-- framebuffer that the user is looking at directly can be processed at
-- high frequency, while distant corners of the image can be processed at
-- lower frequency. Each texel in the shading rate image represents a
-- fixed-size rectangle in the framebuffer, covering 16x16 pixels in the
-- initial implementation of this extension. When rasterizing a primitive
-- covering one of these rectangles, the Vulkan implementation reads a
-- texel in the bound shading rate image and looks up the fetched value in
-- a palette to determine a base shading rate.
--
-- In addition to the API support controlling rasterization, this extension
-- also adds Vulkan support for the
-- {spirv}\/NV\/SPV_NV_shading_rate.html[@SPV_NV_shading_rate@] extension
-- to SPIR-V. That extension provides two fragment shader variable
-- decorations that allow fragment shaders to determine the shading rate
-- used for processing the fragment:
--
-- -   @FragmentSizeNV@, which indicates the width and height of the set of
--     pixels processed by the fragment shader.
--
-- -   @InvocationsPerPixel@, which indicates the maximum number of
--     fragment shader invocations that could be spawned for the pixel(s)
--     covered by the fragment.
--
-- When using SPIR-V in conjunction with the OpenGL Shading Language
-- (GLSL), the fragment shader capabilities are provided by the
-- @GL_NV_shading_rate_image@ language extension and correspond to the
-- built-in variables @gl_FragmentSizeNV@ and @gl_InvocationsPerPixelNV@,
-- respectively.
--
-- == New Commands
--
-- -   'cmdBindShadingRateImageNV'
--
-- -   'cmdSetCoarseSampleOrderNV'
--
-- -   'cmdSetViewportShadingRatePaletteNV'
--
-- == New Structures
--
-- -   'CoarseSampleLocationNV'
--
-- -   'CoarseSampleOrderCustomNV'
--
-- -   'ShadingRatePaletteNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShadingRateImageFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShadingRateImagePropertiesNV'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo':
--
--     -   'PipelineViewportCoarseSampleOrderStateCreateInfoNV'
--
--     -   'PipelineViewportShadingRateImageStateCreateInfoNV'
--
-- == New Enums
--
-- -   'CoarseSampleOrderTypeNV'
--
-- -   'ShadingRatePaletteEntryNV'
--
-- == New Enum Constants
--
-- -   'NV_SHADING_RATE_IMAGE_EXTENSION_NAME'
--
-- -   'NV_SHADING_RATE_IMAGE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_COARSE_SAMPLE_ORDER_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- (1) When using shading rates specifying “coarse” fragments covering
-- multiple pixels, we will generate a combined coverage mask that combines
-- the coverage masks of all pixels covered by the fragment. By default,
-- these masks are combined in an implementation-dependent order. Should we
-- provide a mechanism allowing applications to query or specify an exact
-- order?
--
-- __RESOLVED__: Yes, this feature is useful for cases where most of the
-- fragment shader can be evaluated once for an entire coarse fragment, but
-- where some per-pixel computations are also required. For example, a
-- per-pixel alpha test may want to kill all the samples for some pixels in
-- a coarse fragment. This sort of test can be implemented using an output
-- sample mask, but such a shader would need to know which bit in the mask
-- corresponds to each sample in the coarse fragment. We are including a
-- mechanism to allow aplications to specify the orders of coverage samples
-- for each shading rate and sample count, either as static pipeline state
-- or dynamically via a command buffer. This portion of the extension has
-- its own feature bit.
--
-- We will not be providing a query to determine the
-- implementation-dependent default ordering. The thinking here is that if
-- an application cares enough about the coarse fragment sample ordering to
-- perform such a query, it could instead just set its own order, also
-- using custom per-pixel sample locations if required.
--
-- (2) For the pipeline stage
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV',
-- should we specify a precise location in the pipeline the shading rate
-- image is accessed (after geometry shading, but before the early fragment
-- tests) or leave it under-specified in case there are other
-- implementations that access the image in a different pipeline location?
--
-- __RESOLVED__ We are specifying the pipeline stage to be between the
-- final stage used for vertex processing
-- ('Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT')
-- and before the first stage used for fragment processing
-- ('Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'),
-- which seems to be the natural place to access the shading rate image.
--
-- (3) How do centroid-sampled variables work with fragments larger than
-- one pixel?
--
-- __RESOLVED__ For single-pixel fragments, fragment shader inputs
-- decorated with @Centroid@ are sampled at an implementation-dependent
-- location in the intersection of the area of the primitive being
-- rasterized and the area of the pixel that corresponds to the fragment.
-- With multi-pixel fragments, we follow a similar pattern, using the
-- intersection of the primitive and the __set__ of pixels corresponding to
-- the fragment.
--
-- One important thing to keep in mind when using such “coarse” shading
-- rates is that fragment attributes are sampled at the center of the
-- fragment by default, regardless of the set of pixels\/samples covered by
-- the fragment. For fragments with a size of 4x4 pixels, this center
-- location will be more than two pixels (1.5 * sqrt(2)) away from the
-- center of the pixels at the corners of the fragment. When rendering a
-- primitive that covers only a small part of a coarse fragment, sampling a
-- color outside the primitive can produce overly bright or dark color
-- values if the color values have a large gradient. To deal with this, an
-- application can use centroid sampling on attributes where
-- “extrapolation” artifacts can lead to overly bright or dark pixels. Note
-- that this same problem also exists for multisampling with single-pixel
-- fragments, but is less severe because it only affects certain samples of
-- a pixel and such bright\/dark samples may be averaged with other samples
-- that don’t have a similar problem.
--
-- == Version History
--
-- -   Revision 3, 2019-07-18 (Mathias Schott)
--
--     -   Fully list extension interfaces in this appendix.
--
-- -   Revision 2, 2018-09-13 (Pat Brown)
--
--     -   Miscellaneous edits preparing the specification for publication.
--
-- -   Revision 1, 2018-08-08 (Pat Brown)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'CoarseSampleLocationNV', 'CoarseSampleOrderCustomNV',
-- 'CoarseSampleOrderTypeNV', 'PhysicalDeviceShadingRateImageFeaturesNV',
-- 'PhysicalDeviceShadingRateImagePropertiesNV',
-- 'PipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'PipelineViewportShadingRateImageStateCreateInfoNV',
-- 'ShadingRatePaletteEntryNV', 'ShadingRatePaletteNV',
-- 'cmdBindShadingRateImageNV', 'cmdSetCoarseSampleOrderNV',
-- 'cmdSetViewportShadingRatePaletteNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shading_rate_image Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_shading_rate_image  ( CoarseSampleLocationNV
                                                   , CoarseSampleOrderCustomNV
                                                   , PhysicalDeviceShadingRateImageFeaturesNV
                                                   , PhysicalDeviceShadingRateImagePropertiesNV
                                                   , PipelineViewportCoarseSampleOrderStateCreateInfoNV
                                                   , PipelineViewportShadingRateImageStateCreateInfoNV
                                                   , ShadingRatePaletteNV
                                                   , CoarseSampleOrderTypeNV
                                                   ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data CoarseSampleLocationNV

instance ToCStruct CoarseSampleLocationNV
instance Show CoarseSampleLocationNV

instance FromCStruct CoarseSampleLocationNV


data CoarseSampleOrderCustomNV

instance ToCStruct CoarseSampleOrderCustomNV
instance Show CoarseSampleOrderCustomNV

instance FromCStruct CoarseSampleOrderCustomNV


data PhysicalDeviceShadingRateImageFeaturesNV

instance ToCStruct PhysicalDeviceShadingRateImageFeaturesNV
instance Show PhysicalDeviceShadingRateImageFeaturesNV

instance FromCStruct PhysicalDeviceShadingRateImageFeaturesNV


data PhysicalDeviceShadingRateImagePropertiesNV

instance ToCStruct PhysicalDeviceShadingRateImagePropertiesNV
instance Show PhysicalDeviceShadingRateImagePropertiesNV

instance FromCStruct PhysicalDeviceShadingRateImagePropertiesNV


data PipelineViewportCoarseSampleOrderStateCreateInfoNV

instance ToCStruct PipelineViewportCoarseSampleOrderStateCreateInfoNV
instance Show PipelineViewportCoarseSampleOrderStateCreateInfoNV

instance FromCStruct PipelineViewportCoarseSampleOrderStateCreateInfoNV


data PipelineViewportShadingRateImageStateCreateInfoNV

instance ToCStruct PipelineViewportShadingRateImageStateCreateInfoNV
instance Show PipelineViewportShadingRateImageStateCreateInfoNV

instance FromCStruct PipelineViewportShadingRateImageStateCreateInfoNV


data ShadingRatePaletteNV

instance ToCStruct ShadingRatePaletteNV
instance Show ShadingRatePaletteNV

instance FromCStruct ShadingRatePaletteNV


data CoarseSampleOrderTypeNV

