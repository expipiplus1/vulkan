{-# language CPP #-}
-- | = Name
--
-- VK_EXT_blend_operation_advanced - device extension
--
-- = Registered Extension Number
--
-- 149
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-06-12
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds a number of “advanced” blending operations that
-- /can/ be used to perform new color blending operations, many of which
-- are more complex than the standard blend modes provided by unextended
-- Vulkan. This extension requires different styles of usage, depending on
-- the level of hardware support and the enabled features:
--
-- -   If
--     'PhysicalDeviceBlendOperationAdvancedFeaturesEXT'::@advancedBlendCoherentOperations@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', the new blending
--     operations are supported, but a memory dependency /must/ separate
--     each advanced blend operation on a given sample.
--     'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT'
--     is used to synchronize reads using advanced blend operations.
--
-- -   If
--     'PhysicalDeviceBlendOperationAdvancedFeaturesEXT'::@advancedBlendCoherentOperations@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', advanced blend operations
--     obey primitive order just like basic blend operations.
--
-- In unextended Vulkan, the set of blending operations is limited, and
-- /can/ be expressed very simply. The
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MIN' and
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MAX' blend operations simply
-- compute component-wise minimums or maximums of source and destination
-- color components. The 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_ADD',
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SUBTRACT', and
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_REVERSE_SUBTRACT' modes multiply
-- the source and destination colors by source and destination factors and
-- either add the two products together or subtract one from the other.
-- This limited set of operations supports many common blending operations
-- but precludes the use of more sophisticated transparency and blending
-- operations commonly available in many dedicated imaging APIs.
--
-- This extension provides a number of new “advanced” blending operations.
-- Unlike traditional blending operations using
-- 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_ADD', these blending equations do
-- not use source and destination factors specified by
-- 'Vulkan.Core10.Enums.BlendFactor.BlendFactor'. Instead, each blend
-- operation specifies a complete equation based on the source and
-- destination colors. These new blend operations are used for both RGB and
-- alpha components; they /must/ not be used to perform separate RGB and
-- alpha blending (via different values of color and alpha
-- 'Vulkan.Core10.Enums.BlendOp.BlendOp').
--
-- These blending operations are performed using premultiplied colors,
-- where RGB colors /can/ be considered premultiplied or non-premultiplied
-- by alpha, according to the @srcPremultiplied@ and @dstPremultiplied@
-- members of 'PipelineColorBlendAdvancedStateCreateInfoEXT'. If a color is
-- considered non-premultiplied, the (R,G,B) color components are
-- multiplied by the alpha component prior to blending. For
-- non-premultiplied color components in the range [0,1], the corresponding
-- premultiplied color component would have values in the range [0 × A, 1 ×
-- A].
--
-- Many of these advanced blending equations are formulated where the
-- result of blending source and destination colors with partial coverage
-- have three separate contributions: from the portions covered by both the
-- source and the destination, from the portion covered only by the source,
-- and from the portion covered only by the destination. The blend
-- parameter 'PipelineColorBlendAdvancedStateCreateInfoEXT'::@blendOverlap@
-- /can/ be used to specify a correlation between source and destination
-- pixel coverage. If set to 'BLEND_OVERLAP_CONJOINT_EXT', the source and
-- destination are considered to have maximal overlap, as would be the case
-- if drawing two objects on top of each other. If set to
-- 'BLEND_OVERLAP_DISJOINT_EXT', the source and destination are considered
-- to have minimal overlap, as would be the case when rendering a complex
-- polygon tessellated into individual non-intersecting triangles. If set
-- to 'BLEND_OVERLAP_UNCORRELATED_EXT', the source and destination coverage
-- are assumed to have no spatial correlation within the pixel.
--
-- In addition to the coherency issues on implementations not supporting
-- @advancedBlendCoherentOperations@, this extension has several
-- limitations worth noting. First, the new blend operations have a limit
-- on the number of color attachments they /can/ be used with, as indicated
-- by
-- 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendMaxColorAttachments@.
-- Additionally, blending precision /may/ be limited to 16-bit
-- floating-point, which /may/ result in a loss of precision and dynamic
-- range for framebuffer formats with 32-bit floating-point components, and
-- in a loss of precision for formats with 12- and 16-bit signed or
-- unsigned normalized integer components.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceBlendOperationAdvancedFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceBlendOperationAdvancedPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo':
--
--     -   'PipelineColorBlendAdvancedStateCreateInfoEXT'
--
-- == New Enums
--
-- -   'BlendOverlapEXT'
--
-- == New Enum Constants
--
-- -   'EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME'
--
-- -   'EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.BlendOp.BlendOp':
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_BLUE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_COLORBURN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_COLORDODGE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_CONTRAST_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DARKEN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DIFFERENCE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_ATOP_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_IN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_OUT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_OVER_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_EXCLUSION_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_GREEN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HARDLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HARDMIX_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_COLOR_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_HUE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_LUMINOSITY_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_SATURATION_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_OVG_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_RGB_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LIGHTEN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARBURN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARDODGE_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MINUS_CLAMPED_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MINUS_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MULTIPLY_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_OVERLAY_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PINLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_CLAMPED_ALPHA_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_CLAMPED_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_DARKER_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_RED_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SCREEN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SOFTLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_ATOP_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_IN_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_OUT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_OVER_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_VIVIDLIGHT_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_XOR_EXT'
--
--     -   'Vulkan.Core10.Enums.BlendOp.BLEND_OP_ZERO_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2017-06-12 (Jeff Bolz)
--
--     -   Internal revisions
--
-- -   Revision 2, 2017-06-12 (Jeff Bolz)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'BlendOverlapEXT', 'PhysicalDeviceBlendOperationAdvancedFeaturesEXT',
-- 'PhysicalDeviceBlendOperationAdvancedPropertiesEXT',
-- 'PipelineColorBlendAdvancedStateCreateInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_blend_operation_advanced Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_blend_operation_advanced  ( PhysicalDeviceBlendOperationAdvancedFeaturesEXT
                                                          , PhysicalDeviceBlendOperationAdvancedPropertiesEXT
                                                          , PipelineColorBlendAdvancedStateCreateInfoEXT
                                                          ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceBlendOperationAdvancedFeaturesEXT

instance ToCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT
instance Show PhysicalDeviceBlendOperationAdvancedFeaturesEXT

instance FromCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT


data PhysicalDeviceBlendOperationAdvancedPropertiesEXT

instance ToCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT
instance Show PhysicalDeviceBlendOperationAdvancedPropertiesEXT

instance FromCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT


data PipelineColorBlendAdvancedStateCreateInfoEXT

instance ToCStruct PipelineColorBlendAdvancedStateCreateInfoEXT
instance Show PipelineColorBlendAdvancedStateCreateInfoEXT

instance FromCStruct PipelineColorBlendAdvancedStateCreateInfoEXT

