{-# language CPP #-}
-- | = Name
--
-- VK_EXT_custom_border_color - device extension
--
-- == VK_EXT_custom_border_color
--
-- [__Name String__]
--     @VK_EXT_custom_border_color@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     288
--
-- [__Revision__]
--     12
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Special Uses__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Liam Middlebrook
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_custom_border_color] @liam-middlebrook%0A*Here describe the issue or question you have about the VK_EXT_custom_border_color extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-04-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Philip Rebohle, Valve
--
--     -   Liam Middlebrook, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Tobias Hector, AMD
--
--     -   Jason Ekstrand, Intel
--
--     -   Spencer Fricke, Samsung Electronics
--
--     -   Graeme Leese, Broadcom
--
--     -   Jesse Hall, Google
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Tom Olson, ARM
--
--     -   Stuart Smith, Imagination Technologies
--
--     -   Donald Scorgie, Imagination Technologies
--
--     -   Alex Walters, Imagination Technologies
--
--     -   Peter Quayle, Imagination Technologies
--
-- == Description
--
-- This extension provides cross-vendor functionality to specify a custom
-- border color for use when the sampler address mode
-- 'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER'
-- is used.
--
-- To create a sampler which uses a custom border color set
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'::@borderColor@ to one of:
--
-- -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT'
--
-- -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- When 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT' or
-- 'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT' is used,
-- applications must provide a 'SamplerCustomBorderColorCreateInfoEXT' in
-- the @pNext@ chain for 'Vulkan.Core10.Sampler.SamplerCreateInfo'.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCustomBorderColorFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCustomBorderColorPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerCustomBorderColorCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CUSTOM_BORDER_COLOR_EXTENSION_NAME'
--
-- -   'EXT_CUSTOM_BORDER_COLOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.BorderColor.BorderColor':
--
--     -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_FLOAT_CUSTOM_EXT'
--
--     -   'Vulkan.Core10.Enums.BorderColor.BORDER_COLOR_INT_CUSTOM_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) Should VkClearColorValue be used for the border color value, or
-- should we have our own struct\/union? Do we need to specify the type of
-- the input values for the components? This is more of a concern if
-- VkClearColorValue is used here because it provides a union of
-- float,int,uint types.
--
-- __RESOLVED__: Will reuse existing VkClearColorValue structure in order
-- to easily take advantage of float,int,uint borderColor types.
--
-- 2) For hardware which supports a limited number of border colors what
-- happens if that number is exceeded? Should this be handled by the driver
-- unbeknownst to the application? In Revision 1 we had solved this issue
-- using a new Object type, however that may have lead to additional system
-- resource consumption which would otherwise not be required.
--
-- __RESOLVED__: Added
-- 'PhysicalDeviceCustomBorderColorPropertiesEXT'::@maxCustomBorderColorSamplers@
-- for tracking implementation-specific limit, and Valid Usage statement
-- handling overflow.
--
-- 3) Should this be supported for immutable samplers at all, or by a
-- feature bit? Some implementations may not be able to support custom
-- border colors on immutable samplers — is it worthwhile enabling this to
-- work on them for implementations that can support it, or forbidding it
-- entirely.
--
-- __RESOLVED__: Samplers created with a custom border color are forbidden
-- from being immutable. This resolves concerns for implementations where
-- the custom border color is an index to a LUT instead of being directly
-- embedded into sampler state.
--
-- 4) Should UINT and SINT (unsigned integer and signed integer) border
-- color types be separated or should they be combined into one generic INT
-- (integer) type?
--
-- __RESOLVED__: Separating these does not make much sense as the existing
-- fixed border color types do not have this distinction, and there is no
-- reason in hardware to do so. This separation would also create
-- unnecessary work and considerations for the application.
--
-- == Version History
--
-- -   Revision 1, 2019-10-10 (Joshua Ashton)
--
--     -   Internal revisions.
--
-- -   Revision 2, 2019-10-11 (Liam Middlebrook)
--
--     -   Remove VkCustomBorderColor object and associated functions
--
--     -   Add issues concerning HW limitations for custom border color
--         count
--
-- -   Revision 3, 2019-10-12 (Joshua Ashton)
--
--     -   Re-expose the limits for the maximum number of unique border
--         colors
--
--     -   Add extra details about border color tracking
--
--     -   Fix typos
--
-- -   Revision 4, 2019-10-12 (Joshua Ashton)
--
--     -   Changed maxUniqueCustomBorderColors to a uint32_t from a
--         VkDeviceSize
--
-- -   Revision 5, 2019-10-14 (Liam Middlebrook)
--
--     -   Added features bit
--
-- -   Revision 6, 2019-10-15 (Joshua Ashton)
--
--     -   Type-ize VK_BORDER_COLOR_CUSTOM
--
--     -   Fix const-ness on @pNext@ of
--         VkSamplerCustomBorderColorCreateInfoEXT
--
-- -   Revision 7, 2019-11-26 (Liam Middlebrook)
--
--     -   Renamed maxUniqueCustomBorderColors to maxCustomBorderColors
--
-- -   Revision 8, 2019-11-29 (Joshua Ashton)
--
--     -   Renamed borderColor member of
--         VkSamplerCustomBorderColorCreateInfoEXT to customBorderColor
--
-- -   Revision 9, 2020-02-19 (Joshua Ashton)
--
--     -   Renamed maxCustomBorderColors to maxCustomBorderColorSamplers
--
-- -   Revision 10, 2020-02-21 (Joshua Ashton)
--
--     -   Added format to VkSamplerCustomBorderColorCreateInfoEXT and
--         feature bit
--
-- -   Revision 11, 2020-04-07 (Joshua Ashton)
--
--     -   Dropped UINT\/SINT border color differences, consolidated types
--
-- -   Revision 12, 2020-04-16 (Joshua Ashton)
--
--     -   Renamed VK_BORDER_COLOR_CUSTOM_FLOAT_EXT to
--         VK_BORDER_COLOR_FLOAT_CUSTOM_EXT for consistency
--
-- == See Also
--
-- 'PhysicalDeviceCustomBorderColorFeaturesEXT',
-- 'PhysicalDeviceCustomBorderColorPropertiesEXT',
-- 'SamplerCustomBorderColorCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_custom_border_color Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_custom_border_color  ( PhysicalDeviceCustomBorderColorFeaturesEXT
                                                     , PhysicalDeviceCustomBorderColorPropertiesEXT
                                                     , SamplerCustomBorderColorCreateInfoEXT
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceCustomBorderColorFeaturesEXT

instance ToCStruct PhysicalDeviceCustomBorderColorFeaturesEXT
instance Show PhysicalDeviceCustomBorderColorFeaturesEXT

instance FromCStruct PhysicalDeviceCustomBorderColorFeaturesEXT


data PhysicalDeviceCustomBorderColorPropertiesEXT

instance ToCStruct PhysicalDeviceCustomBorderColorPropertiesEXT
instance Show PhysicalDeviceCustomBorderColorPropertiesEXT

instance FromCStruct PhysicalDeviceCustomBorderColorPropertiesEXT


data SamplerCustomBorderColorCreateInfoEXT

instance ToCStruct SamplerCustomBorderColorCreateInfoEXT
instance Show SamplerCustomBorderColorCreateInfoEXT

