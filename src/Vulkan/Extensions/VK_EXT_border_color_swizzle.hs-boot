{-# language CPP #-}
-- | = Name
--
-- VK_EXT_border_color_swizzle - device extension
--
-- == VK_EXT_border_color_swizzle
--
-- [__Name String__]
--     @VK_EXT_border_color_swizzle@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     412
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_EXT_custom_border_color@ to be enabled for any
--         device-level functionality
--
-- [__Special Uses__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_border_color_swizzle] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_border_color_swizzle extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-10-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Ricardo Garcia, Igalia
--
--     -   Shahbaz Youssefi, Google
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- After the publication of VK_EXT_custom_border_color, it was discovered
-- that some implementations had undefined behavior when combining a
-- sampler that uses a custom border color with image views whose component
-- mapping is not the identity mapping.
--
-- Since VK_EXT_custom_border_color has already shipped, this new extension
-- VK_EXT_border_color_swizzle was created to define the interaction
-- between custom border colors and non-identity image view swizzles, and
-- provide a work-around for implementations that must pre-swizzle the
-- sampler border color to match the image view component mapping it is
-- combined with.
--
-- This extension also defines the behavior between samplers with an opaque
-- black border color and image views with a non-identity component
-- swizzle, which was previously left undefined.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceBorderColorSwizzleFeaturesEXT'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerBorderColorComponentMappingCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_BORDER_COLOR_SWIZZLE_EXTENSION_NAME'
--
-- -   'EXT_BORDER_COLOR_SWIZZLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BORDER_COLOR_SWIZZLE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_BORDER_COLOR_COMPONENT_MAPPING_CREATE_INFO_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2021-10-12 (Piers Daniell)
--
--     -   Internal revisions.
--
-- == See Also
--
-- 'PhysicalDeviceBorderColorSwizzleFeaturesEXT',
-- 'SamplerBorderColorComponentMappingCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_border_color_swizzle Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_border_color_swizzle  ( PhysicalDeviceBorderColorSwizzleFeaturesEXT
                                                      , SamplerBorderColorComponentMappingCreateInfoEXT
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceBorderColorSwizzleFeaturesEXT

instance ToCStruct PhysicalDeviceBorderColorSwizzleFeaturesEXT
instance Show PhysicalDeviceBorderColorSwizzleFeaturesEXT

instance FromCStruct PhysicalDeviceBorderColorSwizzleFeaturesEXT


data SamplerBorderColorComponentMappingCreateInfoEXT

instance ToCStruct SamplerBorderColorComponentMappingCreateInfoEXT
instance Show SamplerBorderColorComponentMappingCreateInfoEXT

instance FromCStruct SamplerBorderColorComponentMappingCreateInfoEXT

