{-# language CPP #-}
-- | = Name
--
-- VK_MESA_image_alignment_control - device extension
--
-- = VK_MESA_image_alignment_control
--
-- [__Name String__]
--     @VK_MESA_image_alignment_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     576
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
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_MESA_image_alignment_control] @HansKristian-Work%0A*Here describe the issue or question you have about the VK_MESA_image_alignment_control extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-05-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- This extension allows applications to request a narrower alignment for
-- images than an implementation would otherwise require. Some
-- implementations internally support multiple image layouts in
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', each with
-- different alignment requirements and performance trade-offs. In some API
-- layering use cases such as D3D12, it is beneficial to be able to control
-- the alignment, since certain alignments for placed resources are
-- guaranteed to be supported, and emulating that expectation requires
-- unnecessary padding of allocations.
--
-- 'ImageAlignmentControlCreateInfoMESA' /can/ be chained to
-- 'Vulkan.Core10.Image.ImageCreateInfo', requesting that the alignment is
-- no more than the provided alignment. If the requested alignment is not
-- supported for a given 'Vulkan.Core10.Image.ImageCreateInfo', a larger
-- alignment /may/ be returned.
--
-- While something similar could be achieved with
-- @VK_EXT_image_drm_format_modifier@ in theory, this is not the intended
-- way to use that extension. Format modifiers are generally used for
-- externally shareable images, and would not be platform portable. It is
-- also a cumbersome API to use just to lower the alignment.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'ImageAlignmentControlCreateInfoMESA'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageAlignmentControlFeaturesMESA'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceImageAlignmentControlPropertiesMESA'
--
-- == New Enum Constants
--
-- -   'MESA_IMAGE_ALIGNMENT_CONTROL_EXTENSION_NAME'
--
-- -   'MESA_IMAGE_ALIGNMENT_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_ALIGNMENT_CONTROL_CREATE_INFO_MESA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_FEATURES_MESA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ALIGNMENT_CONTROL_PROPERTIES_MESA'
--
-- == Version History
--
-- -   Revision 1, 2024-04-05 (Hans-Kristian Arntzen)
--
--     -   Initial specification
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_MESA_image_alignment_control Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_MESA_image_alignment_control  ( ImageAlignmentControlCreateInfoMESA
                                                          , PhysicalDeviceImageAlignmentControlFeaturesMESA
                                                          , PhysicalDeviceImageAlignmentControlPropertiesMESA
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImageAlignmentControlCreateInfoMESA

instance ToCStruct ImageAlignmentControlCreateInfoMESA
instance Show ImageAlignmentControlCreateInfoMESA

instance FromCStruct ImageAlignmentControlCreateInfoMESA


data PhysicalDeviceImageAlignmentControlFeaturesMESA

instance ToCStruct PhysicalDeviceImageAlignmentControlFeaturesMESA
instance Show PhysicalDeviceImageAlignmentControlFeaturesMESA

instance FromCStruct PhysicalDeviceImageAlignmentControlFeaturesMESA


data PhysicalDeviceImageAlignmentControlPropertiesMESA

instance ToCStruct PhysicalDeviceImageAlignmentControlPropertiesMESA
instance Show PhysicalDeviceImageAlignmentControlPropertiesMESA

instance FromCStruct PhysicalDeviceImageAlignmentControlPropertiesMESA

