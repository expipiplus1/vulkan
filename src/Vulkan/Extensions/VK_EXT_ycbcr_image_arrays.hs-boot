{-# language CPP #-}
-- | = Name
--
-- VK_EXT_ycbcr_image_arrays - device extension
--
-- = Registered Extension Number
--
-- 253
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_sampler_ycbcr_conversion@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-15
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows images of a format that requires
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion Y′CBCR conversion>
-- to be created with multiple array layers, which is otherwise restricted.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceYcbcrImageArraysFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME'
--
-- -   'EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-01-15 (Piers Daniell)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PhysicalDeviceYcbcrImageArraysFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_ycbcr_image_arrays Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_ycbcr_image_arrays  (PhysicalDeviceYcbcrImageArraysFeaturesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceYcbcrImageArraysFeaturesEXT

instance ToCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT
instance Show PhysicalDeviceYcbcrImageArraysFeaturesEXT

instance FromCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT

