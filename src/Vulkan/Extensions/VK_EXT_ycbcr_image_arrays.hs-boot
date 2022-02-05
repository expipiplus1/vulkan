{-# language CPP #-}
-- | = Name
--
-- VK_EXT_ycbcr_image_arrays - device extension
--
-- == VK_EXT_ycbcr_image_arrays
--
-- [__Name String__]
--     @VK_EXT_ycbcr_image_arrays@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     253
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_sampler_ycbcr_conversion@
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_ycbcr_image_arrays] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_EXT_ycbcr_image_arrays extension>> >
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
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion Y′CBCR conversion>
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
-- == See Also
--
-- 'PhysicalDeviceYcbcrImageArraysFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_ycbcr_image_arrays Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_ycbcr_image_arrays  (PhysicalDeviceYcbcrImageArraysFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceYcbcrImageArraysFeaturesEXT

instance ToCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT
instance Show PhysicalDeviceYcbcrImageArraysFeaturesEXT

instance FromCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT

