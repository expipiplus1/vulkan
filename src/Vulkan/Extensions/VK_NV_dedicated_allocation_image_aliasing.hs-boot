{-# language CPP #-}
-- | = Name
--
-- VK_NV_dedicated_allocation_image_aliasing - device extension
--
-- == VK_NV_dedicated_allocation_image_aliasing
--
-- [__Name String__]
--     @VK_NV_dedicated_allocation_image_aliasing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     241
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_dedicated_allocation@
--
-- [__Contact__]
--
--     -   Nuno Subtil
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_dedicated_allocation_image_aliasing] @nsubtil%0A<<Here describe the issue or question you have about the VK_NV_dedicated_allocation_image_aliasing extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-04
--
-- [__Contributors__]
--
--     -   Nuno Subtil, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Axel Gneiting, id Software
--
-- == Description
--
-- This extension allows applications to alias images on dedicated
-- allocations, subject to specific restrictions: the extent and the number
-- of layers in the image being aliased must be smaller than or equal to
-- those of the original image for which the allocation was created, and
-- every other image parameter must match.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_EXTENSION_NAME'
--
-- -   'NV_DEDICATED_ALLOCATION_IMAGE_ALIASING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV'
--
-- == Version History
--
-- -   Revision 1, 2019-01-04 (Nuno Subtil)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_dedicated_allocation_image_aliasing Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing  (PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV

instance ToCStruct PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
instance Show PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV

instance FromCStruct PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV

