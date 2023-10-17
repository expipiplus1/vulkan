{-# language CPP #-}
-- | = Name
--
-- VK_NV_descriptor_pool_overallocation - device extension
--
-- == VK_NV_descriptor_pool_overallocation
--
-- [__Name String__]
--     @VK_NV_descriptor_pool_overallocation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     547
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_descriptor_pool_overallocation] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_NV_descriptor_pool_overallocation extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-08-30
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- There are scenarios where the application does not know ahead of time
-- how many descriptor sets it may need to allocate from a descriptor pool,
-- or how many descriptors of any of the descriptor types it may need to
-- allocate from the descriptor pool.
--
-- This extension gives applications the ability to request the
-- implementation allow more sets or descriptors to be allocated than
-- initially specified at descriptor pool creation time, subject to
-- available resources.
--
-- The
-- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_SETS_BIT_NV'
-- flag lets the application allocate more than
-- 'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo'::@maxSets@
-- descriptor sets, and the
-- 'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_POOLS_BIT_NV'
-- lets the application allocate more descriptors than initially specified
-- by 'Vulkan.Core10.DescriptorSet.DescriptorPoolSize'::@descriptorCount@
-- for any descriptor types.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDescriptorPoolOverallocationFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_DESCRIPTOR_POOL_OVERALLOCATION_EXTENSION_NAME'
--
-- -   'NV_DESCRIPTOR_POOL_OVERALLOCATION_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DescriptorPoolCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_POOLS_BIT_NV'
--
--     -   'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_ALLOW_OVERALLOCATION_SETS_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_POOL_OVERALLOCATION_FEATURES_NV'
--
-- == Version History
--
-- -   Revision 1, 2023-08-30 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceDescriptorPoolOverallocationFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_descriptor_pool_overallocation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_descriptor_pool_overallocation  (PhysicalDeviceDescriptorPoolOverallocationFeaturesNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDescriptorPoolOverallocationFeaturesNV

instance ToCStruct PhysicalDeviceDescriptorPoolOverallocationFeaturesNV
instance Show PhysicalDeviceDescriptorPoolOverallocationFeaturesNV

instance FromCStruct PhysicalDeviceDescriptorPoolOverallocationFeaturesNV

