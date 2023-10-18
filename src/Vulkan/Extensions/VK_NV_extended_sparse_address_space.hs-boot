{-# language CPP #-}
-- | = Name
--
-- VK_NV_extended_sparse_address_space - device extension
--
-- == VK_NV_extended_sparse_address_space
--
-- [__Name String__]
--     @VK_NV_extended_sparse_address_space@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     493
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Russell Chou
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_extended_sparse_address_space] @russellcnv%0A*Here describe the issue or question you have about the VK_NV_extended_sparse_address_space extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-10-03
--
-- [__Contributors__]
--
--     -   Russell Chou, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- Implementations may be able to support an extended address space for
-- sparse memory resources, but only for a certain set of usages.
--
-- This extension adds a query for the extended limit, and the supported
-- usages that are allowed for that limit. This limit is an increase to
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@
-- when the 'Vulkan.Core10.Handles.Image' or 'Vulkan.Core10.Handles.Buffer'
-- uses only usages that are supported.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTENDED_SPARSE_ADDRESS_SPACE_EXTENSION_NAME'
--
-- -   'NV_EXTENDED_SPARSE_ADDRESS_SPACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2023-10-03 (Russell Chou)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV',
-- 'PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_extended_sparse_address_space Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_extended_sparse_address_space  ( PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV
                                                              , PhysicalDeviceExtendedSparseAddressSpacePropertiesNV
                                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV

instance ToCStruct PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV
instance Show PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV

instance FromCStruct PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV


data PhysicalDeviceExtendedSparseAddressSpacePropertiesNV

instance ToCStruct PhysicalDeviceExtendedSparseAddressSpacePropertiesNV
instance Show PhysicalDeviceExtendedSparseAddressSpacePropertiesNV

instance FromCStruct PhysicalDeviceExtendedSparseAddressSpacePropertiesNV

