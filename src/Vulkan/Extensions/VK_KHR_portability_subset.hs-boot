{-# language CPP #-}
-- | = Name
--
-- VK_KHR_portability_subset - device extension
--
-- == VK_KHR_portability_subset
--
-- [__Name String__]
--     @VK_KHR_portability_subset@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     164
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__Contact__]
--
--     -   Bill Hollings
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_portability_subset] @billhollings%0A<<Here describe the issue or question you have about the VK_KHR_portability_subset extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Bill Hollings, The Brenwill Workshop Ltd.
--
--     -   Daniel Koch, NVIDIA
--
--     -   Dzmitry Malyshau, Mozilla
--
--     -   Chip Davis, CodeWeavers
--
--     -   Dan Ginsburg, Valve
--
--     -   Mike Weiblen, LunarG
--
--     -   Neil Trevett, NVIDIA
--
--     -   Alexey Knyazev, Independent
--
-- == Description
--
-- The \`VK_KHR_portability_subset extension allows a non-conformant Vulkan
-- implementation to be built on top of another non-Vulkan graphics API,
-- and identifies differences between that implementation and a
-- fully-conformant native Vulkan implementation.
--
-- This extension provides Vulkan implementations with the ability to mark
-- otherwise-required capabilities as unsupported, or to establish
-- additional properties and limits that the application should adhere to
-- in order to guarantee portable behaviour and operation across platforms,
-- including platforms where Vulkan is not natively supported.
--
-- The goal of this specification is to document, and make queryable,
-- capabilities which are required to be supported by a fully-conformant
-- Vulkan 1.0 implementation, but may be optional for an implementation of
-- the Vulkan 1.0 Portability Subset.
--
-- The intent is that this extension will be advertised only on
-- implementations of the Vulkan 1.0 Portability Subset, and not on
-- conformant implementations of Vulkan 1.0. Fully-conformant Vulkan
-- implementations provide all the required capabilies, and so will not
-- provide this extension. Therefore, the existence of this extension can
-- be used to determine that an implementation is likely not fully
-- conformant with the Vulkan spec.
--
-- If this extension is supported by the Vulkan implementation, the
-- application must enable this extension.
--
-- This extension defines several new structures that can be chained to the
-- existing structures used by certain standard Vulkan calls, in order to
-- query for non-conformant portable behavior.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePortabilitySubsetFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePortabilitySubsetPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PORTABILITY_SUBSET_EXTENSION_NAME'
--
-- -   'KHR_PORTABILITY_SUBSET_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2020-07-21 (Bill Hollings)
--
--     -   Initial draft.
--
-- = See Also
--
-- 'PhysicalDevicePortabilitySubsetFeaturesKHR',
-- 'PhysicalDevicePortabilitySubsetPropertiesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_portability_subset Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_portability_subset  ( PhysicalDevicePortabilitySubsetFeaturesKHR
                                                    , PhysicalDevicePortabilitySubsetPropertiesKHR
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePortabilitySubsetFeaturesKHR

instance ToCStruct PhysicalDevicePortabilitySubsetFeaturesKHR
instance Show PhysicalDevicePortabilitySubsetFeaturesKHR

instance FromCStruct PhysicalDevicePortabilitySubsetFeaturesKHR


data PhysicalDevicePortabilitySubsetPropertiesKHR

instance ToCStruct PhysicalDevicePortabilitySubsetPropertiesKHR
instance Show PhysicalDevicePortabilitySubsetPropertiesKHR

instance FromCStruct PhysicalDevicePortabilitySubsetPropertiesKHR

