{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance9 - device extension
--
-- = VK_KHR_maintenance9
--
-- [__Name String__]
--     @VK_KHR_maintenance9@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     585
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance9] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance9 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance9.adoc VK_KHR_maintenance9>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-29
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Shahbaz Youssefi, Google
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Story, Nintendo
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance9 VK_KHR_maintenance9>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   Support VkDevice with no queues. These can be used as effectively an
--     offline compiler to prepopulate pipeline caches, without expensive
--     queue creation or internal memory allocations.
--
-- -   Allow
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdSetEvent2'
--     to not provide a dependency, providing
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent'-style usage using
--     enums from @VK_KHR_synchronization2@
--
-- -   Add a
--     'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QueryPoolCreateFlagBits'::'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QUERY_POOL_CREATE_RESET_BIT_KHR'
--     flag to create a query pool with all queries initialized to the
--     reset state.
--
-- -   Allow any integer bit width for specific bit-wise operations.
--
-- -   Add a property to enable sparse support with
--     @VK_EXT_image_2d_view_of_3d@.
--
-- -   Add a property to indicate the implementation will return (0,0,0,0)
--     or (0,0,0,1) to vertex shaders that read unassigned attributes.
--
-- -   The effects of image memory barriers and image layout transitions on
--     3D images created with VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT are
--     scoped to the slices specified by the user-provided
--     VkImageSubresourceRange.
--
-- -   Queue family ownership transfers are no longer required for buffers
--     and linear images, and a new physical device queue family property
--     is exposed to indicate whether queue family ownership transfers are
--     required for optimal images.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance9FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance9PropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyOwnershipTransferPropertiesKHR'
--
-- == New Enums
--
-- -   'DefaultVertexAttributeValueKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_9_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_9_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits':
--
--     -   'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_ASYMMETRIC_EVENT_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QueryPoolCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.QueryPoolCreateFlagBits.QUERY_POOL_CREATE_RESET_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_9_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_OWNERSHIP_TRANSFER_PROPERTIES_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-05-29 (Contributors)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance9 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance9  ( PhysicalDeviceMaintenance9FeaturesKHR
                                              , PhysicalDeviceMaintenance9PropertiesKHR
                                              , QueueFamilyOwnershipTransferPropertiesKHR
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceMaintenance9FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance9FeaturesKHR
instance Show PhysicalDeviceMaintenance9FeaturesKHR

instance FromCStruct PhysicalDeviceMaintenance9FeaturesKHR


data PhysicalDeviceMaintenance9PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance9PropertiesKHR
instance Show PhysicalDeviceMaintenance9PropertiesKHR

instance FromCStruct PhysicalDeviceMaintenance9PropertiesKHR


data QueueFamilyOwnershipTransferPropertiesKHR

instance ToCStruct QueueFamilyOwnershipTransferPropertiesKHR
instance Show QueueFamilyOwnershipTransferPropertiesKHR

instance FromCStruct QueueFamilyOwnershipTransferPropertiesKHR

