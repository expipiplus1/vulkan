{-# language CPP #-}
-- | = Name
--
-- VK_KHR_get_physical_device_properties2 - instance extension
--
-- == VK_KHR_get_physical_device_properties2
--
-- [__Name String__]
--     @VK_KHR_get_physical_device_properties2@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     60
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_get_physical_device_properties2:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Ian Elliott, Google
--
-- == Description
--
-- This extension provides new entry points to query device features,
-- device properties, and format properties in a way that can be easily
-- extended by other extensions, without introducing any further entry
-- points. The Vulkan 1.0 feature\/limit\/formatproperty structures do not
-- include @sType@\/@pNext@ members. This extension wraps them in new
-- structures with @sType@\/@pNext@ members, so an application can query a
-- chain of feature\/limit\/formatproperty structures by constructing the
-- chain and letting the implementation fill them in. A new command is
-- added for each @vkGetPhysicalDevice*@ command in core Vulkan 1.0. The
-- new feature structure (and a @pNext@ chain of extending structures) can
-- also be passed in to device creation to enable features.
--
-- This extension also allows applications to use the physical-device
-- components of device extensions before
-- 'Vulkan.Core10.Device.createDevice' is called.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'getPhysicalDeviceFeatures2KHR'
--
-- -   'getPhysicalDeviceFormatProperties2KHR'
--
-- -   'getPhysicalDeviceImageFormatProperties2KHR'
--
-- -   'getPhysicalDeviceMemoryProperties2KHR'
--
-- -   'getPhysicalDeviceProperties2KHR'
--
-- -   'getPhysicalDeviceQueueFamilyProperties2KHR'
--
-- -   'getPhysicalDeviceSparseImageFormatProperties2KHR'
--
-- == New Structures
--
-- -   'FormatProperties2KHR'
--
-- -   'ImageFormatProperties2KHR'
--
-- -   'PhysicalDeviceImageFormatInfo2KHR'
--
-- -   'PhysicalDeviceMemoryProperties2KHR'
--
-- -   'PhysicalDeviceProperties2KHR'
--
-- -   'PhysicalDeviceSparseImageFormatInfo2KHR'
--
-- -   'QueueFamilyProperties2KHR'
--
-- -   'SparseImageFormatProperties2KHR'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFeatures2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME'
--
-- -   'KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR'
--
--     -   'STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR'
--
--     -   'STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR'
--
-- == Examples
--
-- >     // Get features with a hypothetical future extension.
-- >     VkHypotheticalExtensionFeaturesKHR hypotheticalFeatures =
-- >     {
-- >         VK_STRUCTURE_TYPE_HYPOTHETICAL_FEATURES_KHR,                            // sType
-- >         NULL,                                                                   // pNext
-- >     };
-- >
-- >     VkPhysicalDeviceFeatures2KHR features =
-- >     {
-- >         VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR,                       // sType
-- >         &hypotheticalFeatures,                                                  // pNext
-- >     };
-- >
-- >     // After this call, features and hypotheticalFeatures have been filled out.
-- >     vkGetPhysicalDeviceFeatures2KHR(physicalDevice, &features);
-- >
-- >     // Properties/limits can be chained and queried similarly.
-- >
-- >     // Enable some features:
-- >     VkHypotheticalExtensionFeaturesKHR enabledHypotheticalFeatures =
-- >     {
-- >         VK_STRUCTURE_TYPE_HYPOTHETICAL_FEATURES_KHR,                            // sType
-- >         NULL,                                                                   // pNext
-- >     };
-- >
-- >     VkPhysicalDeviceFeatures2KHR enabledFeatures =
-- >     {
-- >         VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR,                       // sType
-- >         &enabledHypotheticalFeatures,                                           // pNext
-- >     };
-- >
-- >     enabledFeatures.features.xyz = VK_TRUE;
-- >     enabledHypotheticalFeatures.abc = VK_TRUE;
-- >
-- >     VkDeviceCreateInfo deviceCreateInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,                                   // sType
-- >         &enabledFeatures,                                                       // pNext
-- >         ...
-- >         NULL,                                                                   // pEnabledFeatures
-- >     }
-- >
-- >     VkDevice device;
-- >     vkCreateDevice(physicalDevice, &deviceCreateInfo, NULL, &device);
--
-- == Version History
--
-- -   Revision 1, 2016-09-12 (Jeff Bolz)
--
--     -   Internal revisions
--
-- -   Revision 2, 2016-11-02 (Ian Elliott)
--
--     -   Added ability for applications to use the physical-device
--         components of device extensions before vkCreateDevice is called.
--
-- = See Also
--
-- 'FormatProperties2KHR', 'ImageFormatProperties2KHR',
-- 'PhysicalDeviceFeatures2KHR', 'PhysicalDeviceImageFormatInfo2KHR',
-- 'PhysicalDeviceMemoryProperties2KHR', 'PhysicalDeviceProperties2KHR',
-- 'PhysicalDeviceSparseImageFormatInfo2KHR', 'QueueFamilyProperties2KHR',
-- 'SparseImageFormatProperties2KHR', 'getPhysicalDeviceFeatures2KHR',
-- 'getPhysicalDeviceFormatProperties2KHR',
-- 'getPhysicalDeviceImageFormatProperties2KHR',
-- 'getPhysicalDeviceMemoryProperties2KHR',
-- 'getPhysicalDeviceProperties2KHR',
-- 'getPhysicalDeviceQueueFamilyProperties2KHR',
-- 'getPhysicalDeviceSparseImageFormatProperties2KHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_get_physical_device_properties2  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
                                                                 , pattern STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
                                                                 , getPhysicalDeviceFeatures2KHR
                                                                 , getPhysicalDeviceProperties2KHR
                                                                 , getPhysicalDeviceFormatProperties2KHR
                                                                 , getPhysicalDeviceImageFormatProperties2KHR
                                                                 , getPhysicalDeviceQueueFamilyProperties2KHR
                                                                 , getPhysicalDeviceMemoryProperties2KHR
                                                                 , getPhysicalDeviceSparseImageFormatProperties2KHR
                                                                 , PhysicalDeviceFeatures2KHR
                                                                 , PhysicalDeviceProperties2KHR
                                                                 , FormatProperties2KHR
                                                                 , ImageFormatProperties2KHR
                                                                 , PhysicalDeviceImageFormatInfo2KHR
                                                                 , QueueFamilyProperties2KHR
                                                                 , PhysicalDeviceMemoryProperties2KHR
                                                                 , SparseImageFormatProperties2KHR
                                                                 , PhysicalDeviceSparseImageFormatInfo2KHR
                                                                 , KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION
                                                                 , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION
                                                                 , KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                                                                 , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                                                                 ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceFeatures2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceFormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceImageFormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceMemoryProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceQueueFamilyProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceSparseImageFormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (FormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (ImageFormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceFeatures2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceImageFormatInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceMemoryProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceSparseImageFormatInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (QueueFamilyProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (SparseImageFormatProperties2)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FORMAT_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR = STRUCTURE_TYPE_FORMAT_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR = STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR = STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR = STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2


-- No documentation found for TopLevel "vkGetPhysicalDeviceFeatures2KHR"
getPhysicalDeviceFeatures2KHR = getPhysicalDeviceFeatures2


-- No documentation found for TopLevel "vkGetPhysicalDeviceProperties2KHR"
getPhysicalDeviceProperties2KHR = getPhysicalDeviceProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceFormatProperties2KHR"
getPhysicalDeviceFormatProperties2KHR = getPhysicalDeviceFormatProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceImageFormatProperties2KHR"
getPhysicalDeviceImageFormatProperties2KHR = getPhysicalDeviceImageFormatProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
getPhysicalDeviceQueueFamilyProperties2KHR = getPhysicalDeviceQueueFamilyProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceMemoryProperties2KHR"
getPhysicalDeviceMemoryProperties2KHR = getPhysicalDeviceMemoryProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
getPhysicalDeviceSparseImageFormatProperties2KHR = getPhysicalDeviceSparseImageFormatProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceFeatures2KHR"
type PhysicalDeviceFeatures2KHR = PhysicalDeviceFeatures2


-- No documentation found for TopLevel "VkPhysicalDeviceProperties2KHR"
type PhysicalDeviceProperties2KHR = PhysicalDeviceProperties2


-- No documentation found for TopLevel "VkFormatProperties2KHR"
type FormatProperties2KHR = FormatProperties2


-- No documentation found for TopLevel "VkImageFormatProperties2KHR"
type ImageFormatProperties2KHR = ImageFormatProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceImageFormatInfo2KHR"
type PhysicalDeviceImageFormatInfo2KHR = PhysicalDeviceImageFormatInfo2


-- No documentation found for TopLevel "VkQueueFamilyProperties2KHR"
type QueueFamilyProperties2KHR = QueueFamilyProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceMemoryProperties2KHR"
type PhysicalDeviceMemoryProperties2KHR = PhysicalDeviceMemoryProperties2


-- No documentation found for TopLevel "VkSparseImageFormatProperties2KHR"
type SparseImageFormatProperties2KHR = SparseImageFormatProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceSparseImageFormatInfo2KHR"
type PhysicalDeviceSparseImageFormatInfo2KHR = PhysicalDeviceSparseImageFormatInfo2


type KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION"
pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 2


type KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_physical_device_properties2"

-- No documentation found for TopLevel "VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME"
pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_physical_device_properties2"

