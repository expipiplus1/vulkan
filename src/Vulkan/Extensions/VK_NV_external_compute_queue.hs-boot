{-# language CPP #-}
-- | = Name
--
-- VK_NV_external_compute_queue - device extension
--
-- = VK_NV_external_compute_queue
--
-- [__Name String__]
--     @VK_NV_external_compute_queue@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     557
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Chris Lentini
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_external_compute_queue] @clentini%0A*Here describe the issue or question you have about the VK_NV_external_compute_queue extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_external_compute_queue.adoc VK_NV_external_compute_queue>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-03-24
--
-- [__Contributors__]
--
--     -   Chris Lentini, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Liam Middlebrook, NVIDIA
--
--     -   Lionel Duc, NVIDIA
--
-- == Description
--
-- This extension gives applications the ability to join compatible
-- external compute APIs to a 'Vulkan.Core10.Handles.Device'. In this way,
-- the extension allows an application to achieve simultaneous execution
-- between work submitted from these compatible external APIs and work that
-- has been submitted through the Vulkan API.
--
-- At device creation time, an application /must/ supply a
-- 'ExternalComputeQueueDeviceCreateInfoNV'. This communicates to the
-- implementation the maximum number of external queues that the
-- application /can/ create at once. This information /may/ be used by the
-- implementation to aid in decisions made during device creation.
--
-- After device creation, the function 'createExternalComputeQueueNV' is
-- used by an application to create a new
-- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV' object. The
-- 'Vulkan.Extensions.Handles.ExternalComputeQueueNV' object holds
-- information and reserves resources necessary for a compatible external
-- API to be able to join a 'Vulkan.Core10.Handles.Device'. This
-- information can be queried through the 'getExternalComputeQueueDataNV'
-- function, returning an opaque blob of data which can be passed to
-- compatible external APIs. The application /must/ finally call
-- 'destroyExternalComputeQueueNV' when it is done in order to release the
-- reserved resources.
--
-- This extension introduces a new properties structure,
-- 'PhysicalDeviceExternalComputeQueuePropertiesNV', which can be queried
-- through
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2'.
-- The structure provides information on functional limits to the extension
-- as well as a way of querying the size of the application allocated
-- memory which /must/ be passed to the 'getExternalComputeQueueDataNV'
-- function.
--
-- When creating a 'Vulkan.Extensions.Handles.ExternalComputeQueueNV'
-- through 'createExternalComputeQueueNV', the
-- 'ExternalComputeQueueCreateInfoNV' structure requires an application to
-- supply a 'Vulkan.Core10.Handles.Queue' to aid in external compute queue
-- creation. The supplied 'Vulkan.Core10.Handles.Queue' is a strong
-- scheduling hint about which queue it expects to submit graphics
-- workloads to and with which it expects simultaneous execution of compute
-- workloads submitted through the external API.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.ExternalComputeQueueNV'
--
-- == New Commands
--
-- -   'createExternalComputeQueueNV'
--
-- -   'destroyExternalComputeQueueNV'
--
-- -   'getExternalComputeQueueDataNV'
--
-- == New Structures
--
-- -   'ExternalComputeQueueCreateInfoNV'
--
-- -   'ExternalComputeQueueDataParamsNV'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'ExternalComputeQueueDeviceCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceExternalComputeQueuePropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTERNAL_COMPUTE_QUEUE_EXTENSION_NAME'
--
-- -   'NV_EXTERNAL_COMPUTE_QUEUE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_EXTERNAL_COMPUTE_QUEUE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DATA_PARAMS_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_COMPUTE_QUEUE_DEVICE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_COMPUTE_QUEUE_PROPERTIES_NV'
--
-- While the external queue is now a part of a
-- 'Vulkan.Core10.Handles.Device', idling the device through
-- 'Vulkan.Core10.Queue.deviceWaitIdle' does not wait for the external
-- queue. Draining the work on an external queue /must/ be done through its
-- own external API. External queues /must/ be idled before destroying the
-- associated 'Vulkan.Core10.Handles.Device'.
--
-- In general, synchronization and resource sharing between the external
-- API and Vulkan must still be accomplished via existing cross-API interop
-- mechanisms.
--
-- == Version History
--
-- -   Revision 1, 2024-05-20 (Chris Lentini)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_external_compute_queue Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_external_compute_queue  ( ExternalComputeQueueCreateInfoNV
                                                       , ExternalComputeQueueDataParamsNV
                                                       , ExternalComputeQueueDeviceCreateInfoNV
                                                       , PhysicalDeviceExternalComputeQueuePropertiesNV
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExternalComputeQueueCreateInfoNV

instance ToCStruct ExternalComputeQueueCreateInfoNV
instance Show ExternalComputeQueueCreateInfoNV

instance FromCStruct ExternalComputeQueueCreateInfoNV


data ExternalComputeQueueDataParamsNV

instance ToCStruct ExternalComputeQueueDataParamsNV
instance Show ExternalComputeQueueDataParamsNV

instance FromCStruct ExternalComputeQueueDataParamsNV


data ExternalComputeQueueDeviceCreateInfoNV

instance ToCStruct ExternalComputeQueueDeviceCreateInfoNV
instance Show ExternalComputeQueueDeviceCreateInfoNV

instance FromCStruct ExternalComputeQueueDeviceCreateInfoNV


data PhysicalDeviceExternalComputeQueuePropertiesNV

instance ToCStruct PhysicalDeviceExternalComputeQueuePropertiesNV
instance Show PhysicalDeviceExternalComputeQueuePropertiesNV

instance FromCStruct PhysicalDeviceExternalComputeQueuePropertiesNV

