{-# language CPP #-}
-- | = Name
--
-- VK_KHR_internally_synchronized_queues - device extension
--
-- = VK_KHR_internally_synchronized_queues
--
-- [__Name String__]
--     @VK_KHR_internally_synchronized_queues@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     505
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_internally_synchronized_queues] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_internally_synchronized_queues extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_internally_synchronized_queues.adoc VK_KHR_internally_synchronized_queues>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-02-04
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Daniel Rakos, RasterGrid
--
--     -   Jeff Bolz, Nvidia
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_internally_synchronized_queues VK_KHR_internally_synchronized_queues>
-- allows queues to opt into being internally synchronized via the
-- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_INTERNALLY_SYNCHRONIZED_BIT_KHR'
-- flag.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_INTERNALLY_SYNCHRONIZED_QUEUES_EXTENSION_NAME'
--
-- -   'KHR_INTERNALLY_SYNCHRONIZED_QUEUES_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_INTERNALLY_SYNCHRONIZED_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INTERNALLY_SYNCHRONIZED_QUEUES_FEATURES_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 0, 2025-01-07 (Daniel Rakos)
--
--     -   Initial proposal
--
-- -   Revision 1, 2025-02-04 (Shahbaz Youssefi)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_internally_synchronized_queues Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_internally_synchronized_queues  (PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR

instance ToCStruct PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR
instance Show PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR

instance FromCStruct PhysicalDeviceInternallySynchronizedQueuesFeaturesKHR

