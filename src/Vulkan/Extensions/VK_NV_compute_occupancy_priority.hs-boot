{-# language CPP #-}
-- | = Name
--
-- VK_NV_compute_occupancy_priority - device extension
--
-- = VK_NV_compute_occupancy_priority
--
-- [__Name String__]
--     @VK_NV_compute_occupancy_priority@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     646
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Chris Lentini
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_compute_occupancy_priority] @clentini%0A*Here describe the issue or question you have about the VK_NV_compute_occupancy_priority extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_compute_occupancy_priority.adoc VK_NV_compute_occupancy_priority>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-12-01
--
-- [__Contributors__]
--
--     -   Chris Lentini, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Lionel Duc, NVIDIA
--
--     -   Peter Deayton, NVIDIA
--
-- == Description
--
-- This extension provides applications with control over how their compute
-- workloads utilize GPU compute resources, specifically allowing
-- prioritization relative to other simultaneously executing workloads.
-- Applications can specify the priority with which compute workloads
-- should occupy GPU compute resources, allowing for a fine-grained
-- distinction between workloads that may want to execute at a background
-- priority over a long period of time versus workloads with harder latency
-- requirements.
--
-- The extension introduces a new command
-- 'cmdSetComputeOccupancyPriorityNV' that allows applications to set the
-- occupancy priority for subsequent compute dispatches. The occupancy
-- priority affects how compute workloads utilize GPU compute resources
-- relative to other simultaneously executing workloads.
--
-- The occupancy priority is stateful on a command buffer. All commands
-- listed in the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#dispatch Dispatching Commands>
-- chapter issued subsequent to a 'cmdSetComputeOccupancyPriorityNV' call
-- will be executed with the specified priority parameters until another
-- 'cmdSetComputeOccupancyPriorityNV' call is made.
--
-- For convenience, three named occupancy priority values are defined:
--
-- -   __VK_COMPUTE_OCCUPANCY_PRIORITY_LOW_NV__ - a constant value that can
--     be used for
--     'ComputeOccupancyPriorityParametersNV'::@occupancyPriority@ to
--     specify a low priority level.
--
-- -   __VK_COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV__ - a constant value that
--     can be used for
--     'ComputeOccupancyPriorityParametersNV'::@occupancyPriority@ to
--     specify a normal priority level. This represents the default
--     priority level.
--
-- -   __VK_COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV__ - a constant value that
--     can be used for
--     'ComputeOccupancyPriorityParametersNV'::@occupancyPriority@ to
--     specify a high priority level.
--
-- All command buffers (primary and secondary) start with a priority level
-- equal to the VK_COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV value. The priority
-- state is not inherited by secondary command buffers - each command
-- buffer maintains its own independent priority state.
--
-- == New Commands
--
-- -   'cmdSetComputeOccupancyPriorityNV'
--
-- == New Structures
--
-- -   'ComputeOccupancyPriorityParametersNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceComputeOccupancyPriorityFeaturesNV'
--
-- == New Enum Constants
--
-- -   'Vulkan.Core10.APIConstants.COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV'
--
-- -   'Vulkan.Core10.APIConstants.COMPUTE_OCCUPANCY_PRIORITY_LOW_NV'
--
-- -   'Vulkan.Core10.APIConstants.COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV'
--
-- -   'NV_COMPUTE_OCCUPANCY_PRIORITY_EXTENSION_NAME'
--
-- -   'NV_COMPUTE_OCCUPANCY_PRIORITY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMPUTE_OCCUPANCY_PRIORITY_PARAMETERS_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_OCCUPANCY_PRIORITY_FEATURES_NV'
--
-- The extension only allows specification of occupancy priority for
-- compute workloads, however, the priorities will also impact the
-- prioritization of compute workloads relative to simultaneously executing
-- graphics workloads. In such a scenario, the graphics workload may be
-- thought of as executing at VK_COMPUTE_OCCUPANCY_PRIORITY_NORMAL_NV
-- priority, and so a simultaneously executing compute workload with
-- VK_COMPUTE_OCCUPANCY_PRIORITY_HIGH_NV occupancy priority will
-- preferentially utilize available compute resources.
--
-- Workloads specified with a higher priority may begin execution after
-- workloads specified with a lower priority, at which point they may find
-- GPU compute resources already occupied. So, while they will from that
-- point forward preferentially occupy available compute resources, they
-- may not ramp up to full occupancy until the already present lower
-- priority work has reached a point where it can relinquish compute
-- resources.
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-08-06 (Chris Lentini)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_compute_occupancy_priority Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_compute_occupancy_priority  ( ComputeOccupancyPriorityParametersNV
                                                           , PhysicalDeviceComputeOccupancyPriorityFeaturesNV
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ComputeOccupancyPriorityParametersNV

instance ToCStruct ComputeOccupancyPriorityParametersNV
instance Show ComputeOccupancyPriorityParametersNV

instance FromCStruct ComputeOccupancyPriorityParametersNV


data PhysicalDeviceComputeOccupancyPriorityFeaturesNV

instance ToCStruct PhysicalDeviceComputeOccupancyPriorityFeaturesNV
instance Show PhysicalDeviceComputeOccupancyPriorityFeaturesNV

instance FromCStruct PhysicalDeviceComputeOccupancyPriorityFeaturesNV

