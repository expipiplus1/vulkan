{-# language CPP #-}
-- | = Name
--
-- VK_EXT_memory_budget - device extension
--
-- = Registered Extension Number
--
-- 238
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-08
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
-- == Description
--
-- While running a Vulkan application, other processes on the machine might
-- also be attempting to use the same device memory, which can pose
-- problems. This extension adds support for querying the amount of memory
-- used and the total memory budget for a memory heap. The values returned
-- by this query are implementation-dependent and can depend on a variety
-- of factors including operating system and system load.
--
-- The 'PhysicalDeviceMemoryBudgetPropertiesEXT'::@heapBudget@ values can
-- be used as a guideline for how much total memory from each heap the
-- __current process__ can use at any given time, before allocations may
-- start failing or causing performance degradation. The values may change
-- based on other activity in the system that is outside the scope and
-- control of the Vulkan implementation.
--
-- The 'PhysicalDeviceMemoryBudgetPropertiesEXT'::@heapUsage@ will display
-- the __current process__ estimated heap usage.
--
-- With this information, the idea is for an application at some interval
-- (once per frame, per few seconds, etc) to query @heapBudget@ and
-- @heapUsage@. From here the application can notice if it is over budget
-- and decide how it wants to handle the memory situation (free it, move to
-- host memory, changing mipmap levels, etc). This extension is designed to
-- be used in concert with
-- <VK_EXT_memory_priority.html VK_EXT_memory_priority> to help with this
-- part of memory management.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceMemoryProperties2':
--
--     -   'PhysicalDeviceMemoryBudgetPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MEMORY_BUDGET_EXTENSION_NAME'
--
-- -   'EXT_MEMORY_BUDGET_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-10-08 (Jeff Bolz)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PhysicalDeviceMemoryBudgetPropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_memory_budget Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_memory_budget  (PhysicalDeviceMemoryBudgetPropertiesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceMemoryBudgetPropertiesEXT

instance ToCStruct PhysicalDeviceMemoryBudgetPropertiesEXT
instance Show PhysicalDeviceMemoryBudgetPropertiesEXT

instance FromCStruct PhysicalDeviceMemoryBudgetPropertiesEXT

