{-# language CPP #-}
-- | = Name
--
-- VK_EXT_memory_budget - device extension
--
-- == VK_EXT_memory_budget
--
-- [__Name String__]
--     @VK_EXT_memory_budget@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     238
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_memory_budget] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_memory_budget extension* >
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
-- be used in concert with @VK_EXT_memory_priority@ to help with this part
-- of memory management.
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
-- == See Also
--
-- 'PhysicalDeviceMemoryBudgetPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_memory_budget Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_memory_budget  (PhysicalDeviceMemoryBudgetPropertiesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceMemoryBudgetPropertiesEXT

instance ToCStruct PhysicalDeviceMemoryBudgetPropertiesEXT
instance Show PhysicalDeviceMemoryBudgetPropertiesEXT

instance FromCStruct PhysicalDeviceMemoryBudgetPropertiesEXT

