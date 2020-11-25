{-# language CPP #-}
-- | = Name
--
-- VK_AMD_memory_overallocation_behavior - device extension
--
-- == VK_AMD_memory_overallocation_behavior
--
-- [__Name String__]
--     @VK_AMD_memory_overallocation_behavior@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     190
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Martin Dinkov
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_AMD_memory_overallocation_behavior:%20&body=@mdinkov%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-09-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Martin Dinkov, AMD
--
--     -   Matthaeus Chajdas, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Jon Campbell, AMD
--
-- == Description
--
-- This extension allows controlling whether explicit overallocation beyond
-- the device memory heap sizes (reported by
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties') is
-- allowed or not. Overallocation may lead to performance loss and is not
-- supported for all platforms.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceMemoryOverallocationCreateInfoAMD'
--
-- == New Enums
--
-- -   'MemoryOverallocationBehaviorAMD'
--
-- == New Enum Constants
--
-- -   'AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME'
--
-- -   'AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD'
--
-- == Version History
--
-- -   Revision 1, 2018-09-19 (Martin Dinkov)
--
--     -   Initial draft.
--
-- = See Also
--
-- 'DeviceMemoryOverallocationCreateInfoAMD',
-- 'MemoryOverallocationBehaviorAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_memory_overallocation_behavior Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_memory_overallocation_behavior  (DeviceMemoryOverallocationCreateInfoAMD) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DeviceMemoryOverallocationCreateInfoAMD

instance ToCStruct DeviceMemoryOverallocationCreateInfoAMD
instance Show DeviceMemoryOverallocationCreateInfoAMD

instance FromCStruct DeviceMemoryOverallocationCreateInfoAMD

