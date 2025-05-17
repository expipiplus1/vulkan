{-# language CPP #-}
-- | = Name
--
-- VK_LUNARG_direct_driver_loading - instance extension
--
-- == VK_LUNARG_direct_driver_loading
--
-- [__Name String__]
--     @VK_LUNARG_direct_driver_loading@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     460
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
--     -   Charles Giessen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_LUNARG_direct_driver_loading] @charles-lunarg%0A*Here describe the issue or question you have about the VK_LUNARG_direct_driver_loading extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_LUNARG_direct_driver_loading.adoc VK_LUNARG_direct_driver_loading>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-11-29
--
-- [__Contributors__]
--
--     -   Charles Giessen, LunarG
--
-- == Description
--
-- This extension provides a mechanism for applications to add drivers to
-- the implementation. This allows drivers to be included with an
-- application without requiring installation and is capable of being used
-- in any execution environment, such as a process running with elevated
-- privileges.
--
-- == New Structures
--
-- -   'DirectDriverLoadingInfoLUNARG'
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'DirectDriverLoadingListLUNARG'
--
-- == New Function Pointers
--
-- -   'PFN_vkGetInstanceProcAddrLUNARG'
--
-- == New Enums
--
-- -   'DirectDriverLoadingModeLUNARG'
--
-- == New Bitmasks
--
-- -   'DirectDriverLoadingFlagsLUNARG'
--
-- == New Enum Constants
--
-- -   'LUNARG_DIRECT_DRIVER_LOADING_EXTENSION_NAME'
--
-- -   'LUNARG_DIRECT_DRIVER_LOADING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_INFO_LUNARG'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_LIST_LUNARG'
--
-- == Version History
--
-- -   Revision 1, 2022-11-29 (Charles Giessen)
--
--     -   Initial version
--
-- == See Also
--
-- 'PFN_vkGetInstanceProcAddrLUNARG', 'DirectDriverLoadingFlagsLUNARG',
-- 'DirectDriverLoadingInfoLUNARG', 'DirectDriverLoadingListLUNARG',
-- 'DirectDriverLoadingModeLUNARG'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_LUNARG_direct_driver_loading Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_LUNARG_direct_driver_loading  ( DirectDriverLoadingInfoLUNARG
                                                          , DirectDriverLoadingListLUNARG
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DirectDriverLoadingInfoLUNARG

instance ToCStruct DirectDriverLoadingInfoLUNARG
instance Show DirectDriverLoadingInfoLUNARG

instance FromCStruct DirectDriverLoadingInfoLUNARG


data DirectDriverLoadingListLUNARG

instance ToCStruct DirectDriverLoadingListLUNARG
instance Show DirectDriverLoadingListLUNARG

instance FromCStruct DirectDriverLoadingListLUNARG

