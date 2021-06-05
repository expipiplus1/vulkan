{-# language CPP #-}
-- | = Name
--
-- VK_NVX_binary_import - device extension
--
-- == VK_NVX_binary_import
--
-- [__Name String__]
--     @VK_NVX_binary_import@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     30
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
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NVX_binary_import:%20&body=@ewerness%20 >
--
--     -   Liam Middlebrook
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NVX_binary_import:%20&body=@liam-middlebrook%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-04-09
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Liam Middlebrook, NVIDIA
--
-- == Description
--
-- This extension allows applications to import CuBIN binaries and execute
-- them.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.CuFunctionNVX'
--
-- -   'Vulkan.Extensions.Handles.CuModuleNVX'
--
-- == New Commands
--
-- -   'cmdCuLaunchKernelNVX'
--
-- -   'createCuFunctionNVX'
--
-- -   'createCuModuleNVX'
--
-- -   'destroyCuFunctionNVX'
--
-- -   'destroyCuModuleNVX'
--
-- == New Structures
--
-- -   'CuFunctionCreateInfoNVX'
--
-- -   'CuLaunchInfoNVX'
--
-- -   'CuModuleCreateInfoNVX'
--
-- == New Enum Constants
--
-- -   'NVX_BINARY_IMPORT_EXTENSION_NAME'
--
-- -   'NVX_BINARY_IMPORT_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_CU_FUNCTION_NVX_EXT'
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_CU_MODULE_NVX_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_CU_FUNCTION_NVX'
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_CU_MODULE_NVX'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CU_FUNCTION_CREATE_INFO_NVX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CU_LAUNCH_INFO_NVX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CU_MODULE_CREATE_INFO_NVX'
--
-- == Version History
--
-- -   Revision 1, 2021-04-09 (Eric Werness)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'CuFunctionCreateInfoNVX', 'Vulkan.Extensions.Handles.CuFunctionNVX',
-- 'CuLaunchInfoNVX', 'CuModuleCreateInfoNVX',
-- 'Vulkan.Extensions.Handles.CuModuleNVX', 'cmdCuLaunchKernelNVX',
-- 'createCuFunctionNVX', 'createCuModuleNVX', 'destroyCuFunctionNVX',
-- 'destroyCuModuleNVX'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_binary_import Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NVX_binary_import  ( CuFunctionCreateInfoNVX
                                               , CuLaunchInfoNVX
                                               , CuModuleCreateInfoNVX
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CuFunctionCreateInfoNVX

instance ToCStruct CuFunctionCreateInfoNVX
instance Show CuFunctionCreateInfoNVX

instance FromCStruct CuFunctionCreateInfoNVX


data CuLaunchInfoNVX

instance ToCStruct CuLaunchInfoNVX
instance Show CuLaunchInfoNVX

instance FromCStruct CuLaunchInfoNVX


data CuModuleCreateInfoNVX

instance ToCStruct CuModuleCreateInfoNVX
instance Show CuModuleCreateInfoNVX

instance FromCStruct CuModuleCreateInfoNVX

