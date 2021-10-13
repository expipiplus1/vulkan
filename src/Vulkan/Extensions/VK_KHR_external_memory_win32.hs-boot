{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_memory_win32 - device extension
--
-- == VK_KHR_external_memory_win32
--
-- [__Name String__]
--     @VK_KHR_external_memory_win32@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     74
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory@
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_memory_win32] @cubanismo%0A<<Here describe the issue or question you have about the VK_KHR_external_memory_win32 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-10-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Carsten Rohde, NVIDIA
--
-- == Description
--
-- An application may wish to reference device memory in multiple Vulkan
-- logical devices or instances, in multiple processes, and\/or in multiple
-- APIs. This extension enables an application to export Windows handles
-- from Vulkan memory objects and to import Vulkan memory objects from
-- Windows handles exported from other Vulkan memory objects or from
-- similar resources in other APIs.
--
-- == New Commands
--
-- -   'getMemoryWin32HandleKHR'
--
-- -   'getMemoryWin32HandlePropertiesKHR'
--
-- == New Structures
--
-- -   'MemoryGetWin32HandleInfoKHR'
--
-- -   'MemoryWin32HandlePropertiesKHR'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ExportMemoryWin32HandleInfoKHR'
--
--     -   'ImportMemoryWin32HandleInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR'
--
-- == Issues
--
-- 1) Do applications need to call @CloseHandle@() on the values returned
-- from 'getMemoryWin32HandleKHR' when @handleType@ is
-- 'Vulkan.Extensions.VK_KHR_external_memory_capabilities.EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR'?
--
-- __RESOLVED__: Yes, unless it is passed back in to another driver
-- instance to import the object. A successful get call transfers ownership
-- of the handle to the application. Destroying the memory object will not
-- destroy the handle or the handle’s reference to the underlying memory
-- resource.
--
-- 2) Should the language regarding KMT\/Windows 7 handles be moved to a
-- separate extension so that it can be deprecated over time?
--
-- __RESOLVED__: No. Support for them can be deprecated by drivers if they
-- choose, by no longer returning them in the supported handle types of the
-- instance level queries.
--
-- 3) How should the valid size and memory type for windows memory handles
-- created outside of Vulkan be specified?
--
-- __RESOLVED__: The valid memory types are queried directly from the
-- external handle. The size is determined by the associated image or
-- buffer memory requirements for external handle types that require
-- dedicated allocations, and by the size specified when creating the
-- object from which the handle was exported for other external handle
-- types.
--
-- == Version History
--
-- -   Revision 1, 2016-10-21 (James Jones)
--
--     -   Initial revision
--
-- = See Also
--
-- 'ExportMemoryWin32HandleInfoKHR', 'ImportMemoryWin32HandleInfoKHR',
-- 'MemoryGetWin32HandleInfoKHR', 'MemoryWin32HandlePropertiesKHR',
-- 'getMemoryWin32HandleKHR', 'getMemoryWin32HandlePropertiesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_win32 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_memory_win32  ( ExportMemoryWin32HandleInfoKHR
                                                       , ImportMemoryWin32HandleInfoKHR
                                                       , MemoryGetWin32HandleInfoKHR
                                                       , MemoryWin32HandlePropertiesKHR
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExportMemoryWin32HandleInfoKHR

instance ToCStruct ExportMemoryWin32HandleInfoKHR
instance Show ExportMemoryWin32HandleInfoKHR

instance FromCStruct ExportMemoryWin32HandleInfoKHR


data ImportMemoryWin32HandleInfoKHR

instance ToCStruct ImportMemoryWin32HandleInfoKHR
instance Show ImportMemoryWin32HandleInfoKHR

instance FromCStruct ImportMemoryWin32HandleInfoKHR


data MemoryGetWin32HandleInfoKHR

instance ToCStruct MemoryGetWin32HandleInfoKHR
instance Show MemoryGetWin32HandleInfoKHR

instance FromCStruct MemoryGetWin32HandleInfoKHR


data MemoryWin32HandlePropertiesKHR

instance ToCStruct MemoryWin32HandlePropertiesKHR
instance Show MemoryWin32HandlePropertiesKHR

instance FromCStruct MemoryWin32HandlePropertiesKHR

