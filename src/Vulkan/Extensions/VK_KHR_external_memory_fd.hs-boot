{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_memory_fd - device extension
--
-- == VK_KHR_external_memory_fd
--
-- [__Name String__]
--     @VK_KHR_external_memory_fd@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     75
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_memory_fd] @cubanismo%0A*Here describe the issue or question you have about the VK_KHR_external_memory_fd extension* >
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
-- == Description
--
-- An application may wish to reference device memory in multiple Vulkan
-- logical devices or instances, in multiple processes, and\/or in multiple
-- APIs. This extension enables an application to export POSIX file
-- descriptor handles from Vulkan memory objects and to import Vulkan
-- memory objects from POSIX file descriptor handles exported from other
-- Vulkan memory objects or from similar resources in other APIs.
--
-- == New Commands
--
-- -   'getMemoryFdKHR'
--
-- -   'getMemoryFdPropertiesKHR'
--
-- == New Structures
--
-- -   'MemoryFdPropertiesKHR'
--
-- -   'MemoryGetFdInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMemoryFdInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_MEMORY_FD_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_MEMORY_FD_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR'
--
-- == Issues
--
-- 1) Does the application need to close the file descriptor returned by
-- 'getMemoryFdKHR'?
--
-- __RESOLVED__: Yes, unless it is passed back in to a driver instance to
-- import the memory. A successful get call transfers ownership of the file
-- descriptor to the application, and a successful import transfers it back
-- to the driver. Destroying the original memory object will not close the
-- file descriptor or remove its reference to the underlying memory
-- resource associated with it.
--
-- 2) Do drivers ever need to expose multiple file descriptors per memory
-- object?
--
-- __RESOLVED__: No. This would indicate there are actually multiple memory
-- objects, rather than a single memory object.
--
-- 3) How should the valid size and memory type for POSIX file descriptor
-- memory handles created outside of Vulkan be specified?
--
-- __RESOLVED__: The valid memory types are queried directly from the
-- external handle. The size will be specified by future extensions that
-- introduce such external memory handle types.
--
-- == Version History
--
-- -   Revision 1, 2016-10-21 (James Jones)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ImportMemoryFdInfoKHR', 'MemoryFdPropertiesKHR', 'MemoryGetFdInfoKHR',
-- 'getMemoryFdKHR', 'getMemoryFdPropertiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_external_memory_fd Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_memory_fd  ( ImportMemoryFdInfoKHR
                                                    , MemoryFdPropertiesKHR
                                                    , MemoryGetFdInfoKHR
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImportMemoryFdInfoKHR

instance ToCStruct ImportMemoryFdInfoKHR
instance Show ImportMemoryFdInfoKHR

instance FromCStruct ImportMemoryFdInfoKHR


data MemoryFdPropertiesKHR

instance ToCStruct MemoryFdPropertiesKHR
instance Show MemoryFdPropertiesKHR

instance FromCStruct MemoryFdPropertiesKHR


data MemoryGetFdInfoKHR

instance ToCStruct MemoryGetFdInfoKHR
instance Show MemoryGetFdInfoKHR

instance FromCStruct MemoryGetFdInfoKHR

