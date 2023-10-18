{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_semaphore_fd - device extension
--
-- == VK_KHR_external_semaphore_fd
--
-- [__Name String__]
--     @VK_KHR_external_semaphore_fd@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     80
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_semaphore VK_KHR_external_semaphore>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_semaphore_fd] @cubanismo%0A*Here describe the issue or question you have about the VK_KHR_external_semaphore_fd extension* >
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
--     -   Jesse Hall, Google
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Carsten Rohde, NVIDIA
--
-- == Description
--
-- An application using external memory may wish to synchronize access to
-- that memory using semaphores. This extension enables an application to
-- export semaphore payload to and import semaphore payload from POSIX file
-- descriptors.
--
-- == New Commands
--
-- -   'getSemaphoreFdKHR'
--
-- -   'importSemaphoreFdKHR'
--
-- == New Structures
--
-- -   'ImportSemaphoreFdInfoKHR'
--
-- -   'SemaphoreGetFdInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR'
--
-- == Issues
--
-- 1) Does the application need to close the file descriptor returned by
-- 'getSemaphoreFdKHR'?
--
-- __RESOLVED__: Yes, unless it is passed back in to a driver instance to
-- import the semaphore. A successful get call transfers ownership of the
-- file descriptor to the application, and a successful import transfers it
-- back to the driver. Destroying the original semaphore object will not
-- close the file descriptor or remove its reference to the underlying
-- semaphore resource associated with it.
--
-- == Version History
--
-- -   Revision 1, 2016-10-21 (Jesse Hall)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ImportSemaphoreFdInfoKHR', 'SemaphoreGetFdInfoKHR',
-- 'getSemaphoreFdKHR', 'importSemaphoreFdKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_external_semaphore_fd Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_semaphore_fd  ( ImportSemaphoreFdInfoKHR
                                                       , SemaphoreGetFdInfoKHR
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImportSemaphoreFdInfoKHR

instance ToCStruct ImportSemaphoreFdInfoKHR
instance Show ImportSemaphoreFdInfoKHR

instance FromCStruct ImportSemaphoreFdInfoKHR


data SemaphoreGetFdInfoKHR

instance ToCStruct SemaphoreGetFdInfoKHR
instance Show SemaphoreGetFdInfoKHR

instance FromCStruct SemaphoreGetFdInfoKHR

