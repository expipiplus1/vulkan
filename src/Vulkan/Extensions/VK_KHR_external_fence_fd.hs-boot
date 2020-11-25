{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_fence_fd - device extension
--
-- = Registered Extension Number
--
-- 116
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_external_fence@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-05-08
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
--     -   Cass Everitt, Oculus
--
--     -   Contributors to @VK_KHR_external_semaphore_fd@
--
-- == Description
--
-- An application using external memory may wish to synchronize access to
-- that memory using fences. This extension enables an application to
-- export fence payload to and import fence payload from POSIX file
-- descriptors.
--
-- == New Commands
--
-- -   'getFenceFdKHR'
--
-- -   'importFenceFdKHR'
--
-- == New Structures
--
-- -   'FenceGetFdInfoKHR'
--
-- -   'ImportFenceFdInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_FENCE_FD_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR'
--
-- == Issues
--
-- This extension borrows concepts, semantics, and language from
-- @VK_KHR_external_semaphore_fd@. That extension’s issues apply equally to
-- this extension.
--
-- == Version History
--
-- -   Revision 1, 2017-05-08 (Jesse Hall)
--
--     -   Initial revision
--
-- = See Also
--
-- 'FenceGetFdInfoKHR', 'ImportFenceFdInfoKHR', 'getFenceFdKHR',
-- 'importFenceFdKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_fence_fd Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_fence_fd  ( FenceGetFdInfoKHR
                                                   , ImportFenceFdInfoKHR
                                                   ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data FenceGetFdInfoKHR

instance ToCStruct FenceGetFdInfoKHR
instance Show FenceGetFdInfoKHR

instance FromCStruct FenceGetFdInfoKHR


data ImportFenceFdInfoKHR

instance ToCStruct ImportFenceFdInfoKHR
instance Show ImportFenceFdInfoKHR

instance FromCStruct ImportFenceFdInfoKHR

