{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_fence_fd - device extension
--
-- == VK_KHR_external_fence_fd
--
-- [__Name String__]
--     @VK_KHR_external_fence_fd@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     116
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_fence VK_KHR_external_fence>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_fence_fd] @critsec%0A*Here describe the issue or question you have about the VK_KHR_external_fence_fd extension* >
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
-- == See Also
--
-- 'FenceGetFdInfoKHR', 'ImportFenceFdInfoKHR', 'getFenceFdKHR',
-- 'importFenceFdKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_external_fence_fd Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_fence_fd  ( FenceGetFdInfoKHR
                                                   , ImportFenceFdInfoKHR
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data FenceGetFdInfoKHR

instance ToCStruct FenceGetFdInfoKHR
instance Show FenceGetFdInfoKHR

instance FromCStruct FenceGetFdInfoKHR


data ImportFenceFdInfoKHR

instance ToCStruct ImportFenceFdInfoKHR
instance Show ImportFenceFdInfoKHR

instance FromCStruct ImportFenceFdInfoKHR

