{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_fence_win32 - device extension
--
-- == VK_KHR_external_fence_win32
--
-- [__Name String__]
--     @VK_KHR_external_fence_win32@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     115
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_external_fence@
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_fence_win32] @critsec%0A<<Here describe the issue or question you have about the VK_KHR_external_fence_win32 extension>> >
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
--     -   Contributors to @VK_KHR_external_semaphore_win32@
--
-- == Description
--
-- An application using external memory may wish to synchronize access to
-- that memory using fences. This extension enables an application to
-- export fence payload to and import fence payload from Windows handles.
--
-- == New Commands
--
-- -   'getFenceWin32HandleKHR'
--
-- -   'importFenceWin32HandleKHR'
--
-- == New Structures
--
-- -   'FenceGetWin32HandleInfoKHR'
--
-- -   'ImportFenceWin32HandleInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Fence.FenceCreateInfo':
--
--     -   'ExportFenceWin32HandleInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR'
--
-- == Issues
--
-- This extension borrows concepts, semantics, and language from
-- @VK_KHR_external_semaphore_win32@. That extension’s issues apply equally
-- to this extension.
--
-- 1) Should D3D12 fence handle types be supported, like they are for
-- semaphores?
--
-- __RESOLVED__: No. Doing so would require extending the fence signal and
-- wait operations to provide values to signal \/ wait for, like
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.D3D12FenceSubmitInfoKHR'
-- does. A D3D12 fence can be signaled by importing it into a
-- 'Vulkan.Core10.Handles.Semaphore' instead of a
-- 'Vulkan.Core10.Handles.Fence', and applications can check status or wait
-- on the D3D12 fence using non-Vulkan APIs. The convenience of being able
-- to do these operations on 'Vulkan.Core10.Handles.Fence' objects does not
-- justify the extra API complexity.
--
-- == Version History
--
-- -   Revision 1, 2017-05-08 (Jesse Hall)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ExportFenceWin32HandleInfoKHR', 'FenceGetWin32HandleInfoKHR',
-- 'ImportFenceWin32HandleInfoKHR', 'getFenceWin32HandleKHR',
-- 'importFenceWin32HandleKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_external_fence_win32 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_fence_win32  ( ExportFenceWin32HandleInfoKHR
                                                      , FenceGetWin32HandleInfoKHR
                                                      , ImportFenceWin32HandleInfoKHR
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExportFenceWin32HandleInfoKHR

instance ToCStruct ExportFenceWin32HandleInfoKHR
instance Show ExportFenceWin32HandleInfoKHR

instance FromCStruct ExportFenceWin32HandleInfoKHR


data FenceGetWin32HandleInfoKHR

instance ToCStruct FenceGetWin32HandleInfoKHR
instance Show FenceGetWin32HandleInfoKHR

instance FromCStruct FenceGetWin32HandleInfoKHR


data ImportFenceWin32HandleInfoKHR

instance ToCStruct ImportFenceWin32HandleInfoKHR
instance Show ImportFenceWin32HandleInfoKHR

instance FromCStruct ImportFenceWin32HandleInfoKHR

