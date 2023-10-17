{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_semaphore_win32 - device extension
--
-- == VK_KHR_external_semaphore_win32
--
-- [__Name String__]
--     @VK_KHR_external_semaphore_win32@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     79
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_semaphore VK_KHR_external_semaphore>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_semaphore_win32] @cubanismo%0A*Here describe the issue or question you have about the VK_KHR_external_semaphore_win32 extension* >
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
-- An application using external memory may wish to synchronize access to
-- that memory using semaphores. This extension enables an application to
-- export semaphore payload to and import semaphore payload from Windows
-- handles.
--
-- == New Commands
--
-- -   'getSemaphoreWin32HandleKHR'
--
-- -   'importSemaphoreWin32HandleKHR'
--
-- == New Structures
--
-- -   'ImportSemaphoreWin32HandleInfoKHR'
--
-- -   'SemaphoreGetWin32HandleInfoKHR'
--
-- -   Extending 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo':
--
--     -   'ExportSemaphoreWin32HandleInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo':
--
--     -   'D3D12FenceSubmitInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR'
--
-- == Issues
--
-- 1) Do applications need to call @CloseHandle@() on the values returned
-- from 'getSemaphoreWin32HandleKHR' when @handleType@ is
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_capabilities.EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR'?
--
-- __RESOLVED__: Yes, unless it is passed back in to another driver
-- instance to import the object. A successful get call transfers ownership
-- of the handle to the application. Destroying the semaphore object will
-- not destroy the handle or the handle’s reference to the underlying
-- semaphore resource.
--
-- 2) Should the language regarding KMT\/Windows 7 handles be moved to a
-- separate extension so that it can be deprecated over time?
--
-- __RESOLVED__: No. Support for them can be deprecated by drivers if they
-- choose, by no longer returning them in the supported handle types of the
-- instance level queries.
--
-- 3) Should applications be allowed to specify additional object
-- attributes for shared handles?
--
-- __RESOLVED__: Yes. Applications will be allowed to provide similar
-- attributes to those they would to any other handle creation API.
--
-- 4) How do applications communicate the desired fence values to use with
-- @D3D12_FENCE@-based Vulkan semaphores?
--
-- __RESOLVED__: There are a couple of options. The values for the signaled
-- and reset states could be communicated up front when creating the object
-- and remain static for the life of the Vulkan semaphore, or they could be
-- specified using auxiliary structures when submitting semaphore signal
-- and wait operations, similar to what is done with the keyed mutex
-- extensions. The latter is more flexible and consistent with the keyed
-- mutex usage, but the former is a much simpler API.
--
-- Since Vulkan tends to favor flexibility and consistency over simplicity,
-- a new structure specifying D3D12 fence acquire and release values is
-- added to the 'Vulkan.Core10.Queue.queueSubmit' function.
--
-- == Version History
--
-- -   Revision 1, 2016-10-21 (James Jones)
--
--     -   Initial revision
--
-- == See Also
--
-- 'D3D12FenceSubmitInfoKHR', 'ExportSemaphoreWin32HandleInfoKHR',
-- 'ImportSemaphoreWin32HandleInfoKHR', 'SemaphoreGetWin32HandleInfoKHR',
-- 'getSemaphoreWin32HandleKHR', 'importSemaphoreWin32HandleKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_external_semaphore_win32 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_semaphore_win32  ( D3D12FenceSubmitInfoKHR
                                                          , ExportSemaphoreWin32HandleInfoKHR
                                                          , ImportSemaphoreWin32HandleInfoKHR
                                                          , SemaphoreGetWin32HandleInfoKHR
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data D3D12FenceSubmitInfoKHR

instance ToCStruct D3D12FenceSubmitInfoKHR
instance Show D3D12FenceSubmitInfoKHR

instance FromCStruct D3D12FenceSubmitInfoKHR


data ExportSemaphoreWin32HandleInfoKHR

instance ToCStruct ExportSemaphoreWin32HandleInfoKHR
instance Show ExportSemaphoreWin32HandleInfoKHR

instance FromCStruct ExportSemaphoreWin32HandleInfoKHR


data ImportSemaphoreWin32HandleInfoKHR

instance ToCStruct ImportSemaphoreWin32HandleInfoKHR
instance Show ImportSemaphoreWin32HandleInfoKHR

instance FromCStruct ImportSemaphoreWin32HandleInfoKHR


data SemaphoreGetWin32HandleInfoKHR

instance ToCStruct SemaphoreGetWin32HandleInfoKHR
instance Show SemaphoreGetWin32HandleInfoKHR

instance FromCStruct SemaphoreGetWin32HandleInfoKHR

