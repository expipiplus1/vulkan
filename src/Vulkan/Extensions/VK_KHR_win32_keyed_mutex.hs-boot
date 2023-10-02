{-# language CPP #-}
-- | = Name
--
-- VK_KHR_win32_keyed_mutex - device extension
--
-- == VK_KHR_win32_keyed_mutex
--
-- [__Name String__]
--     @VK_KHR_win32_keyed_mutex@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     76
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_win32 VK_KHR_external_memory_win32>
--
-- [__Contact__]
--
--     -   Carsten Rohde
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_win32_keyed_mutex] @crohde%0A*Here describe the issue or question you have about the VK_KHR_win32_keyed_mutex extension* >
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
-- Applications that wish to import Direct3D 11 memory objects into the
-- Vulkan API may wish to use the native keyed mutex mechanism to
-- synchronize access to the memory between Vulkan and Direct3D. This
-- extension provides a way for an application to access the keyed mutex
-- associated with an imported Vulkan memory object when submitting command
-- buffers to a queue.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.SubmitInfo2':
--
--     -   'Win32KeyedMutexAcquireReleaseInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_WIN32_KEYED_MUTEX_EXTENSION_NAME'
--
-- -   'KHR_WIN32_KEYED_MUTEX_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2016-10-21 (James Jones)
--
--     -   Initial revision
--
-- == See Also
--
-- 'Win32KeyedMutexAcquireReleaseInfoKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_win32_keyed_mutex Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_win32_keyed_mutex  (Win32KeyedMutexAcquireReleaseInfoKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data Win32KeyedMutexAcquireReleaseInfoKHR

instance ToCStruct Win32KeyedMutexAcquireReleaseInfoKHR
instance Show Win32KeyedMutexAcquireReleaseInfoKHR

instance FromCStruct Win32KeyedMutexAcquireReleaseInfoKHR

