{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_fence - device extension
--
-- == VK_KHR_external_fence
--
-- [__Name String__]
--     @VK_KHR_external_fence@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     114
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_external_fence_capabilities@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_external_fence:%20&body=@critsec%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-05-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
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
--     -   Contributors to @VK_KHR_external_semaphore@
--
-- == Description
--
-- An application using external memory may wish to synchronize access to
-- that memory using fences. This extension enables an application to
-- create fences from which non-Vulkan handles that reference the
-- underlying synchronization primitive can be exported.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Fence.FenceCreateInfo':
--
--     -   'ExportFenceCreateInfoKHR'
--
-- == New Enums
--
-- -   'FenceImportFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'FenceImportFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_FENCE_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_FENCE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.FenceImportFlagBits.FenceImportFlagBits':
--
--     -   'FENCE_IMPORT_TEMPORARY_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR'
--
-- == Issues
--
-- This extension borrows concepts, semantics, and language from
-- @VK_KHR_external_semaphore@. That extension’s issues apply equally to
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
-- 'ExportFenceCreateInfoKHR', 'FenceImportFlagBitsKHR',
-- 'FenceImportFlagsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_fence Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_fence  ( pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR
                                                , pattern FENCE_IMPORT_TEMPORARY_BIT_KHR
                                                , FenceImportFlagsKHR
                                                , FenceImportFlagBitsKHR
                                                , ExportFenceCreateInfoKHR
                                                , KHR_EXTERNAL_FENCE_SPEC_VERSION
                                                , pattern KHR_EXTERNAL_FENCE_SPEC_VERSION
                                                , KHR_EXTERNAL_FENCE_EXTENSION_NAME
                                                , pattern KHR_EXTERNAL_FENCE_EXTENSION_NAME
                                                ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_external_fence (ExportFenceCreateInfo)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlagBits)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlags)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlags)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlagBits(FENCE_IMPORT_TEMPORARY_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO_KHR = STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO


-- No documentation found for TopLevel "VK_FENCE_IMPORT_TEMPORARY_BIT_KHR"
pattern FENCE_IMPORT_TEMPORARY_BIT_KHR = FENCE_IMPORT_TEMPORARY_BIT


-- No documentation found for TopLevel "VkFenceImportFlagsKHR"
type FenceImportFlagsKHR = FenceImportFlags


-- No documentation found for TopLevel "VkFenceImportFlagBitsKHR"
type FenceImportFlagBitsKHR = FenceImportFlagBits


-- No documentation found for TopLevel "VkExportFenceCreateInfoKHR"
type ExportFenceCreateInfoKHR = ExportFenceCreateInfo


type KHR_EXTERNAL_FENCE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_FENCE_SPEC_VERSION = 1


type KHR_EXTERNAL_FENCE_EXTENSION_NAME = "VK_KHR_external_fence"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_EXTENSION_NAME = "VK_KHR_external_fence"

