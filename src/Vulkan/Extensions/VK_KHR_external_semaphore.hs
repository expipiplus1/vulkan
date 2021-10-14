{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_semaphore - device extension
--
-- == VK_KHR_external_semaphore
--
-- [__Name String__]
--     @VK_KHR_external_semaphore@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     78
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_external_semaphore_capabilities@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_semaphore] @cubanismo%0A<<Here describe the issue or question you have about the VK_KHR_external_semaphore extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-10-21
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
--     -   Jason Ekstrand, Intel
--
--     -   Jesse Hall, Google
--
--     -   Tobias Hector, Imagination Technologies
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Ray Smith, ARM
--
--     -   Chad Versace, Google
--
-- == Description
--
-- An application using external memory may wish to synchronize access to
-- that memory using semaphores. This extension enables an application to
-- create semaphores from which non-Vulkan handles that reference the
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
-- -   Extending 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo':
--
--     -   'ExportSemaphoreCreateInfoKHR'
--
-- == New Enums
--
-- -   'SemaphoreImportFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'SemaphoreImportFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.SemaphoreImportFlagBits.SemaphoreImportFlagBits':
--
--     -   'SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Should there be restrictions on what side effects can occur when
-- waiting on imported semaphores that are in an invalid state?
--
-- __RESOLVED__: Yes. Normally, validating such state would be the
-- responsibility of the application, and the implementation would be free
-- to enter an undefined state if valid usage rules were violated. However,
-- this could cause security concerns when using imported semaphores, as it
-- would require the importing application to trust the exporting
-- application to ensure the state is valid. Requiring this level of trust
-- is undesirable for many potential use cases.
--
-- 2) Must implementations validate external handles the application
-- provides as input to semaphore state import operations?
--
-- __RESOLVED__: Implementations must return an error to the application if
-- the provided semaphore state handle cannot be used to complete the
-- requested import operation. However, implementations need not validate
-- handles are of the exact type specified by the application.
--
-- == Version History
--
-- -   Revision 1, 2016-10-21 (James Jones)
--
--     -   Initial revision
--
-- = See Also
--
-- 'ExportSemaphoreCreateInfoKHR', 'SemaphoreImportFlagBitsKHR',
-- 'SemaphoreImportFlagsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_semaphore Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_semaphore  ( pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR
                                                    , pattern SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR
                                                    , SemaphoreImportFlagsKHR
                                                    , SemaphoreImportFlagBitsKHR
                                                    , ExportSemaphoreCreateInfoKHR
                                                    , KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION
                                                    , pattern KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION
                                                    , KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
                                                    , pattern KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
                                                    ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore (ExportSemaphoreCreateInfo)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlagBits)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlagBits(SEMAPHORE_IMPORT_TEMPORARY_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR = STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO


-- No documentation found for TopLevel "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR"
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR = SEMAPHORE_IMPORT_TEMPORARY_BIT


-- No documentation found for TopLevel "VkSemaphoreImportFlagsKHR"
type SemaphoreImportFlagsKHR = SemaphoreImportFlags


-- No documentation found for TopLevel "VkSemaphoreImportFlagBitsKHR"
type SemaphoreImportFlagBitsKHR = SemaphoreImportFlagBits


-- No documentation found for TopLevel "VkExportSemaphoreCreateInfoKHR"
type ExportSemaphoreCreateInfoKHR = ExportSemaphoreCreateInfo


type KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1


type KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME = "VK_KHR_external_semaphore"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME = "VK_KHR_external_semaphore"

