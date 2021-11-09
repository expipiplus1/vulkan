{-# language CPP #-}
-- | = Name
--
-- VK_FUCHSIA_external_memory - device extension
--
-- == VK_FUCHSIA_external_memory
--
-- [__Name String__]
--     @VK_FUCHSIA_external_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     365
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory_capabilities@
--
--     -   Requires @VK_KHR_external_memory@
--
-- [__Contact__]
--
--     -   John Rosasco
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_FUCHSIA_external_memory] @rosasco%0A<<Here describe the issue or question you have about the VK_FUCHSIA_external_memory extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-03-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Craig Stout, Google
--
--     -   John Bauman, Google
--
--     -   John Rosasco, Google
--
-- == Description
--
-- Vulkan apps may wish to export or import device memory handles to or
-- from other logical devices, instances or APIs.
--
-- This memory sharing can eliminate copies of memory buffers when
-- different subsystems need to interoperate on them. Sharing memory
-- buffers may also facilitate a better distribution of processing workload
-- for more complex memory manipulation pipelines.
--
-- == New Commands
--
-- -   'getMemoryZirconHandleFUCHSIA'
--
-- -   'getMemoryZirconHandlePropertiesFUCHSIA'
--
-- == New Structures
--
-- -   'MemoryGetZirconHandleInfoFUCHSIA'
--
-- -   'MemoryZirconHandlePropertiesFUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMemoryZirconHandleInfoFUCHSIA'
--
-- == New Enum Constants
--
-- -   'FUCHSIA_EXTERNAL_MEMORY_EXTENSION_NAME'
--
-- -   'FUCHSIA_EXTERNAL_MEMORY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ZIRCON_VMO_BIT_FUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_ZIRCON_HANDLE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_ZIRCON_HANDLE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_ZIRCON_HANDLE_PROPERTIES_FUCHSIA'
--
-- == Issues
--
-- See @VK_KHR_external_memory@ issues list for further information.
--
-- == Version History
--
-- -   Revision 1, 2021-03-01 (John Rosasco)
--
--     -   Initial draft
--
-- == See Also
--
-- 'ImportMemoryZirconHandleInfoFUCHSIA',
-- 'MemoryGetZirconHandleInfoFUCHSIA',
-- 'MemoryZirconHandlePropertiesFUCHSIA', 'getMemoryZirconHandleFUCHSIA',
-- 'getMemoryZirconHandlePropertiesFUCHSIA'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_external_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_FUCHSIA_external_memory  ( ImportMemoryZirconHandleInfoFUCHSIA
                                                     , MemoryGetZirconHandleInfoFUCHSIA
                                                     , MemoryZirconHandlePropertiesFUCHSIA
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImportMemoryZirconHandleInfoFUCHSIA

instance ToCStruct ImportMemoryZirconHandleInfoFUCHSIA
instance Show ImportMemoryZirconHandleInfoFUCHSIA

instance FromCStruct ImportMemoryZirconHandleInfoFUCHSIA


data MemoryGetZirconHandleInfoFUCHSIA

instance ToCStruct MemoryGetZirconHandleInfoFUCHSIA
instance Show MemoryGetZirconHandleInfoFUCHSIA

instance FromCStruct MemoryGetZirconHandleInfoFUCHSIA


data MemoryZirconHandlePropertiesFUCHSIA

instance ToCStruct MemoryZirconHandlePropertiesFUCHSIA
instance Show MemoryZirconHandlePropertiesFUCHSIA

instance FromCStruct MemoryZirconHandlePropertiesFUCHSIA

