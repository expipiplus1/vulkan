{-# language CPP #-}
-- | = Name
--
-- VK_FUCHSIA_external_semaphore - device extension
--
-- == VK_FUCHSIA_external_semaphore
--
-- [__Name String__]
--     @VK_FUCHSIA_external_semaphore@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     366
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
--     -   Requires @VK_KHR_external_semaphore@
--
-- [__Contact__]
--
--     -   John Rosasco
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_FUCHSIA_external_semaphore:%20&body=@rosasco%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-03-08
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
-- An application using external memory may wish to synchronize access to
-- that memory using semaphores. This extension enables an application to
-- export semaphore payload to and import semaphore payload from Zircon
-- event handles.
--
-- == New Commands
--
-- -   'getSemaphoreZirconHandleFUCHSIA'
--
-- -   'importSemaphoreZirconHandleFUCHSIA'
--
-- == New Structures
--
-- -   'ImportSemaphoreZirconHandleInfoFUCHSIA'
--
-- -   'SemaphoreGetZirconHandleInfoFUCHSIA'
--
-- == New Enum Constants
--
-- -   'FUCHSIA_EXTERNAL_SEMAPHORE_EXTENSION_NAME'
--
-- -   'FUCHSIA_EXTERNAL_SEMAPHORE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.ExternalSemaphoreHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits.EXTERNAL_SEMAPHORE_HANDLE_TYPE_ZIRCON_EVENT_BIT_FUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_SEMAPHORE_ZIRCON_HANDLE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_GET_ZIRCON_HANDLE_INFO_FUCHSIA'
--
-- == Issues
--
-- 1) Does the application need to close the Zircon event handle returned
-- by 'getSemaphoreZirconHandleFUCHSIA'?
--
-- __RESOLVED__: Yes, unless it is passed back in to a driver instance to
-- import the semaphore. A successful get call transfers ownership of the
-- Zircon event handle to the application, and a successful import
-- transfers it back to the driver. Destroying the original semaphore
-- object will not close the Zircon event handle nor remove its reference
-- to the underlying semaphore resource associated with it.
--
-- == Version History
--
-- -   Revision 1, 2021-03-08 (John Rosasco)
--
--     -   Initial revision
--
-- = See Also
--
-- 'ImportSemaphoreZirconHandleInfoFUCHSIA',
-- 'SemaphoreGetZirconHandleInfoFUCHSIA',
-- 'getSemaphoreZirconHandleFUCHSIA', 'importSemaphoreZirconHandleFUCHSIA'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_external_semaphore Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_FUCHSIA_external_semaphore  ( ImportSemaphoreZirconHandleInfoFUCHSIA
                                                        , SemaphoreGetZirconHandleInfoFUCHSIA
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImportSemaphoreZirconHandleInfoFUCHSIA

instance ToCStruct ImportSemaphoreZirconHandleInfoFUCHSIA
instance Show ImportSemaphoreZirconHandleInfoFUCHSIA

instance FromCStruct ImportSemaphoreZirconHandleInfoFUCHSIA


data SemaphoreGetZirconHandleInfoFUCHSIA

instance ToCStruct SemaphoreGetZirconHandleInfoFUCHSIA
instance Show SemaphoreGetZirconHandleInfoFUCHSIA

instance FromCStruct SemaphoreGetZirconHandleInfoFUCHSIA

