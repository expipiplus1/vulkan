{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( withCStructExportSemaphoreCreateInfo
  , fromCStructExportSemaphoreCreateInfo
  , ExportSemaphoreCreateInfo(..)
  , SemaphoreImportFlagBits
  , pattern SEMAPHORE_IMPORT_TEMPORARY_BIT
  , SemaphoreImportFlagBitsKHR
  , SemaphoreImportFlags
  , SemaphoreImportFlagsKHR
  , pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkExportSemaphoreCreateInfo(..)
  , VkSemaphoreImportFlagBits(..)
  , pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreHandleTypeFlags
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  )



-- | VkExportSemaphoreCreateInfo - Structure specifying handle types that can
-- be exported from a semaphore
--
-- == Valid Usage
--
-- -   The bits in @handleTypes@ /must/ be supported and compatible, as
--     reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO'
--
-- -   @handleTypes@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreHandleTypeFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ExportSemaphoreCreateInfo = ExportSemaphoreCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ExportSemaphoreCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportSemaphoreCreateInfo" "handleTypes"
  handleTypes :: ExternalSemaphoreHandleTypeFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExportSemaphoreCreateInfo' and
-- marshal a 'ExportSemaphoreCreateInfo' into it. The 'VkExportSemaphoreCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExportSemaphoreCreateInfo :: ExportSemaphoreCreateInfo -> (VkExportSemaphoreCreateInfo -> IO a) -> IO a
withCStructExportSemaphoreCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ExportSemaphoreCreateInfo)) (\pPNext -> cont (VkExportSemaphoreCreateInfo VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO pPNext (handleTypes (marshalled :: ExportSemaphoreCreateInfo))))

-- | A function to read a 'VkExportSemaphoreCreateInfo' and all additional
-- structures in the pointer chain into a 'ExportSemaphoreCreateInfo'.
fromCStructExportSemaphoreCreateInfo :: VkExportSemaphoreCreateInfo -> IO ExportSemaphoreCreateInfo
fromCStructExportSemaphoreCreateInfo c = ExportSemaphoreCreateInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportSemaphoreCreateInfo)))
                                                                   <*> pure (vkHandleTypes (c :: VkExportSemaphoreCreateInfo))

instance Zero ExportSemaphoreCreateInfo where
  zero = ExportSemaphoreCreateInfo Nothing
                                   zero


-- | VkSemaphoreImportFlagBits - Bitmask specifying additional parameters of
-- semaphore payload import
--
-- = Description
--
-- These bits have the following meanings:
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlags'
type SemaphoreImportFlagBits = VkSemaphoreImportFlagBits


{-# complete SEMAPHORE_IMPORT_TEMPORARY_BIT :: SemaphoreImportFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VK_SEMAPHORE_IMPORT_TEMPORARY_BIT'
-- specifies that the semaphore payload will be imported only temporarily,
-- as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-importing Importing Semaphore Payloads>,
-- regardless of the permanence of @handleType@.
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT :: (a ~ SemaphoreImportFlagBits) => a
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT = VK_SEMAPHORE_IMPORT_TEMPORARY_BIT

-- No documentation found for TopLevel "SemaphoreImportFlagBitsKHR"
type SemaphoreImportFlagBitsKHR = SemaphoreImportFlagBits

-- | VkSemaphoreImportFlags - Bitmask of VkSemaphoreImportFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.VkImportSemaphoreWin32HandleInfoKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkSemaphoreImportFlagBits'
type SemaphoreImportFlags = SemaphoreImportFlagBits

-- No documentation found for TopLevel "SemaphoreImportFlagsKHR"
type SemaphoreImportFlagsKHR = SemaphoreImportFlags
