{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( withCStructExportSemaphoreCreateInfo
  , fromCStructExportSemaphoreCreateInfo
  , ExportSemaphoreCreateInfo(..)
  , SemaphoreImportFlagBits
  , SemaphoreImportFlagBitsKHR
  , SemaphoreImportFlags
  , SemaphoreImportFlagsKHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
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


-- No documentation found for TopLevel "ExportSemaphoreCreateInfo"
data ExportSemaphoreCreateInfo = ExportSemaphoreCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ExportSemaphoreCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ExportSemaphoreCreateInfo" "handleTypes"
  vkHandleTypes :: ExternalSemaphoreHandleTypeFlags
  }
  deriving (Show, Eq)
withCStructExportSemaphoreCreateInfo :: ExportSemaphoreCreateInfo -> (VkExportSemaphoreCreateInfo -> IO a) -> IO a
withCStructExportSemaphoreCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: ExportSemaphoreCreateInfo)) (\pPNext -> cont (VkExportSemaphoreCreateInfo VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO pPNext (vkHandleTypes (from :: ExportSemaphoreCreateInfo))))
fromCStructExportSemaphoreCreateInfo :: VkExportSemaphoreCreateInfo -> IO ExportSemaphoreCreateInfo
fromCStructExportSemaphoreCreateInfo c = ExportSemaphoreCreateInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkExportSemaphoreCreateInfo)))
                                                                   <*> pure (vkHandleTypes (c :: VkExportSemaphoreCreateInfo))
instance Zero ExportSemaphoreCreateInfo where
  zero = ExportSemaphoreCreateInfo Nothing
                                   zero
-- No documentation found for TopLevel "SemaphoreImportFlagBits"
type SemaphoreImportFlagBits = VkSemaphoreImportFlagBits
-- No documentation found for TopLevel "SemaphoreImportFlagBitsKHR"
type SemaphoreImportFlagBitsKHR = SemaphoreImportFlagBits
-- No documentation found for TopLevel "SemaphoreImportFlags"
type SemaphoreImportFlags = SemaphoreImportFlagBits
-- No documentation found for TopLevel "SemaphoreImportFlagsKHR"
type SemaphoreImportFlagsKHR = SemaphoreImportFlags
