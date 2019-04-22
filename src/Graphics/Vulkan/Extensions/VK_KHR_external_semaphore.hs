{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore
  ( ExportSemaphoreCreateInfoKHR
  , pattern KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
  , pattern KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION
  , pattern SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR
  , pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR
  , SemaphoreImportFlagsKHR
  , SemaphoreImportFlagBitsKHR
  , pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  , pattern SEMAPHORE_IMPORT_TEMPORARY_BIT
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkSemaphoreImportFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore
  ( pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( ExportSemaphoreCreateInfo(..)
  , pattern SEMAPHORE_IMPORT_TEMPORARY_BIT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( SemaphoreImportFlagBitsKHR
  , SemaphoreImportFlagsKHR
  )


type ExportSemaphoreCreateInfoKHR = ExportSemaphoreCreateInfo
-- TODO: Pattern constructor alias)

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME = VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION

-- No documentation found for TopLevel "SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR"
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR :: VkSemaphoreImportFlagBits
pattern SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR = SEMAPHORE_IMPORT_TEMPORARY_BIT

-- No documentation found for TopLevel "STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR = STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
