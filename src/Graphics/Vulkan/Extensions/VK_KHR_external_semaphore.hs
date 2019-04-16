{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore
  ( ExportSemaphoreCreateInfoKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  , SemaphoreImportFlagsKHR
  , SemaphoreImportFlagBitsKHR
  , pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR
  , pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
  ) where




import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( ExportSemaphoreCreateInfo(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore
  ( pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION
  , pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( SemaphoreImportFlagBitsKHR
  , SemaphoreImportFlagsKHR
  )


type ExportSemaphoreCreateInfoKHR = ExportSemaphoreCreateInfo
-- TODO: Pattern constructor alias)
