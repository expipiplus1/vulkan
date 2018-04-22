{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore
  ( pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME
  , VkSemaphoreImportFlagBitsKHR
  , VkSemaphoreImportFlagsKHR
  , VkExportSemaphoreCreateInfoKHR
  , pattern VkExportSemaphoreCreateInfoKHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR
  , pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkExportSemaphoreCreateInfo(..)
  , VkSemaphoreImportFlagBits(..)
  , VkSemaphoreImportFlags
  , pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlags
  )


-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME = "VK_KHR_external_semaphore"
-- No documentation found for TopLevel "VkSemaphoreImportFlagBitsKHR"
type VkSemaphoreImportFlagBitsKHR = VkSemaphoreImportFlagBits
-- No documentation found for TopLevel "VkSemaphoreImportFlagsKHR"
type VkSemaphoreImportFlagsKHR = VkSemaphoreImportFlags
-- No documentation found for TopLevel "VkExportSemaphoreCreateInfoKHR"
type VkExportSemaphoreCreateInfoKHR = VkExportSemaphoreCreateInfo


-- No documentation found for TopLevel "VkExportSemaphoreCreateInfoKHR"
pattern VkExportSemaphoreCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalSemaphoreHandleTypeFlags) -> VkExportSemaphoreCreateInfoKHR
pattern VkExportSemaphoreCreateInfoKHR vkSType vkPNext vkHandleTypes = VkExportSemaphoreCreateInfo vkSType vkPNext vkHandleTypes
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
-- No documentation found for TopLevel "VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR"
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR :: VkSemaphoreImportFlagBits
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR = VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
