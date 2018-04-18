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
  ( pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  , VkExportSemaphoreCreateInfo(..)
  , VkSemaphoreImportFlags
  , VkSemaphoreImportFlagBits(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlags
  )


pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_SPEC_VERSION = 1
pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_EXTENSION_NAME = "VK_KHR_external_semaphore"
type VkSemaphoreImportFlagBitsKHR = VkSemaphoreImportFlagBits
type VkSemaphoreImportFlagsKHR = VkSemaphoreImportFlags
type VkExportSemaphoreCreateInfoKHR = VkExportSemaphoreCreateInfo


pattern VkExportSemaphoreCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalSemaphoreHandleTypeFlags) -> VkExportSemaphoreCreateInfoKHR
pattern VkExportSemaphoreCreateInfoKHR vkSType vkNext vkHandleTypes = VkExportSemaphoreCreateInfo vkSType vkNext vkHandleTypes
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR :: VkSemaphoreImportFlagBits
pattern VK_SEMAPHORE_IMPORT_TEMPORARY_BIT_KHR = VK_SEMAPHORE_IMPORT_TEMPORARY_BIT
