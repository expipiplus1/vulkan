{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory
  ( pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  , VkExternalMemoryImageCreateInfoKHR
  , pattern VkExternalMemoryImageCreateInfoKHR
  , VkExternalMemoryBufferCreateInfoKHR
  , pattern VkExternalMemoryBufferCreateInfoKHR
  , VkExportMemoryAllocateInfoKHR
  , pattern VkExportMemoryAllocateInfoKHR
  , pattern VK_QUEUE_FAMILY_EXTERNAL_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR
  , pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory
  ( pattern VK_ERROR_INVALID_EXTERNAL_HANDLE
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_QUEUE_FAMILY_EXTERNAL
  , VkExportMemoryAllocateInfo(..)
  , VkExternalMemoryBufferCreateInfo(..)
  , VkExternalMemoryImageCreateInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlags
  )


pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1
pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_KHR_external_memory"
type VkExternalMemoryImageCreateInfoKHR = VkExternalMemoryImageCreateInfo


pattern VkExternalMemoryImageCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalMemoryHandleTypeFlags) -> VkExternalMemoryImageCreateInfoKHR
pattern VkExternalMemoryImageCreateInfoKHR vkSType vkPNext vkHandleTypes = VkExternalMemoryImageCreateInfo vkSType vkPNext vkHandleTypes
type VkExternalMemoryBufferCreateInfoKHR = VkExternalMemoryBufferCreateInfo


pattern VkExternalMemoryBufferCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalMemoryHandleTypeFlags) -> VkExternalMemoryBufferCreateInfoKHR
pattern VkExternalMemoryBufferCreateInfoKHR vkSType vkPNext vkHandleTypes = VkExternalMemoryBufferCreateInfo vkSType vkPNext vkHandleTypes
type VkExportMemoryAllocateInfoKHR = VkExportMemoryAllocateInfo


pattern VkExportMemoryAllocateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalMemoryHandleTypeFlags) -> VkExportMemoryAllocateInfoKHR
pattern VkExportMemoryAllocateInfoKHR vkSType vkPNext vkHandleTypes = VkExportMemoryAllocateInfo vkSType vkPNext vkHandleTypes
pattern VK_QUEUE_FAMILY_EXTERNAL_KHR :: Word32
pattern VK_QUEUE_FAMILY_EXTERNAL_KHR = VK_QUEUE_FAMILY_EXTERNAL
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR = VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR :: VkResult
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR = VK_ERROR_INVALID_EXTERNAL_HANDLE
