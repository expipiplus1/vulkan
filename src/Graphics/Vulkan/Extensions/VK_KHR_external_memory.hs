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
  ( VkExportMemoryAllocateInfo(..)
  , VkExternalMemoryBufferCreateInfo(..)
  , VkExternalMemoryImageCreateInfo(..)
  , pattern VK_ERROR_INVALID_EXTERNAL_HANDLE
  , pattern VK_QUEUE_FAMILY_EXTERNAL
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlags
  )


-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_KHR_external_memory"
-- No documentation found for TopLevel "VkExternalMemoryImageCreateInfoKHR"
type VkExternalMemoryImageCreateInfoKHR = VkExternalMemoryImageCreateInfo


-- No documentation found for TopLevel "VkExternalMemoryImageCreateInfoKHR"
pattern VkExternalMemoryImageCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalMemoryHandleTypeFlags) -> VkExternalMemoryImageCreateInfoKHR
pattern VkExternalMemoryImageCreateInfoKHR vkSType vkPNext vkHandleTypes = VkExternalMemoryImageCreateInfo vkSType vkPNext vkHandleTypes
-- No documentation found for TopLevel "VkExternalMemoryBufferCreateInfoKHR"
type VkExternalMemoryBufferCreateInfoKHR = VkExternalMemoryBufferCreateInfo


-- No documentation found for TopLevel "VkExternalMemoryBufferCreateInfoKHR"
pattern VkExternalMemoryBufferCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalMemoryHandleTypeFlags) -> VkExternalMemoryBufferCreateInfoKHR
pattern VkExternalMemoryBufferCreateInfoKHR vkSType vkPNext vkHandleTypes = VkExternalMemoryBufferCreateInfo vkSType vkPNext vkHandleTypes
-- No documentation found for TopLevel "VkExportMemoryAllocateInfoKHR"
type VkExportMemoryAllocateInfoKHR = VkExportMemoryAllocateInfo


-- No documentation found for TopLevel "VkExportMemoryAllocateInfoKHR"
pattern VkExportMemoryAllocateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleTypes" ::: VkExternalMemoryHandleTypeFlags) -> VkExportMemoryAllocateInfoKHR
pattern VkExportMemoryAllocateInfoKHR vkSType vkPNext vkHandleTypes = VkExportMemoryAllocateInfo vkSType vkPNext vkHandleTypes
-- No documentation found for TopLevel "VK_QUEUE_FAMILY_EXTERNAL_KHR"
pattern VK_QUEUE_FAMILY_EXTERNAL_KHR :: Word32
pattern VK_QUEUE_FAMILY_EXTERNAL_KHR = VK_QUEUE_FAMILY_EXTERNAL
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR = VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
-- No documentation found for TopLevel "VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR"
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR :: VkResult
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR = VK_ERROR_INVALID_EXTERNAL_HANDLE
