{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory
  ( ExportMemoryAllocateInfoKHR
  , ExternalMemoryBufferCreateInfoKHR
  , ExternalMemoryImageCreateInfoKHR
  , pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR
  , pattern VK_ERROR_INVALID_EXTERNAL_HANDLE
  , pattern VK_QUEUE_FAMILY_EXTERNAL_KHR
  ) where




import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory
  ( ExportMemoryAllocateInfo(..)
  , ExternalMemoryBufferCreateInfo(..)
  , ExternalMemoryImageCreateInfo(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory
  ( pattern VK_ERROR_INVALID_EXTERNAL_HANDLE
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory
  ( pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR
  , pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION
  , pattern VK_QUEUE_FAMILY_EXTERNAL_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR
  )


type ExportMemoryAllocateInfoKHR = ExportMemoryAllocateInfo
-- TODO: Pattern constructor alias)

type ExternalMemoryBufferCreateInfoKHR = ExternalMemoryBufferCreateInfo
-- TODO: Pattern constructor alias)

type ExternalMemoryImageCreateInfoKHR = ExternalMemoryImageCreateInfo
-- TODO: Pattern constructor alias)
