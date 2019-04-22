{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_memory
  ( ExportMemoryAllocateInfoKHR
  , ExternalMemoryBufferCreateInfoKHR
  , ExternalMemoryImageCreateInfoKHR
  , pattern ERROR_INVALID_EXTERNAL_HANDLE_KHR
  , pattern KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  , pattern KHR_EXTERNAL_MEMORY_SPEC_VERSION
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern ERROR_INVALID_EXTERNAL_HANDLE
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory
  ( pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_INVALID_EXTERNAL_HANDLE
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory
  ( ExportMemoryAllocateInfo(..)
  , ExternalMemoryBufferCreateInfo(..)
  , ExternalMemoryImageCreateInfo(..)
  )


type ExportMemoryAllocateInfoKHR = ExportMemoryAllocateInfo
-- TODO: Pattern constructor alias)

type ExternalMemoryBufferCreateInfoKHR = ExternalMemoryBufferCreateInfo
-- TODO: Pattern constructor alias)

type ExternalMemoryImageCreateInfoKHR = ExternalMemoryImageCreateInfo
-- TODO: Pattern constructor alias)

-- No documentation found for TopLevel "ERROR_INVALID_EXTERNAL_HANDLE_KHR"
pattern ERROR_INVALID_EXTERNAL_HANDLE_KHR :: VkResult
pattern ERROR_INVALID_EXTERNAL_HANDLE_KHR = ERROR_INVALID_EXTERNAL_HANDLE

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_EXTENSION_NAME = VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_MEMORY_SPEC_VERSION = VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR = STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR = STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR = STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
