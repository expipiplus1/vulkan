{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_external_memory  ( pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR
                                                          , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR
                                                          , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR
                                                          , pattern ERROR_INVALID_EXTERNAL_HANDLE_KHR
                                                          , pattern QUEUE_FAMILY_EXTERNAL_KHR
                                                          , ExternalMemoryImageCreateInfoKHR
                                                          , ExternalMemoryBufferCreateInfoKHR
                                                          , ExportMemoryAllocateInfoKHR
                                                          , KHR_EXTERNAL_MEMORY_SPEC_VERSION
                                                          , pattern KHR_EXTERNAL_MEMORY_SPEC_VERSION
                                                          , KHR_EXTERNAL_MEMORY_EXTENSION_NAME
                                                          , pattern KHR_EXTERNAL_MEMORY_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExportMemoryAllocateInfo)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryBufferCreateInfo)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryImageCreateInfo)
import Graphics.Vulkan.Core10.Enums.Result (Result(ERROR_INVALID_EXTERNAL_HANDLE))
import Graphics.Vulkan.Core10.APIConstants (pattern QUEUE_FAMILY_EXTERNAL)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR = STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR = STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR = STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO


-- No documentation found for TopLevel "VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR"
pattern ERROR_INVALID_EXTERNAL_HANDLE_KHR = ERROR_INVALID_EXTERNAL_HANDLE


-- No documentation found for TopLevel "VK_QUEUE_FAMILY_EXTERNAL_KHR"
pattern QUEUE_FAMILY_EXTERNAL_KHR = QUEUE_FAMILY_EXTERNAL


-- No documentation found for TopLevel "VkExternalMemoryImageCreateInfoKHR"
type ExternalMemoryImageCreateInfoKHR = ExternalMemoryImageCreateInfo


-- No documentation found for TopLevel "VkExternalMemoryBufferCreateInfoKHR"
type ExternalMemoryBufferCreateInfoKHR = ExternalMemoryBufferCreateInfo


-- No documentation found for TopLevel "VkExportMemoryAllocateInfoKHR"
type ExportMemoryAllocateInfoKHR = ExportMemoryAllocateInfo


type KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1


type KHR_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_KHR_external_memory"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_KHR_external_memory"

