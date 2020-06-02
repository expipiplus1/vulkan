{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_get_memory_requirements2  ( pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR
                                                          , pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR
                                                          , pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
                                                          , pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR
                                                          , pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR
                                                          , getBufferMemoryRequirements2KHR
                                                          , getImageMemoryRequirements2KHR
                                                          , getImageSparseMemoryRequirements2KHR
                                                          , BufferMemoryRequirementsInfo2KHR
                                                          , ImageMemoryRequirementsInfo2KHR
                                                          , ImageSparseMemoryRequirementsInfo2KHR
                                                          , MemoryRequirements2KHR
                                                          , SparseImageMemoryRequirements2KHR
                                                          , KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION
                                                          , pattern KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION
                                                          , KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
                                                          , pattern KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (getBufferMemoryRequirements2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (getImageMemoryRequirements2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (getImageSparseMemoryRequirements2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (BufferMemoryRequirementsInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageMemoryRequirementsInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageSparseMemoryRequirementsInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (SparseImageMemoryRequirements2)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR"
pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR = STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR = STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR = STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR"
pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR = STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR"
pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR = STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2


-- No documentation found for TopLevel "vkGetBufferMemoryRequirements2KHR"
getBufferMemoryRequirements2KHR = getBufferMemoryRequirements2


-- No documentation found for TopLevel "vkGetImageMemoryRequirements2KHR"
getImageMemoryRequirements2KHR = getImageMemoryRequirements2


-- No documentation found for TopLevel "vkGetImageSparseMemoryRequirements2KHR"
getImageSparseMemoryRequirements2KHR = getImageSparseMemoryRequirements2


-- No documentation found for TopLevel "VkBufferMemoryRequirementsInfo2KHR"
type BufferMemoryRequirementsInfo2KHR = BufferMemoryRequirementsInfo2


-- No documentation found for TopLevel "VkImageMemoryRequirementsInfo2KHR"
type ImageMemoryRequirementsInfo2KHR = ImageMemoryRequirementsInfo2


-- No documentation found for TopLevel "VkImageSparseMemoryRequirementsInfo2KHR"
type ImageSparseMemoryRequirementsInfo2KHR = ImageSparseMemoryRequirementsInfo2


-- No documentation found for TopLevel "VkMemoryRequirements2KHR"
type MemoryRequirements2KHR = MemoryRequirements2


-- No documentation found for TopLevel "VkSparseImageMemoryRequirements2KHR"
type SparseImageMemoryRequirements2KHR = SparseImageMemoryRequirements2


type KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION"
pattern KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1


type KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME = "VK_KHR_get_memory_requirements2"

-- No documentation found for TopLevel "VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME"
pattern KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME = "VK_KHR_get_memory_requirements2"

