{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_bind_memory2  ( pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR
                                              , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR
                                              , pattern IMAGE_CREATE_ALIAS_BIT_KHR
                                              , bindBufferMemory2KHR
                                              , bindImageMemory2KHR
                                              , BindBufferMemoryInfoKHR
                                              , BindImageMemoryInfoKHR
                                              , KHR_BIND_MEMORY_2_SPEC_VERSION
                                              , pattern KHR_BIND_MEMORY_2_SPEC_VERSION
                                              , KHR_BIND_MEMORY_2_EXTENSION_NAME
                                              , pattern KHR_BIND_MEMORY_2_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (bindBufferMemory2)
import Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (bindImageMemory2)
import Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindBufferMemoryInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindImageMemoryInfo)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_ALIAS_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR = STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR = STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO


-- No documentation found for TopLevel "VK_IMAGE_CREATE_ALIAS_BIT_KHR"
pattern IMAGE_CREATE_ALIAS_BIT_KHR = IMAGE_CREATE_ALIAS_BIT


-- No documentation found for TopLevel "vkBindBufferMemory2KHR"
bindBufferMemory2KHR = bindBufferMemory2


-- No documentation found for TopLevel "vkBindImageMemory2KHR"
bindImageMemory2KHR = bindImageMemory2


-- No documentation found for TopLevel "VkBindBufferMemoryInfoKHR"
type BindBufferMemoryInfoKHR = BindBufferMemoryInfo


-- No documentation found for TopLevel "VkBindImageMemoryInfoKHR"
type BindImageMemoryInfoKHR = BindImageMemoryInfo


type KHR_BIND_MEMORY_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_BIND_MEMORY_2_SPEC_VERSION"
pattern KHR_BIND_MEMORY_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_BIND_MEMORY_2_SPEC_VERSION = 1


type KHR_BIND_MEMORY_2_EXTENSION_NAME = "VK_KHR_bind_memory2"

-- No documentation found for TopLevel "VK_KHR_BIND_MEMORY_2_EXTENSION_NAME"
pattern KHR_BIND_MEMORY_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_BIND_MEMORY_2_EXTENSION_NAME = "VK_KHR_bind_memory2"

