{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_dedicated_allocation"
module Vulkan.Extensions.VK_KHR_dedicated_allocation  ( pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR
                                                      , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR
                                                      , MemoryDedicatedRequirementsKHR
                                                      , MemoryDedicatedAllocateInfoKHR
                                                      , KHR_DEDICATED_ALLOCATION_SPEC_VERSION
                                                      , pattern KHR_DEDICATED_ALLOCATION_SPEC_VERSION
                                                      , KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
                                                      , pattern KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
                                                      ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedAllocateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedRequirements)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR"
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR = STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR = STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO


-- No documentation found for TopLevel "VkMemoryDedicatedRequirementsKHR"
type MemoryDedicatedRequirementsKHR = MemoryDedicatedRequirements


-- No documentation found for TopLevel "VkMemoryDedicatedAllocateInfoKHR"
type MemoryDedicatedAllocateInfoKHR = MemoryDedicatedAllocateInfo


type KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION"
pattern KHR_DEDICATED_ALLOCATION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3


type KHR_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_KHR_dedicated_allocation"

-- No documentation found for TopLevel "VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME"
pattern KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_KHR_dedicated_allocation"

