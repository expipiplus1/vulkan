{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_dedicated_allocation
  ( MemoryDedicatedAllocateInfoKHR
  , MemoryDedicatedRequirementsKHR
  , pattern KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern KHR_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_dedicated_allocation
  ( pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( MemoryDedicatedAllocateInfo(..)
  , MemoryDedicatedRequirements(..)
  )


type MemoryDedicatedAllocateInfoKHR = MemoryDedicatedAllocateInfo
-- TODO: Pattern constructor alias)

type MemoryDedicatedRequirementsKHR = MemoryDedicatedRequirements
-- TODO: Pattern constructor alias)

-- No documentation found for TopLevel "VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME"
pattern KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DEDICATED_ALLOCATION_EXTENSION_NAME = VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION"
pattern KHR_DEDICATED_ALLOCATION_SPEC_VERSION :: Integral a => a
pattern KHR_DEDICATED_ALLOCATION_SPEC_VERSION = VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR = STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR"
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR :: VkStructureType
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR = STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
