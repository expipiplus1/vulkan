{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_dedicated_allocation
  ( MemoryDedicatedAllocateInfoKHR
  , MemoryDedicatedRequirementsKHR
  , pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  ) where




import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( MemoryDedicatedAllocateInfo(..)
  , MemoryDedicatedRequirements(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_dedicated_allocation
  ( pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR
  )


type MemoryDedicatedAllocateInfoKHR = MemoryDedicatedAllocateInfo
-- TODO: Pattern constructor alias)
type MemoryDedicatedRequirementsKHR = MemoryDedicatedRequirements
-- TODO: Pattern constructor alias)
