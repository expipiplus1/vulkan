{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_dedicated_allocation
  ( VkMemoryDedicatedAllocateInfoKHR
  , pattern VkMemoryDedicatedAllocateInfoKHR
  , VkMemoryDedicatedRequirementsKHR
  , pattern VkMemoryDedicatedRequirementsKHR
  , pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( VkMemoryDedicatedAllocateInfo(..)
  , VkMemoryDedicatedRequirements(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkMemoryDedicatedAllocateInfoKHR"
type VkMemoryDedicatedAllocateInfoKHR = VkMemoryDedicatedAllocateInfo


-- No documentation found for TopLevel "VkMemoryDedicatedAllocateInfoKHR"
pattern VkMemoryDedicatedAllocateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("image" ::: VkImage) -> ("buffer" ::: VkBuffer) -> VkMemoryDedicatedAllocateInfoKHR
pattern VkMemoryDedicatedAllocateInfoKHR vkSType vkPNext vkImage vkBuffer = VkMemoryDedicatedAllocateInfo vkSType vkPNext vkImage vkBuffer
-- No documentation found for TopLevel "VkMemoryDedicatedRequirementsKHR"
type VkMemoryDedicatedRequirementsKHR = VkMemoryDedicatedRequirements


-- No documentation found for TopLevel "VkMemoryDedicatedRequirementsKHR"
pattern VkMemoryDedicatedRequirementsKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("prefersDedicatedAllocation" ::: VkBool32) -> ("requiresDedicatedAllocation" ::: VkBool32) -> VkMemoryDedicatedRequirementsKHR
pattern VkMemoryDedicatedRequirementsKHR vkSType vkPNext vkPrefersDedicatedAllocation vkRequiresDedicatedAllocation = VkMemoryDedicatedRequirements vkSType vkPNext vkPrefersDedicatedAllocation vkRequiresDedicatedAllocation
-- No documentation found for TopLevel "VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME"
pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_KHR_dedicated_allocation"
-- No documentation found for TopLevel "VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION"
pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR = VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
