{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  MemoryDedicatedAllocateInfo(..)
  , 
  MemoryDedicatedRequirements(..)
#endif
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  ) where





#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryDedicatedAllocateInfo"
data MemoryDedicatedAllocateInfo = MemoryDedicatedAllocateInfo
  { -- No documentation found for Nested "MemoryDedicatedAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryDedicatedAllocateInfo" "image"
  image :: Image
  , -- No documentation found for Nested "MemoryDedicatedAllocateInfo" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

instance Zero MemoryDedicatedAllocateInfo where
  zero = MemoryDedicatedAllocateInfo Nothing
                                     zero
                                     zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryDedicatedRequirements"
data MemoryDedicatedRequirements = MemoryDedicatedRequirements
  { -- No documentation found for Nested "MemoryDedicatedRequirements" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryDedicatedRequirements" "prefersDedicatedAllocation"
  prefersDedicatedAllocation :: Bool
  , -- No documentation found for Nested "MemoryDedicatedRequirements" "requiresDedicatedAllocation"
  requiresDedicatedAllocation :: Bool
  }
  deriving (Show, Eq)

instance Zero MemoryDedicatedRequirements where
  zero = MemoryDedicatedRequirements Nothing
                                     False
                                     False

#endif
