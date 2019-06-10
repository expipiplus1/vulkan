{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DedicatedAllocationBufferCreateInfoNV(..)
  , 
  DedicatedAllocationImageCreateInfoNV(..)
  , DedicatedAllocationMemoryAllocateInfoNV(..)
#endif
  , pattern NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern NV_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
  )

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
  ( pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDedicatedAllocationBufferCreateInfoNV"
data DedicatedAllocationBufferCreateInfoNV = DedicatedAllocationBufferCreateInfoNV
  { -- No documentation found for Nested "DedicatedAllocationBufferCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DedicatedAllocationBufferCreateInfoNV" "dedicatedAllocation"
  dedicatedAllocation :: Bool
  }
  deriving (Show, Eq)

instance Zero DedicatedAllocationBufferCreateInfoNV where
  zero = DedicatedAllocationBufferCreateInfoNV Nothing
                                               False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDedicatedAllocationImageCreateInfoNV"
data DedicatedAllocationImageCreateInfoNV = DedicatedAllocationImageCreateInfoNV
  { -- No documentation found for Nested "DedicatedAllocationImageCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DedicatedAllocationImageCreateInfoNV" "dedicatedAllocation"
  dedicatedAllocation :: Bool
  }
  deriving (Show, Eq)

instance Zero DedicatedAllocationImageCreateInfoNV where
  zero = DedicatedAllocationImageCreateInfoNV Nothing
                                              False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDedicatedAllocationMemoryAllocateInfoNV"
data DedicatedAllocationMemoryAllocateInfoNV = DedicatedAllocationMemoryAllocateInfoNV
  { -- No documentation found for Nested "DedicatedAllocationMemoryAllocateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DedicatedAllocationMemoryAllocateInfoNV" "image"
  image :: Image
  , -- No documentation found for Nested "DedicatedAllocationMemoryAllocateInfoNV" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

instance Zero DedicatedAllocationMemoryAllocateInfoNV where
  zero = DedicatedAllocationMemoryAllocateInfoNV Nothing
                                                 zero
                                                 zero

#endif

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME"
pattern NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_DEDICATED_ALLOCATION_EXTENSION_NAME = VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION"
pattern NV_DEDICATED_ALLOCATION_SPEC_VERSION :: Integral a => a
pattern NV_DEDICATED_ALLOCATION_SPEC_VERSION = VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
