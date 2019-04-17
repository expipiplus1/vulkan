{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
  ( withCStructDedicatedAllocationBufferCreateInfoNV
  , fromCStructDedicatedAllocationBufferCreateInfoNV
  , DedicatedAllocationBufferCreateInfoNV(..)
  , withCStructDedicatedAllocationImageCreateInfoNV
  , fromCStructDedicatedAllocationImageCreateInfoNV
  , DedicatedAllocationImageCreateInfoNV(..)
  , withCStructDedicatedAllocationMemoryAllocateInfoNV
  , fromCStructDedicatedAllocationMemoryAllocateInfoNV
  , DedicatedAllocationMemoryAllocateInfoNV(..)
  , pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
  )


-- No documentation found for TopLevel "DedicatedAllocationBufferCreateInfoNV"
data DedicatedAllocationBufferCreateInfoNV = DedicatedAllocationBufferCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "DedicatedAllocationBufferCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DedicatedAllocationBufferCreateInfoNV" "dedicatedAllocation"
  vkDedicatedAllocation :: Bool
  }
  deriving (Show, Eq)
withCStructDedicatedAllocationBufferCreateInfoNV :: DedicatedAllocationBufferCreateInfoNV -> (VkDedicatedAllocationBufferCreateInfoNV -> IO a) -> IO a
withCStructDedicatedAllocationBufferCreateInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: DedicatedAllocationBufferCreateInfoNV)) (\pPNext -> cont (VkDedicatedAllocationBufferCreateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV pPNext (boolToBool32 (vkDedicatedAllocation (from :: DedicatedAllocationBufferCreateInfoNV)))))
fromCStructDedicatedAllocationBufferCreateInfoNV :: VkDedicatedAllocationBufferCreateInfoNV -> IO DedicatedAllocationBufferCreateInfoNV
fromCStructDedicatedAllocationBufferCreateInfoNV c = DedicatedAllocationBufferCreateInfoNV <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDedicatedAllocationBufferCreateInfoNV)))
                                                                                           <*> pure (bool32ToBool (vkDedicatedAllocation (c :: VkDedicatedAllocationBufferCreateInfoNV)))
instance Zero DedicatedAllocationBufferCreateInfoNV where
  zero = DedicatedAllocationBufferCreateInfoNV Nothing
                                               False
-- No documentation found for TopLevel "DedicatedAllocationImageCreateInfoNV"
data DedicatedAllocationImageCreateInfoNV = DedicatedAllocationImageCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "DedicatedAllocationImageCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DedicatedAllocationImageCreateInfoNV" "dedicatedAllocation"
  vkDedicatedAllocation :: Bool
  }
  deriving (Show, Eq)
withCStructDedicatedAllocationImageCreateInfoNV :: DedicatedAllocationImageCreateInfoNV -> (VkDedicatedAllocationImageCreateInfoNV -> IO a) -> IO a
withCStructDedicatedAllocationImageCreateInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: DedicatedAllocationImageCreateInfoNV)) (\pPNext -> cont (VkDedicatedAllocationImageCreateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV pPNext (boolToBool32 (vkDedicatedAllocation (from :: DedicatedAllocationImageCreateInfoNV)))))
fromCStructDedicatedAllocationImageCreateInfoNV :: VkDedicatedAllocationImageCreateInfoNV -> IO DedicatedAllocationImageCreateInfoNV
fromCStructDedicatedAllocationImageCreateInfoNV c = DedicatedAllocationImageCreateInfoNV <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDedicatedAllocationImageCreateInfoNV)))
                                                                                         <*> pure (bool32ToBool (vkDedicatedAllocation (c :: VkDedicatedAllocationImageCreateInfoNV)))
instance Zero DedicatedAllocationImageCreateInfoNV where
  zero = DedicatedAllocationImageCreateInfoNV Nothing
                                              False
-- No documentation found for TopLevel "DedicatedAllocationMemoryAllocateInfoNV"
data DedicatedAllocationMemoryAllocateInfoNV = DedicatedAllocationMemoryAllocateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "DedicatedAllocationMemoryAllocateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DedicatedAllocationMemoryAllocateInfoNV" "image"
  vkImage :: Image
  , -- No documentation found for Nested "DedicatedAllocationMemoryAllocateInfoNV" "buffer"
  vkBuffer :: Buffer
  }
  deriving (Show, Eq)
withCStructDedicatedAllocationMemoryAllocateInfoNV :: DedicatedAllocationMemoryAllocateInfoNV -> (VkDedicatedAllocationMemoryAllocateInfoNV -> IO a) -> IO a
withCStructDedicatedAllocationMemoryAllocateInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: DedicatedAllocationMemoryAllocateInfoNV)) (\pPNext -> cont (VkDedicatedAllocationMemoryAllocateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV pPNext (vkImage (from :: DedicatedAllocationMemoryAllocateInfoNV)) (vkBuffer (from :: DedicatedAllocationMemoryAllocateInfoNV))))
fromCStructDedicatedAllocationMemoryAllocateInfoNV :: VkDedicatedAllocationMemoryAllocateInfoNV -> IO DedicatedAllocationMemoryAllocateInfoNV
fromCStructDedicatedAllocationMemoryAllocateInfoNV c = DedicatedAllocationMemoryAllocateInfoNV <$> -- Univalued Member elided
                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDedicatedAllocationMemoryAllocateInfoNV)))
                                                                                               <*> pure (vkImage (c :: VkDedicatedAllocationMemoryAllocateInfoNV))
                                                                                               <*> pure (vkBuffer (c :: VkDedicatedAllocationMemoryAllocateInfoNV))
instance Zero DedicatedAllocationMemoryAllocateInfoNV where
  zero = DedicatedAllocationMemoryAllocateInfoNV Nothing
                                                 zero
                                                 zero
