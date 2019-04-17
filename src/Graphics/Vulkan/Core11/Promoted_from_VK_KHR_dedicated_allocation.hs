{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( withCStructMemoryDedicatedAllocateInfo
  , fromCStructMemoryDedicatedAllocateInfo
  , MemoryDedicatedAllocateInfo(..)
  , withCStructMemoryDedicatedRequirements
  , fromCStructMemoryDedicatedRequirements
  , MemoryDedicatedRequirements(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( VkMemoryDedicatedAllocateInfo(..)
  , VkMemoryDedicatedRequirements(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
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


-- No documentation found for TopLevel "MemoryDedicatedAllocateInfo"
data MemoryDedicatedAllocateInfo = MemoryDedicatedAllocateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryDedicatedAllocateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryDedicatedAllocateInfo" "image"
  vkImage :: Image
  , -- No documentation found for Nested "MemoryDedicatedAllocateInfo" "buffer"
  vkBuffer :: Buffer
  }
  deriving (Show, Eq)
withCStructMemoryDedicatedAllocateInfo :: MemoryDedicatedAllocateInfo -> (VkMemoryDedicatedAllocateInfo -> IO a) -> IO a
withCStructMemoryDedicatedAllocateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryDedicatedAllocateInfo)) (\pPNext -> cont (VkMemoryDedicatedAllocateInfo VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO pPNext (vkImage (from :: MemoryDedicatedAllocateInfo)) (vkBuffer (from :: MemoryDedicatedAllocateInfo))))
fromCStructMemoryDedicatedAllocateInfo :: VkMemoryDedicatedAllocateInfo -> IO MemoryDedicatedAllocateInfo
fromCStructMemoryDedicatedAllocateInfo c = MemoryDedicatedAllocateInfo <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryDedicatedAllocateInfo)))
                                                                       <*> pure (vkImage (c :: VkMemoryDedicatedAllocateInfo))
                                                                       <*> pure (vkBuffer (c :: VkMemoryDedicatedAllocateInfo))
instance Zero MemoryDedicatedAllocateInfo where
  zero = MemoryDedicatedAllocateInfo Nothing
                                     zero
                                     zero
-- No documentation found for TopLevel "MemoryDedicatedRequirements"
data MemoryDedicatedRequirements = MemoryDedicatedRequirements
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryDedicatedRequirements" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryDedicatedRequirements" "prefersDedicatedAllocation"
  vkPrefersDedicatedAllocation :: Bool
  , -- No documentation found for Nested "MemoryDedicatedRequirements" "requiresDedicatedAllocation"
  vkRequiresDedicatedAllocation :: Bool
  }
  deriving (Show, Eq)
withCStructMemoryDedicatedRequirements :: MemoryDedicatedRequirements -> (VkMemoryDedicatedRequirements -> IO a) -> IO a
withCStructMemoryDedicatedRequirements from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryDedicatedRequirements)) (\pPNext -> cont (VkMemoryDedicatedRequirements VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS pPNext (boolToBool32 (vkPrefersDedicatedAllocation (from :: MemoryDedicatedRequirements))) (boolToBool32 (vkRequiresDedicatedAllocation (from :: MemoryDedicatedRequirements)))))
fromCStructMemoryDedicatedRequirements :: VkMemoryDedicatedRequirements -> IO MemoryDedicatedRequirements
fromCStructMemoryDedicatedRequirements c = MemoryDedicatedRequirements <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryDedicatedRequirements)))
                                                                       <*> pure (bool32ToBool (vkPrefersDedicatedAllocation (c :: VkMemoryDedicatedRequirements)))
                                                                       <*> pure (bool32ToBool (vkRequiresDedicatedAllocation (c :: VkMemoryDedicatedRequirements)))
instance Zero MemoryDedicatedRequirements where
  zero = MemoryDedicatedRequirements Nothing
                                     False
                                     False
