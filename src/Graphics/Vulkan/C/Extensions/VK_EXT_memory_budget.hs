{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget
  ( VkPhysicalDeviceMemoryBudgetPropertiesEXT(..)
  , pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME
  , pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VK_MAX_MEMORY_HEAPS
  , VkDeviceSize
  )


-- | VkPhysicalDeviceMemoryBudgetPropertiesEXT - Structure specifying
-- physical device memory budget and usage
--
-- = Description
--
-- The values returned in this structure are not invariant. The
-- @heapBudget@ and @heapUsage@ values /must/ be zero for array elements
-- greater than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'::@memoryHeapCount@.
-- The @heapBudget@ value /must/ be non-zero for array elements less than
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'::@memoryHeapCount@.
-- The @heapBudget@ value /must/ be less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeap'::@size@ for
-- each heap.
--
-- Unresolved directive in VkPhysicalDeviceMemoryBudgetPropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceMemoryBudgetPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceMemoryBudgetPropertiesEXT = VkPhysicalDeviceMemoryBudgetPropertiesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @heapBudget@ is an array of memory budgets, with one element for each
  -- memory heap. A heap’s budget is a rough estimate of how much memory the
  -- process /can/ allocate from that heap before allocations /may/ fail or
  -- cause performance degradation. The budget includes any currently
  -- allocated device memory.
  vkHeapBudget :: Vector VK_MAX_MEMORY_HEAPS VkDeviceSize
  , -- | @heapUsage@ is an array of memory usage, with one element for each
  -- memory heap. A heap’s usage is an estimate of how much memory the
  -- process is currently using in that heap.
  vkHeapUsage :: Vector VK_MAX_MEMORY_HEAPS VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMemoryBudgetPropertiesEXT where
  sizeOf ~_ = 272
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryBudgetPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
                                                       <*> peek (ptr `plusPtr` 144)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkHeapBudget (poked :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))
                *> poke (ptr `plusPtr` 144) (vkHeapUsage (poked :: VkPhysicalDeviceMemoryBudgetPropertiesEXT))

instance Zero VkPhysicalDeviceMemoryBudgetPropertiesEXT where
  zero = VkPhysicalDeviceMemoryBudgetPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
                                                   zero
                                                   zero
                                                   zero

-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_EXTENSION_NAME"
pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_MEMORY_BUDGET_EXTENSION_NAME = "VK_EXT_memory_budget"

-- No documentation found for TopLevel "VK_EXT_MEMORY_BUDGET_SPEC_VERSION"
pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION :: Integral a => a
pattern VK_EXT_MEMORY_BUDGET_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT = VkStructureType 1000237000
